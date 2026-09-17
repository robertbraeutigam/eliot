package com.vanillasource.eliot.eliotc.lsp.server

import cats.effect.IO
import cats.effect.std.Semaphore
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.lsp.index.MainIndex
import com.vanillasource.eliot.eliotc.lsp.buildtool.ProjectModelQuery
import com.vanillasource.eliot.eliotc.lsp.virtual.{VfsUris, VirtualFileSystem}
import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import org.eclipse.lsp4j.{
  Diagnostic,
  DidChangeWatchedFilesRegistrationOptions,
  FileSystemWatcher,
  MessageParams,
  MessageType,
  PublishDiagnosticsParams,
  RelativePattern,
  Registration,
  RegistrationParams
}
import org.eclipse.lsp4j.services.LanguageClient

import java.net.URI
import java.nio.file.{Path, Paths}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.jdk.CollectionConverters.*

/** Bridges the cats-effect resident compile engine to the (Java, `CompletableFuture`-based, thread-driven) lsp4j world.
  *
  * The workspace is compiled as **one [[PackageSession]] per package**, planned by [[WorkspacePlan]]: a folder that is
  * a build-tool project is asked for its model (`./eliotw --project-model`) and each of its packages compiles exactly
  * the roots its build would; any other folder gets one session over guessed roots. A request about a file is answered
  * by the sessions that have the file on their path, the package owning it first ([[sessionsFor]]), and a file's
  * diagnostics are those of every session checking it, merged.
  *
  * The lifecycle is: [[startWorkspace]] plans and starts the sessions in the background, [[requestCompile]] coalesces
  * edit/save/file-watch triggers into recompiles of the sessions a file concerns, a change to a descriptor replans
  * ([[reloadWorkspace]]), and [[shutdown]] releases everything. Starting, replanning and shutting down are serialised,
  * so a replan triggered mid-start waits for the start to finish.
  */
final class EliotCompilationService(runtime: IORuntime) extends Logging {
  private val clientRef    = new AtomicReference[Option[LanguageClient]](None)
  private val sessionsRef  = new AtomicReference[Seq[PackageSession]](Seq.empty)
  private val publishedRef = new AtomicReference[Map[String, Seq[Diagnostic]]](Map.empty)
  private val rootsRef     = new AtomicReference[Seq[Path]](Seq.empty)
  private val codeLensPush = new AtomicBoolean(false)
  private val vfs          = new VirtualFileSystem
  private val lifecycle    = Semaphore[IO](1).unsafeRunSync()(using runtime)

  /** The overlay of unsaved editor buffers. The document service writes live edits here (on open/change/close) before
    * triggering a recompile; the compile's source readers consult it ahead of the on-disk files.
    */
  def virtualFileSystem: VirtualFileSystem = vfs

  /** Remember the remote client so finished compiles can push diagnostics to it. */
  def connect(client: LanguageClient): Unit = clientRef.set(Some(client))

  /** Ask the editor to watch `.els` files and the build tool's descriptor and lockfile across the workspace, and notify
    * us of on-disk changes via `workspace/didChangeWatchedFiles` (handled by
    * [[EliotWorkspaceService.didChangeWatchedFiles]]). Per the LSP spec this notification is *registration-only* — a
    * client sends it only for globs the server registers — so without this the wired handler never fires. The caller
    * gates on the client's dynamic-registration capability; this is a no-op if no client is connected.
    */
  def registerFileWatchers(): Unit =
    clientRef.get.foreach { client =>
      val watchers     = EliotCompilationService.watchedGlobs.map(glob =>
        new FileSystemWatcher(JEither.forLeft[String, RelativePattern](glob))
      )
      val options      = new DidChangeWatchedFilesRegistrationOptions(watchers.asJava)
      val registration = new Registration("eliot-watched-files", "workspace/didChangeWatchedFiles", options)
      val _            = client.registerCapability(new RegistrationParams(List(registration).asJava))
    }

  /** Enable pushing `workspace/codeLens/refresh` after every finished compile. Code lenses are *pulled* by the client
    * (`textDocument/codeLens`), but the [[MainIndex]] backing them is only ready once the asynchronous, coalescing
    * recompile finishes — after the client already answered its post-edit pull from the previous (stale) index. Without
    * a refresh nudge the "Run main" lens stays whatever the racing pull saw: fixing an error and reverting it leaves
    * the lens gone until the file is reopened. This tells the client to re-pull once the fresh index is in place. Gated
    * on the client's `workspace.codeLens.refreshSupport` capability (a no-op otherwise), mirroring
    * [[registerFileWatchers]].
    */
  def enableCodeLensRefresh(): Unit = codeLensPush.set(true)

  /** The indices of every session with `uri` on its path, the session owning it first, then those checking it, then
    * those only mounting it; every session when none has it. A request takes the first non-empty answer, so a file is
    * answered as its own package sees it wherever that package can say anything at all.
    */
  def indicesFor(uri: URI): Seq[PackageSession.Indices] = sessionsFor(uri).map(_.indices)

  /** What the "Run main" lens of `uri` runs, if the document declares a `main`: the session whose closure
    * monomorphized that `main` — the owning package first — or, when none did, the first that knows of it, so a broken
    * `main` still offers a run that reports why. The root holding the file is the build root; the session's other
    * roots are the dependencies the build must put on the path.
    */
  def runTargetFor(uri: URI): Option[EliotCompilationService.RunTarget] = {
    val declaring = sessionsFor(uri).flatMap(session => session.indices.main.mainAt(uri).map(session -> _))
    for {
      (session, entry) <- declaring.find(_._2.monomorphized).orElse(declaring.headOption)
      file             <- fileOf(uri)
      root             <- session.plan.roots.filter(file.startsWith).maxByOption(_.getNameCount)
    } yield EliotCompilationService.RunTarget(root, entry, session.plan.roots.filterNot(_ == root))
  }

  /** Plan the sessions over the editor's workspace folders and start them, in the background: asking a build tool for
    * its model may take a while (its first run fetches the pinned launcher), and requests in the meantime are answered
    * from empty indices.
    */
  def startWorkspace(workspaceRoots: Seq[Path]): Unit = {
    rootsRef.set(workspaceRoots)
    reloadWorkspace()
  }

  /** Replan and restart every session — what a change to a descriptor or lockfile asks for. Unsaved buffers survive,
    * since they live in the shared [[virtualFileSystem]].
    */
  def reloadWorkspace(): Unit = lifecycle.permit.use(_ => restart).unsafeRunAndForget()(using runtime)

  /** Request a recompile of every session. Non-blocking and coalescing (see
    * [[com.vanillasource.eliot.eliotc.compiler.CompilationServer.requestCompile]]).
    */
  def requestCompile(): Unit = sessionsRef.get.traverse_(_.requestCompile).unsafeRunAndForget()(using runtime)

  /** Request a recompile of the sessions `uri` concerns — those with it on their path, or all when none has. */
  def requestCompile(uri: URI): Unit = requestCompile(Seq(uri))

  /** Request a recompile of the sessions any of `uris` concerns, each once. */
  def requestCompile(uris: Seq[URI]): Unit =
    uris.flatMap(sessionsFor).distinct.traverse_(_.requestCompile).unsafeRunAndForget()(using runtime)

  /** Whether a changed file is one the workspace plan was read from: a descriptor or lockfile directly in a workspace
    * folder.
    */
  def isPlanInput(uri: URI): Boolean =
    fileOf(uri).exists(file =>
      EliotCompilationService.planInputs.contains(file.getFileName.toString) &&
        rootsRef.get.exists(root => Option(file.getParent).contains(root))
    )

  /** Release every session (cancelling any compile in flight) and flush their caches to disk. */
  def shutdown(): Unit = lifecycle.permit.use(_ => stopSessions).unsafeRunSync()(using runtime)

  private def restart: IO[Unit] =
    for {
      _        <- stopSessions
      plan     <- WorkspacePlan.of(rootsRef.get, EliotCompilationService.serverVersion, ProjectModelQuery.query)
      _        <- plan.warnings.traverse_(warnUser)
      _        <- info[IO](s"LSP compiling ${plan.sessions.size} package(s): ${plan.sessions.map(_.name).mkString(", ")}")
      sessions <- plan.sessions.traverse(PackageSession.start(_, vfs, _ => publishDiagnostics >> refreshCodeLenses))
      _        <- IO(sessionsRef.set(sessions))
      _        <- sessions.traverse_(_.requestCompile)
    } yield ()

  private def stopSessions: IO[Unit] = IO(sessionsRef.getAndSet(Seq.empty)).flatMap(_.traverse_(_.release))

  /** The sessions with `uri` on their path, in the order they answer for it (see [[indicesFor]]). */
  private def sessionsFor(uri: URI): Seq[PackageSession] = {
    val sessions = sessionsRef.get
    fileOf(uri) match {
      case None       => sessions
      case Some(file) =>
        val mounting = sessions.filter(_.plan.mounts(file))
        if (mounting.isEmpty) sessions
        else mounting.sortBy(session => if (session.plan.owns(file)) 0 else if (session.plan.checks(file)) 1 else 2)
    }
  }

  private def fileOf(uri: URI): Option[Path] =
    try Some(Paths.get(VfsUris.toFileUri(uri)))
    catch { case _: IllegalArgumentException | _: java.nio.file.FileSystemNotFoundException => None }

  /** Publish every session's diagnostics, a file's merged across the sessions checking it ([[EliotCompilationService.merged]]),
    * and clear the files that were reported last time but are clean now. Serialised, since sessions finish concurrently.
    */
  private def publishDiagnostics: IO[Unit] =
    clientRef.get match {
      case None         => IO.unit
      case Some(client) =>
        IO.blocking(synchronized {
          val merged     = EliotCompilationService.merged(sessionsRef.get.map(s => (s.diagnostics, s.indices.main)))
          val previously = publishedRef.getAndSet(merged)
          (previously.keySet diff merged.keySet).foreach(uri =>
            client.publishDiagnostics(new PublishDiagnosticsParams(uri, Seq.empty[Diagnostic].asJava))
          )
          merged.filter((uri, diagnostics) => !previously.get(uri).contains(diagnostics)).foreach((uri, diagnostics) =>
            client.publishDiagnostics(new PublishDiagnosticsParams(uri, diagnostics.asJava))
          )
        })
    }

  /** Tell the user something about the workspace plan: in the editor when a client is connected, and in the log. */
  private def warnUser(message: String): IO[Unit] =
    warn[IO](message) >> IO.blocking(
      clientRef.get.foreach(_.showMessage(new MessageParams(MessageType.Warning, s"Eliot: $message")))
    )

  /** Nudge the client to re-pull code lenses now that a compile's [[MainIndex]] is in place. Fire-and-forget: the
    * request's completion is irrelevant, and it is skipped entirely unless the client advertised refresh support (see
    * [[enableCodeLensRefresh]]).
    */
  private def refreshCodeLenses: IO[Unit] =
    clientRef.get match {
      case Some(client) if codeLensPush.get => IO.blocking(client.refreshCodeLenses()).void
      case _                                => IO.unit
    }
}

object EliotCompilationService {

  /** A runnable `main` and the roots to build it from: `root` holds the file, `dependencyRoots` are the rest of the
    * package's path.
    */
  final case class RunTarget(root: Path, entry: MainIndex.Entry, dependencyRoots: Seq[Path])

  /** The build tool's files a workspace plan is read from; a change to one replans. */
  val planInputs: Set[String] = Set(ProjectModelQuery.descriptorName, "eliot.lock")

  private val watchedGlobs: Seq[String] = "**/*.els" +: planInputs.toSeq.sorted.map(name => s"**/$name")

  /** This server's own eliot version, from its jar's manifest — absent when it runs from class directories. */
  private val serverVersion: Option[String] =
    Option(classOf[EliotCompilationService].getPackage).flatMap(pkg => Option(pkg.getImplementationVersion))

  /** Diagnostics by URI, merged across sessions: each file's union, without the duplicates two sessions checking the
    * same file report alike.
    *
    * **A `main` is judged by the packages that can run it.** Every session checks every `main` it has on its project
    * roots, and in a package whose closure has no platform that check fails — "no implementation of `Console`" — although
    * no build of that package ever runs the `main`. So where a file's `main` monomorphized in some session, the sessions
    * in which it did not are left out of that file's diagnostics. Where it monomorphized nowhere, every session's
    * diagnostics stand: then either nothing can run it, which is worth saying, or it is broken everywhere.
    */
  def merged(perSession: Seq[(Map[String, Seq[Diagnostic]], MainIndex)]): Map[String, Seq[Diagnostic]] =
    perSession
      .flatMap(_._1.keys)
      .distinct
      .map { uri =>
        val main       = (index: MainIndex) => mainAt(index, uri)
        val runnable   = perSession.exists((_, index) => main(index).exists(_.monomorphized))
        val judging    = perSession.filterNot((_, index) => runnable && main(index).exists(!_.monomorphized))
        val diagnostics = judging.flatMap(_._1.getOrElse(uri, Seq.empty))
        uri -> diagnostics.distinctBy(diagnostic => (diagnostic.getRange, diagnostic.getMessage))
      }
      .filter(_._2.nonEmpty)
      .toMap

  private def mainAt(index: MainIndex, uri: String): Option[MainIndex.Entry] =
    try index.mainAt(URI.create(uri))
    catch { case _: IllegalArgumentException => None }
}
