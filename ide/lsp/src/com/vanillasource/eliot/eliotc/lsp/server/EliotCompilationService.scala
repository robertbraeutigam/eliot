package com.vanillasource.eliot.eliotc.lsp.server

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.apidoc.fact.ValueDoc
import com.vanillasource.eliot.eliotc.apidoc.plugin.ApiDocPlugin
import com.vanillasource.eliot.eliotc.compiler.{CompilationResult, CompilationServer, CompilationSession, Compiler}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.lsp.index.{CompletionIndex, DocIndex, MainIndex, PositionIndex, TypeHintIndex}
import com.vanillasource.eliot.eliotc.lsp.plugin.LspPlugin
import com.vanillasource.eliot.eliotc.lsp.virtual.{VfsUris, VirtualFileSystem}
import com.vanillasource.eliot.eliotc.module.fact.ModuleValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.plugin.{Configuration, LangPlugin}
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibPlugin
import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import org.eclipse.lsp4j.{
  DidChangeWatchedFilesRegistrationOptions,
  FileSystemWatcher,
  PublishDiagnosticsParams,
  RelativePattern,
  Registration,
  RegistrationParams
}
import org.eclipse.lsp4j.services.LanguageClient

import java.net.URI
import java.nio.file.Path
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.jdk.CollectionConverters.*

/** Bridges the cats-effect resident compile engine ([[CompilationSession]] + [[CompilationServer]]) to the (Java,
  * `CompletableFuture`-based, thread-driven) lsp4j world.
  *
  * The lifecycle is: [[startWorkspace]] builds a session over the editor's workspace roots and starts the
  * cancel-restart server, [[requestCompile]] coalesces edit/save/file-watch triggers into recompiles, and each finished
  * compile publishes diagnostics through the connected [[LanguageClient]]. Effects are run on the supplied
  * [[IORuntime]] at the lsp4j boundary; the long-lived server `Resource` is held open via `allocated` and released on
  * [[shutdown]].
  */
final class EliotCompilationService(runtime: IORuntime) extends Logging {
  private val clientRef     = new AtomicReference[Option[LanguageClient]](None)
  private val serverRef     = new AtomicReference[Option[(CompilationServer, IO[Unit])]](None)
  private val publishedRef  = new AtomicReference[Set[String]](Set.empty)
  private val indexRef      = new AtomicReference[PositionIndex](PositionIndex.empty)
  private val completionRef = new AtomicReference[CompletionIndex](CompletionIndex.empty)
  private val typeHintRef   = new AtomicReference[TypeHintIndex](TypeHintIndex.empty)
  private val mainRef       = new AtomicReference[MainIndex](MainIndex.empty)
  private val docRef        = new AtomicReference[DocIndex](DocIndex.empty)
  private val rootsRef      = new AtomicReference[Seq[Path]](Seq.empty)
  private val codeLensPush  = new AtomicBoolean(false)
  private val vfs           = new VirtualFileSystem

  /** The overlay of unsaved editor buffers. The document service writes live edits here (on open/change/close) before
    * triggering a recompile; the compile's source readers consult it ahead of the on-disk files.
    */
  def virtualFileSystem: VirtualFileSystem = vfs

  /** Remember the remote client so finished compiles can push diagnostics to it. */
  def connect(client: LanguageClient): Unit = clientRef.set(Some(client))

  /** Ask the editor to watch `.els` files across the workspace and notify us of on-disk changes via
    * `workspace/didChangeWatchedFiles` (handled by [[EliotWorkspaceService.didChangeWatchedFiles]] →
    * [[requestCompile]]). Per the LSP spec this notification is *registration-only* — a client sends it only for globs
    * the server registers — so without this the wired handler never fires. The caller gates on the client's
    * dynamic-registration capability; this is a no-op if no client is connected.
    */
  def registerFileWatchers(): Unit =
    clientRef.get.foreach { client =>
      val watcher      = new FileSystemWatcher(JEither.forLeft[String, RelativePattern]("**/*.els"))
      val options      = new DidChangeWatchedFilesRegistrationOptions(List(watcher).asJava)
      val registration = new Registration("eliot-watched-els", "workspace/didChangeWatchedFiles", options)
      val _            = client.registerCapability(new RegistrationParams(List(registration).asJava))
    }

  /** Enable pushing `workspace/codeLens/refresh` after every finished compile. Code lenses are *pulled* by the client
    * (`textDocument/codeLens`), but the [[MainIndex]] backing them is only ready once the asynchronous, coalescing
    * recompile finishes — after the client already answered its post-edit pull from the previous (stale) index. Without a
    * refresh nudge the "Run main" lens stays whatever the racing pull saw: fixing an error and reverting it leaves the
    * lens gone until the file is reopened. This tells the client to re-pull once the fresh index is in place. Gated on the
    * client's `workspace.codeLens.refreshSupport` capability (a no-op otherwise), mirroring [[registerFileWatchers]].
    */
  def enableCodeLensRefresh(): Unit = codeLensPush.set(true)

  /** The reverse position index from the latest finished compile, for position-based features (definition, hover).
    * Empty until the first compile completes.
    */
  def positionIndex: PositionIndex = indexRef.get

  /** The in-scope-name index from the latest finished compile, for completion. Empty until the first compile completes.
    */
  def completionIndex: CompletionIndex = completionRef.get

  /** The position → concrete-type index from the latest finished compile, for hover type hints. Empty until the first
    * compile that monomorphized a `main` completes.
    */
  def typeHintIndex: TypeHintIndex = typeHintRef.get

  /** The document → runnable-`main` index from the latest finished compile, for the "Run main" code lens. Empty until
    * the first compile completes.
    */
  def mainIndex: MainIndex = mainRef.get

  /** The name → documentation index from the latest finished compile, for hover. Empty until the first compile
    * completes.
    */
  def docIndex: DocIndex = docRef.get

  /** The workspace source root that contains the given document, if any — the longest matching root prefix. This is the
    * root a `main`'s [[ModuleName]] was derived against, so it is the `<path>` the backend must be given (alongside
    * `-m <module>`) to locate and build that module the same way the resident compile did.
    */
  def sourceRootFor(uri: URI): Option[Path] =
    try {
      val file = Path.of(VfsUris.toFileUri(uri))
      rootsRef.get.filter(file.startsWith).maxByOption(_.getNameCount)
    } catch { case _: IllegalArgumentException | _: java.nio.file.FileSystemNotFoundException => None }

  /** Build a session over the workspace source roots, start the cancel-restart server, and trigger the first compile.
    *
    * Since CP1.5 the abstract base + platform layers are *not* discovered on this process's classpath; they are handed in
    * as filesystem roots via the `--compiler-path`/`--runtime-path` configuration keys, resolved from the bundled
    * `eliot.layers` staging directory ([[BundledLayers]]). Only the user's own roots come from the editor workspace; the
    * runtime path additionally gets them through `LangPlugin.pathKey`.
    */
  def startWorkspace(roots: Seq[Path]): Unit = {
    rootsRef.set(roots)
    val lspPlugin                     = LspPlugin(vfs)
    val (compilerPaths, runtimePaths) = BundledLayers.fromSystemProperty
    val configuration                 = Configuration()
      .set(Compiler.targetPathKey, roots.headOption.getOrElse(Path.of(".")).resolve(".eliot-lsp"))
      .set(LangPlugin.pathKey, roots)
      .set(LangPlugin.compilerPathKey, compilerPaths)
      .set(LangPlugin.runtimePathKey, runtimePaths)
    val started                       = (for {
      _       <- warn[IO](
                   s"No bundled layer sources found: system property '${BundledLayers.layersDirectoryProperty}' is unset, " +
                     "so the standard library and platform layers will not resolve."
                 ).whenA(runtimePaths.isEmpty)
      session <- CompilationSession.create(
                   lspPlugin,
                   Seq(lspPlugin, LangPlugin(), StdlibPlugin(), ApiDocPlugin()),
                   configuration,
                   roots.map(_.toString).toList
                 )
      handle  <- CompilationServer.start(session, publishResult).allocated
    } yield handle).unsafeRunSync()(using runtime)
    serverRef.set(Some(started))
    requestCompile()
  }

  /** Request a (re)compile. Non-blocking and coalescing (see [[CompilationServer.requestCompile]]). */
  def requestCompile(): Unit =
    serverRef.get.foreach((server, _) => server.requestCompile.unsafeRunAndForget()(using runtime))

  /** Release the server (cancelling any in-flight compile) and flush its cache to disk. */
  def shutdown(): Unit =
    serverRef.getAndSet(None).foreach((_, release) => release.unsafeRunSync()(using runtime))

  /** Rebuild the reverse position and completion indices from the result, then publish its diagnostics. The indices are
    * built off the request path (once per finished compile) so position-based and completion requests are answered
    * synchronously from memory.
    */
  private def publishResult(result: CompilationResult): IO[Unit] =
    rebuildIndices(result) >> publishDiagnostics(result) >> refreshCodeLenses

  /** Rebuild all indices from the facts this compile materialised: the [[PositionIndex]] from [[ResolvedValue]]s
    * (definition + reference sites), the [[CompletionIndex]] from [[ModuleValue]]s (in-scope dictionaries) plus those
    * same [[ResolvedValue]]s (signatures), the [[TypeHintIndex]] from [[MonomorphicValue]]s (per-node concrete types),
    * and the [[MainIndex]] from those same [[ResolvedValue]]s (documents declaring a runnable `main`). The
    * whole-workspace driver ([[LspPlugin]]) demands every name — so every workspace value's resolved form and module
    * dictionary are present — and additionally monomorphizes each file's own `main`, so the reachable monomorphic values
    * exist for hover type hints, and demands a [[ValueDoc]] per documentable name across every layer so the [[DocIndex]]
    * can show the same documentation the apidoc site would.
    */
  private def rebuildIndices(result: CompilationResult): IO[Unit] =
    result.generator.currentFacts().flatMap { facts =>
      val resolved     = facts.values.collect { case value: ResolvedValue => value }.toSeq
      val moduleValues = facts.values.collect { case value: ModuleValue => value }.toSeq
      val monomorphic  = facts.values.collect { case value: MonomorphicValue => value }.toSeq
      val valueDocs    = facts.values.collect { case value: ValueDoc => value }.toSeq
      IO {
        indexRef.set(PositionIndex.build(resolved))
        completionRef.set(CompletionIndex.build(moduleValues, resolved))
        typeHintRef.set(TypeHintIndex.build(monomorphic))
        mainRef.set(MainIndex.build(resolved))
        docRef.set(DocIndex.build(valueDocs))
      }
    }

  /** Publish the result's diagnostics, clearing files that were reported last time but are now clean. */
  private def publishDiagnostics(result: CompilationResult): IO[Unit] =
    clientRef.get match {
      case None         => IO.unit
      case Some(client) =>
        IO.blocking {
          val byUri      = EliotDiagnostics.byUri(result.errors)
          val previously = publishedRef.getAndSet(byUri.keySet)
          (previously diff byUri.keySet).foreach(uri =>
            client.publishDiagnostics(new PublishDiagnosticsParams(uri, Seq.empty[org.eclipse.lsp4j.Diagnostic].asJava))
          )
          byUri.foreach((uri, diagnostics) =>
            client.publishDiagnostics(new PublishDiagnosticsParams(uri, diagnostics.asJava))
          )
        }
    }

  /** Nudge the client to re-pull code lenses now that this compile's [[MainIndex]] is in place. Fire-and-forget: the
    * request's completion is irrelevant, and it is skipped entirely unless the client advertised refresh support (see
    * [[enableCodeLensRefresh]]).
    */
  private def refreshCodeLenses: IO[Unit] =
    clientRef.get match {
      case Some(client) if codeLensPush.get => IO.blocking(client.refreshCodeLenses()).void
      case _                                => IO.unit
    }
}
