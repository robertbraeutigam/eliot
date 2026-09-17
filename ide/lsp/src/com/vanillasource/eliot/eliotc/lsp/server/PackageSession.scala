package com.vanillasource.eliot.eliotc.lsp.server

import cats.effect.IO
import com.vanillasource.eliot.eliotc.apidoc.fact.ValueDoc
import com.vanillasource.eliot.eliotc.apidoc.plugin.ApiDocPlugin
import com.vanillasource.eliot.eliotc.compiler.{CompilationResult, CompilationServer, CompilationSession, Compiler}
import com.vanillasource.eliot.eliotc.lsp.index.{CompletionIndex, DocIndex, MainIndex, PositionIndex, TypeHintIndex}
import com.vanillasource.eliot.eliotc.lsp.mainroot.LspMainRootSourceProcessor
import com.vanillasource.eliot.eliotc.lsp.plugin.LspPlugin
import com.vanillasource.eliot.eliotc.lsp.virtual.VirtualFileSystem
import com.vanillasource.eliot.eliotc.module.fact.ModuleValue
import com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementTable
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.plugin.{Configuration, LangPlugin}
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibPlugin
import com.vanillasource.eliot.eliotc.used.UsedNames
import org.eclipse.lsp4j.Diagnostic

import java.util.concurrent.atomic.AtomicReference

/** One package's resident compile: a [[CompilationServer]] over exactly the roots [[WorkspacePlan.Session]] names, and
  * the indices and diagnostics of its latest finished compile.
  *
  * Everything a request is answered from lives here, per package, because two packages compile the same file against
  * different closures: a library's `main` has no platform to run on in the library's own package, and does in the
  * application package that deps it. The service picks which session answers for a file.
  */
final class PackageSession private (val plan: WorkspacePlan.Session) {
  private val indicesRef     = new AtomicReference[PackageSession.Indices](PackageSession.Indices.empty)
  private val diagnosticsRef = new AtomicReference[Map[String, Seq[Diagnostic]]](Map.empty)
  private val serverRef      = new AtomicReference[Option[(CompilationServer, IO[Unit])]](None)

  /** The indices of the latest finished compile; empty until the first one finishes. */
  def indices: PackageSession.Indices = indicesRef.get

  /** The diagnostics of the latest finished compile, by document URI, for the files this session checks. */
  def diagnostics: Map[String, Seq[Diagnostic]] = diagnosticsRef.get

  /** Request a recompile; non-blocking and coalescing. */
  def requestCompile: IO[Unit] = IO(serverRef.get).flatMap(_.fold(IO.unit)(_._1.requestCompile))

  /** Cancel any compile in flight and flush the cache to disk. */
  def release: IO[Unit] = IO(serverRef.getAndSet(None)).flatMap(_.fold(IO.unit)(_._2))

  private def start(vfs: VirtualFileSystem, onFinished: PackageSession => IO[Unit]): IO[Unit] = {
    val lspPlugin     = LspPlugin(vfs)
    val configuration = plan.checkedRoots.foldLeft(
      Configuration()
        .set(Compiler.targetPathKey, plan.target)
        .set(LangPlugin.pathKey, plan.roots)
    )((configuration, checked) => configuration.set(LspPlugin.checkedRootsKey, checked))
    for {
      session <- CompilationSession.create(
                   lspPlugin,
                   Seq(lspPlugin, LangPlugin(), StdlibPlugin(), ApiDocPlugin()),
                   configuration
                 )
      handle  <- CompilationServer.start(session, result => absorb(result) >> onFinished(this)).allocated
      _       <- IO(serverRef.set(Some(handle)))
    } yield ()
  }

  /** Rebuild the indices from the facts this compile materialised, and keep the diagnostics of the files this session
    * checks. A `main` counts as runnable here only when its `lspmain` wrapper monomorphized, which is what says this
    * package's closure has a platform to run it on.
    */
  private def absorb(result: CompilationResult): IO[Unit] =
    result.generator.currentFacts().flatMap { facts =>
      val resolved     = facts.values.collect { case value: ResolvedValue => value }.toSeq
      val moduleValues = facts.values.collect { case value: ModuleValue => value }.toSeq
      val monomorphic  = facts.values.collect { case value: MonomorphicValue => value }.toSeq
      val refinements  = facts.values.collect { case value: RefinementTable => value }.toSeq
      val valueDocs    = facts.values.collect { case value: ValueDoc => value }.toSeq
      val runnable     = facts.values.collect { case used: UsedNames => used.rootFQN }
        .flatMap(LspMainRootSourceProcessor.wrappedModule)
        .toSet
      IO {
        indicesRef.set(
          PackageSession.Indices(
            PositionIndex.build(resolved),
            CompletionIndex.build(moduleValues, resolved),
            TypeHintIndex.build(monomorphic, refinements),
            MainIndex.build(resolved, runnable),
            DocIndex.build(valueDocs)
          )
        )
        diagnosticsRef.set(EliotDiagnostics.byUri(result.errors).filter((uri, _) => checksUri(uri)))
      }
    }

  private def checksUri(uri: String): Boolean = EliotDiagnostics.pathOf(uri).exists(plan.checks)
}

object PackageSession {

  /** Everything position-based requests are answered from, as of one finished compile. */
  final case class Indices(
      position: PositionIndex,
      completion: CompletionIndex,
      typeHint: TypeHintIndex,
      main: MainIndex,
      doc: DocIndex
  )

  object Indices {
    val empty: Indices =
      Indices(PositionIndex.empty, CompletionIndex.empty, TypeHintIndex.empty, MainIndex.empty, DocIndex.empty)
  }

  /** Start a session for `plan`, reading unsaved buffers from `vfs`; `onFinished` runs after each compile is absorbed.
    * The session idles until its first [[requestCompile]].
    */
  def start(
      plan: WorkspacePlan.Session,
      vfs: VirtualFileSystem,
      onFinished: PackageSession => IO[Unit]
  ): IO[PackageSession] = {
    val session = new PackageSession(plan)
    session.start(vfs, onFinished).as(session)
  }
}
