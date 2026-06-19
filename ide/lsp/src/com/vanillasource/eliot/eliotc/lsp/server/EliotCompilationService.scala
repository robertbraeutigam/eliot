package com.vanillasource.eliot.eliotc.lsp.server

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.vanillasource.eliot.eliotc.compiler.{CompilationResult, CompilationServer, CompilationSession, Compiler}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.lsp.index.PositionIndex
import com.vanillasource.eliot.eliotc.lsp.plugin.LspPlugin
import com.vanillasource.eliot.eliotc.plugin.{Configuration, LangPlugin}
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue
import com.vanillasource.eliot.eliotc.stdlib.plugin.StdlibPlugin
import org.eclipse.lsp4j.PublishDiagnosticsParams
import org.eclipse.lsp4j.services.LanguageClient

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference
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
  private val clientRef    = new AtomicReference[Option[LanguageClient]](None)
  private val serverRef    = new AtomicReference[Option[(CompilationServer, IO[Unit])]](None)
  private val publishedRef = new AtomicReference[Set[String]](Set.empty)
  private val indexRef     = new AtomicReference[PositionIndex](PositionIndex.empty)

  /** Remember the remote client so finished compiles can push diagnostics to it. */
  def connect(client: LanguageClient): Unit = clientRef.set(Some(client))

  /** The reverse position index from the latest finished compile, for position-based features (definition, hover).
    * Empty until the first compile completes.
    */
  def positionIndex: PositionIndex = indexRef.get

  /** Build a session over the workspace source roots, start the cancel-restart server, and trigger the first compile.
    * Stdlib + platform layers come from this process's classpath (as for the CLI), so only the user's roots are needed.
    */
  def startWorkspace(roots: Seq[Path]): Unit = {
    val lspPlugin     = LspPlugin()
    val configuration = Configuration()
      .set(Compiler.targetPathKey, roots.headOption.getOrElse(Path.of(".")).resolve(".eliot-lsp"))
      .set(LangPlugin.pathKey, roots)
    val started       = (for {
      session <- CompilationSession.create(
                   lspPlugin,
                   Seq(lspPlugin, LangPlugin(), StdlibPlugin()),
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

  /** Rebuild the reverse position index from the result, then publish its diagnostics. The index is built off the
    * request path (once per finished compile) so position-based requests are answered synchronously from memory.
    */
  private def publishResult(result: CompilationResult): IO[Unit] =
    rebuildIndex(result) >> publishDiagnostics(result)

  /** Rebuild the position index from the [[ResolvedValue]] facts materialised by this compile. The whole-workspace
    * driver ([[LspPlugin]]) demands every name, so every workspace value's resolved form is present.
    */
  private def rebuildIndex(result: CompilationResult): IO[Unit] =
    result.generator
      .currentFacts()
      .map(facts => PositionIndex.build(facts.values.collect { case value: ResolvedValue => value }.toSeq))
      .flatMap(index => IO(indexRef.set(index)))

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
}
