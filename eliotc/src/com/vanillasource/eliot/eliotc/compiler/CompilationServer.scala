package com.vanillasource.eliot.eliotc.compiler

import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging

/** A resident compilation server: a single worker loop that drives [[CompilationSession.compileOnce]] with
  * **cancel-restart** semantics. Each [[requestCompile]] supersedes any compile already in flight — the running
  * compilation is cancelled (its session cache left at the last good state, see [[CompilationSession.compileOnce]]) and a
  * fresh one starts, so the latest request always wins. Bursts coalesce: while a compile runs, any number of requests
  * collapse into at most one pending recompile.
  *
  * The worker is request-driven — it idles until the first [[requestCompile]] — and runs as a background fiber for the
  * lifetime of the [[CompilationServer.start]] resource, which cancels it and flushes the cache to disk on release.
  *
  * Completed results are both pushed (the `onResult` callback, e.g. publish diagnostics) and pullable
  * ([[latestResult]], e.g. answer a hover from the live generator).
  */
final class CompilationServer private (
    runCompile: IO[CompilationResult],
    requests: Queue[IO, Unit],
    latest: Ref[IO, Option[CompilationResult]],
    onResult: CompilationResult => IO[Unit]
) extends Logging {

  /** Request a (re)compilation. Non-blocking and coalescing: supersedes an in-flight compile and collapses with any
    * other pending request. Returns as soon as the request is recorded, not when the compile finishes.
    */
  def requestCompile: IO[Unit] = requests.offer(())

  /** The most recently completed compilation, or `None` before the first one finishes. */
  def latestResult: IO[Option[CompilationResult]] = latest.get

  /** The worker loop. Idles on the first request, then keeps the latest request winning: it races the running compile
    * against the arrival of a newer request — if a request arrives first the compile is cancelled and restarted; if the
    * compile finishes first its result is committed and the loop waits for the next request.
    */
  private[compiler] def serve: IO[Unit] = requests.take >> compileLoop

  private def compileLoop: IO[Unit] =
    IO.race(runOnce, requests.take).flatMap {
      case Left(())  => requests.take >> compileLoop // committed a result; await the next request
      case Right(()) => compileLoop                  // superseded by a newer request; recompile immediately
    }

  /** Run one compilation, then commit its result uncancelably so a result that finished computing is either fully
    * recorded and published or not at all. An unexpected failure (never an ordinary compile error — those ride along in
    * the result) is logged and swallowed so the loop survives; cancellation is not an error and propagates normally.
    */
  private def runOnce: IO[Unit] =
    runCompile
      .flatMap(result => (latest.set(Some(result)) >> onResult(result)).uncancelable)
      .handleErrorWith(t => error[IO]("Compilation run failed in the server loop.", t))
}

object CompilationServer {

  /** Start a server over a [[CompilationSession]]. The worker runs as a background fiber for the resource's lifetime; on
    * release it is cancelled (cancelling any in-flight compile) and the session cache is flushed to disk so the next
    * process start is warm.
    *
    * @param onResult
    *   invoked with each completed result, in an uncancelable region (e.g. publish diagnostics).
    */
  def start(
      session: CompilationSession,
      onResult: CompilationResult => IO[Unit] = _ => IO.unit
  ): Resource[IO, CompilationServer] =
    startWith(session.compileOnce(), session.persist(), onResult)

  /** Lifecycle core, decoupled from [[CompilationSession]] for testing: any `runCompile` / `persist` pair. */
  private[compiler] def startWith(
      runCompile: IO[CompilationResult],
      persist: IO[Unit],
      onResult: CompilationResult => IO[Unit]
  ): Resource[IO, CompilationServer] =
    for {
      requests <- Resource.eval(Queue.dropping[IO, Unit](1))
      latest   <- Resource.eval(Ref.of[IO, Option[CompilationResult]](None))
      server    = new CompilationServer(runCompile, requests, latest, onResult)
      _        <- Resource.onFinalize(persist) // released after the worker is cancelled (finalizers run in reverse)
      _        <- server.serve.background
    } yield server
}
