package com.vanillasource.eliot.eliotc.statistics

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey}

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.{FiniteDuration, NANOSECONDS}

/** Wraps the compilation process of a single generation to measure the time that generation spends *waiting* for other
  * facts, so [[TimedCompilerProcessor]] can subtract it from the generation's wall time.
  *
  * One instance per generation — it holds that generation's totals, not the processor's.
  */
final class TimedCompilationProcess(underlying: CompilationProcess) extends CompilationProcess {
  private val waitedNanos   = AtomicLong(0)
  private val requests      = AtomicLong(0)
  private val registrations = AtomicLong(0)

  /** A fact request is a wait: the engine generates the fact on another fiber and blocks this one until it is done, so
    * the whole call belongs to the *other* processor, which measures it itself.
    */
  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K,
      ancestors: List[CompilerFactKey[?]]
  ): IO[Option[V]] =
    for {
      start  <- IO.monotonic
      result <- underlying.getFact(key, ancestors)
      end    <- IO.monotonic
      _      <- IO.delay {
                  waitedNanos.addAndGet((end - start).toNanos)
                  requests.incrementAndGet()
                }
    } yield result

  override def registerFact(value: CompilerFact): IO[Unit] =
    IO.delay(registrations.incrementAndGet()) >> underlying.registerFact(value)

  override def activeFactKeys: IO[List[CompilerFactKey[?]]] = underlying.activeFactKeys

  def waited: FiniteDuration = FiniteDuration(waitedNanos.get(), NANOSECONDS)

  /** Whether the generation touched the compilation process at all — the infrastructure-level signal that the processor
    * recognised the key, as opposed to being offered a key type it does not handle.
    */
  def wasActive: Boolean = requests.get() > 0 || registrations.get() > 0

  def factsRegistered: Long = registrations.get()
}
