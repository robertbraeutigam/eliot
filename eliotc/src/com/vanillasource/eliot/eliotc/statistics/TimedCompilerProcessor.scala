package com.vanillasource.eliot.eliotc.statistics

import cats.data.ReaderT
import cats.effect.{Clock, Sync}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}

/** Wraps a processor to measure how often and for how long it runs. Intercepts the compilation process (see
  * [[TimedCompilationProcess]]) so the time the processor spends *waiting for facts* can be subtracted from its wall
  * time.
  *
  * The remainder is the processor's own time, and it is CPU time in practice: the compiler does no parallel work, and
  * although the engine generates each fact on its own fiber, the requesting fiber immediately blocks on it — so at most
  * one processor is running at any instant. There is also no double counting: a nested generation is measured by its
  * own wrapper, and its full duration is subtracted from the generation that asked for it. A wall clock is used rather
  * than a per-thread CPU clock because fibers migrate between pool threads, which would make the start and end reads of
  * a thread clock come from different threads.
  *
  * A generation that *aborts* — the sanctioned way for a processor to decline, and how an error propagates — is not
  * recorded, since the abort short-circuits before the measurement completes. On a run without errors this only loses
  * declines, so the totals slightly under-count; that is the price of keeping this an entirely additive add-on that
  * needs nothing from `CompilerIO`.
  */
final class TimedCompilerProcessor(underlying: CompilerProcessor, counters: ProcessorCounters) extends CompilerProcessor {

  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    ReaderT { process =>
      val timed = TimedCompilationProcess(process)

      Clock[CompilerIO]
        .timed(underlying.generate(factKey))
        .flatMap { case (elapsed, _) =>
          Sync[CompilerIO].delay(counters.record(elapsed - timed.waited, timed.wasActive, timed.factsRegistered))
        }
        .run(timed)
    }
}
