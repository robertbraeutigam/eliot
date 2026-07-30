package com.vanillasource.eliot.eliotc.statistics

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.{FiniteDuration, NANOSECONDS}

/** The live accumulators of one processor, held for the duration of a compilation run.
  *
  * There is exactly one instance per processor (created when the processor tree is wrapped), so recording a generation
  * is a handful of atomic adds with no map lookup and no allocation on the hot path — the processor tree is offered
  * every requested key, so this runs millions of times per compilation.
  */
final class ProcessorCounters {
  private val invocations       = AtomicLong(0)
  private val activeInvocations = AtomicLong(0)
  private val factsProduced     = AtomicLong(0)
  private val selfNanos         = AtomicLong(0)

  /** Record one completed generation.
    *
    * @param selfTime
    *   the generation's wall time minus what it spent waiting for facts. Clamped at zero: the two clocks are read
    *   around slightly different regions, so a generation that did nothing but wait can come out marginally negative.
    */
  def record(selfTime: FiniteDuration, active: Boolean, facts: Long): Unit = {
    invocations.incrementAndGet()
    if (active) {
      activeInvocations.incrementAndGet(): Unit
    }
    factsProduced.addAndGet(facts)
    selfNanos.addAndGet(math.max(0L, selfTime.toNanos)): Unit
  }

  def snapshot(processorName: String): ProcessorStatistic =
    ProcessorStatistic(
      processorName,
      invocations.get(),
      activeInvocations.get(),
      factsProduced.get(),
      FiniteDuration(selfNanos.get(), NANOSECONDS)
    )
}
