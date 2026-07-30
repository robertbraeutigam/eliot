package com.vanillasource.eliot.eliotc.statistics

import scala.concurrent.duration.FiniteDuration

/** One processor's measurements from a single compilation run.
  *
  * @param processorName
  *   the fully qualified class name of the measured processor
  * @param invocations
  *   how many times the processor was asked to generate a fact. Note that every processor is offered *every* requested
  *   key, so this is dominated by keys the processor does not handle at all.
  * @param activeInvocations
  *   of those, how many actually interacted with the compilation process (requested or registered a fact) — a good
  *   proxy for "the processor recognised the key and did something".
  * @param factsProduced
  *   how many facts the processor registered.
  * @param selfTime
  *   cumulative time spent *in* the processor: the wall time of its generations minus the time they spent blocked
  *   waiting for other facts. See [[TimedCompilerProcessor]] for why that is the CPU time here.
  */
case class ProcessorStatistic(
    processorName: String,
    invocations: Long,
    activeInvocations: Long,
    factsProduced: Long,
    selfTime: FiniteDuration
)
