package com.vanillasource.eliot.eliotc.statistics

import cats.effect.IO
import com.vanillasource.eliot.eliotc.processor.CompilerProcessor

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

/** Collects per-processor timings for one compilation run, and renders them as a report.
  *
  * Processors are measured by wrapping them ([[wrap]]) — no processor knows it is being measured, and nothing outside
  * this package changes behaviour. Note that only *leaf* processors are wrapped: `wrapWith` pushes the wrapper down
  * through container processors, so a container's own overhead is attributed to nobody and shows up in the report's
  * unaccounted remainder, together with the compiler engine itself.
  */
final class ProcessorStatistics private (counters: ConcurrentHashMap[String, ProcessorCounters]) {

  /** Wrap one processor for measurement, accumulating into the (single) counters of the given name. */
  def wrap(processor: CompilerProcessor, processorName: String): CompilerProcessor =
    TimedCompilerProcessor(processor, counters.computeIfAbsent(processorName, _ => ProcessorCounters()))

  /** All measurements so far, slowest first.
    *
    * "So far" is meant literally: a generation makes its fact available from *inside* the processor, so the generating
    * fiber still has a tail to run — including its own measurement — after its requester has already moved on. Take the
    * snapshot once the run has quiesced (the compiler does it after persisting the cache and printing diagnostics), or
    * the last generations in flight are missing from it.
    */
  def snapshot: IO[Seq[ProcessorStatistic]] = IO.delay(
    counters.asScala.toSeq.map((name, processorCounters) => processorCounters.snapshot(name)).sortBy(-_.selfTime.toNanos)
  )

  /** Render the report, with the given total run time to account against. */
  def report(totalTime: FiniteDuration): IO[String] = snapshot.map(render(totalTime, _))

  private def render(totalTime: FiniteDuration, statistics: Seq[ProcessorStatistic]): String = {
    val totalMillis      = totalTime.toMillis
    val processorMillis  = statistics.map(_.selfTime.toMillis).sum
    val unaccounted      = math.max(0L, totalMillis - processorMillis)
    val percentOf: Long => Double = millis => if (totalMillis == 0L) 0.0 else millis * 100.0 / totalMillis

    val header = Seq(
      f"Compiler statistics: $totalMillis%,d ms total, $processorMillis%,d ms (${percentOf(processorMillis)}%.1f%%) in processors",
      f"${"ms"}%10s ${"%"}%6s ${"calls"}%10s ${"active"}%9s ${"facts"}%9s  processor"
    )

    val rows = statistics.map { statistic =>
      val millis = statistic.selfTime.toMillis
      f"$millis%10d ${percentOf(millis)}%6.1f ${statistic.invocations}%10d ${statistic.activeInvocations}%9d " +
        f"${statistic.factsProduced}%9d  ${simplifyProcessorName(statistic.processorName)}%s"
    }

    val remainder =
      f"$unaccounted%10d ${percentOf(unaccounted)}%6.1f ${"-"}%10s ${"-"}%9s ${"-"}%9s  (compiler engine, cache and i/o)"

    (header ++ rows :+ remainder).mkString("\n")
  }

  private def simplifyProcessorName(fullName: String): String = fullName.replaceAll(".*\\.", "")
}

object ProcessorStatistics {
  def create(): IO[ProcessorStatistics] = IO.delay(new ProcessorStatistics(ConcurrentHashMap()))
}
