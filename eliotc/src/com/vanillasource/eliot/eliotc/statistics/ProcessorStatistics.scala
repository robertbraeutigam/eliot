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

  /** Wrap the whole (already individually wrapped) processor tree, to measure what a fact generation costs *inside* the
    * processors as a whole. Whatever that is beyond the sum of the individual processors is the cost of dispatching —
    * the compiler offers every requested key to every processor, and each of those offers is invoked, error-isolated
    * and type-tested even when the processor does not handle that kind of key at all.
    *
    * Its invocation count is therefore the number of fact generations of the run, not the number of processor calls.
    *
    * Measuring is itself per-invocation work, and it lands in this same figure — so read the dispatch line as an upper
    * bound. Comparing a run with and without statistics gives the true cost of both.
    */
  def wrapTree(processorTree: CompilerProcessor): CompilerProcessor =
    wrap(processorTree, ProcessorStatistics.dispatchName)

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

  /** Renders the processors, then the two costs that are nobody's individual doing: dispatching a key through the whole
    * processor tree, and the engine driving it all. Both are derived by subtraction, which is what makes them
    * trustworthy — every millisecond of the run lands in exactly one row.
    */
  private def render(totalTime: FiniteDuration, statistics: Seq[ProcessorStatistic]): String = {
    val (tree, processors) = statistics.partition(_.processorName == ProcessorStatistics.dispatchName)
    val totalMillis        = totalTime.toMillis
    val processorMillis    = processors.map(_.selfTime.toMillis).sum
    val treeMillis         = tree.map(_.selfTime.toMillis).sum
    val dispatchMillis     = math.max(0L, treeMillis - processorMillis)
    val engineMillis       = math.max(0L, totalMillis - processorMillis - dispatchMillis)

    val percentOf: Long => Double = millis => if (totalMillis == 0L) 0.0 else millis * 100.0 / totalMillis

    def row(millis: Long, calls: String, active: String, facts: String, name: String): String =
      f"$millis%10d ${percentOf(millis)}%6.1f $calls%10s $active%9s $facts%9s  $name%s"

    val header = Seq(
      f"Compiler statistics: $totalMillis%,d ms total, $processorMillis%,d ms (${percentOf(processorMillis)}%.1f%%) in processors",
      f"${"ms"}%10s ${"%"}%6s ${"calls"}%10s ${"active"}%9s ${"facts"}%9s  processor"
    )

    val processorRows = processors.map(statistic =>
      row(
        statistic.selfTime.toMillis,
        statistic.invocations.toString,
        statistic.activeInvocations.toString,
        statistic.factsProduced.toString,
        simplifyProcessorName(statistic.processorName)
      )
    )

    val dispatchRows = tree.map(statistic =>
      row(dispatchMillis, statistic.invocations.toString, "-", "-", "(offering every key to every processor)")
    )

    val engineRow = row(engineMillis, "-", "-", "-", "(compiler engine, cache and i/o)")

    (header ++ processorRows ++ dispatchRows :+ engineRow).mkString("\n")
  }

  private def simplifyProcessorName(fullName: String): String = fullName.replaceAll(".*\\.", "")
}

object ProcessorStatistics {

  /** The reserved name under which the whole processor tree is measured (see [[ProcessorStatistics.wrapTree]]); it is
    * not a processor and is reported separately.
    */
  private[statistics] val dispatchName: String = "(processor tree)"

  def create(): IO[ProcessorStatistics] = IO.delay(new ProcessorStatistics(ConcurrentHashMap()))
}
