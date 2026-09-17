package com.vanillasource.eliot.eliotc.progress

import scala.concurrent.duration.{Duration, FiniteDuration}

/** What a run's progress is at one moment, read atomically from a [[ProgressTracker]]. Independent of how it is shown,
  * so a second writer (`--log=json`, the LSP's `$/progress`) can render the same snapshots.
  *
  * @param phase
  *   the phase the run is in
  * @param delivered
  *   distinct facts this run has, however it came to have them
  * @param fromCache
  *   how many of those were accepted from the cache or proven unchanged by it, rather than generated
  * @param total
  *   the facts this run is expected to deliver: what the previous run delivered, or `delivered` once the run has
  *   overtaken that. Absent on a first build.
  * @param activity
  *   what the run is doing right now, if a described fact is being generated
  * @param activityTime
  *   how much time the run has spent on that activity's fact so far
  * @param changed
  *   the subjects of the inputs this run found changed, in the order it found them
  * @param slowSteps
  *   the described facts that took [[ProgressTracker.slowStep]] or more, in the order they finished
  * @param runClass
  *   what kind of run this is, once the cache is loaded
  * @param phaseTimes
  *   the time the run spent in each phase it entered, the current one so far
  * @param worked
  *   the facts worked out, and their own time, by the key type's class name
  * @param inFlight
  *   the facts being worked out, as the key type's class name and their own time so far
  * @param verbTimes
  *   the time charged to each verb of the plugins' descriptions ([[ProgressActivity.verb]]); time no described fact
  *   owns is charged to `working`
  * @param remaining
  *   how long the run has left, if its history can tell ([[ProgressEstimator]])
  */
case class ProgressSnapshot(
    phase: ProgressPhase,
    delivered: Long,
    fromCache: Long,
    total: Option[Long] = None,
    activity: Option[ProgressActivity] = None,
    activityTime: FiniteDuration = Duration.Zero,
    changed: Seq[String] = Seq.empty,
    slowSteps: Seq[ProgressStep] = Seq.empty,
    runClass: Option[ProgressRunClass] = None,
    phaseTimes: Map[ProgressPhase, FiniteDuration] = Map.empty,
    worked: Map[String, ProgressCost] = Map.empty,
    inFlight: Seq[(String, FiniteDuration)] = Seq.empty,
    verbTimes: Map[String, FiniteDuration] = Map.empty,
    remaining: Option[FiniteDuration] = None
) {

  /** Where this run's time went so far, as a history of one run. */
  def history: ProgressHistory =
    ProgressHistory(
      delivered.toDouble,
      ProgressHistory.timedPhases.flatMap(phase => phaseTimes.get(phase).map(phase -> _.toNanos.toDouble)).toMap,
      worked
    )
}
