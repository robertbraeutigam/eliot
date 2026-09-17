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
  */
case class ProgressSnapshot(
    phase: ProgressPhase,
    delivered: Long,
    fromCache: Long,
    total: Option[Long] = None,
    activity: Option[ProgressActivity] = None,
    activityTime: FiniteDuration = Duration.Zero,
    changed: Seq[String] = Seq.empty,
    slowSteps: Seq[ProgressStep] = Seq.empty
)
