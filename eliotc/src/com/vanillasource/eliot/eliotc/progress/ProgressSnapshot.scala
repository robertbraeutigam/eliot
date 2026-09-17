package com.vanillasource.eliot.eliotc.progress

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
  */
case class ProgressSnapshot(phase: ProgressPhase, delivered: Long, fromCache: Long, total: Option[Long] = None)
