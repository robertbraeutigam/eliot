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
  */
case class ProgressSnapshot(phase: ProgressPhase, delivered: Long, fromCache: Long)
