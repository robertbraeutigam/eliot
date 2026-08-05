package com.vanillasource.eliot.eliotc.statistics

import cats.effect.{IO, Ref}
import scala.concurrent.duration.FiniteDuration

/** Coarse, once-per-run wall-clock timings for the compilation phases that sit *outside* the per-processor accounting
  * — loading and saving the incremental cache, building its next-run graph, and computing the cache fingerprints.
  *
  * Unlike [[ProcessorStatistics]], which observes *every* processor invocation and so is created only when
  * `--statistics` is asked for, these are a handful of measurements per run and are therefore taken **always**: the
  * session records them regardless of the flag, and only the report reads them. Two monotonic reads and one `Ref`
  * update per phase is negligible next to the phase itself.
  *
  * `--statistics` uses them to pin the otherwise-invisible time. The cache deserialization — the warm-build "load
  * floor" (`docs/incremental-compilation.md` §7) — happens during session setup, *before* the per-processor total's
  * window opens, and the serialization happens *after* it closes, so neither shows up in that total at all. Recording
  * them here, and subtracting them in [[ProcessorStatistics.report]], is what lets the report account for the full
  * session lifecycle rather than the compile alone.
  *
  * Latest value wins per phase id, so a resident session ([[com.vanillasource.eliot.eliotc.compiler.CompilationSession]]
  * driving many compiles) keeps only the most recent measurement of each phase rather than one entry per compile.
  */
final class PhaseTimings private (timings: Ref[IO, Map[String, FiniteDuration]]) {

  /** Measure the wall time of `action` and record it under `phaseId`, replacing any previous measurement. The timing
    * is recorded only when `action` succeeds — a failed load/save leaves no misleading duration behind.
    */
  def time[A](phaseId: String)(action: IO[A]): IO[A] =
    for {
      start  <- IO.monotonic
      result <- action
      end    <- IO.monotonic
      _      <- timings.update(_.updated(phaseId, end - start))
    } yield result

  /** The measurements taken so far, by phase id. */
  def snapshot: IO[Map[String, FiniteDuration]] = timings.get
}

object PhaseTimings {

  /** Computing the cache fingerprints (the recursive classpath digest dominates), during session setup. */
  val fingerprint: String = "fingerprint"

  /** Deserializing the incremental cache from disk, during session setup — the warm-build load floor. */
  val cacheLoad: String = "cache-load"

  /** Merging this run's facts into the next-run cache graph, at the end of a compile. */
  val buildCache: String = "build-cache"

  /** Serializing the incremental cache back to disk, after a compile. */
  val cacheSave: String = "cache-save"

  def create(): IO[PhaseTimings] = Ref.of[IO, Map[String, FiniteDuration]](Map.empty).map(new PhaseTimings(_))
}
