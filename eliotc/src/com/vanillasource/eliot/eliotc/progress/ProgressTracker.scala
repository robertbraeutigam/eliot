package com.vanillasource.eliot.eliotc.progress

import cats.effect.IO
import com.vanillasource.eliot.eliotc.processor.CompilerFactKey

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

/** The mutable core of `--progress`: which phase the run is in, and how many facts it has delivered out of how many it
  * is expected to (`docs/progress-indication.md` §3.2). Written by the session and the fact engine, read by the
  * [[ProgressLineWriter]]; created only when `--progress` is asked for.
  *
  * A fact counts **once**, at whichever comes first: a demand for it was answered, it was accepted from the cache, or it
  * was proven unchanged by the cache's validation drill. So a cold build and a warm one count towards the same figure,
  * and nothing about generation order matters — the set is concurrent, and each counter only ever grows.
  *
  * @param expected
  *   the facts the previous run delivered ([[ProgressProfile.total]]); absent on a first build
  */
final class ProgressTracker private (
    expected: Option[Long],
    phaseRef: AtomicReference[ProgressPhase],
    keys: java.util.Set[CompilerFactKey[?]],
    deliveredCount: AtomicLong,
    fromCacheCount: AtomicLong
) {

  /** Record that the run has moved on to `phase`. */
  def enter(phase: ProgressPhase): IO[Unit] = IO.delay(phaseRef.set(phase))

  /** Record that the run has the fact of `key`; a key it already had is not counted again. */
  def delivered(key: CompilerFactKey[?], fromCache: Boolean): IO[Unit] =
    IO.delay {
      if (keys.add(key)) {
        deliveredCount.incrementAndGet()
        if (fromCache) fromCacheCount.incrementAndGet(): Unit
      }
    }

  /** The progress so far. `fromCache` is read before `delivered`, and incremented after it, so a snapshot never shows
    * more facts from the cache than facts delivered. A run that has grown past the expected total is measured against
    * what it delivered — `[28,901/28,901]` and still working is honest, 103 % is not.
    */
  def snapshot: IO[ProgressSnapshot] =
    IO.delay {
      val phase     = phaseRef.get()
      val fromCache = fromCacheCount.get()
      val delivered = deliveredCount.get()

      ProgressSnapshot(phase, delivered, fromCache, expected.map(_ max delivered))
    }
}

object ProgressTracker {

  /** A tracker for a run expected to deliver `expected` facts, or an unknown number if absent. */
  def create(expected: Option[Long] = None): IO[ProgressTracker] =
    IO.delay(
      new ProgressTracker(
        expected,
        AtomicReference(ProgressPhase.Starting),
        ConcurrentHashMap.newKeySet(),
        AtomicLong(),
        AtomicLong()
      )
    )
}
