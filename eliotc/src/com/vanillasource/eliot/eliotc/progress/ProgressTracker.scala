package com.vanillasource.eliot.eliotc.progress

import cats.effect.IO
import com.vanillasource.eliot.eliotc.processor.CompilerFactKey

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

/** The mutable core of `--progress`: which phase the run is in, and how many facts it has delivered
  * (`docs/progress-indication.md` §3.2). Written by the session and the fact engine, read by the
  * [[ProgressLineWriter]]; created only when `--progress` is asked for.
  *
  * A fact counts **once**, at whichever comes first: it was registered by a generation, accepted from the cache, or
  * proven unchanged by the cache's validation drill. So a cold build and a warm one count towards the same figure,
  * and nothing about generation order matters — the set is concurrent, and each counter only ever grows.
  */
final class ProgressTracker private (
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
    * more facts from the cache than facts delivered.
    */
  def snapshot: IO[ProgressSnapshot] =
    IO.delay {
      val phase     = phaseRef.get()
      val fromCache = fromCacheCount.get()

      ProgressSnapshot(phase, deliveredCount.get(), fromCache)
    }
}

object ProgressTracker {
  def create(): IO[ProgressTracker] =
    IO.delay(
      new ProgressTracker(
        AtomicReference(ProgressPhase.Starting),
        ConcurrentHashMap.newKeySet(),
        AtomicLong(),
        AtomicLong()
      )
    )
}
