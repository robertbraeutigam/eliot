package com.vanillasource.eliot.eliotc.progress

import cats.effect.IO
import com.vanillasource.eliot.eliotc.processor.CompilerFactKey
import com.vanillasource.eliot.eliotc.progress.ProgressTracker.Generation

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** The mutable core of `--progress`: which phase the run is in, how many facts it has delivered out of how many it is
  * expected to, and what it is doing (`docs/progress-indication.md` §3.2, §3.5). Written by the session and the fact
  * engine, read by the [[ProgressLineWriter]]; created only when `--progress` is asked for.
  *
  * A fact counts **once**, at whichever comes first: a demand for it was answered, it was accepted from the cache, or it
  * was proven unchanged by the cache's validation drill. So a cold build and a warm one count towards the same figure,
  * and nothing about generation order matters — the set is concurrent, and each counter only ever grows.
  *
  * What the run is doing is kept as the set of **generations in flight**, each with its parent and whether it is
  * waiting for a fact. One that waits for nothing is doing the work; its time is charged to its **owner**, the nearest
  * described fact up its parents (itself, if it is described). The activity shown is the owner of the longest-running
  * such generation — a sample of where the time goes, which stays meaningful when several generations run at once.
  *
  * @param expected
  *   the facts the previous run delivered ([[ProgressProfile.total]]); absent on a first build
  */
final class ProgressTracker private (
    expected: Option[Long],
    describer: ProgressDescriber,
    clock: () => Long,
    phaseRef: AtomicReference[ProgressPhase],
    keys: java.util.Set[CompilerFactKey[?]],
    deliveredCount: AtomicLong,
    fromCacheCount: AtomicLong,
    inFlight: ConcurrentHashMap[CompilerFactKey[?], Generation],
    changedRef: AtomicReference[Vector[String]],
    slowStepsRef: AtomicReference[Vector[ProgressStep]]
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

  /** Record that the run started working out `key`'s fact — from the cache or by generating it — for `parent`, the
    * generation that asked for it, if any. The parent waits for it until it [[ended]].
    */
  def started(key: CompilerFactKey[?], parent: Option[CompilerFactKey[?]]): IO[Unit] =
    IO.delay {
      val now        = clock()
      val parentNode = parent.flatMap(p => Option(inFlight.get(p)))

      parentNode.foreach(_.waitFrom(now))
      inFlight.put(key, new Generation(describer.describe(key), parentNode, now)): Unit
    }

  /** Record that the run finished working out `key`'s fact, so the generation that asked for it waits no more. A
    * described fact that took [[ProgressTracker.slowStep]] or more becomes a slow step.
    */
  def ended(key: CompilerFactKey[?]): IO[Unit] =
    IO.delay {
      Option(inFlight.remove(key)).foreach { generation =>
        val now = clock()

        generation.synchronized(if (generation.waits == 0) generation.charge(now - generation.leafSince))
        generation.parent.foreach(_.resumeAt(now))
        generation.activity
          .map(ProgressStep(_, generation.spent.get().nanos))
          .filter(_.took >= ProgressTracker.slowStep)
          .foreach(step => slowStepsRef.updateAndGet(_ :+ step))
      }
    }

  /** Record that the generation of `key` is waiting for a fact it did not start, so its time is not its own until
    * [[resumed]].
    */
  def waiting(key: CompilerFactKey[?]): IO[Unit] =
    IO.delay(Option(inFlight.get(key)).foreach(_.waitFrom(clock())))

  /** Record that a fact the generation of `key` waited for, and did not start, has arrived. */
  def resumed(key: CompilerFactKey[?]): IO[Unit] =
    IO.delay(Option(inFlight.get(key)).foreach(_.resumeAt(clock())))

  /** Record that the fact of `key` came out different from what the cache held. Only an [[ProgressActivity.input]] is
    * worth telling the user about, and each subject once.
    */
  def changed(key: CompilerFactKey[?]): IO[Unit] =
    IO.delay {
      describer
        .describe(key)
        .filter(_.input)
        .foreach(activity =>
          changedRef.updateAndGet(seen => if (seen.contains(activity.subject)) seen else seen :+ activity.subject)
        )
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
      val now       = clock()
      val working   = inFlight.values().asScala.filter(generation => generation.synchronized(generation.waits == 0))
      val sampled   = working.minByOption(_.startedAt).flatMap(leaf => leaf.owner.map(owner => (leaf, owner)))

      ProgressSnapshot(
        phase,
        delivered,
        fromCache,
        expected.map(_ max delivered),
        sampled.flatMap(_._2.activity),
        sampled.fold(Duration.Zero) { case (leaf, owner) =>
          (owner.spent.get() + leaf.synchronized((now - leaf.leafSince) max 0)).nanos
        },
        changedRef.get(),
        slowStepsRef.get()
      )
    }
}

object ProgressTracker {

  /** How long a described fact must take to be shown as a step of its own. */
  val slowStep: FiniteDuration = 1.second

  /** A tracker for a run expected to deliver `expected` facts, or an unknown number if absent, whose facts read the
    * way `describer` says. `clock` answers nanoseconds, and is only replaced by tests.
    */
  def create(
      expected: Option[Long] = None,
      describer: ProgressDescriber = ProgressDescriber.none,
      clock: () => Long = () => System.nanoTime()
  ): IO[ProgressTracker] =
    IO.delay(
      new ProgressTracker(
        expected,
        describer,
        clock,
        AtomicReference(ProgressPhase.Starting),
        ConcurrentHashMap.newKeySet(),
        AtomicLong(),
        AtomicLong(),
        ConcurrentHashMap(),
        AtomicReference(Vector.empty),
        AtomicReference(Vector.empty)
      )
    )

  /** One generation in flight.
    *
    * @param activity
    *   its description, if it has one
    * @param parent
    *   the generation that asked for it, if that is still in flight
    * @param startedAt
    *   when it started
    */
  private[progress] final class Generation(
      val activity: Option[ProgressActivity],
      val parent: Option[Generation],
      val startedAt: Long
  ) {

    /** The time charged to this generation as an owner, in nanoseconds. */
    val spent: AtomicLong = AtomicLong()

    /** How many facts it is waiting for; guarded by this object. */
    var waits: Int = 0

    /** Since when it has been waiting for nothing; guarded by this object. */
    var leafSince: Long = startedAt

    /** The described generation its time is charged to. */
    val owner: Option[Generation] = if (activity.isDefined) Some(this) else parent.flatMap(_.owner)

    def charge(nanos: Long): Unit = owner.foreach(_.spent.addAndGet(nanos max 0)): Unit

    /** Start waiting for one more fact at `now`. */
    def waitFrom(now: Long): Unit =
      synchronized {
        if (waits == 0) charge(now - leafSince)
        waits += 1
      }

    /** Stop waiting for one fact at `now`. */
    def resumeAt(now: Long): Unit =
      synchronized {
        waits -= 1
        if (waits == 0) leafSince = now
      }
  }
}
