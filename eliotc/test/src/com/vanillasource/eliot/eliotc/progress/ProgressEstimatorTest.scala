package com.vanillasource.eliot.eliotc.progress

import com.vanillasource.eliot.eliotc.progress.ProgressPhase.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*
import scala.io.Source

class ProgressEstimatorTest extends AnyFlatSpec with Matchers {
  import ProgressEstimatorTest.*

  "the estimate" should "follow a recorded cold build to within a sixth, from a fifth of the way in" in {
    misses(trace("cold"), 1.0 / 6) shouldBe Seq.empty
  }

  it should "follow a recorded build with nothing changed to within a third of a second" in {
    misses(trace("unchanged"), 0.0) shouldBe Seq.empty
  }

  it should "follow a recorded build after a one-line change to within a sixth, once the change is found" in {
    misses(trace("changed"), 1.0 / 6, _.filter(_.snapshot.runClass.contains(ProgressRunClass.Changed))) shouldBe Seq.empty
  }

  it should "estimate a recorded changed build as an unchanged one until the change is found" in {
    trace("changed").samples
      .find(_.snapshot.runClass.contains(ProgressRunClass.Unchanged))
      .flatMap(estimate(trace("changed")))
      .map(_.toMillis) shouldBe Some(1445)
  }

  it should "have no estimate while a recorded build works past its history's total" in {
    trace("grown").samples
      .filter(sample => sample.snapshot.phase == Working && sample.snapshot.delivered > 3989)
      .flatMap(estimate(trace("grown"))) shouldBe Seq.empty
  }

  it should "estimate the saving of a recorded grown build by its size, to within half, as it begins" in {
    misses(trace("grown"), 1.0 / 2, _.filter(_.snapshot.phase == SavingCache).take(1)) shouldBe Seq.empty
  }

  it should "follow the recorded build after a grown one to within a third, from a fifth of the way in" in {
    misses(trace("regrown"), 1.0 / 3) shouldBe Seq.empty
  }

  it should "have nothing to estimate once the run is running its target" in {
    ProgressEstimator.remaining(10, history, ProgressSnapshot(Running, 10, 0)) shouldBe None
  }

  it should "take the work and the phases left as long as their history said, at the speed the run has shown" in {
    ProgressEstimator.remaining(
      10,
      history,
      ProgressSnapshot(
        Working,
        5,
        0,
        phaseTimes = Map(Working -> 11.seconds),
        worked = Map("parse" -> ProgressCost(5, 5.seconds.toNanos.toDouble))
      )
    ) shouldBe Some(10600.millis)
  }

  it should "count a fact in flight as its time so far, up to its type's average" in {
    ProgressEstimator.remaining(
      10,
      history,
      ProgressSnapshot(
        Working,
        0,
        0,
        phaseTimes = Map(Working -> 5.seconds),
        inFlight = Seq("parse" -> 5.seconds, "parse" -> 400.millis)
      )
    ).map(_.toMillis) shouldBe Some(22250)
  }

  it should "weigh a key type the history does not know at its live time" in {
    ProgressEstimator.remaining(
      10,
      history,
      ProgressSnapshot(
        Working,
        1,
        0,
        phaseTimes = Map(Working -> 2.seconds),
        worked = Map("flash" -> ProgressCost(1, 2.seconds.toNanos.toDouble))
      )
    ) shouldBe Some(10300.millis)
  }

  it should "scale the saving of a run by the facts it delivered" in {
    ProgressEstimator.remaining(
      10,
      history,
      ProgressSnapshot(SavingCache, 20, 0, phaseTimes = Map(SavingCache -> 100.millis))
    ) shouldBe Some(500.millis)
  }

  it should "leave what is left of a timed phase" in {
    ProgressEstimator.remaining(
      10,
      history,
      ProgressSnapshot(LoadingCache, 0, 0, phaseTimes = Map(LoadingCache -> 200.millis))
    ) shouldBe Some(10600.millis)
  }
}

object ProgressEstimatorTest {

  /** Ten facts of type `parse` at a second each, a 500 ms cache load and a 300 ms save. */
  private val history = ProgressHistory(
    10,
    Map(LoadingCache -> 500.millis.toNanos.toDouble, SavingCache -> 300.millis.toNanos.toDouble),
    Map("parse" -> ProgressCost(10, 10.seconds.toNanos.toDouble))
  )

  /** One sample of a recorded run: when it was taken, since the JVM started, and what the tracker said. */
  private case class Sample(now: FiniteDuration, snapshot: ProgressSnapshot)

  /** A recorded run: the profile it started with, its samples, and when it ended. */
  private case class Trace(profile: ProgressProfile, samples: Seq[Sample], end: FiniteDuration)

  /** The estimate of `sample`, the way the tracker makes it. */
  private def estimate(trace: Trace)(sample: Sample): Option[FiniteDuration] =
    for {
      total    <- trace.profile.total
      history  <- sample.snapshot.runClass.flatMap(trace.profile.runs.get)
      estimate <- ProgressEstimator.remaining(total, history, sample.snapshot)
    } yield estimate

  /** The samples, from a fifth of the way into the run, whose estimate is further from the time actually left than
    * `share` of it, or a third of a second, whichever is more; each as the time of the sample, the estimate and the time
    * left. Only the samples `selected` from those are checked, and of them only those with an estimate.
    */
  private def misses(
      trace: Trace,
      share: Double,
      selected: Seq[Sample] => Seq[Sample] = identity
  ): Seq[(FiniteDuration, FiniteDuration, FiniteDuration)] =
    selected(trace.samples.filter(_.now >= trace.end / 5))
      .flatMap(sample => estimate(trace)(sample).map(estimate => (sample.now, estimate, trace.end - sample.now)))
      .filter((_, estimate, left) => (estimate - left).toMillis.abs > (left.toMillis * share).max(333))

  /** The trace in `progress/<name>.trace`: `profile` lines holding the profile, then a `sample` line per sample, followed
    * by its `phase`, `worked` and `flying` lines.
    */
  private def trace(name: String): Trace = {
    val lines = Source.fromResource(s"progress/$name.trace").getLines().map(_.split(" ").toSeq).toSeq
    val end   = lines.collectFirst { case Seq("end", millis) => millis.toLong.millis }.get
    val text  = lines.collect { case "profile" +: record => record.mkString(" ") }.mkString("\n")

    val samples = lines.foldLeft(Vector.empty[Sample]) {
      case (samples, Seq("sample", now, phase, delivered, fromCache, runClass)) =>
        samples :+ Sample(
          now.toLong.millis,
          ProgressSnapshot(
            ProgressPhase.valueOf(phase),
            delivered.toLong,
            fromCache.toLong,
            runClass = ProgressRunClass.fromLabel(runClass)
          )
        )
      case (samples, Seq("phase", phase, nanos))                                 =>
        samples.init :+ edit(samples.last)(s => s.copy(phaseTimes = s.phaseTimes.updated(ProgressPhase.valueOf(phase), nanos.toLong.nanos)))
      case (samples, Seq("worked", name, count, nanos))                          =>
        samples.init :+ edit(samples.last)(s => s.copy(worked = s.worked.updated(name, ProgressCost(count.toDouble, nanos.toDouble))))
      case (samples, Seq("flying", name, nanos))                                 =>
        samples.init :+ edit(samples.last)(s => s.copy(inFlight = s.inFlight :+ (name -> nanos.toLong.nanos)))
      case (samples, _)                                                          => samples
    }

    Trace(ProgressProfile.parse(text), samples, end)
  }

  private def edit(sample: Sample)(change: ProgressSnapshot => ProgressSnapshot): Sample =
    sample.copy(snapshot = change(sample.snapshot))
}
