package com.vanillasource.eliot.eliotc.progress

import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import com.vanillasource.eliot.eliotc.progress.ProgressLineWriter.State
import com.vanillasource.eliot.eliotc.progress.ProgressPhase.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*

class ProgressLineWriterTest extends AnyFlatSpec with Matchers {
  import ProgressLineWriterTest.*

  "progress lines" should "follow a recorded cold build" in {
    linesOf(coldTrace) shouldBe Seq(
      "[   125 facts ] working                                                   2.4s",
      "[   977 facts ] working                                                   3.4s",
      "[ 3,280 facts ] working                                                   4.4s",
      "[ 3,989 facts ] saving cache                                              5.4s"
    )
  }

  it should "follow a recorded cold build against the total of the previous run" in {
    linesOf(coldTrace.map(withTotal(3989))) shouldBe Seq(
      "[  125/3,989]   working                                                   2.4s",
      "[  977/3,989]   working                                                   3.4s",
      "[3,280/3,989]   working                                                   4.4s",
      "[3,989/3,989]   saving cache                                              5.4s"
    )
  }

  it should "measure a run that grew past its total against what it delivered" in {
    ProgressLineWriter.progressLine(ProgressSnapshot(Working, 28901, 0, Some(28901)), 3.seconds) should startWith(
      "[28,901/28,901] working"
    )
  }

  it should "follow a recorded build after a one-line change" in {
    linesOf(changedTrace) shouldBe Seq(
      "[ 1,075 facts ] working                                                   2.4s"
    )
  }

  it should "print nothing within a second of the header, whatever changes" in {
    linesOf(Seq(sample(1500, Working, 10), sample(2000, SavingCache, 5000))) shouldBe Seq.empty
  }

  it should "print a heartbeat after five silent seconds" in {
    linesOf((25 to 90 by 5).map(tenths => sample(tenths * 100, Working, 100))) shouldBe Seq(
      "[   100 facts ] working                                                   2.5s",
      "[   100 facts ] working                                                   7.5s"
    )
  }

  it should "stretch the heartbeat after a minute" in {
    ProgressLineWriter.heartbeatInterval(90.seconds) shouldBe 15.seconds
  }

  it should "never show the running phase" in {
    linesOf(Seq(sample(9000, Running, 100))) shouldBe Seq.empty
  }

  it should "show a time over a minute in minutes and seconds" in {
    ProgressLineWriter.progressLine(ProgressSnapshot(Working, 27181, 0), 65.seconds) should endWith(" 1m05s")
  }

  it should "name what the run is doing while it works" in {
    ProgressLineWriter.progressLine(working.copy(activity = Some(checkingString)), 3.seconds) shouldBe
      "[ 4,279 facts ] checking    eliot.lang.String                             3.0s"
  }

  it should "name the phase rather than a lingering activity once the run stops working" in {
    ProgressLineWriter.progressLine(
      working.copy(phase = SavingCache, activity = Some(checkingString)),
      3.seconds
    ) should startWith("[ 4,279 facts ] saving cache  ")
  }

  it should "cut a long subject at its beginning" in {
    ProgressLineWriter.progressLine(
      working.copy(activity = Some(ProgressActivity("checking", "eliot.build.resolve.internal.dependency.Resolution"))),
      3.seconds
    ) shouldBe "[ 4,279 facts ] checking    …ild.resolve.internal.dependency.Resolution   3.0s"
  }

  it should "say in a heartbeat how long the run has been at its activity" in {
    ProgressLineWriter.step(
      State(1.second, Working, 4279),
      working.copy(activity = Some(checkingString), activityTime = 4200.millis),
      6.seconds
    )._2 shouldBe Some("[ 4,279 facts ] checking    eliot.lang.String … 4.2s                      6.0s")
  }

  it should "name a changed input" in {
    ProgressLineWriter.step(State(1.second, Working, 4279), working.copy(changed = Seq("src/A.els")), 2.seconds)
      ._2 shouldBe Some("[ 4,279 facts ] changed     src/A.els                                     2.0s")
  }

  it should "name the first of several changed inputs, and count the rest" in {
    ProgressLineWriter.step(
      State(1.second, Working, 4279),
      working.copy(changed = Seq("src/A.els", "src/B.els", "src/C.els")),
      2.seconds
    )._2 shouldBe Some("[ 4,279 facts ] changed     src/A.els · and 2 more                        2.0s")
  }

  it should "not name a changed input twice" in {
    val snapshot = working.copy(changed = Seq("src/A.els"))
    val first    = ProgressLineWriter.step(State(1.second, Working, 4279), snapshot, 2.seconds)._1
    ProgressLineWriter.step(first, snapshot, 4.seconds)._2 shouldBe None
  }

  it should "print each slow step with how long it took" in {
    val snapshot =
      working.copy(slowSteps = Seq(ProgressStep(checkingString, 1200.millis), ProgressStep(packaging, 3.seconds)))
    linesOf(Seq(2500.millis -> snapshot, 2700.millis -> snapshot, 3500.millis -> snapshot)) shouldBe Seq(
      "[ 4,279 facts ] checking    eliot.lang.String · 1.2s                      2.5s",
      "[ 4,279 facts ] packaging   HelloWorld.jar · 3.0s                         3.5s"
    )
  }

  "the header" should "name the compiler and the target" in {
    ProgressLineWriter.header(Seq("jvm exe-jar", "HelloWorld"), withHistory, 1.second) shouldBe
      "eliot · jvm exe-jar · HelloWorld"
  }

  it should "say when a run is a first build" in {
    ProgressLineWriter.header(Seq("jvm exe-jar", "HelloWorld"), ProgressSnapshot(LoadingCache, 0, 0), 1.second) shouldBe
      "eliot · jvm exe-jar · HelloWorld · first build"
  }

  it should "say when a run is a full build, and how long it should take" in {
    ProgressLineWriter.header(
      Seq("HelloWorld"),
      withHistory.copy(runClass = Some(ProgressRunClass.Cold), remaining = Some(14800.millis)),
      1200.millis
    ) shouldBe "eliot · HelloWorld · full build, about 15s"
  }

  it should "say a full build even when it cannot say how long it takes" in {
    ProgressLineWriter.header(Seq("HelloWorld"), withHistory.copy(runClass = Some(ProgressRunClass.Cold)), 1.second) shouldBe
      "eliot · HelloWorld · full build"
  }

  it should "say how long a run takes if nothing changed" in {
    ProgressLineWriter.header(
      Seq("HelloWorld"),
      withHistory.copy(runClass = Some(ProgressRunClass.Unchanged), remaining = Some(1100.millis)),
      800.millis
    ) shouldBe "eliot · HelloWorld · about 2s if nothing changed"
  }

  "the time left" should "be rounded to seconds under ten seconds" in {
    ProgressLineWriter.left(7400.millis) shouldBe "~7s left"
  }

  it should "be rounded to five seconds under a minute" in {
    ProgressLineWriter.left(23.seconds) shouldBe "~25s left"
  }

  it should "be rounded to fifteen seconds above a minute" in {
    ProgressLineWriter.left(68.seconds) shouldBe "~1m15s left"
  }

  it should "read finishing under a second" in {
    ProgressLineWriter.left(900.millis) shouldBe "finishing"
  }

  it should "follow the elapsed time on a progress line" in {
    ProgressLineWriter.progressLine(withHistory.copy(phase = Working, remaining = Some(7.seconds)), 8400.millis) shouldBe
      "[4,279/9,000]   working                                                   8.4s   ~7s left"
  }

  "the time line" should "show the time of each verb, the longest first, and the cache's last" in {
    ProgressLineWriter.timeLine(
      working.copy(
        verbTimes = Map("parsing" -> 1500.millis, "checking" -> 3100.millis, "working" -> Duration.Zero),
        phaseTimes = Map(LoadingCache -> 100.millis, Working -> 5.seconds, SavingCache -> 4.seconds)
      )
    ) shouldBe Some("time   checking 3.1s · parsing 1.5s · cache 4.1s")
  }

  it should "be left out when nothing took long enough to show" in {
    ProgressLineWriter.timeLine(working.copy(verbTimes = Map("parsing" -> 10.millis))) shouldBe None
  }

  "the closing line" should "state how many facts came from the cache" in {
    ProgressLineWriter.closingLine(ProgressSnapshot(Running, 27944, 27487), Seq.empty, true, 7800.millis) shouldBe
      "ok     27,944 facts, 27,487 from cache · 7.8s"
  }

  it should "say all when every fact came from the cache" in {
    ProgressLineWriter.closingLine(ProgressSnapshot(Running, 278, 278), Seq.empty, true, 2.seconds) shouldBe
      "ok     278 facts, all from cache · 2.0s"
  }

  it should "repeat the error count and the first error's position on failure" in {
    ProgressLineWriter.closingLine(
      ProgressSnapshot(Running, 900, 0),
      Seq(errorAt("Version.els", 56), errorAt("Other.els", 3), errorAt("Other.els", 4)),
      false,
      4100.millis
    ) shouldBe "failed 3 errors · first at Version.els:56 · 4.1s"
  }

  it should "leave out the position of an error that has none" in {
    ProgressLineWriter.closingLine(ProgressSnapshot(Running, 900, 0), Seq(CompilerError.global("x")), false, 1.second) shouldBe
      "failed 1 error · 1.0s"
  }

  it should "fail a run that produced nothing without an error" in {
    ProgressLineWriter.closingLine(ProgressSnapshot(Running, 900, 0), Seq.empty, false, 1.second) shouldBe
      "failed nothing produced · 1.0s"
  }
}

object ProgressLineWriterTest {

  private val working        = ProgressSnapshot(Working, 4279, 0)
  private val withHistory    = ProgressSnapshot(LoadingCache, 4279, 0, Some(9000))
  private val checkingString = ProgressActivity("checking", "eliot.lang.String")
  private val packaging      = ProgressActivity("packaging", "HelloWorld.jar")

  /** The header is printed shortly before the first sample, at the start of the run's `main`. */
  private val headerAt = 1400.millis

  /** `examples/src/Strings.els` compiled cold, sampled by the writer every 200 ms (elapsed millis since JVM start). */
  private val coldTrace = Seq(
    sample(1624, Starting, 0),
    sample(1830, Working, 7),
    sample(2031, Working, 8),
    sample(2232, Working, 77),
    sample(2433, Working, 125),
    sample(2639, Working, 213),
    sample(2839, Working, 342),
    sample(3040, Working, 478),
    sample(3240, Working, 732),
    sample(3441, Working, 977),
    sample(3642, Working, 1357),
    sample(3842, Working, 1932),
    sample(4043, Working, 2424),
    sample(4243, Working, 2653),
    sample(4443, Working, 3280),
    sample(4645, Working, 3752),
    sample(4845, SavingCache, 3989),
    sample(5046, SavingCache, 3989),
    sample(5246, SavingCache, 3989),
    sample(5447, SavingCache, 3989)
  )

  /** The same program after a one-line change, with the cold build's cache. */
  private val changedTrace = Seq(
    sample(1592, Starting, 0),
    sample(1801, LoadingCache, 0),
    sample(2003, Working, 348, 263),
    sample(2208, Working, 1074, 842),
    sample(2409, Working, 1075, 842),
    sample(2614, SavingCache, 3989, 3723),
    sample(2814, SavingCache, 3989, 3723)
  )

  private def sample(millis: Long, phase: ProgressPhase, delivered: Long, fromCache: Long = 0): (FiniteDuration, ProgressSnapshot) =
    millis.millis -> ProgressSnapshot(phase, delivered, fromCache)

  private def withTotal(total: Long)(sample: (FiniteDuration, ProgressSnapshot)): (FiniteDuration, ProgressSnapshot) =
    sample._1 -> sample._2.copy(total = Some(total))

  private def linesOf(trace: Seq[(FiniteDuration, ProgressSnapshot)]): Seq[String] =
    trace
      .foldLeft((State(headerAt, Starting, 0), Seq.empty[String])) { case ((state, lines), (now, snapshot)) =>
        val (next, line) = ProgressLineWriter.step(state, snapshot, now)
        (next, lines ++ line)
      }
      ._2

  private def errorAt(file: String, line: Int): CompilerError =
    CompilerError("x", Seq.empty, file, "", PositionRange(Position(line, 1), Position(line, 2)))
}
