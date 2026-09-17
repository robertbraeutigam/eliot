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

  "the header" should "name the compiler and the target" in {
    ProgressLineWriter.header(Seq("jvm exe-jar", "HelloWorld"), false) shouldBe "eliot · jvm exe-jar · HelloWorld"
  }

  it should "say when a run is a first build" in {
    ProgressLineWriter.header(Seq("jvm exe-jar", "HelloWorld"), true) shouldBe
      "eliot · jvm exe-jar · HelloWorld · first build"
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
