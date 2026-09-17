package com.vanillasource.eliot.eliotc.progress

import cats.effect.std.Console
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.pos.PositionRange

import java.lang.management.ManagementFactory
import java.util.Locale
import scala.concurrent.duration.*

/** Prints a run's progress to stderr as **append-only lines**: a header, progress lines while the run works, and a
  * closing line (`docs/progress-indication.md` §4). Every line is final once printed, so the output is the same bytes in
  * a terminal, a CI log and a pipe, and anything else writing to the terminal simply lands between two lines — nothing
  * is repainted, and no stream is taken over.
  *
  * A progress line looks like
  * {{{
  * [ 4,279 facts ] working                                                 3.4s
  * }}}
  * and is written when a trigger fires *and* at least a second has passed since the previous line (the header
  * included), which is what keeps the output readable. The decision is a pure function ([[ProgressLineWriter.step]]) of
  * what the line before showed, a [[ProgressSnapshot]] and the elapsed time; this class only samples the tracker a few
  * times a second and prints what that function answers.
  *
  * Times are measured from the start of the JVM, so the part of a run that precedes `main` is in the figures.
  */
final class ProgressLineWriter private (tracker: ProgressTracker, startedAtMillis: Long) {

  /** The time since the JVM started. */
  val elapsed: IO[FiniteDuration] = IO.realTime.map(_ - startedAtMillis.millis)

  /** Print the header naming `target`, then progress lines until the resource is released. */
  def lines(target: Seq[String]): Resource[IO, Unit] =
    Resource
      .eval(
        for {
          now      <- elapsed
          snapshot <- tracker.snapshot
          _        <- Console[IO].errorln(ProgressLineWriter.header(target))
        } yield ProgressLineWriter.State(now, snapshot.phase, snapshot.delivered)
      )
      .flatMap(initial => loop(initial).background.void)

  /** Print the closing line of a run that reported `errors` and did, or did not, produce its target. */
  def close(errors: Seq[CompilerError], targetProduced: Boolean): IO[Unit] =
    for {
      now      <- elapsed
      snapshot <- tracker.snapshot
      _        <- Console[IO].errorln(ProgressLineWriter.closingLine(snapshot, errors, targetProduced, now))
    } yield ()

  private def loop(state: ProgressLineWriter.State): IO[Nothing] =
    for {
      _        <- IO.sleep(ProgressLineWriter.samplingInterval)
      now      <- elapsed
      snapshot <- tracker.snapshot
      (next, line) = ProgressLineWriter.step(state, snapshot, now)
      _        <- line.traverse_(Console[IO].errorln)
      result   <- loop(next)
    } yield result
}

object ProgressLineWriter {

  /** What the last printed line showed, which is all the triggers compare against.
    *
    * @param lastLineAt
    *   when the last line (the header included) was printed
    * @param shownPhase
    *   the phase the run was in at that line
    * @param shownCount
    *   the facts delivered at that line
    */
  case class State(lastLineAt: FiniteDuration, shownPhase: ProgressPhase, shownCount: Long)

  /** How often the tracker is sampled. Frequent enough that a line is written close to its trigger, rare enough that
    * the build does not notice.
    */
  val samplingInterval: FiniteDuration = 200.millis

  /** The least time between two lines: whatever triggers, the output never scrolls faster than it can be read. */
  val lineFloor: FiniteDuration = 1.second

  private val verbWidth    = 12
  private val subjectWidth = 44

  def create(tracker: ProgressTracker): IO[ProgressLineWriter] =
    // The JVM's own record, in milliseconds. `ProcessHandle`'s start instant is derived from the boot time, which Linux
    // keeps in whole seconds, and read up to a second late.
    IO.delay(new ProgressLineWriter(tracker, ManagementFactory.getRuntimeMXBean.getStartTime))

  /** Whether a line is due at `now`, and the line if so. A line is due when at least [[lineFloor]] passed since the
    * last one and one of these holds:
    *
    *   - the run is in a different phase than the last line showed;
    *   - the facts delivered at least doubled since the last line;
    *   - nothing was printed for a heartbeat interval ([[heartbeatInterval]]).
    *
    * The [[ProgressPhase.Running]] phase is never shown: the closing line is printed before it begins.
    */
  def step(state: State, snapshot: ProgressSnapshot, now: FiniteDuration): (State, Option[String]) = {
    val sinceLast = now - state.lastLineAt
    val due       =
      sinceLast >= lineFloor && snapshot.phase != ProgressPhase.Running && (
        snapshot.phase != state.shownPhase ||
          (snapshot.delivered > 0 && snapshot.delivered >= 2 * state.shownCount) ||
          sinceLast >= heartbeatInterval(now)
      )

    if (due) (State(now, snapshot.phase, snapshot.delivered), Some(progressLine(snapshot, now)))
    else (state, None)
  }

  /** How long the output may stay silent: five seconds, stretching to 15 after a minute and to 60 after ten, so a long
    * step is a few lines rather than hundreds.
    */
  def heartbeatInterval(now: FiniteDuration): FiniteDuration =
    if (now < 1.minute) 5.seconds
    else if (now < 10.minutes) 15.seconds
    else 1.minute

  /** The first line of a run, e.g. `eliot · jvm exe-jar · HelloWorld`. */
  def header(target: Seq[String]): String = ("eliot" +: target).mkString(" · ")

  /** A progress line: the facts delivered so far, what the run is doing, and the time elapsed. */
  def progressLine(snapshot: ProgressSnapshot, now: FiniteDuration): String = {
    val counter = f"[${grouped(snapshot.delivered)}%6s facts ]"

    f"$counter ${snapshot.phase.label.padTo(verbWidth, ' ')}${"".padTo(subjectWidth, ' ')}${duration(now)}%6s"
  }

  /** The line a run ends on, e.g. `ok     27,181 facts, 0 from cache · 15.7s`, or
    * `failed 3 errors · first at Version.els:56 · 4.1s`. The first error's position is repeated so it survives the
    * scroll; the diagnostics themselves are printed above it.
    */
  def closingLine(
      snapshot: ProgressSnapshot,
      errors: Seq[CompilerError],
      targetProduced: Boolean,
      now: FiniteDuration
  ): String =
    if (errors.isEmpty && targetProduced) {
      val cached =
        if (snapshot.delivered > 0 && snapshot.fromCache == snapshot.delivered) "all"
        else grouped(snapshot.fromCache)

      s"ok     ${grouped(snapshot.delivered)} facts, $cached from cache · ${duration(now)}"
    } else {
      val what  = errors.size match {
        case 0 => "nothing produced"
        case 1 => "1 error"
        case n => s"${grouped(n)} errors"
      }
      val first = errors.headOption
        .filter(_.sourceRange != PositionRange.zero)
        .map(error => s"first at ${error.contentSource}:${error.sourceRange.from.line}")

      (Seq(s"failed $what") ++ first :+ duration(now)).mkString(" · ")
    }

  // Formatted in the root locale, so a count reads `27,181` and a time `1.0s` whatever the machine is set to.
  private def grouped(count: Long): String = "%,d".formatLocal(Locale.ROOT, count)

  private def duration(time: FiniteDuration): String =
    if (time < 1.minute) "%.1fs".formatLocal(Locale.ROOT, time.toMillis / 1000.0)
    else "%dm%02ds".formatLocal(Locale.ROOT, time.toMinutes, time.toSeconds % 60)
}
