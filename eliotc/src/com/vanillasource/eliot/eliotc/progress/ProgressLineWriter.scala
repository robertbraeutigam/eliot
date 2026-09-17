package com.vanillasource.eliot.eliotc.progress

import cats.effect.std.Console
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.pos.PositionRange

import java.lang.management.ManagementFactory
import java.time.LocalTime
import java.util.Locale
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.*

/** Prints a run's progress to stderr as **append-only lines**: a header, progress lines while the run works, and a
  * closing line (`docs/progress-indication.md` §4). Every line is final once printed, so the output is the same bytes
  * in a terminal, a CI log and a pipe, and anything else writing to the terminal simply lands between two lines —
  * nothing is repainted, and no stream is taken over.
  *
  * A progress line looks like
  * {{{
  * [ 4,279/27,181] checking    eliot.build.git.Git                           3.4s   ~12s left
  * }}}
  * or, on a first build, which has no total to show and no history to estimate from, `[ 4,279 facts ] checking …`. The
  * activity comes from the plugins' [[ProgressDescriber]]s; a phase with no activity is named instead (`saving cache`).
  * A line is written when a trigger fires *and* at least a second has passed since the previous line (the header
  * included), which is what keeps the output readable. The decision is a pure function ([[ProgressLineWriter.step]]) of
  * what the line before showed, a [[ProgressSnapshot]] and the elapsed time; this class only samples the tracker a few
  * times a second and prints what that function answers.
  *
  * Times are measured from the start of the JVM, so the part of a run that precedes `main` is in the figures. How the
  * lines are decorated — colour, Unicode, a time of day on each line — is the [[ProgressStyle]]'s decision.
  */
final class ProgressLineWriter private (
    tracker: ProgressTracker,
    style: ProgressStyle,
    startedAtMillis: Long,
    headerShown: AtomicBoolean,
    progressShown: AtomicBoolean
) {

  /** The time since the JVM started. */
  val elapsed: IO[FiniteDuration] = IO.realTime.map(_ - startedAtMillis.millis)

  /** Print the header naming `target`, then progress lines until the resource is released. The header waits for the
    * cache to be loaded, since whether there was one decides the estimate it states, but not for longer than
    * [[ProgressLineWriter.lineFloor]].
    */
  def lines(target: Seq[String]): Resource[IO, Unit] =
    Resource.eval(elapsed).flatMap(start => loop(target, start, None).background.void)

  /** Print the closing block of a run that reported `errors` and did, or did not, produce its target, which `measures`
    * measure: where the time went, if the run printed any progress line, the measures if there are more than the
    * closing line holds, and the closing line. `previous` is what the last successful run measured.
    */
  def close(
      target: Seq[String],
      errors: Seq[CompilerError],
      targetProduced: Boolean,
      measures: Seq[ProgressMeasure],
      previous: Map[String, Long]
  ): IO[Unit] =
    for {
      now      <- elapsed
      snapshot <- tracker.snapshot
      _        <- print(ProgressLineWriter.header(target, snapshot, now, style)).unlessA(headerShown.get())
      _        <- ProgressLineWriter.timeLine(snapshot, style).filter(_ => progressShown.get()).traverse_(print)
      block     = ProgressLineWriter.closingBlock(snapshot, errors, targetProduced, now, measures, previous, style)
      _        <- block.traverse_(print)
    } yield ()

  private def print(line: String): IO[Unit] =
    IO.delay(LocalTime.now()).flatMap(time => Console[IO].errorln(style.stamped(line, time)))

  private def loop(target: Seq[String], start: FiniteDuration, shown: Option[ProgressLineWriter.State]): IO[Nothing] =
    for {
      now      <- elapsed
      snapshot <- tracker.snapshot
      next     <- shown match {
                    case Some(state)                                                                        =>
                      val (next, line) = ProgressLineWriter.step(state, snapshot, now, style)
                      line.traverse_(text => print(text) >> IO.delay(progressShown.set(true))).as(Some(next))
                    case None if snapshot.runClass.isDefined || now - start >= ProgressLineWriter.lineFloor =>
                      print(ProgressLineWriter.header(target, snapshot, now, style)) >>
                        IO.delay(headerShown.set(true))
                          .as(Some(ProgressLineWriter.State(now, snapshot.phase, snapshot.delivered)))
                    case None                                                                               => IO.pure(None)
                  }
      _        <- IO.sleep(ProgressLineWriter.samplingInterval)
      result   <- loop(target, start, next)
    } yield result
}

object ProgressLineWriter {

  /** What the lines printed so far showed, which is all the triggers compare against.
    *
    * @param lastLineAt
    *   when the last line (the header included) was printed
    * @param shownPhase
    *   the phase the run was in at the last progress line
    * @param shownCount
    *   the facts delivered at the last line
    * @param shownChanged
    *   how many of the run's changed inputs have been named
    * @param shownSlowSteps
    *   how many of the run's slow steps have been printed
    */
  case class State(
      lastLineAt: FiniteDuration,
      shownPhase: ProgressPhase,
      shownCount: Long,
      shownChanged: Int = 0,
      shownSlowSteps: Int = 0
  )

  /** How often the tracker is sampled. Frequent enough that a line is written close to its trigger, rare enough that
    * the build does not notice.
    */
  val samplingInterval: FiniteDuration = 200.millis

  /** The least time between two lines: whatever triggers, the output never scrolls faster than it can be read. */
  val lineFloor: FiniteDuration = 1.second

  private val counterWidth = 15
  private val verbWidth    = 12
  private val subjectWidth = 44

  def create(tracker: ProgressTracker, style: ProgressStyle): IO[ProgressLineWriter] =
    // The JVM's own record, in milliseconds. `ProcessHandle`'s start instant is derived from the boot time, which Linux
    // keeps in whole seconds, and read up to a second late.
    IO.delay(
      new ProgressLineWriter(
        tracker,
        style,
        ManagementFactory.getRuntimeMXBean.getStartTime,
        AtomicBoolean(false),
        AtomicBoolean(false)
      )
    )

  /** Whether a line is due at `now`, and the line if so. A line is due when at least [[lineFloor]] passed since the
    * last one and one of these holds, the first that does deciding what the line says:
    *
    *   - the run found an input changed that no line named yet — `changed examples/src/Strings.els`, and how many more
    *     were found since the last line;
    *   - a described fact took [[ProgressTracker.slowStep]] or more and has no line yet — its activity and how long it
    *     took, one line per step;
    *   - the run is in a different phase than the last progress line showed, or the facts delivered crossed another
    *     tenth of the total since the last line (on a first build: at least doubled) — what the run is doing now;
    *   - nothing was printed for a heartbeat interval ([[heartbeatInterval]]) — what the run is doing now, and how long
    *     it has been at it.
    *
    * The line is decorated as `style` says, except for the time of day, which is added when it is printed.
    *
    * What the run is doing is the snapshot's activity while it works, and its phase otherwise. The
    * [[ProgressPhase.Running]] phase is never shown: the closing line is printed before it begins.
    */
  def step(
      state: State,
      snapshot: ProgressSnapshot,
      now: FiniteDuration,
      style: ProgressStyle = ProgressStyle.undecorated
  ): (State, Option[String]) = {
    val sinceLast = now - state.lastLineAt
    val printed   = state.copy(lastLineAt = now, shownCount = snapshot.delivered)

    if (sinceLast < lineFloor || snapshot.phase == ProgressPhase.Running) (state, None)
    else if (snapshot.changed.size > state.shownChanged) {
      val pending = snapshot.changed.drop(state.shownChanged)
      val more    = Option.when(pending.size > 1)(s"and ${grouped(pending.size - 1L)} more")

      (
        printed.copy(shownChanged = snapshot.changed.size),
        Some(line(snapshot, style.warn, "changed", pending.head, more.fold("")(" · " + _), now, style))
      )
    } else if (snapshot.slowSteps.size > state.shownSlowSteps) {
      val slow = snapshot.slowSteps(state.shownSlowSteps)
      val took = s" · ${duration(slow.took)}"

      (
        printed.copy(shownSlowSteps = state.shownSlowSteps + 1),
        Some(line(snapshot, style.muted, slow.activity.verb, slow.activity.subject, took, now, style))
      )
    } else if (snapshot.phase != state.shownPhase || advanced(state.shownCount, snapshot))
      (printed.copy(shownPhase = snapshot.phase), Some(progressLine(snapshot, now, style = style)))
    else if (sinceLast >= heartbeatInterval(now, style))
      (printed.copy(shownPhase = snapshot.phase), Some(progressLine(snapshot, now, heartbeat = true, style)))
    else (state, None)
  }

  private def advanced(shownCount: Long, snapshot: ProgressSnapshot): Boolean =
    snapshot.total match {
      case Some(total) => tenths(snapshot.delivered, total) > tenths(shownCount, total)
      case None        => snapshot.delivered > 0 && snapshot.delivered >= 2 * shownCount
    }

  private def tenths(count: Long, total: Long): Long = if (total == 0) 10 else count * 10 / total

  /** How long the output may stay silent: five seconds, stretching to 15 after a minute and to 60 after ten, so a long
    * step is a few lines rather than hundreds. A log is read later and as a whole, and only needs to show the run is
    * alive, so a timestamped style is silent for at least 30 seconds.
    */
  def heartbeatInterval(now: FiniteDuration, style: ProgressStyle = ProgressStyle.undecorated): FiniteDuration = {
    val interval =
      if (now < 1.minute) 5.seconds
      else if (now < 10.minutes) 15.seconds
      else 1.minute

    if (style.timestamped) interval max 30.seconds else interval
  }

  /** The first line of a run at `now`, e.g. `eliot · jvm exe-jar · HelloWorld · full build, about 16s`: the target, and
    * what the run is expected to be — a first build, with no total to measure it against; a full build, one with no
    * cache; or one with a cache — with how long the whole of it should take, if its history tells.
    */
  def header(
      target: Seq[String],
      snapshot: ProgressSnapshot,
      now: FiniteDuration,
      style: ProgressStyle = ProgressStyle.undecorated
  ): String = {
    val about    = snapshot.remaining.map(left => s"about ${rough(now + left)}")
    val expected =
      if (snapshot.total.isEmpty) Some("first build")
      else
        snapshot.runClass match {
          case Some(ProgressRunClass.Cold)      => Some(("full build" +: about.toSeq).mkString(", "))
          case Some(ProgressRunClass.Unchanged) => about.map(_ + " if nothing changed")
          case _                                => about
        }

    (("eliot" +: target) ++ expected).mkString(style.separator)
  }

  /** Where the run's time went, by verb and with the cache's loading and saving as `cache`, e.g. `time checking 3.1s ·
    * parsing 1.5s · cache 4.1s`; `None` if nothing took long enough to show.
    */
  def timeLine(snapshot: ProgressSnapshot, style: ProgressStyle = ProgressStyle.undecorated): Option[String] = {
    val cache = Seq(ProgressPhase.LoadingCache, ProgressPhase.SavingCache)
      .flatMap(snapshot.phaseTimes.get)
      .foldLeft(Duration.Zero)(_ + _)
    val parts = (snapshot.verbTimes.toSeq.sortBy((verb, time) => (-time, verb)) :+ ("cache" -> cache))
      .filter(_._2 >= shownTime)
      .map((verb, time) => s"$verb ${style.faint(duration(time))}")

    Option.when(parts.nonEmpty)(s"${style.muted("time")}   ${parts.mkString(style.separator)}")
  }

  /** The least time the `time` line shows. */
  private val shownTime = 50.millis

  /** How long the run has left, rounded to a step that grows with it: `~7s left`, `~25s left`, `~1m15s left`, or
    * `finishing` under a second.
    */
  def left(remaining: FiniteDuration): String =
    if (remaining < 1.second) "finishing" else s"~${rough(remaining)} left"

  /** A time rounded to the nearest second under ten seconds, to five under a minute, and to fifteen above. */
  private def rough(time: FiniteDuration): String = {
    val step    = if (time < 10.seconds) 1L else if (time < 1.minute) 5L else 15L
    val seconds = (math.round(time.toMillis / 1000.0 / step) * step) max step

    if (seconds < 60) s"${seconds}s" else "%dm%02ds".formatLocal(Locale.ROOT, seconds / 60, seconds % 60)
  }

  /** A progress line: the facts delivered so far out of the total, what the run is doing, the time elapsed and the time
    * left. A heartbeat also says how long the run has been at what it is doing.
    */
  def progressLine(
      snapshot: ProgressSnapshot,
      now: FiniteDuration,
      heartbeat: Boolean = false,
      style: ProgressStyle = ProgressStyle.undecorated
  ): String =
    snapshot.activity.filter(_ => snapshot.phase == ProgressPhase.Working) match {
      case Some(activity) =>
        val detail =
          if (heartbeat && snapshot.activityTime >= lineFloor) s" ${style.ellipsis} ${duration(snapshot.activityTime)}"
          else ""
        line(snapshot, style.muted, activity.verb, activity.subject, detail, now, style)
      case None           => line(snapshot, style.muted, snapshot.phase.label, "", "", now, style)
    }

  /** A line in the progress columns: the counter, a verb painted by `paint`, a subject followed by `detail` (empty, or
    * starting with its separator), the time elapsed, and the time left if the run has an estimate. A subject too long
    * for its column loses its beginning, the least specific part of a module or a path. The columns are laid out on the
    * text, and only then painted.
    */
  private def line(
      snapshot: ProgressSnapshot,
      paint: String => String,
      verb: String,
      subject: String,
      detail: String,
      now: FiniteDuration,
      style: ProgressStyle
  ): String = {
    val counter = snapshot.total match {
      case Some(total) =>
        val shownTotal = grouped(total)
        val shownCount = grouped(snapshot.delivered)
        s"[${" " * (shownTotal.length - shownCount.length)}$shownCount/$shownTotal]".padTo(counterWidth, ' ')
      case None        => f"[${grouped(snapshot.delivered)}%6s facts ]"
    }
    val shownDetail = if (style.unicode) detail else detail.replace(" · ", " - ")
    val room        = subjectWidth - 1 - shownDetail.length
    val fitted      =
      if (subject.length <= room) subject else style.ellipsis + subject.takeRight(room - style.ellipsis.length)
    val padding     = " " * (subjectWidth - fitted.length - shownDetail.length)
    val eta         = snapshot.remaining.fold("")(remaining => f"${left(remaining)}%11s")

    style.faint(counter) + " " + paint(verb) + " " * (verbWidth - verb.length) + fitted + style.faint(shownDetail) +
      padding + style.faint(f"${duration(now)}%6s$eta")
  }

  /** The lines a run ends on: the closing line, preceded by a `size` line per measure when there are more than
    * [[measuresOnClosingLine]]. See [[closingLine]].
    */
  def closingBlock(
      snapshot: ProgressSnapshot,
      errors: Seq[CompilerError],
      targetProduced: Boolean,
      now: FiniteDuration,
      measures: Seq[ProgressMeasure],
      previous: Map[String, Long],
      style: ProgressStyle
  ): Seq[String] =
    if (measures.size <= measuresOnClosingLine || !succeeded(errors, targetProduced))
      Seq(closingLine(snapshot, errors, targetProduced, now, measures, previous, style))
    else
      measuresShown(snapshot, measures, previous, style).map(measure => s"${style.muted("size")}   $measure") :+
        closingLine(snapshot, errors, targetProduced, now, style = style)

  /** How many measures the closing line holds; more are listed above it. */
  val measuresOnClosingLine: Int = 2

  /** The line a run ends on, e.g. `ok 27,181 facts, 0 from cache · 15.7s`, or `failed 3 errors · first at
    * Version.els:56 · 4.1s`. The first error's position is repeated so it survives the scroll; the diagnostics
    * themselves are printed above it.
    *
    * A successful run's line starts with the `measures` of what it produced, and how much each moved since the
    * `previous` run measured it: `ok HelloWorld.jar 412 KB (+88 B) · …`. A run that found nothing changed says its
    * target is `up to date` instead of how much it moved.
    */
  def closingLine(
      snapshot: ProgressSnapshot,
      errors: Seq[CompilerError],
      targetProduced: Boolean,
      now: FiniteDuration,
      measures: Seq[ProgressMeasure] = Seq.empty,
      previous: Map[String, Long] = Map.empty,
      style: ProgressStyle = ProgressStyle.undecorated
  ): String =
    if (succeeded(errors, targetProduced)) {
      val cached   =
        if (snapshot.delivered > 0 && snapshot.fromCache == snapshot.delivered) "all"
        else grouped(snapshot.fromCache)
      val produced = measuresShown(snapshot, measures, previous, style)
      val upToDate = Option.when(produced.nonEmpty && unchanged(snapshot))("up to date")
      val parts    = produced ++ upToDate ++ Seq(
        s"${grouped(snapshot.delivered)} facts, $cached from cache",
        style.faint(duration(now))
      )

      s"${style.pass("ok")}     ${parts.mkString(style.separator)}"
    } else {
      val what  = errors.size match {
        case 0 => "nothing produced"
        case 1 => "1 error"
        case n => s"${grouped(n)} errors"
      }
      val first = errors.headOption
        .filter(_.sourceRange != PositionRange.zero)
        .map(error => s"first at ${error.contentSource}:${error.sourceRange.from.line}")

      (Seq(s"${style.fail("failed")} $what") ++ first :+ style.faint(duration(now))).mkString(style.separator)
    }

  private def succeeded(errors: Seq[CompilerError], targetProduced: Boolean): Boolean =
    errors.isEmpty && targetProduced

  private def unchanged(snapshot: ProgressSnapshot): Boolean =
    snapshot.runClass.contains(ProgressRunClass.Unchanged)

  // A run that changed nothing produced what the last one did, so how much that moved is not news
  private def measuresShown(
      snapshot: ProgressSnapshot,
      measures: Seq[ProgressMeasure],
      previous: Map[String, Long],
      style: ProgressStyle
  ): Seq[String] =
    measures.map(measure => measure.show(previous.get(measure.name).filterNot(_ => unchanged(snapshot)), style))

  // Formatted in the root locale, so a count reads `27,181` and a time `1.0s` whatever the machine is set to.
  private def grouped(count: Long): String = "%,d".formatLocal(Locale.ROOT, count)

  private def duration(time: FiniteDuration): String =
    if (time < 1.minute) "%.1fs".formatLocal(Locale.ROOT, time.toMillis / 1000.0)
    else "%dm%02ds".formatLocal(Locale.ROOT, time.toMinutes, time.toSeconds % 60)
}
