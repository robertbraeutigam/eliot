package com.vanillasource.eliot.eliotc.progress

import cats.effect.IO

import java.time.LocalTime
import java.time.format.DateTimeFormatter
import scala.util.Try

/** How progress output is decorated for where it goes (`docs/progress-indication.md` §4.5). Decoration never carries a
  * meaning of its own: every status is a word, so the same lines read the same with colour, without it, and in ASCII.
  *
  * Colour follows the design system's terminal roles — brackets, times and separators faint, verbs muted, `ok` green,
  * `changed` copper, `failed` red — as 24-bit colour under `COLORTERM=truecolor` and as the nearest ANSI colour
  * otherwise. Output that is not going to a terminal is the log form: no colour, and each line stamped with the time of
  * day (`11:42:03 [14,211/27,181] checking …`), because a log has no other clock.
  *
  * @param colour
  *   how colours are written, if at all
  * @param unicode
  *   whether `·` and `…` may be written; `-` and `...` are written instead otherwise
  * @param timestamped
  *   whether each line starts with the time of day, which is also what makes heartbeats rarer
  */
case class ProgressStyle(colour: ProgressStyle.Colour, unicode: Boolean, timestamped: Boolean) {
  import ProgressStyle.Colour

  /** Between two parts of a line: `" · "`. */
  def separator: String = faint(if (unicode) " · " else " - ")

  /** What stands for a cut-off part of a text. */
  def ellipsis: String = if (unicode) "…" else "..."

  /** Brackets, times and separators. */
  def faint(text: String): String = paint(text, "90", "107;113;120")

  /** Verbs and labels. */
  def muted(text: String): String = paint(text, "2", "154;160;166")

  /** A success. */
  def pass(text: String): String = paint(text, "32", "46;194;126")

  /** Something the user should notice, but not a failure. */
  def warn(text: String): String = paint(text, "33", "201;123;67")

  /** A failure. */
  def fail(text: String): String = paint(text, "31", "214;88;79")

  /** `line` as printed at `time`. */
  def stamped(line: String, time: LocalTime): String =
    if (timestamped) s"${faint(time.format(ProgressStyle.timeOfDay))} $line" else line

  private def paint(text: String, ansi: String, rgb: String): String =
    colour match {
      case _ if text.isEmpty => text
      case Colour.None       => text
      case Colour.Ansi       => s"\u001b[${ansi}m$text\u001b[0m"
      case Colour.TrueColour => s"\u001b[38;2;${rgb}m$text\u001b[0m"
    }
}

object ProgressStyle {

  /** How colours are written. */
  enum Colour {
    case None, Ansi, TrueColour
  }

  /** No decoration at all, with the Unicode separators: what the rendering functions default to. */
  val undecorated: ProgressStyle = ProgressStyle(Colour.None, unicode = true, timestamped = false)

  private val timeOfDay = DateTimeFormatter.ofPattern("HH:mm:ss")

  /** The style for this process's stderr. */
  val detect: IO[ProgressStyle] =
    IO.delay(
      from(
        sys.env,
        terminal,
        Option(System.getProperty("stderr.encoding")).orElse(Option(System.getProperty("native.encoding"))).getOrElse("")
      )
    )

  /** The style for output going to a `terminal` or not, under the environment `env`, whose characters are written in
    * `encoding`. A dumb terminal is written to as a log; `NO_COLOR` (when not empty) only takes the colour away.
    */
  def from(env: Map[String, String], terminal: Boolean, encoding: String): ProgressStyle = {
    val log    = !terminal || env.get("TERM").contains("dumb")
    val colour =
      if (log || env.get("NO_COLOR").exists(_.nonEmpty)) Colour.None
      else if (env.get("COLORTERM").exists(Set("truecolor", "24bit").contains)) Colour.TrueColour
      else Colour.Ansi
    val utf8   = Set("utf-8", "utf8").contains(encoding.toLowerCase)

    ProgressStyle(colour, utf8, log)
  }

  /** Whether this process writes to a terminal. Before JDK 22 a console exists only when it does; from 22 on a console
    * always exists and says so itself. Getting it wrong costs only decoration.
    */
  private def terminal: Boolean =
    Option(System.console()).exists(console =>
      Try(console.getClass.getMethod("isTerminal").invoke(console).asInstanceOf[Boolean]).getOrElse(true)
    )
}
