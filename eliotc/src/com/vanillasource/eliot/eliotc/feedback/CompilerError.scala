package com.vanillasource.eliot.eliotc.feedback

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.User.compilerGenericError
import com.vanillasource.eliot.eliotc.pos.PositionRange

import scala.io.AnsiColor.{BOLD, MAGENTA, RED, RESET}

case class CompilerError(
    message: String,
    description: Seq[String],
    contentSource: String,
    content: String,
    sourceRange: PositionRange
) {
  private def print(): IO[Unit] = {
    val fromLine        = sourceRange.from.line
    val fromCol         = sourceRange.from.col
    val toLine          = sourceRange.to.line
    val toCol           = sourceRange.to.col
    val lineMarker      = fromLine.toString
    val markerSpaces    = " ".repeat(lineMarker.length)
    val multiLinePoints = if (fromLine === sourceRange.to.line) "" else "..."

    content.linesIterator.toSeq
      .drop(fromLine - 1)
      .headOption
      .map { line =>
        if (fromLine === toLine && fromCol === toCol) {
          Seq(
            s"$BOLD$contentSource$RESET:$BOLD${RED}error$RESET:1:1:$message"
          )
        } else {
          Seq(
            s"$BOLD$contentSource$RESET:$BOLD${RED}error$RESET:$fromLine:$fromCol:$message",
            s"$MAGENTA$markerSpaces | ",
            if (fromLine === toLine) {
              // Single line error
              s"$MAGENTA$lineMarker |$RESET ${safeSubstring(line, 0, fromCol - 1)}$BOLD$RED${safeSubstring(line, fromCol - 1, toCol - 1)}$RESET${safeSubstring(line, toCol - 1, line.length)}"
            } else {
              // Multi line error
              s"$MAGENTA$lineMarker |$RESET ${line.substring(0, fromCol - 1)}$BOLD$RED${line.substring(fromCol - 1)}$RESET"
            },
            s"$MAGENTA$markerSpaces | ${" ".repeat(fromCol - 1)}$BOLD$RED${"^".repeat(toCol - fromCol)}$multiLinePoints$RESET"
          ) ++ (if (description.isEmpty) Seq.empty
                else (description ++ Seq("")).map(d => s"$MAGENTA$markerSpaces |$RESET  $d"))
        }
      }
      .map(f = lines => compilerGenericError(lines.mkString("\n")))
      .getOrElse(IO.unit)
  }

  private def safeSubstring(line: String, from: Int, to: Int): String =
    if (from >= line.length) {
      ""
    } else if (to >= line.length) {
      line.substring(from)
    } else {
      line.substring(from, to)
    }
}
