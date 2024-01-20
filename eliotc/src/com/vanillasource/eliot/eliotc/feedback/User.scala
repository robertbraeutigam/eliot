package com.vanillasource.eliot.eliotc.feedback

import cats.effect.IO
import cats.effect.std.Console
import cats.syntax.all._
import scala.io.AnsiColor._

import java.io.File

trait User {
  self: Logging =>

  def compilerError(msg: String): IO[Unit] =
    error(s"$msg (repeated to console)") >>
      Console[IO].errorln(s"eliotc: $msg")

  def compilerError(
      file: File,
      content: String,
      fromLine: Int,
      fromCol: Int,
      toLine: Int,
      toCol: Int,
      message: String
  ): IO[Unit] = {
    val lineMarker      = fromLine.toString
    val markerSpaces    = " ".repeat(lineMarker.length)
    val multiLinePoints = if (fromLine === toLine) "" else "..."

    content.linesIterator.toSeq
      .drop(fromLine - 1)
      .headOption
      .map { line =>
        Seq(
          s"$BOLD$file$RESET:$BOLD${RED}error$RESET:$fromLine:$fromCol:$message",
          s"$MAGENTA$markerSpaces | ",
          if (fromLine === toLine) {
            // Single line error
            s"$MAGENTA$lineMarker |$RESET ${line.substring(0, fromCol - 1)}$BOLD$RED${line
                .substring(fromCol - 1, toCol - 1)}$RESET${line.substring(toCol - 1)}"
          } else {
            // Multi line error
            s"$MAGENTA$lineMarker |$RESET ${line.substring(0, fromCol - 1)}$BOLD$RED${line.substring(fromCol - 1)}$RESET"
          },
          s"$MAGENTA$markerSpaces | ${" ".repeat(fromCol - 1)}$BOLD$RED${"^".repeat(toCol - fromCol)}$multiLinePoints$RESET"
        )
      }
      .map(lines => Console[IO].errorln(lines.mkString("\n")))
      .getOrElse(IO.unit)
  }

}
