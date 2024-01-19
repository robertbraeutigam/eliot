package com.vanillasource.eliot.eliotc.feedback

import cats.effect.IO
import cats.effect.std.Console
import scala.io.AnsiColor._

import java.io.File

trait User {
  self: Logging =>

  def compilerError(msg: String): IO[Unit] =
    error(s"$msg (repeated to console)") >>
      Console[IO].errorln(s"eliotc: $msg")

  // TODO: synchronize this
  def compilerError(
      file: File,
      content: String,
      fromLine: Int,
      fromCol: Int,
      toLine: Int,
      toCol: Int,
      message: String
  ): IO[Unit] = {
    val lineMarker   = fromLine.toString
    val markerSpaces = " ".repeat(lineMarker.length)

    content.linesIterator.toSeq.drop(fromLine - 1).headOption match
      case Some(line) =>
        for {
          _ <- Console[IO].errorln(s"$BOLD$file$RESET:$BOLD${RED}error${RESET}:$fromLine:$fromCol:$message")
          _ <- Console[IO].errorln(s"$MAGENTA$markerSpaces | ")
          _ <-
            Console[IO].errorln(
              s"$MAGENTA$lineMarker |$RESET ${line.substring(0, fromCol - 1)}$BOLD$RED${line
                  .substring(fromCol - 1, toCol - 1)}$RESET${line.substring(toCol - 1)}"
            )
          _ <- Console[IO].errorln(
                 s"$MAGENTA$markerSpaces | ${" ".repeat(fromCol - 1)}$BOLD$RED${"^".repeat(toCol - fromCol)}$RESET"
               )

        } yield ()
      case None       => IO.unit
  }

}
