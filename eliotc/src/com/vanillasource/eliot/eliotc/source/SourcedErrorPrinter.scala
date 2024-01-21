package com.vanillasource.eliot.eliotc.source

import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import scala.io.AnsiColor._
import cats.syntax.all._

import java.io.File

class SourcedErrorPrinter extends CompilerProcessor with Logging with User {
  override def process(fact: CompilerFact)(using process: CompilationProcess): IO[Unit] = fact match {
    case SourcedError(file, Sourced(PositionRange(Position(fromLine, fromCol), Position(toLine, toCol)), message)) =>
      for {
        contentOption <- process.getFact(SourceContent.Key(file))
        _             <- contentOption match
                           case Some(content) =>
                             compilerSourcedError(file, content.content, fromLine, fromCol, toLine, toCol, message)
                           case None          => compilerGlobalError(s"File contents for $file are not available.")
      } yield ()
    case _                                                                                                         => IO.unit
  }

  private def compilerSourcedError(
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
      .map(lines => compilerGenericError(lines.mkString("\n")))
      .getOrElse(IO.unit)
  }

}
