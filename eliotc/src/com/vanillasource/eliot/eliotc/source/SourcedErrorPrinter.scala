package com.vanillasource.eliot.eliotc.source

import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

import scala.io.AnsiColor.*
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.Position.{Column, Line}

import java.io.File
import scala.math.*
import com.vanillasource.util.CatsOps.*

class SourcedErrorPrinter extends CompilerProcessor with Logging with User {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match {
    case SourcedError(Sourced(file, PositionRange(Position(fromLine, fromCol), Position(toLine, toCol)), message)) =>
      process(file, fromLine, fromCol, toLine, toCol, message)
    case _                                                                                                         => IO.unit
  }

  private def process(file: File, fromLine: Line, fromCol: Column, toLine: Line, toCol: Column, message: String)(using
      process: CompilationProcess
  ): IO[Unit] = {
    (for {
      content <- process.getFact(SourceContent.Key(file)).toOptionT
      _       <- compilerSourcedError(file, content.content, fromLine, fromCol, toLine, toCol, message).liftOptionT
    } yield ()).getOrElseF(compilerGlobalError(s"File contents for $file are not available."))
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
        if (fromLine === toLine && fromCol === toCol) {
          Seq(
            s"$BOLD$file$RESET:$BOLD${RED}error$RESET:1:1:$message"
          )
        } else {
          Seq(
            s"$BOLD$file$RESET:$BOLD${RED}error$RESET:$fromLine:$fromCol:$message",
            s"$MAGENTA$markerSpaces | ",
            if (fromLine === toLine) {
              // Single line error
              s"$MAGENTA$lineMarker |$RESET ${safeSubstring(line, 0, fromCol - 1)}$BOLD$RED${safeSubstring(line, fromCol - 1, toCol - 1)}$RESET${safeSubstring(line, toCol - 1, line.length)}"
            } else {
              // Multi line error
              s"$MAGENTA$lineMarker |$RESET ${line.substring(0, fromCol - 1)}$BOLD$RED${line.substring(fromCol - 1)}$RESET"
            },
            s"$MAGENTA$markerSpaces | ${" ".repeat(fromCol - 1)}$BOLD$RED${"^".repeat(toCol - fromCol)}$multiLinePoints$RESET"
          )
        }
      }
      .map(lines => compilerGenericError(lines.mkString("\n")))
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
