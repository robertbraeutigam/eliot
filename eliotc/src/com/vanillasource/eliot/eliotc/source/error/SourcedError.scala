package com.vanillasource.eliot.eliotc.source.error

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.User.*
import com.vanillasource.eliot.eliotc.source.content.SourceContent
import com.vanillasource.eliot.eliotc.source.pos.Position.{Column, Line}
import com.vanillasource.eliot.eliotc.source.pos.{Position, PositionRange, Sourced}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerSignal}
import com.vanillasource.util.CatsOps.*

import java.io.File
import scala.io.AnsiColor.{BOLD, MAGENTA, RED, RESET}

object SourcedError {
  def registerCompilerError(message: Sourced[String], description: Seq[String] = Seq.empty)(using
      process: CompilationProcess
  ): IO[Unit] =
    printError(
      message.file,
      message.range.from.line,
      message.range.from.col,
      message.range.to.line,
      message.range.to.col,
      message.value,
      description
    )

  def registerCompilerError(file: File, message: String)(using process: CompilationProcess): IO[Unit] =
    registerCompilerError(Sourced(file, PositionRange(Position(1, 1), Position(1, 1)), message))

  private def printError(
      file: File,
      fromLine: Line,
      fromCol: Column,
      toLine: Line,
      toCol: Column,
      message: String,
      description: Seq[String]
  )(using
      process: CompilationProcess
  ): IO[Unit] = {
    (for {
      content <- process.getFact(SourceContent.Key(file)).toOptionT
      _       <-
        compilerSourcedError(
          file,
          content.content.value,
          fromLine,
          fromCol,
          toLine,
          toCol,
          message,
          description
        ).liftOptionT
    } yield ()).getOrElseF(compilerGlobalError(s"File contents for $file are not available."))
  }

  private def compilerSourcedError(
      file: File,
      content: String,
      fromLine: Int,
      fromCol: Int,
      toLine: Int,
      toCol: Int,
      message: String,
      description: Seq[String]
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
