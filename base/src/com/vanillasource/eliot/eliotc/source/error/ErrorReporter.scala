package com.vanillasource.eliot.eliotc.source.error

import cats.effect.IO
import cats.effect.std.Console
import cats.Monad
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.feedback.User.*
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}
import com.vanillasource.eliot.eliotc.source.content.SourceContent
import com.vanillasource.eliot.eliotc.pos.Position.{Column, Line}

import java.io.File
import scala.io.AnsiColor.{BOLD, MAGENTA, RED, RESET}

class ErrorReporter()(using Console[IO]) extends CompilerProcessor {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    factKey match {
      case SourcedError.Key(error, description) =>
        for {
          _ <- registerFactIfClear(SourcedError(error, description))
          _ <- printError(
                 error.file,
                 error.range.from.line,
                 error.range.from.col,
                 error.range.to.line,
                 error.range.to.col,
                 error.value,
                 description
               )
        } yield ()
      case _                                    => Monad[CompilerIO].unit
    }

  private def printError(
      file: File,
      fromLine: Line,
      fromCol: Column,
      toLine: Line,
      toCol: Column,
      message: String,
      description: Seq[String]
  ): CompilerIO[Unit] =
    for {
      maybeContent <- getFactOrAbort(SourceContent.Key(file)).attempt
      _            <- maybeContent match {
                        case Right(content) =>
                          IO(()).to[CompilerIO] >>
                          IO(compilerSourcedError(
                            file,
                            content.content.value,
                            fromLine,
                            fromCol,
                            toLine,
                            toCol,
                            message,
                            description
                          )).to[CompilerIO]
                        case Left(_)        =>
                          IO(()).to[CompilerIO] >>
                          IO(compilerGlobalError(s"File contents for $file are not available.")).to[CompilerIO]
                      }
    } yield ()

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
