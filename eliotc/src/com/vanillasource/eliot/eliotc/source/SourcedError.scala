package com.vanillasource.eliot.eliotc.source

import cats.effect.IO
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerSignal}

import java.io.File

case class SourcedError(file: File, message: Sourced[String]) extends CompilerSignal

object SourcedError {
  def compilerError(file: File, message: Sourced[String])(using process: CompilationProcess): IO[Unit] =
    process.registerFact(SourcedError(file, message))

  def compilerError(file: File, message: String)(using process: CompilationProcess): IO[Unit] =
    compilerError(file, Sourced(PositionRange(Position(1, 1), Position(1, 1)), message))
}
