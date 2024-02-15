package com.vanillasource.eliot.eliotc.source

import cats.effect.IO
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerSignal}

import java.io.File

case class SourcedError(message: Sourced[String]) extends CompilerSignal

object SourcedError {
  def compilerError(message: Sourced[String])(using process: CompilationProcess): IO[Unit] =
    process.registerFact(SourcedError(message))

  def compilerError(file: File, message: String)(using process: CompilationProcess): IO[Unit] =
    compilerError(Sourced(file, PositionRange(Position(1, 1), Position(1, 1)), message))
}
