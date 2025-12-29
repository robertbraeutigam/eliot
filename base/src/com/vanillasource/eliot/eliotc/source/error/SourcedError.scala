package com.vanillasource.eliot.eliotc.source.error

import cats.effect.IO
import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange, Sourced}
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.processor.CompilationProcess.{getFact, registerFact}

import java.io.File

case class SourcedError(error: Sourced[String], description: Seq[String]) extends CompilerFact {
  override def key(): CompilerFactKey[SourcedError] = SourcedError.Key(error, description)
}

object SourcedError {
  case class Key(error: Sourced[String], description: Seq[String]) extends CompilerFactKey[SourcedError]

  def registerCompilerError(message: Sourced[String], description: Seq[String] = Seq.empty)(using
      CompilationProcess
  ): IO[Unit] = getFact(SourcedError.Key(message, description)).void

  def registerCompilerError(file: File, message: String)(using CompilationProcess): IO[Unit] =
    registerCompilerError(Sourced(file, PositionRange(Position(1, 1), Position(1, 1)), message))
}
