package com.vanillasource.eliot.eliotc.source.error

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.io.File

case class SourcedError(error: Sourced[String], description: Seq[String]) extends CompilerFact {
  override def key(): CompilerFactKey[SourcedError] = SourcedError.Key(error, description)
}

object SourcedError {
  case class Key(error: Sourced[String], description: Seq[String]) extends CompilerFactKey[SourcedError]

  def registerCompilerError(message: Sourced[String], description: Seq[String] = Seq.empty): CompilerIO[Unit] =
    getFactOrAbort(SourcedError.Key(message, description)).void

  def registerCompilerError(file: File, message: String): CompilerIO[Unit] =
    registerCompilerError(Sourced(file, PositionRange(Position(1, 1), Position(1, 1)), message))
}
