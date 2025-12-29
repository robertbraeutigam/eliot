package com.vanillasource.eliot.eliotc.source.content

import cats.effect.IO
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.processor.CompilationProcess.registerFact
import com.vanillasource.eliot.eliotc.source.pos.{PositionRange, Sourced}

import java.io.File

/** The contents of a source file.
  */
case class SourceContent(file: File, content: Sourced[String]) extends CompilerFact {
  override def key(): CompilerFactKey[SourceContent] = SourceContent.Key(file)
}

object SourceContent {
  case class Key(file: File) extends CompilerFactKey[SourceContent]

  def addSource(file: File, content: String)(using CompilationProcess): IO[Unit] =
    registerFact(SourceContent(file, Sourced(file, PositionRange.zero, content)))
}
