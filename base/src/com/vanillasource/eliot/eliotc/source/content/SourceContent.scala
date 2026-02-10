package com.vanillasource.eliot.eliotc.source.content

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.io.File
import java.nio.file.Path

/** The contents of a source file.
  */
case class SourceContent(file: File, content: Sourced[String]) extends CompilerFact {
  override def key(): CompilerFactKey[SourceContent] = SourceContent.Key(file)
}

object SourceContent {
  case class Key(file: File) extends CompilerFactKey[SourceContent]

  def addSource(path: Path, file: File, content: String): CompilerIO[Unit] = {
    // TODO: fix file to URI, since this is not really a file
    registerFactIfClear(SourceContent(file, Sourced(file, PositionRange.zero, content))) >>
      registerFactIfClear(PathScan(path, Seq(file)))
  }
}
