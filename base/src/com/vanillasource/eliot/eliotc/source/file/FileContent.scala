package com.vanillasource.eliot.eliotc.source.file

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.io.File
import java.nio.file.Path

/** The contents of a source file.
  */
case class FileContent(file: File, content: String) extends CompilerFact {
  override def key(): CompilerFactKey[FileContent] = FileContent.Key(file)
}

object FileContent {
  case class Key(file: File) extends CompilerFactKey[FileContent]
}
