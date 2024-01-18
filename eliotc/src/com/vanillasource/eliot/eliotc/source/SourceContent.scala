package com.vanillasource.eliot.eliotc.source

import com.vanillasource.eliot.eliotc.CompilerFact

import java.io.File

/** The contents of a source file.
  */
case class SourceContent(file: File, content: String) extends CompilerFact[SourceContent.Key] {
  override def key(): SourceContent.Key = SourceContent.Key(file)
}

object SourceContent {
  case class Key(file: File)
}
