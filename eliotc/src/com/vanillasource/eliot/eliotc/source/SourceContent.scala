package com.vanillasource.eliot.eliotc.source

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.io.File

/** The contents of a source file.
  */
case class SourceContent(file: File, content: String) extends CompilerFact {
  override def key(): SourceContent.Key = SourceContent.Key(file)
}

object SourceContent {
  case class Key(file: File) extends CompilerFactKey {
    override type FactType = SourceContent
  }
}
