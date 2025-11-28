package com.vanillasource.eliot.eliotc.source

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.nio.file.Path

/** The contents of a source file.
  */
case class SourceContent(path: Path, rootPath: Path, content: String) extends CompilerFact {
  override def key(): SourceContent.Key = SourceContent.Key(path)
}

object SourceContent {
  case class Key(path: Path) extends CompilerFactKey {
    override type FactType = SourceContent
  }
}
