package com.vanillasource.eliot.eliotc.source.resolve

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.nio.file.Path

case class ResolvedSourceContent(path: Path, rootPath: Path, content: String) extends CompilerFact {
  override def key(): ResolvedSourceContent.Key = ResolvedSourceContent.Key(path)
}

object ResolvedSourceContent {
  case class Key(path: Path) extends CompilerFactKey {
    override type FactType = ResolvedSourceContent
  }
}
