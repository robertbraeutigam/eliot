package com.vanillasource.eliot.eliotc.source.resolve

import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.nio.file.Path

case class ResolvedSourceContent(path: Path, contents: Seq[Sourced[String]]) extends CompilerFact {
  override def key(): CompilerFactKey[ResolvedSourceContent] = ResolvedSourceContent.Key(path)
}

object ResolvedSourceContent {
  case class Key(path: Path) extends CompilerFactKey[ResolvedSourceContent]
}
