package com.vanillasource.eliot.eliotc.token

import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.nio.file.Path

case class SourceTokens(path: Path, rootPath: Path, tokens: Seq[Sourced[Token]]) extends CompilerFact {
  override def key(): SourceTokens.Key = SourceTokens.Key(path)
}

object SourceTokens {
  case class Key(path: Path) extends CompilerFactKey {
    override type FactType = SourceTokens
  }
}
