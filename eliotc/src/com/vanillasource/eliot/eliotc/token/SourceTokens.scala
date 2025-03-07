package com.vanillasource.eliot.eliotc.token

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.Sourced

import java.io.File

case class SourceTokens(file: File, rootPath: File, tokens: Seq[Sourced[Token]]) extends CompilerFact {
  override def key(): SourceTokens.Key = SourceTokens.Key(file)
}

object SourceTokens {
  case class Key(file: File) extends CompilerFactKey {
    override type FactType = SourceTokens
  }
}
