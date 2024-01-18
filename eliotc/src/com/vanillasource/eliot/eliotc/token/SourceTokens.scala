package com.vanillasource.eliot.eliotc.token

import com.vanillasource.eliot.eliotc.CompilerFact

import java.io.File

case class SourceTokens(file: File, tokens: Seq[Sourced[Token]]) extends CompilerFact[SourceTokens.Key] {
  override def key(): SourceTokens.Key = SourceTokens.Key(file)
}

object SourceTokens {
  case class Key(file: File)
}
