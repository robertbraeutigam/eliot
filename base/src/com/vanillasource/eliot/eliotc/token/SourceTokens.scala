package com.vanillasource.eliot.eliotc.token

import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File

case class SourceTokens(file: File, tokens: Sourced[Seq[Sourced[Token]]]) extends CompilerFact {
  override def key(): SourceTokens.Key = SourceTokens.Key(file)
}

object SourceTokens {
  case class Key(file: File) extends CompilerFactKey[SourceTokens]
}
