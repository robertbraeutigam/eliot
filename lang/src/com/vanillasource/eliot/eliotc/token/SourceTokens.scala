package com.vanillasource.eliot.eliotc.token

import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.net.URI

case class SourceTokens(uri: URI, tokens: Sourced[Seq[Sourced[Token]]]) extends CompilerFact {
  override def key(): SourceTokens.Key = SourceTokens.Key(uri)
}

object SourceTokens {
  case class Key(uri: URI) extends CompilerFactKey[SourceTokens]
}
