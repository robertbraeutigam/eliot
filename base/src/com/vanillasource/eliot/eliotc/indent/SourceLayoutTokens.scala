package com.vanillasource.eliot.eliotc.indent

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.net.URI

case class SourceLayoutTokens(uri: URI, tokens: Sourced[Seq[Sourced[LayoutToken]]]) extends CompilerFact {
  override def key(): SourceLayoutTokens.Key = SourceLayoutTokens.Key(uri)
}

object SourceLayoutTokens {
  case class Key(uri: URI) extends CompilerFactKey[SourceLayoutTokens]
}
