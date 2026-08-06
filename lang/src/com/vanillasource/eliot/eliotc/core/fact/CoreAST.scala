package com.vanillasource.eliot.eliotc.core.fact

import com.vanillasource.eliot.eliotc.codec.LangFactCodecs

import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.net.URI

case class CoreAST(uri: URI, ast: Sourced[AST]) extends CompilerFact {
  override def key(): CompilerFactKey[CoreAST] = CoreAST.Key(uri)
}

object CoreAST {
  case class Key(uri: URI) extends CompilerFactKey[CoreAST] {
    override def valueCodec: Option[FactCodec[CoreAST]] = Some(LangFactCodecs.coreASTCodec)
  }
}
