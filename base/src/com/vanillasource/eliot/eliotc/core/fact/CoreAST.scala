package com.vanillasource.eliot.eliotc.core.fact

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.net.URI

case class CoreAST(uri: URI, ast: Sourced[AST]) extends CompilerFact {
  override def key(): CompilerFactKey[CoreAST] = CoreAST.Key(uri)
}

object CoreAST {
  case class Key(uri: URI) extends CompilerFactKey[CoreAST]
}
