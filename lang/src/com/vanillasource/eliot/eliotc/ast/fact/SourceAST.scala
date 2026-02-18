package com.vanillasource.eliot.eliotc.ast.fact

import com.vanillasource.eliot.eliotc.ast.fact.{AST, SourceAST}
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.io.File
import java.net.URI
import java.nio.file.Path

case class SourceAST(uri: URI, ast: Sourced[AST]) extends CompilerFact {
  override def key(): CompilerFactKey[SourceAST] = SourceAST.Key(uri)
}

object SourceAST {
  case class Key(uri: URI) extends CompilerFactKey[SourceAST]
}
