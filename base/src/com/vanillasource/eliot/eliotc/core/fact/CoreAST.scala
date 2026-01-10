package com.vanillasource.eliot.eliotc.core.fact

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.io.File

case class CoreAST(file: File, ast: Sourced[AST]) extends CompilerFact {
  override def key(): CompilerFactKey[CoreAST] = CoreAST.Key(file)
}

object CoreAST {
  case class Key(file: File) extends CompilerFactKey[CoreAST]
}
