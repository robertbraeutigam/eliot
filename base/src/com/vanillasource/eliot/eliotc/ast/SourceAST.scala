package com.vanillasource.eliot.eliotc.ast

import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File
import java.nio.file.Path

case class SourceAST(file: File, ast: Sourced[AST]) extends CompilerFact {
  override def key(): CompilerFactKey[SourceAST] = SourceAST.Key(file)
}

object SourceAST {
  case class Key(file: File) extends CompilerFactKey[SourceAST]
}
