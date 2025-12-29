package com.vanillasource.eliot.eliotc.sugar

import com.vanillasource.eliot.eliotc.ast.AST
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File

case class DesugaredSourceAST(file: File, sourcedAst: Sourced[AST]) extends CompilerFact {
  override def key(): CompilerFactKey[DesugaredSourceAST] = DesugaredSourceAST.Key(file)
}

object DesugaredSourceAST {
  case class Key(file: File) extends CompilerFactKey[DesugaredSourceAST]
}
