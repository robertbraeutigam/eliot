package com.vanillasource.eliot.eliotc.datafunctions

import com.vanillasource.eliot.eliotc.ast.fact.AST
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File

case class DataFunctionsSourceAST(file: File, sourcedAst: Sourced[AST]) extends CompilerFact {
  override def key(): CompilerFactKey[DataFunctionsSourceAST] = DataFunctionsSourceAST.Key(file)
}

object DataFunctionsSourceAST {
  case class Key(file: File) extends CompilerFactKey[DataFunctionsSourceAST]
}
