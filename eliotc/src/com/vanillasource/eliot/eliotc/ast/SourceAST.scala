package com.vanillasource.eliot.eliotc.ast

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.io.File

case class SourceAST(file: File, rootPath: File, ast: AST) extends CompilerFact {
  override def key(): CompilerFactKey = SourceAST.Key(file)
}

object SourceAST {
  case class Key(file: File) extends CompilerFactKey {
    override type FactType = SourceAST
  }
}
