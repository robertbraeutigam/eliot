package com.vanillasource.eliot.eliotc.sugar

import com.vanillasource.eliot.eliotc.ast.AST
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.io.File

case class DesugaredSourceAST(file: File, rootPath: File, ast: AST) extends CompilerFact {
  override def key(): CompilerFactKey = DesugaredSourceAST.Key(file)
}

object DesugaredSourceAST {
  case class Key(file: File) extends CompilerFactKey {
    override type FactType = DesugaredSourceAST
  }
}
