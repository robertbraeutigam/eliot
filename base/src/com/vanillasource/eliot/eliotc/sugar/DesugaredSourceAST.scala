package com.vanillasource.eliot.eliotc.sugar

import com.vanillasource.eliot.eliotc.ast.AST
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.nio.file.Path

case class DesugaredSourceAST(path: Path, sourcedAst: Sourced[AST]) extends CompilerFact {
  override def key(): CompilerFactKey[DesugaredSourceAST] = DesugaredSourceAST.Key(path)
}

object DesugaredSourceAST {
  case class Key(path: Path) extends CompilerFactKey[DesugaredSourceAST]
}
