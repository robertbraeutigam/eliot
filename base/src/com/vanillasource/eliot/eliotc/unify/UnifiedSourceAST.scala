package com.vanillasource.eliot.eliotc.unify

import com.vanillasource.eliot.eliotc.ast.AST
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.nio.file.Path

case class UnifiedSourceAST(path: Path, sourcedAst: Sourced[AST]) extends CompilerFact {
  override def key(): CompilerFactKey[UnifiedSourceAST] = UnifiedSourceAST.Key(path)
}

object UnifiedSourceAST {
  case class Key(path: Path) extends CompilerFactKey[UnifiedSourceAST]
}
