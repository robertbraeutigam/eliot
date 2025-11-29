package com.vanillasource.eliot.eliotc.ast

import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.nio.file.Path

case class SourceAST(path: Path, ast: Sourced[AST]) extends CompilerFact {
  override def key(): CompilerFactKey = SourceAST.Key(path)
}

object SourceAST {
  case class Key(path: Path) extends CompilerFactKey {
    override type FactType = SourceAST
  }
}
