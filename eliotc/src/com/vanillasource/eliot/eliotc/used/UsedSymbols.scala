package com.vanillasource.eliot.eliotc.used

import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

case class UsedSymbols(usedFunctions: Map[FunctionFQN, Sourced[_]]) extends CompilerFact {
  override def key(): CompilerFactKey = UsedSymbols.Key()
}

object UsedSymbols {
  case class Key() extends CompilerFactKey {
    override type FactType = UsedSymbols
  }
}
