package com.vanillasource.eliot.eliotc.used

import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, TypeFQN}
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

case class UsedSymbols(usedFunctions: Seq[Sourced[FunctionFQN]], usedTypes: Seq[Sourced[TypeFQN]])
    extends CompilerFact {
  override def key(): CompilerFactKey = UsedSymbols.Key()
}

object UsedSymbols {
  case class Key() extends CompilerFactKey {
    override type FactType = UsedSymbols
  }
}
