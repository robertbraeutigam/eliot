package com.vanillasource.eliot.eliotc.used

import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, TypeFQN}
import com.vanillasource.eliot.eliotc.pos.Sourced
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class UsedSymbols(ffqn: FunctionFQN, usedFunctions: Seq[Sourced[FunctionFQN]], usedTypes: Seq[Sourced[TypeFQN]])
    extends CompilerFact {
  override def key(): CompilerFactKey[UsedSymbols] = UsedSymbols.Key(ffqn)
}

object UsedSymbols {
  case class Key(ffqn: FunctionFQN) extends CompilerFactKey[UsedSymbols]
}
