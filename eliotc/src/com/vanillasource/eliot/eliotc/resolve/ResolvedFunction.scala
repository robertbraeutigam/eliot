package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.module.FunctionFQN

case class ResolvedFunction(ffqn: FunctionFQN, definition: FunctionDefinition) extends CompilerFact {
  override def key(): CompilerFactKey = ResolvedFunction.Key(ffqn)
}

object ResolvedFunction {
  case class Key(ffqn: FunctionFQN) extends CompilerFactKey {
    override type FactType = ResolvedFunction
  }
}
