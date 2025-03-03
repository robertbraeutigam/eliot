package com.vanillasource.eliot.eliotc.resolve.fact

import com.vanillasource.eliot.eliotc.module.FunctionFQN
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

case class ResolvedFunction(ffqn: FunctionFQN, definition: FunctionDefinition) extends CompilerFact {
  override def key(): CompilerFactKey = ResolvedFunction.Key(ffqn)
}

object ResolvedFunction {
  case class Key(ffqn: FunctionFQN) extends CompilerFactKey {
    override type FactType = ResolvedFunction
  }
}
