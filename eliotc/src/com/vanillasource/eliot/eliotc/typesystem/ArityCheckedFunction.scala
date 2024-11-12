package com.vanillasource.eliot.eliotc.typesystem

import com.vanillasource.eliot.eliotc.module.FunctionFQN
import com.vanillasource.eliot.eliotc.resolve.FunctionDefinition
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

case class ArityCheckedFunction(ffqn: FunctionFQN, definition: FunctionDefinition) extends CompilerFact {
  override def key(): CompilerFactKey = ArityCheckedFunction.Key(ffqn)
}

object ArityCheckedFunction {
  case class Key(ffqn: FunctionFQN) extends CompilerFactKey {
    override type FactType = ArityCheckedFunction
  }
}
