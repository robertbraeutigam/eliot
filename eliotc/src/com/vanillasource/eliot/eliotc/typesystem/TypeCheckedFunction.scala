package com.vanillasource.eliot.eliotc.typesystem

import com.vanillasource.eliot.eliotc.module.FunctionFQN
import com.vanillasource.eliot.eliotc.resolve.fact.FunctionDefinition
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

case class TypeCheckedFunction(ffqn: FunctionFQN, definition: FunctionDefinition) extends CompilerFact {
  override def key(): CompilerFactKey = TypeCheckedFunction.Key(ffqn)
}

object TypeCheckedFunction {
  case class Key(ffqn: FunctionFQN) extends CompilerFactKey {
    override type FactType = TypeCheckedFunction
  }
}
