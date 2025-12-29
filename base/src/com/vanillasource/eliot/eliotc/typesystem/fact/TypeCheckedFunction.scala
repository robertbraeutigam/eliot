package com.vanillasource.eliot.eliotc.typesystem.fact

import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class TypeCheckedFunction(ffqn: FunctionFQN, definition: TypedFunctionDefinition) extends CompilerFact {
  override def key(): CompilerFactKey[TypeCheckedFunction] = TypeCheckedFunction.Key(ffqn)
}

object TypeCheckedFunction {
  case class Key(ffqn: FunctionFQN) extends CompilerFactKey[TypeCheckedFunction]
}
