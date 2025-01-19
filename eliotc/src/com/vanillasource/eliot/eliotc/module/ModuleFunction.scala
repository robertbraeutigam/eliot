package com.vanillasource.eliot.eliotc.module

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.ast.FunctionDefinition

case class ModuleFunction(
    ffqn: FunctionFQN,
    functionDictionary: Map[String, FunctionFQN],
    typeDictionary: Map[String, TypeFQN],
    functionDefinition: FunctionDefinition
) extends CompilerFact {
  override def key(): CompilerFactKey = ModuleFunction.Key(ffqn)
}

object ModuleFunction {
  case class Key(ffqn: FunctionFQN) extends CompilerFactKey {
    override type FactType = ModuleFunction
  }
}
