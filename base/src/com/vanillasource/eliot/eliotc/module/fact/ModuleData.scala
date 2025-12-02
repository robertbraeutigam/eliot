package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.ast.{DataDefinition, FunctionDefinition}
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

case class ModuleData(
    tfqn: TypeFQN,
    typeDictionary: Map[String, TypeFQN],
    dataDefinition: DataDefinition
) extends CompilerFact {
  override def key(): CompilerFactKey[ModuleData] = ModuleData.Key(tfqn)
}

object ModuleData {
  case class Key(tfqn: TypeFQN) extends CompilerFactKey[ModuleData]
}
