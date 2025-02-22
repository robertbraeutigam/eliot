package com.vanillasource.eliot.eliotc.module

import com.vanillasource.eliot.eliotc.ast.{DataDefinition, FunctionDefinition}
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

case class ModuleData(
    tfqn: TypeFQN,
    typeDictionary: Map[String, TypeFQN],
    dataDefinition: DataDefinition
) extends CompilerFact {
  override def key(): CompilerFactKey = ModuleData.Key(tfqn)
}

object ModuleData {
  case class Key(tfqn: TypeFQN) extends CompilerFactKey {
    override type FactType = ModuleData
  }
}
