package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.ast.{DataDefinition, FunctionDefinition}
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

case class UnifiedModuleData(
    tfqn: TypeFQN,
    typeDictionary: Map[String, TypeFQN],
    dataDefinition: DataDefinition
) extends CompilerFact {
  override def key(): CompilerFactKey[UnifiedModuleData] = UnifiedModuleData.Key(tfqn)
}

object UnifiedModuleData {
  case class Key(tfqn: TypeFQN) extends CompilerFactKey[UnifiedModuleData]
}
