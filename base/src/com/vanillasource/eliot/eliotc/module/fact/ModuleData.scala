package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.ast.{DataDefinition, FunctionDefinition}
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.io.File

case class ModuleData(
    file: File,
    tfqn: TypeFQN,
    typeDictionary: Map[String, TypeFQN],
    dataDefinition: DataDefinition
) extends CompilerFact {
  override def key(): CompilerFactKey[ModuleData] = ModuleData.Key(file, tfqn)
}

object ModuleData {
  case class Key(file: File, tfqn: TypeFQN) extends CompilerFactKey[ModuleData]
}
