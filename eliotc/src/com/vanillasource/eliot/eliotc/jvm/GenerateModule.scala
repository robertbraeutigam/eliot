package com.vanillasource.eliot.eliotc.jvm

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.source.pos.Sourced

case class GenerateModule(
    moduleName: ModuleName,
    usedFunctions: Seq[Sourced[FunctionFQN]],
    usedTypes: Seq[Sourced[TypeFQN]]
) extends CompilerFact {
  override def key(): CompilerFactKey = GenerateModule.Key(moduleName)
}

object GenerateModule {
  case class Key(moduleName: ModuleName) extends CompilerFactKey {
    override type FactType = GenerateModule
  }
}
