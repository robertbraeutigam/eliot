package com.vanillasource.eliot.eliotc.jvm.classgen

import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class GenerateModule(
    moduleName: ModuleName,
    usedFunctions: Seq[Sourced[FunctionFQN]],
    usedTypes: Seq[Sourced[TypeFQN]]
) extends CompilerFact {
  override def key(): CompilerFactKey[GenerateModule] = GenerateModule.Key(moduleName)
}

object GenerateModule {
  case class Key(moduleName: ModuleName) extends CompilerFactKey[GenerateModule]
}
