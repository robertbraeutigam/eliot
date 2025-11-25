package com.vanillasource.eliot.eliotc.jvm

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import com.vanillasource.eliot.eliotc.source.Sourced

case class GenerateClass(moduleName: ModuleName, usedFunctions: Seq[Sourced[FunctionFQN]]) extends CompilerFact {
  override def key(): CompilerFactKey = GenerateClass.Key(moduleName)
}

object GenerateClass {
  case class Key(moduleName: ModuleName) extends CompilerFactKey {
    override type FactType = GenerateClass
  }
}
