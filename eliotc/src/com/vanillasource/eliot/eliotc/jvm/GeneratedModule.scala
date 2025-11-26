package com.vanillasource.eliot.eliotc.jvm

import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.nio.file.Path

case class GeneratedModule(moduleName: ModuleName, bytecode: Array[Byte]) extends CompilerFact {
  override def key(): CompilerFactKey = GeneratedModule.Key(moduleName)
}

object GeneratedModule {
  case class Key(moduleName: ModuleName) extends CompilerFactKey {
    override type FactType = GeneratedModule
  }
}
