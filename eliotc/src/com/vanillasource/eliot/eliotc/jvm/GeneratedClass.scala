package com.vanillasource.eliot.eliotc.jvm

import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.nio.file.Path

case class GeneratedClass(moduleName: ModuleName, bytecode: Array[Byte]) extends CompilerFact {
  override def key(): CompilerFactKey = GeneratedClass.Key(moduleName)
}

object GeneratedClass {
  case class Key(moduleName: ModuleName) extends CompilerFactKey {
    override type FactType = GeneratedClass
  }
}
