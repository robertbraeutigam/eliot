package com.vanillasource.eliot.eliotc.jvm.classgen

import com.vanillasource.eliot.eliotc.jvm.asm.ClassFile
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class GeneratedModule(moduleName: ModuleName, ffqn: FunctionFQN, classFiles: Seq[ClassFile]) extends CompilerFact {
  override def key(): CompilerFactKey[GeneratedModule] = GeneratedModule.Key(moduleName, ffqn)
}

object GeneratedModule {
  case class Key(moduleName: ModuleName, ffqn: FunctionFQN) extends CompilerFactKey[GeneratedModule]
}
