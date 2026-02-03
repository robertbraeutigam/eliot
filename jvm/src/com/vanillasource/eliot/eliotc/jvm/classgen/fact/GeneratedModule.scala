package com.vanillasource.eliot.eliotc.jvm.classgen.fact

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class GeneratedModule(moduleName: ModuleName, vfqn: ValueFQN, classFiles: Seq[ClassFile]) extends CompilerFact {
  override def key(): CompilerFactKey[GeneratedModule] = GeneratedModule.Key(moduleName, vfqn)
}

object GeneratedModule {
  case class Key(moduleName: ModuleName, vfqn: ValueFQN) extends CompilerFactKey[GeneratedModule]
}
