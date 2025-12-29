package com.vanillasource.eliot.eliotc.jvm.jargen

import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class GenerateExecutableJar(ffqn: FunctionFQN) extends CompilerFact {
  override def key(): CompilerFactKey[GenerateExecutableJar] = GenerateExecutableJar.Key(ffqn)
}

object GenerateExecutableJar {
  case class Key(ffqn: FunctionFQN) extends CompilerFactKey[GenerateExecutableJar]
}
