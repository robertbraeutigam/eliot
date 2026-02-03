package com.vanillasource.eliot.eliotc.jvm.jargen

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class GenerateExecutableJar(vfqn: ValueFQN) extends CompilerFact {
  override def key(): CompilerFactKey[GenerateExecutableJar] = GenerateExecutableJar.Key(vfqn)
}

object GenerateExecutableJar {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[GenerateExecutableJar]
}
