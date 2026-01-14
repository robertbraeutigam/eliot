package com.vanillasource.eliot.eliotc.module2.fact

import com.vanillasource.eliot.eliotc.core.fact.NamedValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File

case class ModuleValue(
    file: File,
    vfqn: ValueFQN,
    dictionary: Map[String, ValueFQN],
    namedValue: NamedValue
) extends CompilerFact {
  override def key(): CompilerFactKey[ModuleValue] = ModuleValue.Key(file, vfqn)
}

object ModuleValue {
  case class Key(file: File, vfqn: ValueFQN) extends CompilerFactKey[ModuleValue]
}
