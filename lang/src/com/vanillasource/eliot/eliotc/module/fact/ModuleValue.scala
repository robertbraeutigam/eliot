package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.core.fact.QualifiedName
import com.vanillasource.eliot.eliotc.core.fact.NamedValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.net.URI

case class ModuleValue(
    uri: URI,
    vfqn: ValueFQN,
    dictionary: Map[QualifiedName, ValueFQN],
    namedValue: NamedValue,
    privateNames: Map[QualifiedName, ValueFQN] = Map.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[ModuleValue] = ModuleValue.Key(uri, vfqn)
}

object ModuleValue {
  case class Key(uri: URI, vfqn: ValueFQN) extends CompilerFactKey[ModuleValue]
}
