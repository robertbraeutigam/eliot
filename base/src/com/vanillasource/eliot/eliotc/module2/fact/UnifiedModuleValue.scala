package com.vanillasource.eliot.eliotc.module2.fact

import com.vanillasource.eliot.eliotc.core.fact.NamedValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class UnifiedModuleValue(
    vfqn: ValueFQN,
    dictionary: Map[String, ValueFQN],
    namedValue: NamedValue
) extends CompilerFact {
  override def key(): CompilerFactKey[UnifiedModuleValue] = UnifiedModuleValue.Key(vfqn)
}

object UnifiedModuleValue {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[UnifiedModuleValue]
}
