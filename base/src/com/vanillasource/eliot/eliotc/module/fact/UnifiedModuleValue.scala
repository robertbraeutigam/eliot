package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.ast.fact.QualifiedName
import com.vanillasource.eliot.eliotc.core.fact.NamedValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class UnifiedModuleValue(
    vfqn: ValueFQN,
    dictionary: Map[QualifiedName, ValueFQN],
    namedValue: NamedValue
) extends CompilerFact {
  override def key(): CompilerFactKey[UnifiedModuleValue] = UnifiedModuleValue.Key(vfqn)
}

object UnifiedModuleValue {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[UnifiedModuleValue]
}
