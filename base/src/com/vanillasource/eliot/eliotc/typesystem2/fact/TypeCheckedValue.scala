package com.vanillasource.eliot.eliotc.typesystem2.fact

import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

case class TypeCheckedValue(vfqn: ValueFQN, definition: TypedValueDefinition) extends CompilerFact {
  override def key(): CompilerFactKey[TypeCheckedValue] = TypeCheckedValue.Key(vfqn)
}

object TypeCheckedValue {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[TypeCheckedValue]
}
