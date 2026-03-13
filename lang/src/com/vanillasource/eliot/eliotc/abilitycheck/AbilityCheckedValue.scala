package com.vanillasource.eliot.eliotc.abilitycheck

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.{QualifiedName, QuantifiedType, SymbolicType, TypedExpression}

case class AbilityCheckedValue(
    vfqn: ValueFQN,
    name: Sourced[QualifiedName],
    signature: QuantifiedType,
    runtime: Option[Sourced[TypedExpression.Expression]]
) extends CompilerFact {
  override def key(): CompilerFactKey[AbilityCheckedValue] = AbilityCheckedValue.Key(vfqn)
}

object AbilityCheckedValue {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[AbilityCheckedValue]
}
