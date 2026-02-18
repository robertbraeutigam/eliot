package com.vanillasource.eliot.eliotc.abilitycheck

import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.{QualifiedName, TypedExpression}

case class AbilityCheckedValue(
    vfqn: ValueFQN,
    name: Sourced[QualifiedName],
    signature: ExpressionValue,
    runtime: Option[Sourced[TypedExpression.Expression]]
) extends CompilerFact {
  override def key(): CompilerFactKey[AbilityCheckedValue] = AbilityCheckedValue.Key(vfqn)
}

object AbilityCheckedValue {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[AbilityCheckedValue]
}
