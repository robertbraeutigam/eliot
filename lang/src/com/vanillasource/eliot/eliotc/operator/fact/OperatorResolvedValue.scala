package com.vanillasource.eliot.eliotc.operator.fact

import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityFQN, QualifiedName}
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class OperatorResolvedValue(
    vfqn: ValueFQN,
    name: Sourced[QualifiedName],
    runtime: Option[Sourced[OperatorResolvedExpression]],
    typeStack: Sourced[TypeStack[OperatorResolvedExpression]],
    paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]] = Map.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[OperatorResolvedValue] = OperatorResolvedValue.Key(vfqn)
}

object OperatorResolvedValue {
  case class ResolvedAbilityConstraint(abilityFQN: AbilityFQN, typeArgs: Seq[OperatorResolvedExpression])

  case class Key(vfqn: ValueFQN) extends CompilerFactKey[OperatorResolvedValue]
}
