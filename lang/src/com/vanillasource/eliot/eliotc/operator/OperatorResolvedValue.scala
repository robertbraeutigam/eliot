package com.vanillasource.eliot.eliotc.operator

import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, QualifiedName, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class OperatorResolvedValue(
    vfqn: ValueFQN,
    name: Sourced[QualifiedName],
    runtime: Option[Sourced[Expression]],
    typeStack: Sourced[TypeStack[Expression]],
    paramConstraints: Map[String, Seq[ResolvedValue.ResolvedAbilityConstraint]] = Map.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[OperatorResolvedValue] = OperatorResolvedValue.Key(vfqn)
}

object OperatorResolvedValue {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[OperatorResolvedValue]
}
