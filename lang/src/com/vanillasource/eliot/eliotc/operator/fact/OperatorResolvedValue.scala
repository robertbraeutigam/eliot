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
    paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]] = Map.empty,
    opaque: Boolean = false
) extends CompilerFact {
  override def key(): CompilerFactKey[OperatorResolvedValue] = OperatorResolvedValue.Key(vfqn)

  /** The body as seen during type checking: an `opaque` definition presents as body-less (stuck), so checker-phase
    * evaluators never unfold it (keeping e.g. `Int[0,255]` distinct from `Int[0,1000]`). The body stays in [[runtime]]
    * for post-checking phases (representation lowering) to unfold. Mirrors the guard in `UserValueNativesProcessor`;
    * every checker-phase reader that builds an evaluator from a value's own body must use this, not [[runtime]].
    */
  def checkingRuntime: Option[Sourced[OperatorResolvedExpression]] =
    if (opaque) None else runtime
}

object OperatorResolvedValue {
  case class ResolvedAbilityConstraint(abilityFQN: AbilityFQN, typeArgs: Seq[OperatorResolvedExpression])

  case class Key(vfqn: ValueFQN) extends CompilerFactKey[OperatorResolvedValue]
}
