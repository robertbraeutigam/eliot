package com.vanillasource.eliot.eliotc.operator.fact

import com.vanillasource.eliot.eliotc.core.fact.{RoleHint, TypeStack}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityFQN, QualifiedName}
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class OperatorResolvedValue(
    vfqn: ValueFQN,
    name: Sourced[QualifiedName],
    runtime: Option[Sourced[OperatorResolvedExpression]],
    typeStack: Sourced[TypeStack[OperatorResolvedExpression]],
    paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]] = Map.empty,
    inferableArity: Int = 0,
    roleHint: RoleHint = RoleHint.NoHint,
    // True when the return position is a bare under-applied omittable (`auto`) reference (e.g. a bare `Int` or a
    // W2-grown `Counter` return), so the return is *calculated* from the body rather than read from the source type
    // stack (implicit-generics, W3). Set by `SaturatedValueProcessor`; consumed by the monomorphize
    // checker — the callee infers the return from its body, the caller reads it off the callee's `MonomorphicValue`.
    calculatedReturn: Boolean = false,
    platform: Platform = Platform.Runtime,
    // The ability FQNs this value discharges (the negative `{…, -E}` members). The effect accounting
    // ([[com.vanillasource.eliot.eliotc.effect.processor.EffectUsageCollector]]) subtracts these from a caller's
    // used-effect set when the caller invokes this value directly (discharge-aware accounting, Step 2). Empty for
    // every ordinary value; populated only for the discharger primitives (`else`, `catch`, `runStateTo…`).
    dischargedEffects: Seq[AbilityFQN] = Seq.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[OperatorResolvedValue] = OperatorResolvedValue.Key(vfqn, platform)
}

object OperatorResolvedValue {
  case class ResolvedAbilityConstraint(abilityFQN: AbilityFQN, typeArgs: Seq[OperatorResolvedExpression])

  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[OperatorResolvedValue]
}
