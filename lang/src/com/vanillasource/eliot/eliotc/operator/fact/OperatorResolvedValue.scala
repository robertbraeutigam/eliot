package com.vanillasource.eliot.eliotc.operator.fact

import com.vanillasource.eliot.eliotc.core.fact.RoleHint
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
    signature: Sourced[OperatorResolvedExpression],
    paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]] = Map.empty,
    inferableArity: Int = 0,
    roleHint: RoleHint = RoleHint.NoHint,
    platform: Platform = Platform.Runtime,
    // The ability FQNs this value discharges (the negative `{…, -E}` members). This fed the retired pre-mono
    // discharge accounting; the monomorphize-phase residual check now reads discharge structurally (an effect on an
    // inner transformer carrier is simply absent from the ambient residual), so the field is currently unconsumed and
    // is scheduled for removal along with the `-E` surface syntax (effect-accounting-in-monomorphize.md, Step 4).
    // Empty for every ordinary value; populated only for the discharger primitives (`else`, `catch`, `runStateTo…`).
    dischargedEffects: Seq[AbilityFQN] = Seq.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[OperatorResolvedValue] = OperatorResolvedValue.Key(vfqn, platform)
}

object OperatorResolvedValue {
  case class ResolvedAbilityConstraint(abilityFQN: AbilityFQN, typeArgs: Seq[OperatorResolvedExpression])

  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[OperatorResolvedValue]
}
