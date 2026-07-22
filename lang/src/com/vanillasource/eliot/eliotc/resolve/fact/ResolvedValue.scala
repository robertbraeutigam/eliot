package com.vanillasource.eliot.eliotc.resolve.fact

import com.vanillasource.eliot.eliotc.ast.fact.{EffectRow, Fixity}
import com.vanillasource.eliot.eliotc.core.fact.RoleHint
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class ResolvedValue(
    vfqn: ValueFQN,
    name: Sourced[QualifiedName],
    runtime: Option[Sourced[Expression]],
    signature: Sourced[Expression],
    paramConstraints: Map[String, Seq[ResolvedValue.ResolvedAbilityConstraint]] = Map.empty,
    fixity: Fixity = Fixity.Application,
    precedence: Seq[PrecedenceDeclaration] = Seq.empty,
    inferableArity: Int = 0,
    roleHint: RoleHint = RoleHint.NoHint,
    platform: Platform = Platform.Runtime,
    // The effects-as-channel declared effect row (effects-as-channel Phase 1, dark) — forwarded from [[NamedValue]]
    // with its entries resolved to [[ResolvedValue.ResolvedAbilityConstraint]]. Inert; never part of `signatureEquality`.
    effectRow: EffectRow[ResolvedValue.ResolvedAbilityConstraint] = EffectRow.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[ResolvedValue] = ResolvedValue.Key(vfqn, platform)
}

object ResolvedValue {
  case class ResolvedAbilityConstraint(abilityFQN: AbilityFQN, typeArgs: Seq[Expression])

  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[ResolvedValue]
}
