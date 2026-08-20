package com.vanillasource.eliot.eliotc.operator.fact

import com.vanillasource.eliot.eliotc.codec.LangFactCodecs

import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec

import com.vanillasource.eliot.eliotc.ast.fact.EffectRow
import com.vanillasource.eliot.eliotc.core.fact.RoleHint
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityConstraint, QualifiedName}
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class OperatorResolvedValue(
    vfqn: ValueFQN,
    name: Sourced[QualifiedName],
    runtime: Option[Sourced[OperatorResolvedExpression]],
    signature: Sourced[OperatorResolvedExpression],
    paramConstraints: Map[String, Seq[AbilityConstraint[OperatorResolvedExpression]]] = Map.empty,
    inferableArity: Int = 0,
    roleHint: RoleHint = RoleHint.NoHint,
    platform: Platform = Platform.Runtime,
    // The effects-as-channel declared effect row (effects-as-channel Phase 1, dark) — forwarded from
    // [[com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredValue]] with its entry type-arguments resolved to
    // [[OperatorResolvedExpression]]. Carried unchanged through the termination/saturate wrappers to the monomorphize
    // phase. Inert; never part of `signatureEquality`.
    effectRow: EffectRow[AbilityConstraint[OperatorResolvedExpression]] = EffectRow.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[OperatorResolvedValue] = OperatorResolvedValue.Key(vfqn, platform)
}

object OperatorResolvedValue {
  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[OperatorResolvedValue] {
    override def valueCodec: Option[FactCodec[OperatorResolvedValue]] = Some(LangFactCodecs.operatorResolvedValueCodec)
  }
}
