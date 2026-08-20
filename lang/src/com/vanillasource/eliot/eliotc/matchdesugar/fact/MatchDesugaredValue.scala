package com.vanillasource.eliot.eliotc.matchdesugar.fact

import com.vanillasource.eliot.eliotc.codec.LangFactCodecs

import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec

import com.vanillasource.eliot.eliotc.ast.fact.{EffectRow, Fixity}
import com.vanillasource.eliot.eliotc.core.fact.RoleHint
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityConstraint, PrecedenceDeclaration, QualifiedName}
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class MatchDesugaredValue(
    vfqn: ValueFQN,
    name: Sourced[QualifiedName],
    runtime: Option[Sourced[MatchDesugaredExpression]],
    signature: Sourced[MatchDesugaredExpression],
    paramConstraints: Map[String, Seq[AbilityConstraint[MatchDesugaredExpression]]] = Map.empty,
    fixity: Fixity = Fixity.Application,
    precedence: Seq[PrecedenceDeclaration] = Seq.empty,
    inferableArity: Int = 0,
    roleHint: RoleHint = RoleHint.NoHint,
    platform: Platform = Platform.Runtime,
    // The effects-as-channel declared effect row (effects-as-channel Phase 1, dark) — forwarded from
    // [[BlockDesugaredValue]] with its entry type-arguments re-expressed as [[MatchDesugaredExpression]]. Inert.
    effectRow: EffectRow[AbilityConstraint[MatchDesugaredExpression]] = EffectRow.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[MatchDesugaredValue] = MatchDesugaredValue.Key(vfqn, platform)
}

object MatchDesugaredValue {
  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[MatchDesugaredValue] {
    override def valueCodec: Option[FactCodec[MatchDesugaredValue]] = Some(LangFactCodecs.matchDesugaredValueCodec)
  }
}
