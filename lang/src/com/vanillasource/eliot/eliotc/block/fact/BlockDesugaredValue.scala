package com.vanillasource.eliot.eliotc.block.fact

import com.vanillasource.eliot.eliotc.codec.LangFactCodecs

import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec

import com.vanillasource.eliot.eliotc.ast.fact.{EffectRow, Fixity}
import com.vanillasource.eliot.eliotc.core.fact.RoleHint
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.{
  AbilityConstraint,
  Expression,
  PrecedenceDeclaration,
  QualifiedName,
  ResolvedValue
}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A resolved value with every `{ … }` [[Expression.BlockExpression]] in its runtime body lowered to a tower of
  * immediately-applied lambdas (see `block.processor.BlockDesugaringProcessor`). Structurally identical to
  * [[ResolvedValue]] otherwise — it is the input the match desugarer (and everything after it) consumes, so the block
  * node never reaches those phases.
  */
case class BlockDesugaredValue(
    vfqn: ValueFQN,
    name: Sourced[QualifiedName],
    runtime: Option[Sourced[Expression]],
    signature: Sourced[Expression],
    paramConstraints: Map[String, Seq[AbilityConstraint[Expression]]] = Map.empty,
    fixity: Fixity = Fixity.Application,
    precedence: Seq[PrecedenceDeclaration] = Seq.empty,
    inferableArity: Int = 0,
    roleHint: RoleHint = RoleHint.NoHint,
    platform: Platform = Platform.Runtime,
    // The effects-as-channel declared effect row (effects-as-channel Phase 1, dark) — carried through unchanged from
    // [[ResolvedValue]] (block desugaring does not touch signatures), reusing the resolve entry representation.
    effectRow: EffectRow[AbilityConstraint[Expression]] = EffectRow.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[BlockDesugaredValue] = BlockDesugaredValue.Key(vfqn, platform)
}

object BlockDesugaredValue {
  case class Key(vfqn: ValueFQN, platform: Platform = Platform.Runtime) extends CompilerFactKey[BlockDesugaredValue] {
    override def valueCodec: Option[FactCodec[BlockDesugaredValue]] = Some(LangFactCodecs.blockDesugaredValueCodec)
  }
}
