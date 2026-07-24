package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN

/** The effects-as-channel **effect accounting** of one monomorphic instance (docs/effects-as-channel.md §5) — the
  * *derived effect row* of a value's body, computed post-monomorphization by a bottom-up walk and verified against the
  * value's *declared* row.
  *
  * This is the effects analogue of [[RefinementTable]] (the same [[RefinementChannelProcessor]] template): a rider on
  * the already-computed [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue]] that reads the body's
  * effect operations (abstract effect-ability references, machinery excluded) and propagates each ordinary callee's
  * declared row, unioning them into the value's derived row. Producing the fact is contingent on the verification
  * `derived ⊆ declared` passing — an undeclared effect is reported at the value and the fact declines (aborts), the
  * fail-safe that keeps a leaking value from reaching the weaver.
  *
  * The **sole effect verifier** (U4-c-2): the pre-mono in-checker `EffectResidualChecker` is deleted, and this fact's
  * production is a codegen precondition. It is also the intended LSP hover source for a value's effect row.
  *
  * @param vfqn
  *   The value this accounting belongs to (the same instance identity as its `MonomorphicValue`).
  * @param typeArguments
  *   The concrete type arguments of the instance.
  * @param derivedRow
  *   The user-facing effect abilities the body performs or propagates (machinery excluded). A subset of the value's
  *   declared row — verified so before the fact is produced.
  */
case class EffectAccounting(
    vfqn: ValueFQN,
    typeArguments: Seq[GroundValue],
    derivedRow: Set[AbilityFQN]
) extends CompilerFact {
  override def key(): CompilerFactKey[EffectAccounting] = EffectAccounting.Key(vfqn, typeArguments)
}

object EffectAccounting {

  /** Keyed exactly like [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue.Key]] — the same `vfqn` at
    * different type arguments is a different instance, hence a different accounting.
    */
  case class Key(vfqn: ValueFQN, typeArguments: Seq[GroundValue]) extends CompilerFactKey[EffectAccounting]
}
