package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.codec.LangFactCodecs
import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The post-monomorphization witness that every **supplied** row entry at every call in one instance's body knows what
  * it supplies (`docs/effects.md` §2.2, §3.3) — a rider on
  * [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue]] on the same template as
  * [[MetaTransferAccounting]].
  *
  * A parameter row lowers to a thunk, which erases the entry's own type arguments from the *type*, so where nothing
  * determines them the call would compile against a frame it will not meet. Producing the fact is contingent on the
  * check passing: an undetermined argument is reported at the call and the fact declines (aborts), which
  * [[WovenValueProcessor]]'s `getFactOrAbort` turns into blocked code generation.
  *
  * This is **not** an effect verifier. Verifying `derived ⊆ declared` was this fact's job under v5 and through the v6
  * flag day; the measurement D7 asked for showed the post-mono derivation could only ever see a strict subset of what
  * the pre-mono scope check ([[com.vanillasource.eliot.eliotc.row.BindingWriter]]) already reports at the reference, so
  * it was retired (`docs/effects.md` §11, D7). The effects channel has one verifier, and it is the scope check.
  *
  * The fact carries no payload beyond its identity — a pass/fail witness — but it is nonetheless **persisted**, like
  * its [[MetaTransferAccounting]] peer: a build materialises it, and it holds only an FQN and ground type arguments, so
  * it is equality-stable and can state a codec.
  *
  * @param vfqn
  *   The value this witness belongs to (the same instance identity as its `MonomorphicValue`).
  * @param typeArguments
  *   The concrete type arguments of the instance.
  */
case class SuppliedRowArguments(
    vfqn: ValueFQN,
    typeArguments: Seq[GroundValue]
) extends CompilerFact {
  override def key(): CompilerFactKey[SuppliedRowArguments] = SuppliedRowArguments.Key(vfqn, typeArguments)
}

object SuppliedRowArguments {

  /** Keyed exactly like [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue.Key]] — the same `vfqn` at
    * different type arguments is a different instance, hence a different witness.
    */
  case class Key(vfqn: ValueFQN, typeArguments: Seq[GroundValue]) extends CompilerFactKey[SuppliedRowArguments] {
    override def valueCodec: Option[FactCodec[SuppliedRowArguments]] = Some(LangFactCodecs.suppliedRowArgumentsCodec)
  }
}
