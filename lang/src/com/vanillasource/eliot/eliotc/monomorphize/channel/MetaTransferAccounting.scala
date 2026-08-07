package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** The **meta-transfer accounting** of one monomorphic instance (docs/total-meta-transfers.md §2/§P2, closing the
  * TODO *"A native that produces a meta-carrying type must state its meta-information"*) — the post-monomorphization
  * witness that a value satisfies **R2**: every body-less value (a native leaf) whose return carries non-`Unit`
  * meta-information states a `^Meta` transfer for it.
  *
  * A rider on [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue]], built on the same template as
  * [[EffectAccountingProcessor]] / [[RefinementChannelProcessor]]. Producing the fact is contingent on the check
  * passing: a leaf that originates a meta-carrying return but states no transfer is reported at the value and the fact
  * declines (aborts).
  *
  * '''Not yet armed.''' The mechanism is landed and tested but not wired as a codegen precondition: nothing demands this
  * fact in a normal build yet, so it is dormant. Arming it is a one-line `getFactOrAbort(MetaTransferAccounting.Key…)`
  * in [[WovenValueProcessor]] beside the effect-accounting precondition — deferred until the meta-carrying stdlib
  * leaves (`String::length`, `parseIntInternal`, `Process::exitCode`, …) state their transfers, whose platform-dependent
  * bounds are a separate step (docs/total-meta-transfers.md §5/§P2).
  *
  * The fact carries no payload beyond its identity — it is a pass/fail witness, so it is not persisted
  * (`valueCodec = None`), like [[com.vanillasource.eliot.eliotc.monomorphize.fact.NativeBinding]].
  *
  * @param vfqn
  *   The value this accounting belongs to (the same instance identity as its `MonomorphicValue`).
  * @param typeArguments
  *   The concrete type arguments of the instance.
  */
case class MetaTransferAccounting(
    vfqn: ValueFQN,
    typeArguments: Seq[GroundValue]
) extends CompilerFact {
  override def key(): CompilerFactKey[MetaTransferAccounting] = MetaTransferAccounting.Key(vfqn, typeArguments)
}

object MetaTransferAccounting {

  /** Keyed exactly like [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue.Key]] — the same `vfqn` at
    * different type arguments is a different instance, hence a different accounting.
    */
  case class Key(vfqn: ValueFQN, typeArguments: Seq[GroundValue]) extends CompilerFactKey[MetaTransferAccounting] {
    override def valueCodec: Option[FactCodec[MetaTransferAccounting]] = None
  }
}
