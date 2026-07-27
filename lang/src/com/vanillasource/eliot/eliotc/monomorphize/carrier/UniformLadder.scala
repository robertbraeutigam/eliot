package com.vanillasource.eliot.eliotc.monomorphize.carrier

import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The **one ladder** (docs/effects-as-channel.md §3), productionised from the U2 spike onto real [[SemValue]] /
  * [[Unifier]] — **not yet wired into the [[com.vanillasource.eliot.eliotc.monomorphize.check.Checker]]**, so the
  * default path stays byte-identical. At every application slot the ladder reads the *expected* slot's classified shape
  * and picks exactly one arm, with **no guards and no speculative first attempt**:
  *
  *   - [[ExpectedSlot.Generic]] — a bare flex binder `?A` (`fold`'s arm): **pass-through-whole**, the whole
  *     carrier-headed action becomes `?A`'s value (the *extensible-conditionals* mechanism — a lazy combinator's arms
  *     pass through by ordinary parametric instantiation, zero compiler knowledge of its name);
  *   - [[ExpectedSlot.CarrierSlot]] — an effect-carrier form (the ambient / a callee's `F ~ Effect` binder, or a known
  *     carrier / pinned stack like `runMain`'s `IO[A]`): **pass-join**, the carrier joins ([[CarrierJoin]]), the payload
  *     unifies, and a *pure* (bottom-carriered) actual is flagged for the caller's re-carrying lift;
  *   - [[ExpectedSlot.PayloadSlot]] — a data / Functor / concrete slot (`printLine`'s `String`, `map`'s `xs: F[A]`):
  *     **bind**, the payload fills the slot and the carrier is sequenced at the call site.
  *
  * The load-bearing distinction is [[ExpectedSlot.CarrierSlot]] vs [[ExpectedSlot.PayloadSlot]] when both are
  * structurally `Head[arg]`: they are told apart *only* by the effect-carrier **tag on the expected binder** (set at
  * elaboration — [[classifyExpected]]'s `isEffectCarrierSlot` predicate, which the wiring reads from
  * [[Unifier.isEffectCarrier]] / the value's ambient carriers), never by shape detection of the *actual*. That
  * positional tag is the only "recognition" the uniform foundation keeps.
  *
  * This module produces the *decisions*; node splicing reuses the existing
  * [[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter]] `pureWrapNode`/`bindWrap` mechanics (reshaped, not
  * rebuilt) in the checker-side bridge.
  */
object UniformLadder {

  /** The callee's declared parameter slot, classified off its elaborated signature. */
  sealed trait ExpectedSlot
  object ExpectedSlot {

    /** A bare flex binder `?A` — the suspended action passes through whole. */
    case class Generic(metaId: MetaId) extends ExpectedSlot

    /** An effect-carrier form, split into its expected `carrier` (a meta for the ambient / a concrete carrier) and
      * `payload`. The carrier joins; the payload unifies.
      */
    case class CarrierSlot(carrier: Carrier, payload: SemValue) extends ExpectedSlot

    /** A data / Functor / concrete slot — the whole expected type against which the actual's *payload* is bound. */
    case class PayloadSlot(shape: SemValue) extends ExpectedSlot
  }

  /** What the ladder decided at a slot. */
  sealed trait Outcome
  object Outcome {

    /** Generic slot: the whole action was passed through as the binder's value. */
    case object PassWhole extends Outcome

    /** Carrier slot: the carrier joined. `pureActual` is set when the actual was pure (bottom-carriered), so the
      * caller re-carries it into the expected carrier with a `pure` lift.
      */
    case class PassJoin(pureActual: Boolean) extends Outcome

    /** Payload slot: the effect binds at the call site over `carrier`. */
    case class Bound(carrier: Carrier) extends Outcome
  }

  /** Resolve one application slot: `actual` is the argument's (carrier-headed) type, `expected` the classified slot.
    * Returns the updated unifier and the ladder outcome. No guards, no pre-arms — the classification picks the arm.
    */
  def resolveSlot(
      unifier: Unifier,
      actual: SemValue,
      expected: ExpectedSlot,
      context: Sourced[String]
  ): (Unifier, Outcome) =
    expected match {
      // Generic slot ⇒ the whole carrier-headed action is the value of ?A; runs where the consumer sequences it.
      case ExpectedSlot.Generic(metaId)         =>
        (commit(unifier, VMeta(metaId, Spine.SNil), actual, context), Outcome.PassWhole)

      // Effect-carrier slot ⇒ carrier joins (Id no-op), payload unifies; ONLY a pure (bottom-carriered) actual is
      // flagged, so the caller re-carries it as `pure@Effect[C]` — a node that erases when `C` defaults to `Id`.
      case ExpectedSlot.CarrierSlot(cExpected, pExpected) =>
        val (actualCarrier, actualPayload) = Carrier.split(actual)
        val joined                          = CarrierJoin.joinToward(unifier, actualCarrier, cExpected, context)
        val unified                         = commit(joined, actualPayload, pExpected, context)
        val pureActual                      = CarrierJoin.resolve(unifier, actualCarrier) == Carrier.Bottom
        (unified, Outcome.PassJoin(pureActual))

      // Payload slot ⇒ bind: the effect runs at the call site; the payload fills the slot.
      case ExpectedSlot.PayloadSlot(shape)      =>
        val (actualCarrier, actualPayload) = Carrier.split(actual)
        (commit(unifier, actualPayload, shape, context), Outcome.Bound(actualCarrier))
    }

  /** Classify an *expected* slot type off its elaborated shape (the surviving positional recognition):
    *   - a bare flex meta `?A` ⇒ [[ExpectedSlot.Generic]];
    *   - a `Head[arg]` whose head is tagged an effect carrier (`isEffectCarrierSlot`) ⇒ [[ExpectedSlot.CarrierSlot]]
    *     (split into carrier + payload);
    *   - anything else (a data / Functor / concrete slot) ⇒ [[ExpectedSlot.PayloadSlot]] over the whole type.
    *
    * `isEffectCarrierSlot` is the tag the wiring reads from [[Unifier.isEffectCarrier]] / the value's ambient carriers,
    * on the **expected** side — never a shape guess about the actual.
    */
  def classifyExpected(expected: SemValue, isEffectCarrierSlot: SemValue => Boolean): ExpectedSlot =
    expected match {
      case VMeta(metaId, Spine.SNil)              => ExpectedSlot.Generic(metaId)
      case _ if hasSpine(expected) && isEffectCarrierSlot(expected) =>
        val (carrier, payload) = Carrier.split(expected)
        ExpectedSlot.CarrierSlot(carrier, payload)
      case _                                      => ExpectedSlot.PayloadSlot(expected)
    }

  private def hasSpine(sv: SemValue): Boolean = sv match {
    case VMeta(_, Spine.SApp(_, _))      => true
    case VTopDef(_, _, Spine.SApp(_, _)) => true
    case _                               => false
  }

  /** Commit a payload unification (recording a mismatch into the unifier's errors on failure). Payload unification only
    * ever sees payloads — the carrier head was split off first — so ordinary [[Unifier.unify]] can never steal a
    * carrier meta.
    */
  private def commit(unifier: Unifier, actual: SemValue, expected: SemValue, context: Sourced[String]): Unifier =
    unifier.unify(actual, expected, context)
}
