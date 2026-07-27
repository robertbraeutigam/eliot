package com.vanillasource.eliot.eliotc.monomorphize.carrier

import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The **one ladder** (docs/effects-as-channel.md §3). At every application slot it reads two classifications — the
  * *expected* slot's shape and the *actual*'s [[ActualForm]] — and picks exactly one arm, with **no guards and no
  * speculative first attempt**:
  *
  *   - [[ExpectedSlot.Generic]] — a bare flex binder `?A` (`fold`'s arm): **pass-through-whole**, the whole action
  *     becomes `?A`'s value (the *extensible-conditionals* mechanism — a lazy combinator's arms pass through by
  *     ordinary parametric instantiation, zero compiler knowledge of its name);
  *   - [[ExpectedSlot.CarrierSlot]] — an effect-carrier form (the ambient / a callee's `F ~ Effect` binder, or a known
  *     carrier / pinned stack like `runMain`'s `IO[A]`): **pass-join**, the carrier joins ([[CarrierJoin]]) and the
  *     payload unifies; a *pure* actual additionally asks the caller for a `pure` lift into that carrier;
  *   - [[ExpectedSlot.PayloadSlot]] — a data / Functor / concrete slot (`printLine`'s `String`, `map`'s `xs: F[A]`):
  *     the payload fills the slot.
  *
  * The load-bearing distinction is [[ExpectedSlot.CarrierSlot]] vs [[ExpectedSlot.PayloadSlot]] when both are
  * structurally `Head[arg]`: they are told apart *only* by the effect-carrier **tag on the expected binder** (set at
  * elaboration — [[classifyExpected]]'s `isEffectCarrierSlot` predicate, which the wiring reads from
  * [[Unifier.isEffectCarrier]] / the value's ambient carriers), never by shape detection of the *actual*. On the actual
  * side the same positional recognition produces the [[ActualForm]]. Those two tags are the only "recognition" the
  * uniform foundation keeps.
  *
  * Every arm now *passes*: sequencing an effect is the row desugar's rewrite, not this ladder's
  * (docs/effects-as-rows.md A.8.9/A.8.10), so what is left here is carrier-safe **unification** — splitting the carrier
  * off before any payload unification so a carrier meta can never be stolen. This module produces the *decisions*;
  * node splicing reuses the existing [[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter]] `pureWrapNode`
  * mechanics (reshaped, not rebuilt) in the checker-side bridge.
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

  /** How the *actual* argument presents itself, as the checker's **positional carrier recognition** classified it —
    * never a shape guess. The three forms are what a slot arm needs to tell apart now that the checker no longer
    * manufactures an `Id` head for every pure judgment (docs/effects-as-rows.md A.8.10): a genuine `Id[T]` value and a
    * plain `T` are different terms, and only the former carries a payload to project.
    */
  sealed trait ActualForm {

    /** The argument's type as one whole value — what a generic slot's binder takes on. */
    def whole: SemValue

    /** The payload a slot unifies against: the carried value for a headed form, the whole type for a pure one. */
    def payload: SemValue
  }
  object ActualForm {

    /** `C[T']` on a **recognized** effect carrier (an ambient / role-flagged head). */
    case class Carried(whole: SemValue, carrier: Carrier, payload: SemValue) extends ActualForm

    /** A genuine `Id[T']` value — the identity carrier as ordinary data (a `runId` accessor's argument, an
      * `Effect[Id]` instance's own result). Its payload is projected with `runId`, never assumed away.
      */
    case class IdCarried(whole: SemValue, payload: SemValue) extends ActualForm

    /** A plain judgment with no carrier at all: the whole type *is* the payload and the term is used as it stands. */
    case class Pure(whole: SemValue) extends ActualForm {
      override def payload: SemValue = whole
    }
  }

  /** What the ladder decided at a slot. Every outcome is a **pass** — the elaboration decisions (bind, hoist) belong
    * to the row desugar and its post-drain resolver — differing only in the node the caller must build around the
    * argument to make its form match the slot's.
    */
  sealed trait Outcome
  object Outcome {

    /** Generic slot: the whole action was passed through as the binder's value. */
    case object PassWhole extends Outcome

    /** Carrier slot, carried actual: the carriers joined and the term passes through unchanged. */
    case object PassJoin extends Outcome

    /** Carrier slot, actual not (or no longer) on a carrier: the caller re-carries it into the expected carrier with a
      * `pure` lift. Erases downstream when the carrier is `Id`. `projectPayload` is set when the actual is
      * nevertheless carrier-**headed** — a genuine `Id[T]`, or a computation whose carrier meta has already been
      * solved to `Id` — so its payload must be projected with `runId` before being re-carried.
      */
    case class PureLift(projectPayload: Boolean) extends Outcome

    /** Payload slot, pure actual: the term already *is* its payload and passes through unchanged. */
    case object PayloadPass extends Outcome

    /** Payload slot, `Id`-carried actual: the caller projects the payload with `runId`. */
    case object PayloadUnwrap extends Outcome

    /** Payload slot, **carried** actual: the payload fills the slot and the effect must be sequenced at the call
      * site. That is the *hoist* shape, which the row desugar and its post-drain resolver own — the checker suspends
      * such a slot rather than routing it here, so the bridge treats this outcome as a compiler bug. The ladder still
      * produces it (and still unifies payload-with-payload, never letting the slot steal the carrier), which is what
      * the mechanism suite pins down.
      */
    case object PayloadBound extends Outcome
  }

  /** Resolve one application slot: `actual` is the argument's classified form, `expected` the classified slot. Returns
    * the updated unifier and the ladder outcome. No guards, no pre-arms — the two classifications pick the arm.
    */
  def resolveSlot(
      unifier: Unifier,
      actual: ActualForm,
      expected: ExpectedSlot,
      context: Sourced[String]
  ): (Unifier, Outcome) =
    (expected, actual) match {
      // Generic slot ⇒ the whole action is the value of ?A; runs where the consumer sequences it.
      case (ExpectedSlot.Generic(metaId), _)                                              =>
        (commit(unifier, VMeta(metaId, Spine.SNil), actual.whole, context), Outcome.PassWhole)

      // Effect-carrier slot, carried actual ⇒ the carriers join (never first-contact unify) and the payloads unify.
      // A carrier that has already been resolved to `Id` contributes nothing to the join, so the actual is a pure
      // value in carrier clothing and still needs the caller's `pure` lift — over a `runId` projection, since its
      // *value* is an `Id[T]` wrapper.
      case (ExpectedSlot.CarrierSlot(cExpected, pExpected), ActualForm.Carried(_, c, p)) =>
        val pureActual = CarrierJoin.resolve(unifier, c) == Carrier.Bottom
        val joined     = CarrierJoin.joinToward(unifier, c, cExpected, context)
        val unified    = commit(joined, p, pExpected, context)
        (unified, if (pureActual) Outcome.PureLift(projectPayload = true) else Outcome.PassJoin)

      // Effect-carrier slot, genuine `Id` actual ⇒ project its payload, unify, and lift it into the expected carrier.
      case (ExpectedSlot.CarrierSlot(_, pExpected), ActualForm.IdCarried(_, p))          =>
        (commit(unifier, p, pExpected, context), Outcome.PureLift(projectPayload = true))

      // Effect-carrier slot, pure actual ⇒ the whole type unifies with the payload and the term lifts in as it stands.
      case (ExpectedSlot.CarrierSlot(_, pExpected), _)                                   =>
        (commit(unifier, actual.payload, pExpected, context), Outcome.PureLift(projectPayload = false))

      // Payload slot ⇒ the payload fills the slot; an `Id`-carried actual has one to project, a pure one already is
      // it, and a carried one is the hoist shape the desugar owns.
      case (ExpectedSlot.PayloadSlot(shape), ActualForm.Carried(_, _, p))                =>
        (commit(unifier, p, shape, context), Outcome.PayloadBound)
      case (ExpectedSlot.PayloadSlot(shape), ActualForm.IdCarried(_, p))                 =>
        (commit(unifier, p, shape, context), Outcome.PayloadUnwrap)
      case (ExpectedSlot.PayloadSlot(shape), _)                                          =>
        (commit(unifier, actual.payload, shape, context), Outcome.PayloadPass)
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
