package com.vanillasource.eliot.eliotc.monomorphize.check

import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes
import com.vanillasource.eliot.eliotc.monomorphize.carrier.{Carrier, CarrierJoin, UniformLadder}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The **checker-side bridge** for the uniform-carrier foundation (docs/effects-as-channel.md §3, U3a) — it lifts the
  * pure domain mechanism ([[com.vanillasource.eliot.eliotc.monomorphize.carrier.Carrier]] /
  * [[com.vanillasource.eliot.eliotc.monomorphize.carrier.CarrierJoin]] /
  * [[com.vanillasource.eliot.eliotc.monomorphize.carrier.UniformLadder]], landed in U3a-1) into [[CheckIO]], reading and
  * writing the shared [[CheckState.unifier]] the way [[EffectLifter]] and [[CarrierKindChecker]] do.
  *
  * It is the **checker-side half of U3a-2**, built and unit-tested in isolation (like [[EffectLifterTest]]) **before the
  * spine-loop flip constructs and calls it** — so nothing in the default compiler path references it yet and the path
  * stays byte-identical (it also avoids the `desugarChannel`/`EffectAccounting` coupling the U3-0b finding flagged,
  * which the actual flip must untangle together). When the flip lands, the [[com.vanillasource.eliot.eliotc.monomorphize.check.Checker]]
  * constructs this beside [[EffectLifter]] and routes the spine slots through it; the node *splicing* of a materialised
  * lift reuses [[EffectLifter]]'s existing `bindWrap`/`tryPureWrap` mechanics (reshaped, not rebuilt).
  *
  * =The §12-Q1 decision: check-time carrier-wrapping=
  *
  * Pure signatures/terms are brought into carrier-headed form **here, at check time** ([[intoCarrierHeaded]]: a pure
  * `T` ⤳ `Id[T]`), rather than by a `core`-phase desugar rewrite. This localises the uniform elaboration to the
  * checker (the flip's home), leaves `EffectSugarDesugarer` and the surface untouched, and keeps the change clear of the
  * `desugarChannel` deletion. The recognition it needs — "is this return already carrier-headed?" — is *not* the
  * undecidable "is an arbitrary type a carrier?" the [[EffectLifter]] treadmill fought: it is the **positional** read of
  * the value's own already-recorded carrier bookkeeping ([[CheckState.ambientCarriers]] / [[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier.carrierRoles]]
  * via the reused `effectCarrierSplit`) plus the compiler-owned `Id` head.
  *
  * @param force
  *   Force a SemValue through the current meta store — the checker's `force`.
  * @param effectCarrierSplit
  *   The effect-carrier recognition on the *expected* side (the surviving positional tag) — reused verbatim from
  *   [[EffectLifter.effectCarrierSplit]], which reads [[CheckState.ambientCarriers]] and the effect-carrier role flags.
  */
class UniformCarrierChecker(
    force: SemValue => CheckIO[SemValue],
    effectCarrierSplit: SemValue => CheckIO[Option[(SemValue, SemValue)]]
) {

  /** The identity carrier `Id`, unapplied — the pure carrier a non-carrier-headed judgment is wrapped in. */
  private val idCarrier: SemValue = VTopDef(WellKnownTypes.idFQN, None, Spine.SNil)

  /** Bring a runtime term judgment into carrier-headed form (the §12-Q1 check-time wrapping): a judgment already headed
    * by an effect carrier (ambient / role-flagged) or by `Id` is left as-is; anything else is a pure judgment and is
    * wrapped `Id[tpe]`. A **type-level** judgment ([[SemValue.VType]]) is never wrapped — the §8 compile-time boundary
    * (the type language stays carrier-free); the caller only ever applies this to runtime term judgments, and this guard
    * is the defensive backstop.
    */
  def intoCarrierHeaded(tpe: SemValue): CheckIO[SemValue] =
    force(tpe).flatMap {
      case VType  => pure(tpe)
      case forced =>
        isCarrierHeaded(forced).map(if (_) tpe else Evaluator.applyValue(idCarrier, tpe))
    }

  /** Whether the forced outermost head of `tpe` is already an effect carrier (ambient / role-flagged, via
    * `effectCarrierSplit`) or the compiler-owned `Id`.
    */
  def isCarrierHeaded(tpe: SemValue): CheckIO[Boolean] =
    for {
      forced <- force(tpe)
      split  <- effectCarrierSplit(forced)
    } yield split.nonEmpty || isIdHeaded(forced)

  private def isIdHeaded(forced: SemValue): Boolean = forced match {
    case VTopDef(fqn, _, Spine.SApp(_, _)) => fqn == WellKnownTypes.idFQN
    case _                                 => false
  }

  /** Classify an *expected* application slot via the uniform ladder, reading the effect-carrier tag from the value's
    * carrier bookkeeping (the surviving positional recognition — on the expected side, never a shape guess about the
    * actual). See [[UniformLadder.classifyExpected]].
    */
  def classifyExpectedSlot(expected: SemValue): CheckIO[UniformLadder.ExpectedSlot] =
    for {
      forced <- force(expected)
      tagged <- effectCarrierSplit(forced).map(_.nonEmpty)
    } yield UniformLadder.classifyExpected(forced, _ => tagged)

  /** Resolve one application slot's *decision* through the uniform ladder, threading the updated unifier back into the
    * state. Returns the [[UniformLadder.Outcome]]; the caller collects the deferred lift (if any) and materialises it at
    * the value boundary via [[finalizeAndMaterialize]]. The node splicing of a materialised lift reuses
    * [[EffectLifter]]'s mechanics (added at the flip).
    */
  def resolveSlot(
      actual: SemValue,
      expected: UniformLadder.ExpectedSlot,
      context: Sourced[String]
  ): CheckIO[UniformLadder.Outcome] =
    for {
      unifier          <- inspect(_.unifier)
      (updated, outcome) = UniformLadder.resolveSlot(unifier, actual, expected, context)
      _                <- modify(_.withUnifier(updated))
    } yield outcome

  /** At the value boundary: default every still-unsolved carrier metavariable to `Id` (the join lattice's boundary rule)
    * and materialise the recorded deferred lifts decision-free — a carrier that resolved to `Id` yields an *erased*
    * lift, a non-`Id` one a `pure`/`flatMap` at that carrier. The carrier metas are exactly the higher-kinded
    * instantiation metas the checker already tracks in [[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier.carrierRoles]].
    */
  def finalizeAndMaterialize(
      lifts: List[UniformLadder.DeferredLift]
  ): CheckIO[List[UniformLadder.MaterializedLift]] =
    for {
      unifier   <- inspect(_.unifier)
      carrierIds = unifier.carrierRoles.keySet.toList.map(SemValue.MetaId(_))
      finalized  = CarrierJoin.finalize(unifier, carrierIds)
      _         <- modify(_.withUnifier(finalized))
    } yield UniformLadder.materialize(finalized, lifts)
}
