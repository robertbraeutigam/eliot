package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes
import com.vanillasource.eliot.eliotc.monomorphize.carrier.{Carrier, CarrierJoin, UniformLadder}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
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

  /** Bring a runtime term's **value** into carrier-headed form (the term-level dual of [[intoCarrierHeaded]], for the
    * eager elaboration): a pure term `expr : T` ⤳ `pure@Effect[Id, T](expr) : Id[T]`, so its value matches its
    * `Id`-carried type (the Id-normalization stage erases the `pure@Id` again). A term already carrier-headed
    * (ambient/role/`Id`) or type-level (`VType`) is returned unchanged. `infer` uses this on its pure leaves (literals,
    * pure references) so every judgment is carrier-headed by construction.
    */
  def intoCarrierHeadedTerm(expr: SemExpression, source: Sourced[?]): CheckIO[SemExpression] =
    for {
      forced <- force(expr.expressionType)
      headed <- isCarrierHeaded(forced)
    } yield
      if (forced == VType || headed) expr
      else EffectLifter.pureWrapNode(EffectLifter.idCarrier, forced, Evaluator.applyValue(EffectLifter.idCarrier, forced), expr, source)

  /** Check a value/lambda **body** against its declared return type (the uniform successor of the checker's
    * `checkAgainst` return boundary). The declared return is brought into carrier-headed form ([[intoCarrierHeaded]] —
    * a pure return becomes `Id[T]`), then the body's carrier **joins** the return's and the payloads unify; a **pure**
    * body is re-carried into the declared carrier via [[UniformCarrierChecker.carrierSlotLift]] (which erases when the
    * carrier is `Id`, so a pure body against a pure return costs nothing). A genuinely effectful body against a pure
    * (`Id`) declared return leaves its carrier meta to default to `Id`, where the effect operation's instance
    * (`Console[Id]`, …) fails to resolve — the loud fail-safe, exactly as on the default path; the friendly
    * "declared pure but performs an effect" diagnostic remains the post-mono accounting's job (§5).
    *
    * Both the return and the body are carrier-headed by the elaboration invariant, so [[Carrier.split]] is total here.
    */
  def checkReturnBoundary(
      bodyExpr: SemExpression,
      bodyType: SemValue,
      declaredReturn: SemValue,
      source: Sourced[?]
  ): CheckIO[SemExpression] =
    for {
      expected              <- intoCarrierHeaded(declaredReturn)
      (cExpected, pExpected) = Carrier.split(expected)
      (cBody, pBody)         = Carrier.split(bodyType)
      unifier               <- inspect(_.unifier)
      bodyIsPure             = CarrierJoin.resolve(unifier, cBody) == Carrier.Bottom
      joined                 = CarrierJoin.joinToward(unifier, cBody, cExpected, source.as("Type mismatch."))
      _                     <- modify(_.withUnifier(joined.unify(pBody, pExpected, source.as("Type mismatch."))))
    } yield
      if (bodyIsPure)
        UniformCarrierChecker.carrierSlotLift(Carrier.toSemValue(cExpected), pExpected, bodyExpr, source)
      else bodyExpr

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

  /** Resolve one application argument slot into an actual [[SemExpression]] outcome — the node-producing counterpart of
    * the decision-only [[resolveSlot]], and the uniform successor of the checker's `checkArgumentSlot` ladder (a
    * [[UniformCarrierChecker.UniformSlotOutcome]] mirrors the `SlotOutcome` the spine loop already threads). The
    * classification picks the arm; the ladder runs the join + payload unification; then the node is built by **reusing**
    * [[EffectLifter]]'s insertion mechanics (reshape, not rebuild):
    *
    *   - [[UniformLadder.ExpectedSlot.Generic]] ⇒ pass the whole carrier-headed action through unchanged (`fold`'s arm);
    *   - [[UniformLadder.ExpectedSlot.CarrierSlot]] ⇒ pass-join; a **pure** (bottom-carriered) actual is re-carried into
    *     the expected carrier via [[UniformCarrierChecker.carrierSlotLift]] (`pure@Effect[?G](runId(actual))`, whose
    *     `?G` the join solves and the Id-normalizer erases when it defaults to `Id`), an already-effectful actual passes
    *     through;
    *   - [[UniformLadder.ExpectedSlot.PayloadSlot]] with an **effectful** actual ⇒ **bind**: the slot receives a fresh
    *     `$eff$N` reference at the payload and an [[EffectLifter.Bind]] is recorded for the spine's `wrapBinds` — the
    *     effect runs at the call site (`printLine(readLine)`);
    *   - [[UniformLadder.ExpectedSlot.PayloadSlot]] with a **pure** (`Id`, bottom) actual ⇒ **pass** its payload
    *     directly (`runId`, erased downstream), *not* a bind. A pure actual has no effect to sequence, and binding it
    *     would be unsound: `wrapBinds`/`bindWrap` unifies the bind's carrier with the enclosing core's, so an `Id` bind
    *     reaching an *effectful* core (`printLine(greeting)`, core `F[Unit]`) would wrongly solve `F := Id` and strip
    *     the effect. Passing the payload directly is what the default path does for a pure argument, and byte-identical
    *     after the `runId` erases.
    *
    * `argType` is the actual's carrier-headed type (the elaboration invariant); the ladder never sees an un-split
    * carrier because [[Carrier.split]] peels it off before any payload unification.
    */
  def resolveArgumentSlot(
      arg: Sourced[OperatorResolvedExpression],
      argExpr: SemExpression,
      argType: SemValue,
      expected: SemValue
  ): CheckIO[UniformCarrierChecker.UniformSlotOutcome] =
    for {
      slot          <- classifyExpectedSlot(expected)
      unifier       <- inspect(_.unifier)
      (updated, out) = UniformLadder.resolveSlot(unifier, argType, slot, arg.as("Type mismatch."))
      _             <- modify(_.withUnifier(updated))
      result        <- (slot, out) match {
                         case (_, UniformLadder.Outcome.PassWhole)      =>
                           pure(UniformCarrierChecker.UniformSlotOutcome.Passed(argExpr))
                         case (UniformLadder.ExpectedSlot.CarrierSlot(cExpected, pExpected),
                               UniformLadder.Outcome.PassJoin(Some(_)))  =>
                           pure(
                             UniformCarrierChecker.UniformSlotOutcome.Passed(
                               UniformCarrierChecker.carrierSlotLift(Carrier.toSemValue(cExpected), pExpected, argExpr, arg)
                             )
                           )
                         case (_, UniformLadder.Outcome.PassJoin(None)) =>
                           pure(UniformCarrierChecker.UniformSlotOutcome.Passed(argExpr))
                         case (UniformLadder.ExpectedSlot.PayloadSlot(shape), UniformLadder.Outcome.Bound(Carrier.Bottom)) =>
                           // A pure (bottom) actual passes its payload directly (`runId`) — no bind. See the doc above:
                           // binding an `Id`-carriered actual would let `wrapBinds` unify `Id` with an effectful core's
                           // carrier and strip the effect. The `runId` erases downstream, matching the default path.
                           pure(UniformCarrierChecker.UniformSlotOutcome.Passed(EffectLifter.runIdNode(shape, argExpr, arg)))
                         case (UniformLadder.ExpectedSlot.PayloadSlot(shape), UniformLadder.Outcome.Bound(actualCarrier)) =>
                           freshLiftName.map { name =>
                             UniformCarrierChecker.UniformSlotOutcome.Bound(
                               SemExpression(shape, SemExpression.ParameterReference(arg.as(name))),
                               EffectLifter.Bind(name, arg, argExpr, argType, Carrier.toSemValue(actualCarrier), shape)
                             )
                           }
                         case (s, o)                                    =>
                           throw new IllegalStateException(s"uniform slot outcome mismatch: slot=$s outcome=$o")
                       }
    } yield result

  /** The next fresh lift-binder name (`$eff$0`, `$eff$1`, …), threading [[CheckState.liftCounter]] — the same
    * convention (and counter) [[EffectLifter]] uses, so uniform-path and default-path binders never collide.
    */
  private def freshLiftName: CheckIO[String] =
    for {
      n <- inspect(_.liftCounter)
      _ <- modify(s => s.copy(liftCounter = n + 1))
    } yield s"$$eff$$$n"
}

object UniformCarrierChecker {

  /** The outcome of a node-producing argument-slot resolution ([[UniformCarrierChecker.resolveArgumentSlot]]) — the
    * uniform mirror of the checker's `SlotOutcome`.
    */
  sealed trait UniformSlotOutcome {

    /** The expression this slot contributes to the application: the (possibly re-carried) argument for [[Passed]], the
      * fresh `$eff$N` reference for [[Bound]].
      */
    def slotExpr: SemExpression
  }

  object UniformSlotOutcome {

    /** Generic pass-through or carrier-slot pass-join — the slot takes the argument (a pure carrier-slot actual
      * re-carried via [[carrierSlotLift]]); no sequencing.
      */
    case class Passed(slotExpr: SemExpression) extends UniformSlotOutcome

    /** Payload slot — the effect binds at the call site: the slot receives `slotExpr` (a fresh `$eff$N` reference) and
      * `bind` is folded around the spine core by [[EffectLifter.wrapBinds]].
      */
    case class Bound(slotExpr: SemExpression, bind: EffectLifter.Bind) extends UniformSlotOutcome
  }

  /** Re-carry a **pure** (`Id`-headed) actual into an effect-carrier slot: `pure[carrier, payload](runId(argExpr))`,
    * built entirely from [[EffectLifter]]'s extracted node mechanics. The `runId` projects the payload out of the pure
    * `Id`-carried actual and `pure` re-wraps it at the expected `carrier` (a carrier meta the join solves, or a concrete
    * carrier). When the carrier defaults to `Id` the whole thing (`pure@Effect[Id](runId(Id(x)))`) is erased by the
    * Id-normalization stage — no machinery ships for a pure conditional. `carrier` is the carrier as a [[SemValue]]
    * ([[Carrier.toSemValue]]).
    */
  def carrierSlotLift(carrier: SemValue, payload: SemValue, argExpr: SemExpression, source: Sourced[?]): SemExpression =
    EffectLifter.pureWrapNode(
      carrier,
      payload,
      Evaluator.applyValue(carrier, payload),
      EffectLifter.runIdNode(payload, argExpr, source),
      source
    )
}
