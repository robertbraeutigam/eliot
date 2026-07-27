package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes
import com.vanillasource.eliot.eliotc.monomorphize.carrier.{Carrier, CarrierJoin, UniformLadder}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.unify.UnifyResult
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
    * a pure return becomes `Id[T]`), then the body's carrier **joins** the return's and the payloads unify. Three
    * shapes, told apart by the two carriers (`Carrier.resolve`d), with the payload unified in all of them:
    *
    *   - **pure body** (carrier resolves to `Id`/[[Carrier.Bottom]]) into any return: re-carried into the declared
    *     carrier via [[UniformCarrierChecker.carrierSlotLift]] — `pure@Effect[cExpected](runId(body))`, which erases
    *     when `cExpected` is `Id` (so a pure body into a pure return costs nothing), and lifts when it is a real carrier
    *     (a pure body under a `{Console} T` declared return);
    *   - **discharge-to-pure** — a still-flex carrier meta body (`?G[T]`, a fully-discharged computation whose residual
    *     carrier `runAbort`/`runThrow` left unbound) meeting a **pure** (`Id`) declared return — the uniform successor
    *     of [[EffectLifter.tryIdDefault]]: default the body carrier to `Id` ([[CarrierJoin.finalize]] over that one
    *     meta) and unwrap the body with `runId` ([[EffectLifter.runIdNode]]), so `def sign(f: Bool): String = if(f, "+")
    *     else "-"` drops straight into pure code (byte-identical to the default path's `tryIdDefault`). Sound because
    *     `Id` has no `Suspend` instance: a body that genuinely performs I/O cannot resolve its effect operation at `Id`
    *     and fails loudly at `resolve-abilities`, never silently;
    *   - **effectful body into an effect-carrier (non-`Id`) return** (`main : {Console} Unit`'s body): the carriers
    *     **join** and the body passes through unchanged (`?F` solved to the platform's `IO` at the entry, never
    *     defaulted to `Id`).
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
      expected              <- intoCarrierHeaded(declaredReturn).flatMap(force)
      (cExpected, pExpected) = Carrier.split(expected)
      forcedBody            <- force(bodyType)
      (cBody, pBody)         = Carrier.split(forcedBody)
      unifier               <- inspect(_.unifier)
      resolvedBody           = CarrierJoin.resolve(unifier, cBody)
      result                <- (cExpected, resolvedBody) match {
                                 // A flex carrier-meta body meets a pure `Id` return. Two shapes hide here, told apart by
                                 // whether the body's **payload** fits the pure return (speculative, like the default
                                 // path's `tryIdDefault`):
                                 //   - **discharge-to-pure** (payload fits): a fully-discharged residual carrier
                                 //     (`sign`'s `?G[String]` into a `String` return) — default the carrier to `Id`
                                 //     ([[CarrierJoin.finalize]]) and unwrap the body with `runId`.
                                 //   - **spuriously-flagged HKT ability binder** (payload does NOT fit): the body head is
                                 //     not a discharge residual but a callee's `[F[_]]` binder that `CarrierKindChecker`
                                 //     flags as a carrier *unfiltered* (`wrap`'s `?F[String]` into a `Box[String]`
                                 //     return). It must be resolved by ordinary whole-type injectivity (`?F := Box`) on
                                 //     the raw declared return and the body passed through — exactly as the default path
                                 //     (`checkAgainstDefault`) does, since that binder is decided by the call context, not
                                 //     a carrier to default to `Id`.
                                 case (Carrier.Bottom, Carrier.Var(id)) =>
                                   unifier.tryUnify(pBody, pExpected, source.as("Type mismatch.")) match {
                                     case UnifyResult.Unified(u)       =>
                                       modify(_.withUnifier(CarrierJoin.finalize(u, List(id))))
                                         .as(EffectLifter.runIdNode(pExpected, bodyExpr, source))
                                     case UnifyResult.Contradiction(_) =>
                                       // The payload does not fit the pure return. Injectivity (`?F := Box`) is the right
                                       // resolution ONLY when the return is itself a rigid application of matching arity
                                       // (`?F[String] ~ Box[String]`, the spuriously-flagged HKT binder) — a clean
                                       // decomposition. Against a **nullary** return (`?F[Unit] ~ String`) whole-unify
                                       // would DEGENERATELY solve `?F := const String`, silently stripping a genuine
                                       // effect (`twice(s -> printLine(s))` under a pure codomain), so there the payload
                                       // mismatch (`Unit ~ String`) is reported instead — the fail-safe.
                                       force(pExpected).flatMap {
                                         case rigid @ VTopDef(_, _, Spine.SApp(_, _)) =>
                                           modify(s => s.withUnifier(s.unifier.unify(forcedBody, rigid, source.as("Type mismatch."))))
                                             .as(bodyExpr)
                                         case _                                       =>
                                           modify(s => s.withUnifier(s.unifier.unify(pBody, pExpected, source.as("Type mismatch."))))
                                             .as(EffectLifter.runIdNode(pExpected, bodyExpr, source))
                                       }
                                   }
                                 case _                                 =>
                                   val bodyIsPure = resolvedBody == Carrier.Bottom
                                   modify(s =>
                                     s.withUnifier(
                                       CarrierJoin
                                         .joinToward(s.unifier, cBody, cExpected, source.as("Type mismatch."))
                                         .unify(pBody, pExpected, source.as("Type mismatch."))
                                     )
                                   ).as(
                                     if (bodyIsPure)
                                       UniformCarrierChecker.carrierSlotLift(Carrier.toSemValue(cExpected), pExpected, bodyExpr, source)
                                     else bodyExpr
                                   )
                               }
    } yield result

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
    *
    * `forcePinnedCarrier` (docs/effects-as-channel.md §7 step 4, finding 14) is the fact-carried **pinned-row
    * recognition tag** consulted at the capture seam: a callee parameter whose declared type is a pinned row
    * (`catch`'s `{Throw[E] | G} A` ⤳ `ThrowCarrier[E, G, A]`) is a canonical carrier stack the value's own ambient
    * `effectCarrierSplit` does *not* recognise (a discharge stack is neither the ambient nor a role-flagged meta).
    * The caller sets this true from the callee's `EffectRow.pinnedParameterIndices`, so the domain is split as a
    * carrier slot by the tag rather than by a shape/name guess (which would miscompile — finding 14).
    */
  def classifyExpectedSlot(
      expected: SemValue,
      forcePinnedCarrier: Boolean = false
  ): CheckIO[UniformLadder.ExpectedSlot] =
    for {
      forced <- force(expected)
      tagged <- effectCarrierSplit(forced).map(_.nonEmpty)
    } yield UniformLadder.classifyExpected(forced, _ => tagged || forcePinnedCarrier)

  /** Resolve one application argument slot into the [[SemExpression]] the slot contributes. The classification picks
    * the arm; the ladder runs the join + payload unification; then the node is built by **reusing** [[EffectLifter]]'s
    * insertion mechanics (reshape, not rebuild):
    *
    *   - [[UniformLadder.ExpectedSlot.Generic]] ⇒ pass the whole carrier-headed action through unchanged (`fold`'s arm);
    *   - [[UniformLadder.ExpectedSlot.CarrierSlot]] ⇒ pass-join; a **pure** (bottom-carriered) actual is re-carried into
    *     the expected carrier via [[UniformCarrierChecker.carrierSlotLift]] (`pure@Effect[?G](runId(actual))`, whose
    *     `?G` the join solves and the Id-normalizer erases when it defaults to `Id`), an already-effectful actual passes
    *     through;
    *   - [[UniformLadder.ExpectedSlot.PayloadSlot]] with a **pure** (`Id`, bottom) actual ⇒ **pass** its payload
    *     directly (`runId`, erased downstream). A pure actual has no effect to sequence.
    *
    * A payload slot receiving an **effectful** actual is the *hoist* shape, and hoisting is the desugar's rewrite: the
    * caller suspends such a slot instead of resolving it here, and the post-drain
    * [[com.vanillasource.eliot.eliotc.monomorphize.check.ModeResolver]] splices the bind chain
    * (docs/effects-as-rows.md A.8.8). It is therefore unreachable by construction — the checker only routes here for a
    * carrier-slot domain, a pinned capture, or a pure actual — and is reported as a compiler bug rather than being
    * elaborated a second way.
    *
    * `argType` is the actual's carrier-headed type (the elaboration invariant); the ladder never sees an un-split
    * carrier because [[Carrier.split]] peels it off before any payload unification.
    */
  def resolveArgumentSlot(
      arg: Sourced[OperatorResolvedExpression],
      argExpr: SemExpression,
      argType: SemValue,
      expected: SemValue,
      forcePinnedCarrier: Boolean = false
  ): CheckIO[SemExpression] =
    for {
      slot          <- classifyExpectedSlot(expected, forcePinnedCarrier)
      forcedActual  <- force(argType)
      unifier       <- inspect(_.unifier)
      (updated, out) = UniformLadder.resolveSlot(unifier, forcedActual, slot, arg.as("Type mismatch."))
      _             <- modify(_.withUnifier(updated))
    } yield (slot, out) match {
      case (_, UniformLadder.Outcome.PassWhole)                                                  => argExpr
      case (UniformLadder.ExpectedSlot.CarrierSlot(cExpected, pExpected), UniformLadder.Outcome.PassJoin(true)) =>
        UniformCarrierChecker.carrierSlotLift(Carrier.toSemValue(cExpected), pExpected, argExpr, arg)
      case (_, UniformLadder.Outcome.PassJoin(_))                                                => argExpr
      case (UniformLadder.ExpectedSlot.PayloadSlot(shape), UniformLadder.Outcome.Bound(Carrier.Bottom)) =>
        EffectLifter.runIdNode(shape, argExpr, arg)
      case (s, o)                                                                                =>
        throw new IllegalStateException(s"uniform slot outcome mismatch: slot=$s outcome=$o")
    }
}

object UniformCarrierChecker {

  /** Re-carry a **pure** (`Id`-headed) actual into an effect-carrier slot as a **single** clean `pure@Effect[carrier]`
    * node (docs/effects-as-channel.md §3, pinned finding 3). When `argExpr` is itself the checker-inserted
    * `pure@Effect[Id](inner)` wrapper (as `intoCarrierHeadedTerm` produces for a pure body/actual), its payload `inner`
    * is unwrapped and re-carried directly — `pure@Effect[carrier](inner)` — **not** `pure@Effect[carrier](runId(pure@Id(inner)))`.
    * The double-wrap is semantically identical but its inner `pure@Id` confuses the codegen `Id`-normalizer's
    * impl-keyed erasure of the *outer* `pure` when `carrier` resolves to a **non-`Id`** carrier (the effectful-handler
    * discharge — a pure `catch` handler `err -> err` into `?G[A]`), mis-erasing it and shipping a raw payload where a
    * `G[..]` is expected (a runtime `ClassCastException`). Emitting the single node is finding 3's fix, now also at the
    * return boundary. For an `Id`-carried actual that is *not* the `pure@Id` wrapper node, the payload is projected with
    * `runId` as before. When `carrier` defaults to `Id` the whole node erases downstream — no machinery ships for pure
    * code. `carrier` is the carrier as a [[SemValue]] ([[Carrier.toSemValue]]).
    */
  def carrierSlotLift(carrier: SemValue, payload: SemValue, argExpr: SemExpression, source: Sourced[?]): SemExpression =
    EffectLifter.pureWrapNode(
      carrier,
      payload,
      Evaluator.applyValue(carrier, payload),
      unwrapPureId(argExpr).getOrElse(EffectLifter.runIdNode(payload, argExpr, source)),
      source
    )

  /** The payload of a checker-inserted `pure@Effect[Id](inner)` wrapper node ([[EffectLifter.pureWrapNode]] at the `Id`
    * carrier, as [[UniformCarrierChecker.intoCarrierHeadedTerm]] emits for a pure term), recognised by the abstract
    * `pure` FQN and its `Id` carrier type-argument — so [[carrierSlotLift]] can re-carry `inner` directly rather than
    * round-tripping it through `runId` (avoiding the double-wrap; see there). [[None]] for any other node.
    */
  private def unwrapPureId(expr: SemExpression): Option[SemExpression] = expr.expression match {
    case SemExpression.FunctionApplication(target, argument) =>
      target.value.expression match {
        case SemExpression.ValueReference(vf, Seq(carrier, _))
            if vf.value == WellKnownTypes.effectPureFQN && carrier == EffectLifter.idCarrier =>
          Some(argument.value)
        case _ => None
      }
    case _ => None
  }
}
