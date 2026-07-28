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
  * [[com.vanillasource.eliot.eliotc.monomorphize.carrier.UniformLadder]]) into [[CheckIO]], reading and writing the
  * shared [[CheckState.unifier]] the way [[EffectLifter]] and [[CarrierKindChecker]] do.
  *
  * What survives here is **carrier-safe unification**, not effect elaboration: the row desugar
  * ([[com.vanillasource.eliot.eliotc.row.RowElaborator]]) writes every bind and classifies every position from its
  * declaration (§1 rule 4), so this bridge only ever *passes* a slot — joining carriers so a carrier meta is never
  * stolen by first-contact unification, and lifting a pure term into a carrier position with `Effect.pure`.
  *
  * =No manufactured `Id` (docs/effects-as-rows.md A.8.10)=
  *
  * The v2 form of this bridge made every runtime judgment carrier-headed by **wrapping** pure ones (`T` ⤳ `Id[T]`, the
  * term ⤳ `pure@Effect[Id](term)`) so its slot arms could split a carrier off unconditionally; the `Id`-normalizer then
  * erased the apparatus again. An arm-liveness trace over the full gate measured that round trip at ~95% identity, so
  * the wrapping is gone: a judgment is *classified* instead ([[actualForm]]) by the same positional recognition the
  * rest of the effect machinery uses — the value's own carrier bookkeeping ([[CheckState.ambientCarriers]] /
  * [[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier.carrierRoles]] via the reused `effectCarrierSplit`) plus
  * the compiler-owned `Id` head. It is never the undecidable "is an arbitrary type a carrier?" question the
  * [[EffectLifter]] treadmill fought.
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

  /** Classify a runtime term judgment's **form** — the one recognition the bridge performs, and always the *positional*
    * one: the value's own carrier bookkeeping ([[CheckState.ambientCarriers]] / the unifier's carrier-role flags, via
    * the reused `effectCarrierSplit`), plus the compiler-owned `Id` head.
    *
    * Before the effects-as-rows slices this was not a classification at all: the checker **manufactured** an `Id` head
    * for every pure judgment (`T` ⤳ `Id[T]`, the term ⤳ `pure@Effect[Id](term)`) so the slot arms could split a
    * carrier off unconditionally, and the `Id`-normalizer erased the whole apparatus again downstream. Across the
    * full gate that round trip was ~95% identity — 6,641 `Id` wraps against 4,504 `runId` unwraps at payload slots,
    * and 756 of 793 distinct return boundaries lifting into `Id` itself — so the wrapping is gone and a pure judgment
    * is simply [[UniformLadder.ActualForm.Pure]] (docs/effects-as-rows.md A.8.10). A **genuine** `Id[T]` value (the
    * identity carrier used as ordinary data, inside `runId` / an `Effect[Id]` instance) stays distinct from a plain
    * `T`: it is [[UniformLadder.ActualForm.IdCarried]] and its payload is still projected with `runId`.
    */
  def actualForm(tpe: SemValue): CheckIO[UniformLadder.ActualForm] =
    for {
      forced <- force(tpe)
      split  <- effectCarrierSplit(forced)
    } yield split match {
      case Some((carrier, payload)) => UniformLadder.ActualForm.Carried(forced, Carrier.ofHead(carrier), payload)
      case None                     =>
        forced match {
          case VTopDef(fqn, _, Spine.SApp(_, payload)) if fqn == WellKnownTypes.idFQN =>
            UniformLadder.ActualForm.IdCarried(forced, payload)
          case _                                                                      => UniformLadder.ActualForm.Pure(forced)
        }
    }

  /** Check a value/lambda **body** against its declared return type (the uniform successor of the checker's
    * `checkAgainst` return boundary). Both sides are classified ([[actualForm]]), then the body's carrier **joins**
    * the return's and the payloads unify. Three shapes, told apart by the two carriers (`Carrier.resolve`d), with the
    * payload unified in all of them:
    *
    *   - **pure body** (carrier resolves to `Id`/[[Carrier.Bottom]]) into a carrier-**headed** return: lifted into the
    *     declared carrier via [[UniformCarrierChecker.pureLift]] — `pure@Effect[cExpected](body)`, a pure body under a
    *     `{Console} T` declared return. Into a plain declared return there is nothing to lift into and the body stands
    *     as it is (A.8.10 — the `pure@Id`/`runId` round trip that used to run here is gone);
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
    */
  def checkReturnBoundary(
      bodyExpr: SemExpression,
      bodyType: SemValue,
      declaredReturn: SemValue,
      source: Sourced[?]
  ): CheckIO[SemExpression] =
    for {
      expected              <- force(declaredReturn)
      expectedForm          <- actualForm(expected)
      (cExpected, pExpected) = (carrierOf(expectedForm), expectedForm.payload)
      bodyForm              <- actualForm(bodyType)
      forcedBody             = bodyForm.whole
      (cBody, pBody)         = (carrierOf(bodyForm), bodyForm.payload)
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
                                         case other                                   =>
                                           modify(s => s.withUnifier(s.unifier.unify(pBody, pExpected, source.as("Type mismatch."))))
                                             .as(EffectLifter.runIdNode(pExpected, bodyExpr, source))
                                       }
                                   }
                                 case _                                 =>
                                   // A pure body needs a `pure` lift only into a genuinely **headed** return — an
                                   // effect carrier, or the identity carrier used as ordinary data. Into a plain
                                   // declared return (`def f: String`) there is nothing to lift into and the body
                                   // stands as it is: that is the whole `Id`-manufacturing round trip this slice
                                   // removed (A.8.10).
                                   val bodyIsPure = resolvedBody == Carrier.Bottom
                                   val lift       = bodyIsPure && headed(expectedForm)
                                   modify(s =>
                                     s.withUnifier(
                                       CarrierJoin
                                         .joinToward(s.unifier, cBody, cExpected, source.as("Type mismatch."))
                                         .unify(pBody, pExpected, source.as("Type mismatch."))
                                     )
                                   ).as(
                                     if (lift)
                                       UniformCarrierChecker.pureLift(
                                         Carrier.toSemValue(cExpected),
                                         pExpected,
                                         headed(bodyForm),
                                         bodyExpr,
                                         source
                                       )
                                     else bodyExpr
                                   )
                               }
    } yield result

  /** The [[Carrier]] of a classified form — [[Carrier.Bottom]] for both a pure judgment and a genuine `Id` one, since
    * `Id` *is* the lattice bottom.
    */
  private def carrierOf(form: UniformLadder.ActualForm): Carrier = form match {
    case UniformLadder.ActualForm.Carried(_, carrier, _) => carrier
    case _                                               => Carrier.Bottom
  }

  /** Whether a form is carrier-**headed** at all (`C[T]` or `Id[T]`), as opposed to a plain judgment. On the expected
    * side this decides whether a pure body needs a `pure` lift to fit the return at all; on the body side it decides
    * whether that lift must first project the body's payload with `runId` (a headed body whose carrier resolved to
    * `Id` is an `Id[T]` *wrapper*, not a bare value).
    */
  private def headed(form: UniformLadder.ActualForm): Boolean = form match {
    case _: UniformLadder.ActualForm.Pure => false
    case _                                => true
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

  /** Resolve one application argument slot into the [[SemExpression]] the slot contributes. The two classifications —
    * the expected slot's ([[classifyExpectedSlot]]) and the actual's ([[actualForm]]) — pick the arm; the ladder runs
    * the join + payload unification; then the node, when one is needed at all, is built by **reusing**
    * [[EffectLifter]]'s insertion mechanics (reshape, not rebuild):
    *
    *   - [[UniformLadder.ExpectedSlot.Generic]] ⇒ pass the whole action through unchanged (`fold`'s arm);
    *   - [[UniformLadder.ExpectedSlot.CarrierSlot]] ⇒ pass-join; a **pure** actual is lifted into the expected carrier
    *     via [[UniformCarrierChecker.pureLift]] (`pure@Effect[?G](actual)`, whose `?G` the join solves and the
    *     Id-normalizer erases when it defaults to `Id`), an already-effectful actual passes through;
    *   - [[UniformLadder.ExpectedSlot.PayloadSlot]] with a **pure** actual ⇒ pass it through: the term already *is*
    *     its payload, and a pure actual has no effect to sequence. Only a genuine `Id[T]` value needs its payload
    *     projected (`runId`).
    *
    * A payload slot receiving a **carried** actual is the *hoist* shape, and hoisting is the desugar's rewrite —
    * which since §1 rule 4 it always writes, a computation at a rowless slot being rejected at the definition before
    * the checker runs. It is therefore unreachable by construction and is reported as a compiler bug rather than
    * being elaborated a second way.
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
      form          <- actualForm(argType)
      unifier       <- inspect(_.unifier)
      (updated, out) = UniformLadder.resolveSlot(unifier, form, slot, arg.as("Type mismatch."))
      _             <- modify(_.withUnifier(updated))
    } yield (slot, out) match {
      case (_, UniformLadder.Outcome.PassWhole | UniformLadder.Outcome.PassJoin | UniformLadder.Outcome.PayloadPass) =>
        argExpr
      case (UniformLadder.ExpectedSlot.CarrierSlot(cExpected, pExpected), UniformLadder.Outcome.PureLift(project))   =>
        UniformCarrierChecker.pureLift(Carrier.toSemValue(cExpected), pExpected, project, argExpr, arg)
      case (UniformLadder.ExpectedSlot.PayloadSlot(shape), UniformLadder.Outcome.PayloadUnwrap)                      =>
        EffectLifter.runIdNode(shape, argExpr, arg)
      case (_, UniformLadder.Outcome.PayloadBound)                                                                  =>
        throw new IllegalStateException(
          s"a carried actual reached a payload slot: the hoist is the desugar's rewrite, so such a slot must suspend ($arg)"
        )
      case (s, o)                                                                                                   =>
        throw new IllegalStateException(s"uniform slot outcome mismatch: slot=$s outcome=$o")
    }
}

object UniformCarrierChecker {

  /** Lift a term that is **not** on the position's carrier into it, as a **single** clean `pure@Effect[carrier]` node
    * (docs/effects-as-channel.md §3, pinned finding 3): a plain judgment is wrapped directly, while a genuine `Id`
    * value (`idCarried`) has its payload projected with `runId` first, since the identity carrier's own wrapper is
    * real data and re-carrying it whole would ship an `Id[..]` where a payload is expected.
    *
    * Emitting the *single* node matters: a `pure@Effect[carrier](pure@Effect[Id](inner))` double-wrap is semantically
    * identical but its inner `pure@Id` confuses the codegen `Id`-normalizer's impl-keyed erasure of the *outer* `pure`
    * when `carrier` resolves to a **non-`Id`** carrier (the effectful-handler discharge — a pure `catch` handler
    * `err -> err` into `?G[A]`), mis-erasing it and shipping a raw payload where a `G[..]` is expected (a runtime
    * `ClassCastException`). Since the checker no longer manufactures `Id` heads at all (A.8.10), the double-wrap can
    * no longer arise. When `carrier` defaults to `Id` the whole node erases downstream — no machinery ships for pure
    * code. `carrier` is the carrier as a [[SemValue]] ([[Carrier.toSemValue]]).
    */
  def pureLift(
      carrier: SemValue,
      payload: SemValue,
      idCarried: Boolean,
      argExpr: SemExpression,
      source: Sourced[?]
  ): SemExpression =
    EffectLifter.pureWrapNode(
      carrier,
      payload,
      Evaluator.applyValue(carrier, payload),
      if (idCarried) EffectLifter.runIdNode(payload, argExpr, source) else argExpr,
      source
    )
}
