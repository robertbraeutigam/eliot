package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.unify.UnifyResult
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The residual carrier machinery of the v2 effect auto-lift (docs/effect-lift-in-checker.md), reduced by the
  * effects-as-rows deletion slices (docs/effects-as-rows.md §4) to the pieces the row elaboration still needs:
  *
  *   - the **carrier recognition** ([[effectCarrierSplit]]) every effect-aware collaborator reads;
  *   - the **doomed-postponement probe** ([[mustPureWrapBeforeUnify]]) and the **pure-wrap arm** ([[tryPureWrap]] — a
  *     pure term into a carrier-typed position, `Effect.pure`);
  *   - the **bind splicing** ([[bindWrap]]) the one surviving bind producer feeds — the immediately-applied-lambda
  *     `let` rule.
  *
  * What the slices removed, because the desugar ([[com.vanillasource.eliot.eliotc.row.RowElaborator]]) now writes the
  * bind — and, since §1 rule 4, classifies *every* position from its declaration — so no gate shape reached them: the
  * `tryBindLift` arm (every ladder call site was dead), the pure-boundary `tryIdDefault` arm (superseded by the
  * elaborator's own written `Id` at the two pure boundaries), and the `wrapBinds` fold, whose only caller was the spine
  * loop's mid-spine bind (slice 2).
  *
  * **effects-v5 step 4** removed one more, `mustLiftBeforeUnify` — the bind-lift direction of the probe above, with its
  * `equalArityNonCarrier` / `isFlexMeta` guards. Both shapes it recognized (an under-applied and an equal-arity rigid
  * head meeting a carrier *metavariable* application) need the carrier to be a meta, and since the elaborator writes
  * the carrier a runtime-track carrier never is; the compile track's are pinned by `Track.Compiler.pinCarriers`. §5 Q1
  * of docs/effects-v5-one-carrier.md is the measurement that retired it: across the whole gate and all 45 examples the
  * arm fired exactly once, in its own unit test, and its `{State[List[String]]}` regression
  * ([[com.vanillasource.eliot.eliotc.jvm.ExamplesIntegrationTest2]]) passes without it. The other five arms measured
  * **live** and stay — which is why this class was not deleted whole, as step 4 anticipated.
  *
  * The surviving arms are still not definitional equality: `unify` never lifts — [[tryPureWrap]] verifies its
  * elaboration by *speculative* unification (payload against expected), committing only on success.
  *
  * What counts as an effect carrier (the head of `C[T']` after forcing):
  *   - a *metavariable* peeled from a callee's higher-kinded binder
  *     ([[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier.isHigherKindedMeta]]) — since the elaborator writes
  *     the carrier (A.11.4) this only ever holds on the **compiler track**, where an inline guard's carrier is still
  *     inferred and pinned post-hoc by `Track.Compiler.pinCarriers`; on the runtime track the arm was measured
  *     decision-free (docs/effects-as-rows.md A.11.8-3), or
  *   - a head recorded in [[CheckState.ambientCarriers]] (the value-under-check's own carrier binders) — a recorded
  *     *meta* head is re-forced at query time, so a carrier pinned/solved to a concrete constructor after recording
  *     (the compiler track's `Either[E]`) is still recognized.
  *
  * A rigid non-ambient head (`Box[String]`, `C[_, _]`) matches neither and is never lifted.
  *
  * Node assembly splices [[SemExpression]]s directly (no ORE is ever rewritten): the combinator reference is
  * `ValueReference(fqn, [C, T', R])` (ability binder first, matching the `[abilityParams ++ methodParams]` order
  * ability resolution slices), the continuation a `FunctionLiteral($eff$N, T', core)` under `VPi(T', _ => coreType)`,
  * applied to the action. Because insertion happens *during* the body check, the ordinary `resolve-abilities`
  * saturation pass finds and resolves the inserted `Effect` references — no new resolution machinery. Its binder name
  * is the `let`'s own, since the sole remaining bind producer is the immediately-applied-lambda rule.
  *
  * Operates over [[CheckIO]], reading the shared [[CheckState]] (unifier roles, ambient carriers) through
  * `get`/`modify`/`inspect`. It depends on exactly two checker primitives, passed at construction — that narrow surface
  * is the module boundary.
  *
  * @param force
  *   Force a SemValue through the current meta store — the checker's `force`.
  * @param doUnify
  *   Unify two semantic values, updating the unifier in the state — the checker's `doUnify`. Used by [[bindWrap]] to
  *   assert the carrier-consistency constraint the spliced combinator's signature implies (`flatMap[C]` sequences a
  *   `C[T']` action into a `T' -> C[R]` continuation — one `C`), connecting the action's carrier meta to the core's.
  */
class EffectLifter(
    force: SemValue => CheckIO[SemValue],
    doUnify: (SemValue, SemValue, Sourced[String]) => CheckIO[Unit]
) {
  import EffectLifter.*

  /** Split a type into its effect-carrier head and payload — `Some((C, T'))` iff the forced type is `C[T']` for an
    * effect carrier `C` (a higher-kinded instantiation meta head, or a head in [[CheckState.ambientCarriers]]) applied
    * to a non-empty spine. This is the `isEffectCarrierHeaded` read of the design; the split form is what both arms and
    * the wrap step consume. For a multi-applied head (`AbortCarrier[G, A]`) the carrier keeps the leading prefix
    * (`AbortCarrier[G]`) and the payload is the last argument (`A`).
    */
  def effectCarrierSplit(tpe: SemValue): CheckIO[Option[(SemValue, SemValue)]] =
    for {
      forced <- force(tpe)
      state  <- get
    } yield {
      val ambient = effectiveAmbientHeads(state)
      forced match {
        case VMeta(id, Spine.SApp(prefix, payload))
            if ambient.contains(CheckState.CarrierHead.Meta(id.value)) ||
              state.unifier.isHigherKindedMeta(id.value) =>
          Some((VMeta(id, prefix), payload))
        case topDef @ VTopDef(fqn, _, Spine.SApp(prefix, payload), _)
            if ambient.contains(CheckState.CarrierHead.TopDef(fqn)) =>
          Some((topDef.copy(spine = prefix), payload))
        case _ => None
      }
    }

  /** The ambient carrier heads with recorded *meta* heads re-forced through the current meta store: a carrier meta
    * solved after recording (the compiler track pins `{Throw[E]}` carriers to `Either[E]` right after the ambient read)
    * is recognized by its solution's head, not the stale meta id.
    */
  private def effectiveAmbientHeads(state: CheckState): Set[CheckState.CarrierHead] =
    state.ambientCarriers.map {
      case m @ CheckState.CarrierHead.Meta(id) =>
        Evaluator.force(VMeta(MetaId(id), Spine.SNil), state.unifier.metaStore) match {
          case VTopDef(fqn, _, _, _) => CheckState.CarrierHead.TopDef(fqn)
          case VMeta(solved, _)      => CheckState.CarrierHead.Meta(solved.value)
          case _                     => m
        }
      case concrete                            => concrete
    }

  /** The *expected* side is headed by an effect-carrier *metavariable*
    * and the pure actual is a rigid head applied to fewer arguments (`String ~ ?F[Unit]`), which unification can only
    * *degenerately* solve (`?F := const String`) — a solution that miscompiles because the carrier and its payload have
    * different runtime representations. Consulting pure-wrap first inserts the correct `Effect.pure` lift. This covers
    * both the def's own ambient carrier and a *callee's* ability-constrained carrier parameter (`echo`'s / `if`'s `F[_]
    * ~ Effect`), so a bare pure value supplied to any effect-carrier slot lifts rather than miscompiling.
    */
  def mustPureWrapBeforeUnify(actual: SemValue, expected: SemValue): CheckIO[Boolean] =
    effectCarrierSplit(expected).flatMap {
      case Some((VMeta(_, prefix), _)) => force(actual).map(underApplied(_, prefix.toList.length + 1, allowType = true))
      case _                           => pure(false)
    }

  /** A rigid head applied to fewer arguments than the carrier meta's application arity — the unsatisfiable postponement
    * shape (mirrors `CarrierKindChecker.unsatisfiableApplication`). Non-rigid shapes (a meta, a `VPi`) are legitimately
    * postponable and stay with definitional equality.
    *
    * `VType` (the type of types — a rigid nullary head, applied to zero arguments) counts as under-applied only when
    * `allowType` is set, which is exactly the **pure-wrap** direction ([[mustPureWrapBeforeUnify]]): a pure *type*
    * flowing into a carrier *value* slot (`if(COND, String[])`'s pure arm) must be `Effect.pure`-wrapped rather than
    * degenerately unified. `allowType` is a parameter rather than a constant because the retired bind-lift direction
    * passed `false` (effects-v5 step 4); [[mustPureWrapBeforeUnify]] is now the only caller, and it always passes
    * `true`.
    */
  private def underApplied(rigid: SemValue, arity: Int, allowType: Boolean): Boolean = rigid match {
    case VType                      => allowType && 0 < arity
    case VTopDef(_, None, spine, _) => spine.toList.length < arity
    case VNeutral(_, spine)         => spine.toList.length < arity
    case _                          => false
  }

  /** The pure-wrap arm (ladder arm 4): if the *expected* type forces to `C[T]` headed by an effect carrier — the def's
    * own ambient carrier *or* a callee's ability-constrained carrier parameter (`echo`'s / `if`'s `F[_] ~ Effect`) —
    * the inferred type is itself pure (not effect-carrier-headed — never double-wrap), and it speculatively unifies
    * with the payload `T`, wrap the term with `Effect.pure` (`ValueReference(pureFQN, [C, T])` applied to the term,
    * typed at the expected carrier type). Subsumes the effect phase's body-level `pureWrap`. Returns [[None]] when the
    * arm does not apply.
    */
  def tryPureWrap(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      actual: SemValue,
      expected: SemValue
  ): CheckIO[Option[SemExpression]] =
    effectCarrierSplit(expected).flatMap {
      case None                     => pure(None)
      case Some((carrier, payload)) =>
        effectCarrierSplit(actual).flatMap {
          case Some(_) => pure(None)
          case None    =>
            for {
              forcedExpected <- force(expected)
              state          <- get
              result         <- state.unifier.tryUnify(actual, payload, tm.as("Type mismatch.")) match {
                                  case UnifyResult.Unified(unified) =>
                                    modify(_.withUnifier(unified))
                                      .as(Some(EffectLifter.pureWrapNode(carrier, payload, forcedExpected, expr, tm)))
                                  case UnifyResult.Contradiction(_) => pure(None)
                                }
            } yield result
        }
    }

  /** Wrap one bind around a continuation core: `flatMap([C, T', R])(($eff$N : T') -> core, action)` when the core's
    * forced type is carrier-headed (`C[R]`), `map` when it is pure (`R` = the core type itself; a core type still flex
    * at wrap time counts as pure and defaults to `map` — a wrong default surfaces as a loud type error downstream,
    * never a silent miscompile). All inserted nodes reuse the action's [[Sourced]] position.
    */
  def bindWrap(bind: Bind, core: SemExpression, coreType: SemValue): CheckIO[(SemExpression, SemValue)] =
    for {
      forcedCore <- force(coreType)
      coreSplit  <- effectCarrierSplit(forcedCore)
      // The combinator's signature has ONE carrier: `flatMap[C](f: T' -> C[R], action: C[T']): C[R]`. Unify the
      // action's carrier with the core's, so the spliced reference's `C` connects to the surrounding carrier flow
      // (and ultimately the ambient carrier) instead of dangling as an unsolved meta.
      _          <- coreSplit match {
                      case Some((coreCarrier, _)) => doUnify(bind.carrier, coreCarrier, bind.source.as("Type mismatch."))
                      case None                   => pure(())
                    }
      src         = bind.source
    } yield {
      val (combinatorFqn, resultPayload, resultType) = coreSplit match {
        case Some((_, corePayload)) => (WellKnownTypes.effectFlatMapFQN, corePayload, forcedCore)
        case None                   => (WellKnownTypes.effectMapFQN, forcedCore, Evaluator.applyValue(bind.carrier, forcedCore))
      }
      val continuationType                           = VPi(bind.payload, _ => forcedCore)
      val combinator                                 = SemExpression(
        VPi(continuationType, _ => VPi(bind.actionType, _ => resultType)),
        SemExpression.ValueReference(src.as(combinatorFqn), Seq(bind.carrier, bind.payload, resultPayload))
      )
      val continuation                               = SemExpression(
        continuationType,
        SemExpression.FunctionLiteral(src.as(bind.name), bind.payload, src.as(core))
      )
      val applied                                    = SemExpression(
        VPi(bind.actionType, _ => resultType),
        SemExpression.FunctionApplication(src.as(combinator), src.as(continuation))
      )
      (SemExpression(resultType, SemExpression.FunctionApplication(src.as(applied), src.as(bind.action))), resultType)
    }

}

object EffectLifter {

  /** The identity carrier `Id`, in the canonical unapplied [[VTopDef]] form the compiler track pins `Either` carriers
    * with. It is never *manufactured* for pure judgments: a pure term is simply not carried (docs/effects-as-rows.md
    * A.8.10) and the elaborator writes `Id` where a discharge lands on a pure boundary, so this only builds the type of
    * a genuine `Id` value.
    */
  val idCarrier: SemValue = VTopDef(WellKnownTypes.idFQN, None, Spine.SNil)

  /** Build an `Effect.pure` lift node — `pure[carrier, payload](expr) : resultType` (`resultType` = `carrier[payload]`)
    * — reusing the [[Sourced]] position of `source` for every inserted node. Split out from
    * [[EffectLifter.tryPureWrap]], which passes its already-forced `carrier[payload]` as `resultType`.
    */
  def pureWrapNode[S](
      carrier: SemValue,
      payload: SemValue,
      resultType: SemValue,
      expr: SemExpression,
      source: Sourced[S]
  ): SemExpression = {
    val pureRef = SemExpression(
      VPi(payload, _ => resultType),
      SemExpression.ValueReference(source.as(WellKnownTypes.effectPureFQN), Seq(carrier, payload))
    )
    SemExpression(resultType, SemExpression.FunctionApplication(source.as(pureRef), source.as(expr)))
  }

  /** Build a `runId` unwrap node — `runId[payload](expr) : payload` — reusing the [[Sourced]] position of `source`.
    * `expr` is an `Id[payload]`-carried term and the node projects out its payload — a total, effect-free projection.
    */
  def runIdNode[S](payload: SemValue, expr: SemExpression, source: Sourced[S]): SemExpression = {
    val runIdRef = SemExpression(
      VPi(Evaluator.applyValue(idCarrier, payload), _ => payload),
      SemExpression.ValueReference(source.as(WellKnownTypes.runIdFQN), Seq(payload))
    )
    SemExpression(payload, SemExpression.FunctionApplication(source.as(runIdRef), source.as(expr)))
  }

  /** One recorded effect bind: the fresh binder `name` standing for the action's payload in the spine core, the
    * effectful `action` expression with its carrier-headed `actionType` (`C[T']`), split into the `carrier` (`C`) and
    * the `payload` (`T'`). `source` anchors every inserted node's position (the action's own [[Sourced]], as the effect
    * phase's desugarer did).
    */
  case class Bind(
      name: String,
      source: Sourced[OperatorResolvedExpression],
      action: SemExpression,
      actionType: SemValue,
      carrier: SemValue,
      payload: SemValue
  )
}
