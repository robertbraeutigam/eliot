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
  * effects-as-rows deletion slices (docs/effects-as-rows.md §4) to the pieces the row elaboration and the post-drain
  * [[ModeResolver]] still need:
  *
  *   - the **carrier recognition** ([[effectCarrierSplit]]) every effect-aware collaborator reads;
  *   - the two **doomed-postponement probes** ([[mustLiftBeforeUnify]] / [[mustPureWrapBeforeUnify]]) and the
  *     **pure-wrap arm** ([[tryPureWrap]] — a pure term into a carrier-typed position, `Effect.pure`);
  *   - the **bind splicing** ([[bindWrap]]) the one surviving bind producer feeds — the immediately-applied-lambda
  *     `let` rule.
  *
  * What the slices removed, because the desugar ([[com.vanillasource.eliot.eliotc.row.RowElaborator]]) now writes the
  * bind and the mode resolver decides the deferred positions, so no gate shape reached them: the `tryBindLift` arm
  * (every ladder call site was dead), the pure-boundary `tryIdDefault` arm (superseded by the uniform return
  * boundary's own `Id` discharge, [[UniformCarrierChecker.checkReturnBoundary]]), and the `wrapBinds` fold, whose
  * only caller was the spine loop's mid-spine bind (slice 2 — an argument the elaboration sequences now suspends and
  * is spliced by [[ModeResolver]] instead).
  *
  * The surviving arms are still not definitional equality: `unify` never lifts — [[tryPureWrap]] verifies its
  * elaboration by *speculative* unification (payload against expected), committing only on success.
  *
  * What counts as an effect carrier (the head of `C[T']` after forcing):
  *   - a metavariable whose [[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier.CarrierRole.effectCarrier]]
  *     flag is set (a callee's higher-kinded binder — `printLine`'s `F`), or
  *   - a head recorded in [[CheckState.ambientCarriers]] (the value-under-check's own carrier binders) — a recorded
  *     *meta* head is re-forced at query time, so a carrier pinned/solved to a concrete constructor after recording
  *     (the compiler track's `Either[E]`) is still recognized.
  *
  * A bare unconstrained HKT head (`Box[String]`, `C[_, _]`) matches neither and is never lifted.
  *
  * Node assembly splices [[SemExpression]]s directly (no ORE is ever rewritten):
  * the combinator reference is `ValueReference(fqn, [C, T', R])` (ability binder first, matching the
  * `[abilityParams ++ methodParams]` order ability resolution slices), the continuation a
  * `FunctionLiteral($eff$N, T', core)` under `VPi(T', _ => coreType)`, applied to the action. Because insertion
  * happens *during* the body check, the ordinary `resolve-abilities` saturation pass finds and resolves the inserted
  * `Effect` references — no new resolution machinery. Its binder name is the `let`'s own, since the sole remaining
  * bind producer is the immediately-applied-lambda rule.
  *
  * Operates over [[CheckIO]], reading the shared [[CheckState]] (unifier roles, ambient carriers) through
  * `get`/`modify`/`inspect`. It depends on exactly two checker primitives, passed at construction — that narrow
  * surface is the module boundary.
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
    * effect carrier `C` (a role-flagged instantiation meta head, or a head in [[CheckState.ambientCarriers]]) applied
    * to a non-empty spine. This is the `isEffectCarrierHeaded` read of the design; the split form is what both arms
    * and the wrap step consume. For a multi-applied head (`AbortCarrier[G, A]`) the carrier keeps the leading prefix
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
              state.unifier.isEffectCarrier(id.value) =>
          Some((VMeta(id, prefix), payload))
        case VTopDef(fqn, cached, Spine.SApp(prefix, payload))
            if ambient.contains(CheckState.CarrierHead.TopDef(fqn)) =>
          Some((VTopDef(fqn, cached, prefix), payload))
        case _                                                     => None
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
          case VTopDef(fqn, _, _) => CheckState.CarrierHead.TopDef(fqn)
          case VMeta(solved, _)   => CheckState.CarrierHead.Meta(solved.value)
          case _                  => m
        }
      case concrete                            => concrete
    }

  /** Whether the resolution ladder must consult the bind-lift arm *before* attempting definitional equality: the
    * argument is carrier-headed on a *metavariable* carrier and the expected side is a rigid head against which plain
    * unification would produce a wrong result. Two such shapes exist:
    *
    *   - **Under-applied** — `?F[T'] ~ H r..` with `arity(H) < arity(?F's spine)` (e.g. `?F[String] ~ String`), which
    *     pattern unification can only *postpone*, never solve (no injective `F` exists — the same unsatisfiability shape
    *     `CarrierKindChecker.verifyCarrierKinds` reports post-drain). Waiting for a unification failure would mask the
    *     lift behind that doomed postponement. This arm is unconditional (as it always was): a `VType` return position
    *     is excluded by `allowType = false`, an already-recognized carrier expected is never under-applied.
    *   - **Equal-arity spurious success** — `?F[?S] ~ H[r]` where `H` is a fully-applied data-type constructor of the
    *     *same* arity as the carrier application ([[equalArityNonCarrier]]). Here unification does *not* fail: it binds
    *     the carrier meta to the data constructor (`?F := List`, `?S := r`), silently swapping the effect carrier for a
    *     container (e.g. `State[List[X]]`'s `state : ?F[?S]` flowing into an `S = List[X]` slot, which would otherwise
    *     resolve the `State` ability at `[X, List]` instead of `[List[X], StateCarrier..]`). This arm is tightly guarded
    *     (see [[equalArityNonCarrier]]) — flex payload, ambient carrier present, expected not a recognized carrier —
    *     because unlike the under-applied shape a concrete-carrier expected (`?C[?B] ~ IO[Unit]` at a `main : IO[Unit]`
    *     boundary) unifies *correctly* to `?C := IO`, and that carrier is exactly the *unrecognized* concrete kind the
    *     ambient guard keeps clear of.
    *
    * A *concrete* carrier head (`IO[String]` against `String`) mismatches properly, so it takes the ordinary failure
    * path.
    */
  def mustLiftBeforeUnify(actual: SemValue, expected: SemValue): CheckIO[Boolean] =
    effectCarrierSplit(actual).flatMap {
      case Some((VMeta(_, prefix), payload)) =>
        val arity = prefix.toList.length + 1
        force(expected).flatMap { forcedExpected =>
          if (underApplied(forcedExpected, arity, allowType = false)) pure(true)
          else
            for {
              state         <- get
              expectedSplit <- effectCarrierSplit(expected)
              forcedPayload <- force(payload)
            } yield state.ambientCarriers.nonEmpty &&
              expectedSplit.isEmpty &&
              isFlexMeta(forcedPayload) &&
              equalArityNonCarrier(forcedExpected, arity)
        }
      case _                                 => pure(false)
    }

  /** The *equal-arity spurious-success* shape (companion to [[underApplied]]): a rigid **type constructor** head (a
    * body-less `VTopDef` — `List`, `Pair`) applied to *exactly* as many arguments as the flex effect-carrier meta
    * application (`?F[?S] ~ List[X]`, both arity 1). Unlike an under-applied head this DOES unify — by binding the whole
    * carrier meta to the data constructor and its flex payload to the argument (`?F := List`, `?S := X`), a miscompile
    * that silently swaps the effect carrier for a container (`State[List[X]]`'s `state : ?F[?S]` flowing into an
    * `S = List[X]` slot resolves the `State` ability at `[X, List]` instead of `[List[X], StateCarrier..]`) — so the
    * bind-lift arm must be consulted first here too.
    *
    * Three [[mustLiftBeforeUnify]] guards keep this arm from stealing a legitimate carrier unification, since the
    * expected being a genuine effect carrier is *not* syntactically distinguishable from a plain container here (`IO` and
    * `List` are both `VTopDef` constructors):
    *   - **Ambient carrier present.** The lift binds the effect onto the value-under-check's own ambient carrier, so it
    *     only makes sense inside an effect-polymorphic value ([[CheckState.ambientCarriers]] non-empty). A value with a
    *     *concrete* return and no ambient (`main : IO[Unit]`, `demo : Pair[..]`) has nothing to lift into — its body's
    *     carrier meta must unify with the concrete expected (`?C := IO`, `?G := Id`), never lift. This is the load-bearing
    *     guard against the concrete-but-*unrecognized* carrier (`IO`/user `Id`, absent from `ambientCarriers`).
    *   - **Flex payload only.** When the payload is *concrete* (`wrap : ?F[String]` — a higher-kinded ability's dispatch
    *     parameter), `?F[String] ~ Box[String]` unifies correctly to `?F := Box`. Only a flex payload lets unification
    *     *steal* the expected's inner structure into `?S`, the spurious case.
    *   - **Equal arity only.** An *over-applied* head (`?F[Unit] ~ StateCarrier[S, Id, Unit]`, arity 1 vs 3) unifies
    *     *correctly* by partial application — `?F := StateCarrier[S, Id]`, the carrier taking the leading prefix and the
    *     last argument the payload — which is how a pinned carrier stack feeds an open-row result.
    *
    * A *recognized* carrier expected (ambient/role-flagged) is additionally excluded by the `effectCarrierSplit(expected)`
    * guard; `VType` and bound-variable (`VNeutral`) heads are left to [[underApplied]] (arity strictly less), so the
    * return-boundary discharge and effectful-signatures kind acceptance are unaffected.
    */
  private def equalArityNonCarrier(rigid: SemValue, arity: Int): Boolean = rigid match {
    case VTopDef(_, None, spine) => spine.toList.length == arity
    case _                       => false
  }

  /** A bare, still-unsolved metavariable — the payload the equal-arity [[equalArityNonCarrier]] lift arm requires. */
  private def isFlexMeta(sv: SemValue): Boolean = sv match {
    case VMeta(_, Spine.SNil) => true
    case _                    => false
  }

  /** The pure-wrap dual of [[mustLiftBeforeUnify]]: the *expected* side is headed by an effect-carrier *metavariable*
    * and the pure actual is a rigid head applied to fewer arguments (`String ~ ?F[Unit]`), which unification can only
    * *degenerately* solve (`?F := const String`) — a solution that miscompiles because the carrier and its payload have
    * different runtime representations. Consulting pure-wrap first inserts the correct `Effect.pure` lift. This covers
    * both the def's own ambient carrier and a *callee's* ability-constrained carrier parameter (`echo`'s / `if`'s
    * `F[_] ~ Effect`), so a bare pure value supplied to any effect-carrier slot lifts rather than miscompiling.
    */
  def mustPureWrapBeforeUnify(actual: SemValue, expected: SemValue): CheckIO[Boolean] =
    effectCarrierSplit(expected).flatMap {
      case Some((VMeta(_, prefix), _)) => force(actual).map(underApplied(_, prefix.toList.length + 1, allowType = true))
      case _                           => pure(false)
    }

  /** A rigid head applied to fewer arguments than the carrier meta's application arity — the unsatisfiable
    * postponement shape (mirrors `CarrierKindChecker.unsatisfiableApplication`). Non-rigid shapes (a meta, a `VPi`) are
    * legitimately postponable and stay with definitional equality.
    *
    * `VType` (the type of types — a rigid nullary head, applied to zero arguments) counts as under-applied only when
    * `allowType` is set, which is exactly the **pure-wrap** direction ([[mustPureWrapBeforeUnify]]): a pure *type*
    * flowing into a carrier *value* slot (`if(COND, String[])`'s pure arm) must be `Effect.pure`-wrapped rather than
    * degenerately unified. The **bind-lift** direction ([[mustLiftBeforeUnify]]) passes `allowType = false`: there the
    * rigid head is the *expected* slot, and `expected = VType` means an effectful carrier-headed term is meeting a
    * type/return position (e.g. a guarded signature reducing to `Either[String, A]`). Bind-lifting there would strip the
    * carrier and silently drop the effect (collapsing a satisfied guard to `Left`), so that boundary stays a hard
    * mismatch / return-position discharge, never a lift.
    */
  private def underApplied(rigid: SemValue, arity: Int, allowType: Boolean): Boolean = rigid match {
    case VType                   => allowType && 0 < arity
    case VTopDef(_, None, spine) => spine.toList.length < arity
    case VNeutral(_, spine)      => spine.toList.length < arity
    case _                       => false
  }

  /** The pure-wrap arm (ladder arm 4): if the *expected* type forces to `C[T]` headed by an effect carrier — the def's
    * own ambient carrier *or* a callee's ability-constrained carrier parameter (`echo`'s / `if`'s `F[_] ~ Effect`) —
    * the inferred type is itself pure (not effect-carrier-headed — never double-wrap), and it speculatively unifies with
    * the payload `T`, wrap the term with `Effect.pure` (`ValueReference(pureFQN, [C, T])` applied to the term, typed
    * at the expected carrier type). Subsumes the effect phase's body-level `pureWrap`. Returns [[None]] when the arm
    * does not apply.
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
    * with — the value [[EffectLifter.tryIdDefault]] solves a pure boundary's still-flex residual carrier to, and the
    * carrier of a pure term under uniform carriers (docs/effects-as-channel.md).
    */
  val idCarrier: SemValue = VTopDef(WellKnownTypes.idFQN, None, Spine.SNil)

  /** Build an `Effect.pure` lift node — `pure[carrier, payload](expr) : resultType` (`resultType` = `carrier[payload]`)
    * — reusing the [[Sourced]] position of `source` for every inserted node. Extracted from [[EffectLifter.tryPureWrap]]
    * so the uniform-carrier checker ([[UniformCarrierChecker]]) reuses the exact same node mechanics (reshape, not
    * rebuild); the default-path [[EffectLifter.tryPureWrap]] passes its already-forced `carrier[payload]` as
    * `resultType`, the uniform path passes `Evaluator.applyValue(carrier, payload)` (definitionally the same value).
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
    * Extracted from [[EffectLifter.tryIdDefault]]; `expr` is an `Id[payload]`-carried term and the node projects out its
    * payload (a total, effect-free projection). The uniform path composes it under [[EffectLifter.pureWrapNode]] to
    * re-carry a pure `Id`-headed actual at a different (effect-carrier) expected slot.
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
    * the `payload` (`T'`). `source` anchors every inserted node's position (the action's own [[Sourced]], as the
    * effect phase's desugarer did).
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
