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

/** The type-directed effect auto-lift (the fifth checker collaborator — docs/effect-lift-in-checker.md): the check-mode
  * elaboration that decides, per argument slot, whether an effectful term (type `C[T']` for an effect carrier `C`)
  * flowing into a pure position must be *bound* (sequenced with `Effect.flatMap`/`map`) or a pure term flowing into a
  * carrier-typed position *lifted* (`Effect.pure`). The resolution ladder runs unify → bind-lift (argument positions
  * only) → pure-wrap → mismatch. (It once ended in a `Coerce` widening probe before the mismatch; that probe was
  * removed at Step 7a — `docs/bounds-as-refinements.md` — once `Int == Int` made widening unnecessary.)
  *
  * None of this is definitional equality: `unify` never lifts — the arms fire only after speculative unification
  * failed (or, for the two shapes unification can only *postpone*, the [[mustLiftBeforeUnify]] /
  * [[mustPureWrapBeforeUnify]] pre-arms consult them first), and both verify their elaboration by *speculative*
  * unification (payload against expected), committing only on success. The bind-lift arm is consulted only from
  * argument-position resolution (the spine slots and the immediately-applied-lambda `let` rule), never from a return
  * boundary — stripping a carrier at a return boundary would silently drop the effect, so those remain hard
  * mismatches.
  *
  * What counts as an effect carrier (the head of `C[T']` after forcing):
  *   - a metavariable whose [[com.vanillasource.eliot.eliotc.monomorphize.domain.MetaRole.Instantiation.effectCarrier]]
  *     role is set (a callee's ability-constrained higher-kinded binder — `printLine`'s `F`), or
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
  * `Effect` references — no new resolution machinery. Fresh binder names come from [[CheckState.liftCounter]] (the
  * established `$eff$N` convention; `$` is not a user identifier character).
  *
  * Operates over [[CheckIO]], reading the shared [[CheckState]] (unifier roles, ambient carriers, lift counter)
  * through `get`/`modify`/`inspect`. It depends on exactly two checker primitives, passed at construction — that
  * narrow surface is the module boundary.
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
    * argument is carrier-headed on a *metavariable* carrier and the expected side is a rigid head applied to fewer
    * arguments — the shape `?F[T'] ~ H r..` with `arity(H) < arity(?F's spine)` (e.g. `?F[String] ~ String`), which
    * pattern unification can only *postpone*, never solve (no injective `F` exists — the same unsatisfiability shape
    * `CarrierKindChecker.verifyCarrierKinds` reports post-drain). Waiting for a unification failure would mask the
    * lift behind that doomed postponement. A `Coerce` cannot fire on this shape either (the unsolved carrier meta
    * cannot quote to ground), so consulting the lift arm first preserves the ladder's semantics exactly. A *concrete*
    * carrier head (`IO[String]` against `String`) mismatches properly, so it takes the ordinary failure path.
    */
  def mustLiftBeforeUnify(actual: SemValue, expected: SemValue): CheckIO[Boolean] =
    effectCarrierSplit(actual).flatMap {
      case Some((VMeta(_, prefix), _)) => force(expected).map(underApplied(_, prefix.toList.length + 1))
      case _                           => pure(false)
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
      case Some((VMeta(_, prefix), _)) => force(actual).map(underApplied(_, prefix.toList.length + 1))
      case _                           => pure(false)
    }

  /** A rigid head applied to fewer arguments than the carrier meta's application arity — the unsatisfiable
    * postponement shape (mirrors `CarrierKindChecker.unsatisfiableApplication`). Non-rigid shapes (a meta, a `VPi`)
    * are legitimately postponable and stay with definitional equality.
    */
  private def underApplied(rigid: SemValue, arity: Int): Boolean = rigid match {
    case VTopDef(_, None, spine) => spine.toList.length < arity
    case VNeutral(_, spine)      => spine.toList.length < arity
    case _                       => false
  }

  /** The bind-lift arm (ladder arm 3, argument positions only): if the argument's type forces to `C[T']` for an effect
    * carrier `C` and the payload `T'` *speculatively unifies* with the expected type (committed only on success),
    * record a bind — the slot receives a fresh `$eff$N` reference at `T'`, and the caller wraps the enclosing spine
    * with the sequencing combinator ([[wrapBinds]]). Returns the slot reference and the [[EffectLifter.Bind]] record,
    * or [[None]] when the argument is not effect-carrier-headed or its payload does not fit the slot.
    */
  def tryBindLift(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      actual: SemValue,
      expected: SemValue
  ): CheckIO[Option[(SemExpression, Bind)]] =
    effectCarrierSplit(actual).flatMap {
      case None                     => pure(None)
      case Some((carrier, payload)) =>
        for {
          state  <- get
          result <- state.unifier.tryUnify(payload, expected, tm.as("Type mismatch.")) match {
                      case UnifyResult.Unified(unified) =>
                        for {
                          _    <- modify(_.withUnifier(unified))
                          name <- freshLiftName
                        } yield Some(
                          (
                            SemExpression(payload, SemExpression.ParameterReference(tm.as(name))),
                            Bind(name, tm, expr, actual, carrier, payload)
                          )
                        )
                      case UnifyResult.Contradiction(_) => pure(None)
                    }
        } yield result
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
                                    modify(_.withUnifier(unified)).as {
                                      val pureRef = SemExpression(
                                        VPi(payload, _ => forcedExpected),
                                        SemExpression
                                          .ValueReference(tm.as(WellKnownTypes.effectPureFQN), Seq(carrier, payload))
                                      )
                                      Some(
                                        SemExpression(
                                          forcedExpected,
                                          SemExpression.FunctionApplication(tm.as(pureRef), tm.as(expr))
                                        )
                                      )
                                    }
                                  case UnifyResult.Contradiction(_) => pure(None)
                                }
            } yield result
        }
    }

  /** Fold the recorded binds around the spine core, innermost last-bind-first: the bind nearest the core selects the
    * combinator by the core's forced type ([[bindWrap]] — `map` lifts a pure core into the carrier, `flatMap` chains a
    * carrier-headed one), and because each wrap's own result is carrier-headed, every outer bind naturally selects
    * `flatMap`. Returns the wrapped expression with its (carrier-headed) type — the effect is never dropped, it rides
    * the wrapping combinator's result type.
    */
  def wrapBinds(
      core: SemExpression,
      coreType: SemValue,
      binds: Seq[Bind]
  ): CheckIO[(SemExpression, SemValue)] =
    binds.foldRight(pure((core, coreType))) { (bind, acc) =>
      acc.flatMap { case (expr, tpe) => bindWrap(bind, expr, tpe) }
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

  /** The next fresh lift-binder name (`$eff$0`, `$eff$1`, …), threading [[CheckState.liftCounter]]. */
  private def freshLiftName: CheckIO[String] =
    for {
      n <- inspect(_.liftCounter)
      _ <- modify(s => s.copy(liftCounter = n + 1))
    } yield s"$$eff$$$n"
}

object EffectLifter {

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
