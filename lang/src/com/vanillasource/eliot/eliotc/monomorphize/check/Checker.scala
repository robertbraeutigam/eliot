package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.EffectCarrierNaming
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{
  BodyValueReferences,
  CompilerMonomorphicValue,
  GroundValue,
  RunBoundaryFunction
}
import com.vanillasource.eliot.eliotc.monomorphize.unify.UnifyResult
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.SignatureView
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Bidirectional type checker for the NbE pipeline. All state is threaded via the CheckIO state monad.
  *
  *   - `check(tm, expected)` checks a term against a known type.
  *   - `infer(tm)` infers a term's type.
  *
  * The checker produces [[SemExpression]]s with [[SemValue]] in every type slot. All ground-type conversion is deferred
  * to a post-drain pass in [[TypeStackLoop]], using [[com.vanillasource.eliot.eliotc.monomorphize.eval.Quoter]]. This
  * avoids any silent "default to Type" behaviour for unsolved metas — they surface as explicit errors at quoting time.
  */
class Checker(
    fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
    resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
    track: Track,
    // Whether this checker serves a signature-twin mono (the signature split, Step 6). A signature twin's own mono walks
    // its signature (which may reference self-in-signature type constructors like `Function`/`Type`), so the callee flip
    // must **not** fire there — reading `CompilerMonomorphicValue(Function@Signature, …)` while computing exactly that
    // fact is a demand cycle. The flip therefore fires only for body checks (a runtime/compiler *value* mono), whose
    // callees are other values; the twin those bodies read then computes its signature in place, bottoming the demand.
    signatureOnly: Boolean = false
) {

  /** The track's platform — fact keys read it off the [[track]] rather than threading a bare [[Platform]]. */
  private val platform: Platform = track.platform

  /** The higher-kinded-carrier kind checker (D8): seeds each `[F[_]]` carrier instantiation meta with its expected kind
    * and verifies the solution post-drain. A non-equality *kind system*, kept out of this checker's definitional
    * equality concern. Called from [[instantiatePolymorphic]] (seed) and [[TypeStackLoop]]'s `carrier-kinds` pass
    * (verify). See [[CarrierKindChecker]].
    */
  private[check] val carriers: CarrierKindChecker =
    new CarrierKindChecker(force, (tm, env) => evalExpr(tm, env), doUnify, platform)

  /** The calculated-return back-edge (D7): fills a value's bare omittable return from its monomorphized body
    * (implicit-generics W3/W4). A non-equality *non-local inference*, kept out of this checker's definitional equality
    * concern. Called from [[infer]] / [[applyInferred]] (read sides) and [[TypeStackLoop]] (callee-side
    * `installReturnMeta`). See [[CalculatedReturnResolver]].
    */
  private[check] val calcReturns: CalculatedReturnResolver =
    new CalculatedReturnResolver(force, freshMeta, platform)

  /** The ability-resolution saturation concern: discovering ability-qualified references and resolving each to its
    * concrete impl. A non-equality *saturation* pass, kept out of this checker's definitional equality concern. Called
    * only from [[TypeStackLoop]] (`processIO` seeds refs via `collectAbilityRefs`; the `resolve-abilities` post-drain
    * pass drives `resolveAbilities`). Constructed with the two CompilerIO primitives it needs plus the platform. See
    * [[AbilityResolver]].
    */
  private[check] val abilityResolver: AbilityResolver =
    new AbilityResolver(resolveAbility, platform)

  /** The residual carrier machinery of the v2 effect auto-lift (docs/effect-lift-in-checker.md): the carrier
    * recognition, the pure-wrap arm of the resolution ladder ([[resolveLadder]]) and the `Effect.flatMap`/`map`/`pure`
    * node assembly the surviving bind producers splice. A non-equality *elaboration* concern, kept out of this
    * checker's definitional-equality core. Consulted from the shared resolution ladder and the
    * immediately-applied-lambda `let` rule ([[typeImmediateLambda]]). See [[EffectLifter]].
    */
  private[check] val lifter: EffectLifter = new EffectLifter(force, doUnify)

  /** The **uniform-carrier** checker-side bridge (docs/effects-as-channel.md §3): the successor spine mechanism —
    * carrier-headed judgments, the classify-by-expected-slot ladder, and the join solver. It is the live path for
    * **runtime**-track value returns and argument slots (`platform == Platform.Runtime`); the compile-time track and the
    * runtime shapes it declines fall back to the default ladder ([[checkAgainstDefault]]/[[defaultArgSlot]], §8). Its
    * node splicing reuses [[EffectLifter]]'s `pureWrapNode`/`bindWrap` mechanics (reshape, not rebuild), so uniform-path
    * and default-path binders share the one `$eff$N` counter — which is why the default ladder and the `EffectLifter`
    * arms are the shared substrate the bridge sits on (see docs/effects-as-channel.md §7).
    */
  private[check] val uniformChecker: UniformCarrierChecker =
    new UniformCarrierChecker(force, lifter.effectCarrierSplit)

  /** The post-drain **mode resolver** (docs/effects-as-rows.md A.8.7): finishes the suspended slot-mode obligations
    * ([[CheckState.modeObligations]]) and deferred `let` bindings ([[CheckState.letObligations]]) from the solved meta
    * store at post-drain quiescence — the replacement of the former mid-spine Phase-B decision on the runtime track.
    * Driven only from [[TypeStackLoop]]'s post-drain fixpoint, before ability resolution in each round. See
    * [[ModeResolver]].
    */
  private[check] val modeResolver: ModeResolver = new ModeResolver(force, doUnify, lifter)

  /** The "declared pure but performs an effect" fail-safe: the one effect diagnostic the post-mono
    * [[com.vanillasource.eliot.eliotc.monomorphize.channel.EffectAccountingProcessor]] cannot voice (its value's mono
    * fails, so no fact is produced). The `derived ⊆ declared` subset check now lives entirely in that processor
    * (U4-c-2, which deleted the old `EffectResidualChecker`). A non-equality *verification* concern, kept out of this
    * checker's definitional-equality core. Called from [[TypeStackLoop.runPostDrainResolution]] after the final drain.
    * See [[DeclaredPureChecker]].
    */
  private[check] val declaredPure: DeclaredPureChecker = new DeclaredPureChecker(platform)

  /** Ensure a NativeBinding is in the cache, fetching it via CompilerIO if needed. */
  private def ensureBinding(vfqn: ValueFQN): CheckIO[Option[SemValue]] =
    for {
      cached <- inspect(_.bindingCache.get(vfqn))
      result <- cached match {
                  case Some(value) => pure(value)
                  case None        =>
                    for {
                      opt <- liftF(fetchBinding(vfqn))
                      _   <- modify(_.cacheBinding(vfqn, opt)) // cache before recursing so cycles short-circuit
                      _   <- ensureBodyBindings(vfqn)
                    } yield opt
                }
    } yield result

  /** Ensure the bindings of everything `vfqn`'s checking body references transitively — reached via the memoized
    * [[BodyValueReferences]] fact (walked once per value, never re-walked here) and recursing through `ensureBinding`,
    * whose binding-cache short-circuit both dedups and terminates on the (recursion-free, but diamond-shaped) reference
    * DAG. This is what lets `renormalize` re-fire a nested stuck native once its bound metavariables solve: a native
    * reached only through a bodied helper (e.g. `multiply`/`lessThanOrEqual`/`fold` inside a derived `multiplyMin` used
    * in the `*` result type) must already be in the flat [[CheckState.bindingCache]] the re-fire lookup consults.
    */
  private def ensureBodyBindings(vfqn: ValueFQN): CheckIO[Unit] =
    for {
      refs <- liftF(getFactIfProduced(BodyValueReferences.Key(vfqn, platform)))
      _    <- refs.fold(Set.empty[ValueFQN])(_.references).toList.traverse_(ensureBinding)
    } yield ()

  /** Evaluate an ORE expression against an env (defaulting to the current state's env). Prefetches every reachable
    * binding into [[CheckState.bindingCache]] first — including rewriting abstract associated-ability-types to fresh
    * metas via [[ensureBinding]] — so that the pure [[Evaluator]] has everything it needs.
    */
  def evalExpr(tm: OperatorResolvedExpression, env: Option[Env] = None): CheckIO[SemValue] =
    for {
      _ <- prefetchBindings(tm)
      s <- get
    } yield s.makeEvaluator.eval(env.getOrElse(s.rho), tm)

  /** Force a SemValue through the current meta store. */
  private[check] def force(v: SemValue): CheckIO[SemValue] =
    inspect(s => Evaluator.force(v, s.unifier.metaStore))

  /** Deeply normalise a SemValue, re-firing stuck native applications (e.g. the dependent-bounds `add` in
    * `Int[add(LMin,RMin), …]`) whose bound arguments have since been solved. Uses the binding cache as the native
    * lookup. See [[Evaluator.renormalize]].
    */
  private[check] def renormalize(v: SemValue): CheckIO[SemValue] =
    inspect(s => Evaluator.renormalize(v, s.unifier.metaStore, fqn => s.bindingCache.getOrElse(fqn, None)))

  /** Unify two semantic values, updating the unifier in the state. */
  private def doUnify(l: SemValue, r: SemValue, context: Sourced[String]): CheckIO[Unit] =
    modify(s => s.withUnifier(s.unifier.unify(l, r, context)))

  /** Speculatively unify, committing the solutions on success and leaving the state untouched (no error either) on
    * contradiction — the definitional-equality step (arm 1) of the check-mode resolution ladder.
    */
  private def tryUnifyCommitting(actual: SemValue, expected: SemValue, context: Sourced[String]): CheckIO[Boolean] =
    get.flatMap(s =>
      s.unifier.tryUnify(actual, expected, context) match {
        case UnifyResult.Unified(u)       => modify(_.withUnifier(u)).as(true)
        case UnifyResult.Contradiction(_) => pure(false)
      }
    )

  /** Allocate a fresh metavariable. */
  private[check] def freshMeta: CheckIO[VMeta] =
    for {
      s                   <- get
      (metaId, freshStore) = s.unifier.metaStore.fresh
      _                   <- modify(_.withUnifier(s.unifier.copy(metaStore = freshStore)))
    } yield VMeta(metaId, Spine.SNil)

  /** Check a term against a known expected type. */
  def check(
      tm: Sourced[OperatorResolvedExpression],
      expected: SemValue
  ): CheckIO[SemExpression] =
    for {
      forcedExpected <- force(expected)
      result         <- tm.value match {
                          // FunctionLiteral against a known VPi — unify domain, bind param, check body against codomain.
                          // Works for both annotated (unify annotated paramType with domain) and unannotated (use domain
                          // as paramType). Attribution falls to the body on codomain mismatches.
                          case OperatorResolvedExpression.FunctionLiteral(paramName, paramTypeStack, body)
                              if forcedExpected.isInstanceOf[VPi] =>
                            val VPi(domain, codomain) = forcedExpected: @unchecked
                            for {
                              paramType    <- paramTypeStack match {
                                                case Some(ts) =>
                                                  for {
                                                    pt <- evalExpr(ts.value)
                                                    _  <- doUnify(pt, domain, paramName.as("Type mismatch."))
                                                  } yield pt
                                                case None     => pure(domain)
                                              }
                              // Genuine dependent Π: bind the parameter's *type* in Γ and a fresh neutral standing for
                              // its (unknown) runtime *value* in ρ, then check the body against `codomain(neutral)` —
                              // never `codomain(paramType)`, which would substitute the parameter's type where its value
                              // belongs. Today every `VPi` codomain is constant (the `Function` native / `infer` build
                              // `_ => B`), so the two agree; the neutral is the correct form once dependent types land.
                              paramNeutral <- inspect(_.paramNeutral(paramName.value))
                              _            <- modify(_.bindValueParam(paramName.value, paramType))
                              bodyExpr     <- check(body, codomain(paramNeutral))
                            } yield SemExpression(
                              forcedExpected,
                              SemExpression.FunctionLiteral(paramName, paramType, body.as(bodyExpr))
                            )

                          // Unannotated FunctionLiteral against non-VPi expected — cannot infer param type.
                          case OperatorResolvedExpression.FunctionLiteral(_, None, _) =>
                            liftF(compilerError(tm.as("Cannot infer type of unannotated lambda.")) >> abort)

                          // An immediately-applied unannotated lambda `(x -> body)(arg)` checked against a known type — a
                          // `let`. Infer the argument's type for the binder and push `expected` down into the body. This
                          // is the shape a non-effectful block `val`/statement lowers to.
                          case OperatorResolvedExpression.FunctionApplication(target, arg)
                              if isUnannotatedLambda(target.value) =>
                            val OperatorResolvedExpression.FunctionLiteral(paramName, _, body) =
                              target.value: @unchecked
                            typeImmediateLambda(target, paramName, body, arg, Some(forcedExpected)).map(_._1)

                          case _ =>
                            for {
                              (expr, inferred) <- infer(tm)
                              checkedResult    <- checkAgainst(tm, expr, inferred, expected)
                            } yield checkedResult
                        }
    } yield result

  /** Check-mode resolution at a *return boundary* (a lambda body against its codomain, a def body against its declared
    * return): the shared [[resolveGuardedLadder]]. The ladder never produces a [[SlotOutcome.Bound]] anymore (its
    * bind-lift arms were deleted with the effects-as-rows slices), so a non-`Resolved` outcome here is a compiler bug.
    */
  private def checkAgainst(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      inferred: SemValue,
      expected: SemValue
  ): CheckIO[SemExpression] =
    uniformReturnRoutable(tm, inferred, expected).flatMap {
      case true  => uniformReturnBoundary(tm, expr, expected)
      case false => checkAgainstDefault(tm, expr, inferred, expected)
    }

  /** The default (carrier-based) return-boundary resolution: [[resolveGuardedLadder]] (see [[checkAgainst]]'s doc).
    * The live path for the whole compile-time track (§8) and for every runtime boundary the uniform bridge declines.
    */
  private def checkAgainstDefault(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      inferred: SemValue,
      expected: SemValue
  ): CheckIO[SemExpression] =
    resolveGuardedLadder(tm, expr, inferred, expected).map {
      case SlotOutcome.Resolved(e) => e
      case other                   =>
        throw new IllegalStateException(s"Return-boundary resolution produced a non-Resolved outcome: $other")
    }

  /** The uniform return boundary (docs/effects-as-channel.md §3, U3a-2b(ii)): resolve the body against the declared
    * return through the bridge ([[UniformCarrierChecker.checkReturnBoundary]] — join the carriers, unify the payloads,
    * lift a pure body into a carrier-headed return). A pure body meeting a plain declared return needs no node at all
    * (A.8.10 — the checker no longer manufactures an `Id` head just to strip it again downstream).
    */
  private def uniformReturnBoundary(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      expected: SemValue
  ): CheckIO[SemExpression] =
    uniformChecker.checkReturnBoundary(expr, expr.expressionType, expected, tm)

  /** Effects-as-channel U3a-2b(ii) (docs/effects-as-channel.md §10) — gated on [[uniformCarrier]]: whether a return
    * boundary is a **value** return the uniform bridge resolves today, pure *or* effect-carrier-headed
    * ([[uniformValueReturn]]) — a plain `VTopDef` (`String`, `Unit`, `List[..]`) re-carried via `Id`, or an effect
    * carrier (`?F[Unit]`, the ambient of `main : {Console} Unit`) passed through — provided the body's inferred type
    * already fits the declared return by pure definitional equality (a *non-committing* speculative unify; the real
    * unification runs inside [[UniformCarrierChecker.checkReturnBoundary]], where a carrier joined toward itself is a
    * no-op via the self-join guard).
    *
    * Restricted to the **runtime** track: the §8 boundary keeps the compile-time track's returns (the `Either` guard
    * discharge) carrier-free. Also falls back to [[checkAgainstDefault]] for the guard / calculated-return / W3
    * discharge and the §8 type-level boundary (all `VType`-expected), function/polytype returns (`VPi`/`VLam`), a bare
    * unresolved metavariable, and any genuine definitional-equality *miss* (an ordinary mismatch the default path
    * commits — there is no `Int` widening `Coerce` to reconcile a near-miss; it was deleted when `Int` became nullary
    * with its bounds in the separate refinement channel — `Int == Int` definitionally, bounds checked post-mono by
    * [[com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementChannelProcessor]]).
    */
  private def uniformReturnRoutable(
      tm: Sourced[OperatorResolvedExpression],
      inferred: SemValue,
      expected: SemValue
  ): CheckIO[Boolean] =
    if (platform != Platform.Runtime) pure(false)
    else
      for {
        valueExpected <- uniformValueReturn(expected)
        valueInferred <- uniformValueReturn(inferred)
        routable      <- if (valueExpected && valueInferred) unifiesDefinitionally(inferred, expected, tm.as("Type mismatch."))
                         else pure(false)
      } yield routable

  /** Whether `tpe` forces to a **value** return the uniform boundary resolves — a runtime term's type, pure or
    * effect-carrier-headed: a plain non-carrier `VTopDef` ([[uniformPlainValueType]] — `String`, `Unit`, `List[..]`)
    * *or* an effect-carrier-headed type (`effectCarrierSplit` non-empty — the ambient `?F[Unit]`, a recognized carrier).
    * Excludes `VType` (the §8 type-level boundary, guards, calculated returns), functions/polytypes (`VPi`/`VLam` — a
    * function value never carrier-heads), and an unresolved carrier/type metavariable head.
    */
  private def uniformValueReturn(tpe: SemValue): CheckIO[Boolean] =
    for {
      plain <- uniformPlainValueType(tpe)
      split <- force(tpe).flatMap(lifter.effectCarrierSplit)
    } yield plain || split.nonEmpty

  /** Whether `tpe` forces to a plain, non-carrier-headed **value** type the uniform boundary can safely re-carry: a
    * `VTopDef`-headed type (`String`, `Int`, `List[..]`, `Unit`) that is not an effect carrier (`effectCarrierSplit`
    * empty). Excludes `VType` (the §8 type-level boundary), functions/polytypes (`VPi`/`VLam` — only a fully-applied
    * result carrier-heads, never a function value), and metavariable heads (a carrier meta or an unresolved type), all
    * of which must stay on the default path in this slice.
    */
  private def uniformPlainValueType(tpe: SemValue): CheckIO[Boolean] =
    for {
      forced <- force(tpe)
      split  <- lifter.effectCarrierSplit(forced)
    } yield forced match {
      case VTopDef(_, _, _) => split.isEmpty
      case _                => false
    }

  /** Speculatively unify `actual` with `expected` by pure definitional equality, **without committing** — the routing
    * probe for [[uniformReturnRoutable]] (the actual unification runs later inside the bridge). Distinct from
    * [[tryUnifyCommitting]], which commits the solutions on success.
    */
  private def unifiesDefinitionally(actual: SemValue, expected: SemValue, context: Sourced[String]): CheckIO[Boolean] =
    inspect(s =>
      s.unifier.tryUnify(actual, expected, context) match {
        case UnifyResult.Unified(_)       => true
        case UnifyResult.Contradiction(_) => false
      }
    )

  /** The check-mode resolution ladder shared by return boundaries ([[checkAgainst]]) and spine argument slots
    * ([[checkArgumentSlot]]), fronted by the W2b guard-kind acceptance. A value whose type is on the compile-time
    * `Throw[String]` carrier types as `Either[..]`, not `Type`; where a `Type` kind is expected, accept it as a
    * *guarded type* (discharged to its payload — or rejected — by the signature/read-site discharge) rather than
    * letting the unifier reject `Either[..]` ≠ `Type`. Otherwise runs the plain [[resolveLadder]]. The expectation is
    * forced here (not at the callers): by the time the ladder runs, inference of the term may have solved metas in it.
    *
    * This is the single entry the two fresh-check sites share ([[check]]'s fallback via [[checkAgainst]], and
    * [[checkArgumentSlot]]); the deferred-slot re-entry ([[resolveDeferredSlot]]) calls [[resolveLadder]] directly,
    * where a guard acceptance never applied.
    */
  private def resolveGuardedLadder(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      inferred: SemValue,
      expected: SemValue,
  ): CheckIO[SlotOutcome] =
    for {
      forcedExpected <- force(expected)
      guardKind      <- forcedExpected match {
                          case VType => calcReturns.isGuardCarrier(inferred)
                          case _     => pure(false)
                        }
      // The W3-hole acceptance (signature-unification §3.3, arm 3): an under-applied omittable constructor (a bare
      // `Int`, a bare W2-grown `Counter`) in a `Type` position denotes a *calculated return* — its missing arguments
      // the body computes — so accept it as-is rather than unifying its type-constructor kind against `Type` (which
      // would reject `BigInteger -> Type` ≠ `Type`). Stateless: the under-applied head is published unchanged and the
      // consumer's read recognises it (`isCalculatedReturn`). This fires only where a bare omittable constructor is
      // checked against `Type` — in practice the signature twin's arrow chain, since the value mono flattens its own
      // calculated return to a `Type` placeholder before ever checking it.
      w3Hole         <- (forcedExpected, guardKind) match {
                          case (VType, false) => calcReturns.isCalculatedReturnExpr(tm.value)
                          case _              => pure(false)
                        }
      outcome        <- if (guardKind) pure(SlotOutcome.Resolved(expr): SlotOutcome)
                        else if (w3Hole) pure(SlotOutcome.Resolved(expr): SlotOutcome)
                        else resolveLadder(tm, expr, inferred, expected)
    } yield outcome

  /** The check-mode resolution ladder proper — the algorithm shared verbatim by return boundaries and argument slots
    * (R3-1 dedup): polytype instantiation, then the pure-wrap pre-arm for the shape definitional equality can only
    * *degenerately* solve, then unify → pure-wrap → committed mismatch.
    *
    * Position no longer changes the ladder. Under effects-as-rows the desugar writes every bind the elaboration needs
    * and the post-drain [[ModeResolver]] decides the deferred positions, so the position-dependent arms the v2 ladder
    * carried — the bind-lift pre-arm and failure arm (argument slots only) and the pure-boundary `Id` defaulting
    * (return boundaries only) — became unreachable and were deleted with the `allowBindLift` flag that selected them
    * (docs/effects-as-rows.md §4; every one of their call sites fired zero times across the full gate). A carrier
    * reaching a pure position that this ladder cannot reconcile is now a committed mismatch, which is the fail-safe
    * direction: a loud error, never a silently stripped effect.
    */
  private def resolveLadder(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      inferred: SemValue,
      expected: SemValue
  ): CheckIO[SlotOutcome] =
    for {
      (updatedExpr, instantiated) <- instantiatePolymorphic(expr, inferred)
      // The pure-wrap pre-arm: a pure actual against a carrier-meta-headed expectation is the shape unification can
      // only solve *degenerately* (`?F := const String`), so it must be consulted before definitional equality —
      // waiting for a failure that never comes would miscompile. See [[EffectLifter.mustPureWrapBeforeUnify]].
      prePure                     <- lifter
                                       .mustPureWrapBeforeUnify(instantiated, expected)
                                       .flatMap(
                                         if (_) lifter.tryPureWrap(tm, updatedExpr, instantiated, expected)
                                         else pure(Option.empty[SemExpression])
                                       )
      out                         <- prePure match {
                                       case Some(wrapped) =>
                                         pure(SlotOutcome.Resolved(wrapped): SlotOutcome)
                                       case None          => resolveFailureLadder(tm, updatedExpr, instantiated, expected)
                                     }
    } yield out

  /** The failure ladder consulted when definitional equality (arm 1) does not immediately unify: try the pure-wrap
    * arm, then commit the exact mismatch.
    */
  private def resolveFailureLadder(
      tm: Sourced[OperatorResolvedExpression],
      updatedExpr: SemExpression,
      instantiated: SemValue,
      expected: SemValue
  ): CheckIO[SlotOutcome] =
    tryUnifyCommitting(instantiated, expected, tm.as("Type mismatch.")).flatMap {
      case true  =>
        pure(SlotOutcome.Resolved(updatedExpr): SlotOutcome)
      case false =>
        lifter.tryPureWrap(tm, updatedExpr, instantiated, expected).flatMap {
          case Some(wrapped) =>
            pure(SlotOutcome.Resolved(wrapped): SlotOutcome)
          case None          =>
            commitMismatch(instantiated, expected, tm, updatedExpr)
        }
    }

  /** Commit the exact `instantiated`/`expected` mismatch into the unifier (deferred to drain, like all mismatches) and
    * yield the un-coerced expression as the slot's contribution.
    */
  private def commitMismatch(
      instantiated: SemValue,
      expected: SemValue,
      tm: Sourced[OperatorResolvedExpression],
      fallbackExpr: SemExpression
  ): CheckIO[SlotOutcome] =
    modify(st => st.withUnifier(st.unifier.addMismatch(instantiated, expected, tm.as("Type mismatch."))))
      .as(SlotOutcome.Resolved(fallbackExpr): SlotOutcome)

  /** Peel leading VLam closures by substituting fresh metas; return the non-VLam head together with the fresh metas in
    * order.
    *
    * @param bindInEnv
    *   Whether to bind each peeled parameter name to its fresh meta in the env. `false` (the default) is required for
    *   polytype instantiation inside the checker, so the callee's type-parameter names don't shadow any caller-scope
    *   parameter with the same name. `true` is used only by the top-level signature walk, where leftover type
    *   parameters should become in-scope names.
    */
  private[check] def peelLams(
      sem: SemValue,
      bindInEnv: Boolean = false
  ): CheckIO[(SemValue, Seq[SemValue])] = {
    def loop(s: SemValue, acc: Seq[SemValue]): CheckIO[(SemValue, Seq[SemValue])] =
      for {
        forced <- force(s)
        result <- forced match {
                    case VLam(name, closure) =>
                      for {
                        meta <- freshMeta
                        _    <- if (bindInEnv) modify(_.bindTypeParam(name, meta)) else pure(())
                        rest <- loop(closure(meta), acc :+ meta)
                      } yield rest
                    case other               => pure((other, acc))
                  }
      } yield result
    loop(sem, Seq.empty)
  }

  /** Infer the type of a term. */
  def infer(
      tm: Sourced[OperatorResolvedExpression]
  ): CheckIO[(SemExpression, SemValue)] = tm.value match {
    case OperatorResolvedExpression.IntegerLiteral(value) =>
      // Use the same VTopDef shape that DataTypeNativesProcessor binds for BigInteger, so the unifier sees a
      // single canonical form for this type rather than a VConst(Structure) vs VTopDef mismatch.
      val tpe = VTopDef(WellKnownTypes.bigIntFQN, None, Spine.SNil)
      pure((SemExpression(tpe, SemExpression.IntegerLiteral(value)), tpe))

    case OperatorResolvedExpression.StringLiteral(value) =>
      val tpe = VTopDef(WellKnownTypes.stringFQN, None, Spine.SNil)
      pure((SemExpression(tpe, SemExpression.StringLiteral(value)), tpe))

    case OperatorResolvedExpression.ParameterReference(name) =>
      for {
        state  <- get
        // A parameter's type is read straight from Γ ([[CheckState.gamma]]). Γ already holds the right type for every
        // kind of binder: a runtime value parameter's declared type, an erased type parameter's recovered type,
        // a peeled instantiation meta. (Type-position references go through the evaluator against ρ, never this path.)
        result <- state.gamma.lookupByName(name.value) match {
                    case Some(tpe) =>
                      pure((SemExpression(tpe, SemExpression.ParameterReference(name)), tpe))
                    case None      =>
                      liftF(compilerError(tm.as("Name not defined.")) >> abort)
                  }
      } yield result

    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs) =>
      for {
        explicitTypeArgs <- typeArgs.traverse(ta => evalExpr(ta.value))
        result           <- inferValueReference(tm, vfqn, explicitTypeArgs)
      } yield result

    case OperatorResolvedExpression.FunctionApplication(_, _) =>
      inferSpine(tm)

    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramTypeExpr), body) =>
      for {
        paramType            <- evalExpr(paramTypeExpr.value)
        _                    <- modify(_.bindValueParam(paramName.value, paramType))
        (bodyExpr, bodyType) <- infer(body)
        tpe                   = VPi(paramType, _ => bodyType)
      } yield (
        SemExpression(
          tpe,
          SemExpression.FunctionLiteral(paramName, paramType, body.as(bodyExpr))
        ),
        tpe
      )

    case OperatorResolvedExpression.FunctionLiteral(_, None, _) =>
      liftF(compilerError(tm.as("Cannot infer type of unannotated lambda.")) >> abort)
  }

  /** Infer a value reference's type from its saturated signature, applying the given (already-evaluated) explicit type
    * arguments.
    */
  private def inferValueReference(
      tm: Sourced[OperatorResolvedExpression],
      vfqn: Sourced[ValueFQN],
      explicitTypeArgs: Seq[SemValue]
  ): CheckIO[(SemExpression, SemValue)] =
      for {
        _      <- ensureBinding(vfqn.value)
        svOpt  <- liftF(getFactIfProduced(SaturatedValue.Key(vfqn.value, platform)))
        result <- svOpt match {
                    case Some(sv) =>
                      // Read the *saturated* signature, so a callee's parameter-position bare omittable references
                      // (e.g. bare `Int`) present as ordinary leading generic binders that the instantiation machinery
                      // solves from this call's arguments. Signatures reference only their own parameters or top-level
                      // values, so they evaluate under an empty env — outer-session bindings are not in scope.
                      for {
                        // The Step-6 callee flip: when this reference fully applies the callee (an argument per binder)
                        // and every argument is ground, read the callee's reduced ground signature from its signature
                        // twin's mono (`CompilerMonomorphicValue(callee@Signature, groundArgs)`) instead of evaluating the
                        // callee's signature in place. Otherwise (a partially-applied callee whose remaining binders this
                        // call infers, a not-yet-ground argument, a marker, or an unproduced twin) fall back to the
                        // in-place evaluation, which handles inference metas the ground read cannot.
                        appliedSig       <- flippedCalleeSignature(vfqn, sv, explicitTypeArgs).flatMap {
                                              case Some(flipped) => pure(flipped)
                                              case None          =>
                                                evalExpr(sv.value.signature.value, env = Some(Env.empty))
                                                  .map(sig => explicitTypeArgs.foldLeft(sig)(Evaluator.applyValue))
                                            }
                        // W4 (deferred W3 item 1): a calculated-return value referenced as a *complete* value — no
                        // parameters left to apply, so its whole type is its (under-applied) source return (`def y: Int
                        // = x` ⟹ a bare `Int`) — is resolved from its monomorphized return here, so a no-argument
                        // producer used by name works instead of leaking the under-applied return into a mismatch.
                        // `resolveCompleteCalculatedReturn` self-gates on that under-application; the applied case keeps
                        // a `VPi` here (resolved by `applyInferred`), and a calculated-return *function* passed
                        // unapplied keeps the under-applied return inside its codomain (the higher-order limit, out of
                        // scope).
                        calcReturn       <- calcReturns.resolveCompleteCalculatedReturn(vfqn, explicitTypeArgs, appliedSig)
                        afterCalc         = calcReturn.getOrElse(appliedSig)
                        // Discharge a `{Throw[String]}` guard on a *complete* (fully applied) value read by name (W2b):
                        // `def y: Bar = foo` where `foo`'s return is `Right(Bar)`. A guarded *function* read unapplied
                        // stays a `VPi`/`VLam` (the guard is in its codomain), so it is left untouched here.
                        discharged       <- calcReturns.dischargeGuardedReturn(afterCalc, vfqn).map(_.getOrElse(afterCalc))
                      } yield (
                        SemExpression(discharged, SemExpression.ValueReference(vfqn, explicitTypeArgs)),
                        discharged
                      )
                    case None     =>
                      liftF(compilerError(tm.as("Name not defined.")) >> abort)
                  }
      } yield result

  /** The Step-6 callee flip (signature split): a callee reference's *own* reduced ground signature, read from
    * `CompilerMonomorphicValue(callee@Signature, groundArgs)` — the same twin mono the value's own signature already
    * flips to. Applies only when the reference **fully applies** the callee (an explicit argument per binder, so the
    * ground read's per-instantiation defaulting matches the caller's intent) and **every argument is ground** (a
    * quotable [[SemValue]]); a partial application whose remaining binders this call infers, or an argument still an
    * unsolved meta, keeps the in-place evaluation that handles those. `None` falls back. Acyclic: the twin's own mono
    * checks no body, so it never re-enters this path.
    */
  private def flippedCalleeSignature(
      vfqn: Sourced[ValueFQN],
      sv: SaturatedValue,
      explicitTypeArgs: Seq[SemValue]
  ): CheckIO[Option[SemValue]] =
    if (
      signatureOnly ||
      explicitTypeArgs.sizeIs != SignatureView.of(sv.value.signature).binders.size
    ) pure(None)
    else
      get.flatMap { s =>
        explicitTypeArgs.toList.traverse(a => Quoter.quote(0, a, s.unifier.metaStore)) match {
          case Right(groundArgs) =>
            liftF(
              getFactIfProduced(
                CompilerMonomorphicValue.Key(vfqn.value.copy(name = vfqn.value.name.signatureTwin), groundArgs)
              )
            ).map(_.map(cmv => Evaluator.groundToSemPi(cmv.signature)))
          case Left(_)           => pure(None)
        }
      }

  /** Infer a function application by operating on its full spine: decompose the nested (curried) applications at the
    * root, resolve the head once — an immediately-applied unannotated lambda `(x -> body)(arg)` routes to
    * [[typeImmediateLambda]] (it is a `let`, the shape a non-effectful block `val`/statement lowers to; the lambda
    * alone has no inferable parameter type, so it is inferred from the first argument), anything else is inferred —
    * then resolve the arguments in two phases (docs/effect-lift-in-checker.md):
    *
    *   - **Phase A** (left to right, [[applyInferred]]): each slot runs the resolution ladder immediately, *except* a
    *     slot whose domain is a bare flex metavariable receiving an effect-carrier-headed argument, which is
    *     *deferred* — resolving it eagerly would solve the meta to the carrier type before later arguments could
    *     rigidify it (the `readLine.f` shape).
    *   - **Phase B** ([[resolveDeferredSlot]], left to right): a deferred slot's domain rigidified by later arguments
    *     runs the full ladder (unify / coerce / bind-lift); a still-flex one prefers pass-through (unify with the
    *     carrier-headed type), so the effectful result propagates upward and the parent's slot decides.
    *   - **Assemble** ([[assembleSpine]]): rebuild the chain if Phase B changed a slot, then fold the recorded
    *     effect-binds around the core ([[EffectLifter.wrapBinds]]).
    *
    * Each fold step receives the intermediate application node's own [[Sourced]] target, so diagnostics and the
    * rebuilt [[SemExpression]] keep the exact positions the former per-curried-node recursion produced.
    */
  private def inferSpine(tm: Sourced[OperatorResolvedExpression]): CheckIO[(SemExpression, SemValue)] = {
    val (head, apps) = decomposeSpine(tm)
    inferSpineApplications(tm, head, apps)
  }

  private def inferSpineApplications(
      tm: Sourced[OperatorResolvedExpression],
      head: Sourced[OperatorResolvedExpression],
      apps: List[(Sourced[OperatorResolvedExpression], Sourced[OperatorResolvedExpression])]
  ): CheckIO[(SemExpression, SemValue)] = {
    for {
      (start, rest)    <- head.value match {
                            case OperatorResolvedExpression.FunctionLiteral(paramName, None, body) =>
                              typeImmediateLambda(head, paramName, body, apps.head._2, None).map((_, apps.tail))
                            case _                                                                 =>
                              infer(head).map((_, apps))
                          }
      // §7 step 4 (finding 14): the head callee's carrier-stack recognition tag — the value-parameter positions whose
      // declared type is a pinned row (`catch`'s `computation`). The fold index below is the value-parameter index
      // (generics are peeled / on the reference, never spine applications), so `pinnedParams.contains(i)` classifies
      // the i-th slot. Empty for a non-value-reference head (a lambda, a nested application).
      pinnedParams     <- calleePinnedParams(head)
      (built, records) <- rest.zipWithIndex.foldLeftM((start, Vector.empty[SlotRecord])) {
                            case (((targetExpr, targetType), recs), ((target, arg), index)) =>
                              applyInferred(target, targetExpr, targetType, arg, pinnedParams.contains(index)).map {
                                case (expr, tpe, record) => ((expr, tpe), recs :+ record)
                              }
                          }
      hadDeferred       = records.exists(_.outcome.isInstanceOf[SlotOutcome.Deferred])
      finalRecords     <- records.traverse(resolveDeferredSlot(_, built._2))
      result           <- assembleSpine(built, finalRecords, hadDeferred)
    } yield result
  }

  /** The head callee's **carrier-stack recognition tag** (docs/effects-as-channel.md §7 step 4, finding 14): the set of
    * its value-parameter positions whose expected slot is a canonical carrier stack the checker must split before
    * payload unification. Two disjoint sources are unioned:
    *
    *   - **source (i)** — the value-parameter positions whose declared type is a *pinned row* (`catch`'s
    *     `computation: {Throw[E] | G} A`), read from the callee's `OperatorResolvedValue.effectRow.pinnedParameterIndices`
    *     via its [[SaturatedValue]];
    *   - **source (ii)** — a platform **run boundary** ([[RunBoundaryFunction]]; the jvm `runMain`), whose carrier
    *     capture is its parameter 0 (`io: IO[A]`). A concrete platform carrier like `IO` cannot be spelled as a pinned
    *     row and is not lang-nameable, so the owning platform declares the boundary by construction and the checker reads
    *     it here — never guessing carrier-ness from the domain's shape (which would miscompile a data container, finding
    *     14).
    *
    * Empty for a head that is not a plain value reference (an immediately-applied lambda, a nested application) or whose
    * facts are both absent — so the join routing simply never fires there and the whole-unify path is unchanged.
    */
  private def calleePinnedParams(head: Sourced[OperatorResolvedExpression]): CheckIO[Set[Int]] =
    head.value match {
      case OperatorResolvedExpression.ValueReference(vfqn, _) =>
        for {
          declared <- liftF(getFactIfProduced(SaturatedValue.Key(vfqn.value, platform)))
                        .map(_.map(_.value.effectRow.pinnedParameterIndices).getOrElse(Set.empty))
          boundary <- liftF(getFactIfProduced(RunBoundaryFunction.Key(vfqn.value)))
                        .map(_.map(_ => Set(0)).getOrElse(Set.empty[Int]))
        } yield declared ++ boundary
      case _                                                  => pure(Set.empty)
    }

  /** The outcome of resolving one spine argument slot. */
  private sealed trait SlotOutcome {

    /** The expression this slot contributes to the application chain — final for `Resolved` (the ladder ran),
      * provisional (the uninstantiated argument) for `Deferred`.
      */
    def slotExpr: SemExpression
  }

  private object SlotOutcome {

    /** The ladder resolved the slot in place (unified, coerced, or pure-wrapped). */
    case class Resolved(slotExpr: SemExpression) extends SlotOutcome

    /** Phase-A deferral (bare flex domain + effect-carrier-headed argument); decided in Phase B. */
    case class Deferred(slotExpr: SemExpression, argType: SemValue, domain: SemValue) extends SlotOutcome

    /** A.8.7 suspension (runtime track): the computation met a bare-generic slot, whose mode only the instantiation
      * decides — recorded as a [[CheckState.ModeObligation]] and resolved at post-drain quiescence by
      * [[ModeResolver]], never mid-spine. The slot expression is the instantiated argument, passed through
      * provisionally: correct as-is for the pass/capture resolutions, and discarded wholesale by the splice-restart
      * for a hoist.
      */
    case class Suspended(slotExpr: SemExpression) extends SlotOutcome
  }

  /** One spine slot's record: the intermediate application node's [[Sourced]] target and argument, the instantiated
    * target expression used to build the node, the node's return type, and the slot's (possibly still deferred)
    * outcome.
    */
  private case class SlotRecord(
      target: Sourced[OperatorResolvedExpression],
      arg: Sourced[OperatorResolvedExpression],
      updatedTarget: SemExpression,
      retType: SemValue,
      outcome: SlotOutcome
  )

  /** Phase B: decide a deferred slot.
    *
    * On the **runtime** track there is no mid-spine decision anymore (docs/effects-as-rows.md A.8.7): the deferred
    * computation becomes a **suspended obligation** — recorded with its instantiated type, the slot's domain meta and
    * the spine's result type, with *no* unification into the slot (first-contact unification is itself a mode
    * decision) — and the post-drain [[ModeResolver]] classifies it against the solved store at quiescence.
    *
    * On the **compiler** track the mid-spine decision is kept (the §8 boundary — the compile-time track keeps the
    * default ladder by design), reduced by the effects-as-rows deletion slices to the two arms the gate exercises: a
    * still-bare-flex domain **adopts** the carrier-headed argument (pass-through — the effect rides up into the
    * domain meta and the enclosing slot decides), and anything else runs the ladder. The bind-sequencing arms of both
    * (the ride-up-vs-bind default and the sequence-before-unify guard) fired zero times across the full gate on either
    * track and were deleted with the `tryBindLift` arm they called.
    */
  private def resolveDeferredSlot(record: SlotRecord, spineType: SemValue): CheckIO[SlotRecord] = record.outcome match {
    case SlotOutcome.Deferred(argExpr, argType, domain) if platform == Platform.Runtime =>
      modify(_.recordModeObligation(CheckState.ModeObligation(record.arg, argType, domain, record.retType, spineType)))
        .as(record.copy(outcome = SlotOutcome.Suspended(argExpr)))
    case SlotOutcome.Deferred(argExpr, argType, domain)                                 =>
      for {
        forcedDomain <- force(domain)
        outcome      <- forcedDomain match {
                          case VMeta(id, Spine.SNil) =>
                            for {
                              (updated, instantiated) <- instantiatePolymorphic(argExpr, argType)
                              _                       <- doUnify(VMeta(id, Spine.SNil), instantiated, record.arg.as("Type mismatch."))
                            } yield SlotOutcome.Resolved(updated): SlotOutcome
                          case _                     =>
                            // A meta-*applied* domain (`?G[?A]`, a generic container parameter) and a rigidified one
                            // alike: the carrier can legitimately ride into the domain, so the ladder decides.
                            resolveLadder(record.arg, argExpr, argType, domain)
                        }
      } yield record.copy(outcome = outcome)
    case _                                                                              => pure(record)
  }

  /** Assemble the spine result: rebuild the application chain when Phase B changed a deferred slot's expression. The
    * checker no longer folds effect-binds around a spine core — every argument the elaboration sequences is either
    * hoisted by the desugar up front or suspended and spliced by the post-drain [[ModeResolver]] — so with no deferral
    * this is the Phase-A build unchanged.
    */
  private def assembleSpine(
      built: (SemExpression, SemValue),
      records: Vector[SlotRecord],
      hadDeferred: Boolean
  ): CheckIO[(SemExpression, SemValue)] =
    pure((if (hadDeferred) rebuildChain(records) else built._1, built._2))

  /** Rebuild the application chain with each slot's final expression (needed only when Phase B changed a deferred
    * slot, so the Phase-A build holds a provisional argument). Node types are the Phase-A computed return types; the
    * head-level target keeps its instantiated form, and each inner node carries the type the per-slot instantiation
    * assigned it.
    */
  private def rebuildChain(records: Vector[SlotRecord]): SemExpression =
    records
      .foldLeft(Option.empty[SemExpression]) { case (prev, record) =>
        val targetExpr = prev match {
          case None       => record.updatedTarget
          case Some(node) => node.copy(expressionType = record.updatedTarget.expressionType)
        }
        Some(
          SemExpression(
            record.retType,
            SemExpression.FunctionApplication(record.target.as(targetExpr), record.arg.as(record.outcome.slotExpr))
          )
        )
      }
      .getOrElse(throw new IllegalStateException("Rebuilding an empty application spine."))

  /** Decompose a nested (curried) application into its head and, for each argument, the intermediate application
    * node's target paired with that argument — e.g. `f(a)(b)` yields `(f, [(f, a), (f(a), b)])`. The intermediate
    * targets carry the [[Sourced]] positions the per-argument logic attributes errors to.
    */
  private def decomposeSpine(
      tm: Sourced[OperatorResolvedExpression]
  ): (Sourced[OperatorResolvedExpression], List[(Sourced[OperatorResolvedExpression], Sourced[OperatorResolvedExpression])]) =
    tm.value match {
      case OperatorResolvedExpression.FunctionApplication(target, arg) =>
        val (head, apps) = decomposeSpine(target)
        (head, apps :+ (target, arg))
      case _                                                           => (tm, Nil)
    }

  /** Apply one argument of a spine ([[inferSpine]]'s Phase-A fold step): peel any polytype (`VLam`) layers with fresh
    * metas, then apply the argument to the resulting monotype. If the monotype isn't already `VPi`, it gets unified
    * against a fresh one. The implicit metas introduced by peeling are baked into the target reference. The argument
    * itself is resolved by [[checkArgumentSlot]] (the ladder, the flex-slot deferral, the bind-lift); the returned
    * [[SlotRecord]] carries the slot's outcome for Phase B and the spine assembly.
    */
  private def applyInferred(
      target: Sourced[OperatorResolvedExpression],
      targetExpr: SemExpression,
      targetType: SemValue,
      arg: Sourced[OperatorResolvedExpression],
      pinned: Boolean
  ): CheckIO[(SemExpression, SemValue, SlotRecord)] =
    for {
      (updatedTarget, peeled) <- instantiatePolymorphic(targetExpr, targetType)
      vpi                     <- peeled match {
                                   case p: VPi => pure(p)
                                   case _      =>
                                     for {
                                       domMeta <- freshMeta
                                       codMeta <- freshMeta
                                       p        = VPi(domMeta, _ => codMeta)
                                       _       <- doUnify(peeled, p, target.as("Not a function."))
                                     } yield p
                                 }
      outcome                 <- checkArgumentSlot(arg, vpi.domain, pinned)
      argExpr                  = outcome.slotExpr
      argSem                  <- evalExpr(arg.value)
      // The codomain may embed a native applied to the target's instantiation metas — e.g. a dependent result type
      // `Int[add(LMin,RMin), …]`. Those bounds are solved by the argument checks just above, so renormalise the
      // codomain now to re-fire the natives (`add(3,4) ⤳ 7`) before the type reaches unification or quoting. A *bare*
      // metavariable result (a result-position type parameter, e.g. `pick[A](a,b): A`) has nothing to re-fire and is
      // left untouched; it resolves through the meta store wherever it is next forced.
      rawRetType               = vpi.codomain(argSem)
      retType                 <- rawRetType match {
                                   case _: VMeta => pure(rawRetType)
                                   case other    =>
                                     calcReturns.resolveCalculatedReturn(updatedTarget, other).flatMap {
                                       case Some(resolved) => pure(resolved)
                                       // Not a calculated return: renormalise the codomain (re-firing any stuck guard
                                       // natives now that the argument checks solved their bounds), then discharge a
                                       // `{Throw[String]}` guard the callee returns (W2b) — `Right(t)` ⤳ `t`, `Left`
                                       // aborts, a still-stuck guard is left to defer at this generic caller's own site.
                                       case None           =>
                                         renormalize(other).flatMap(rn =>
                                           calcReturns.dischargeGuardedReturn(rn, target).map(_.getOrElse(rn))
                                         )
                                     }
                                 }
    } yield (
      SemExpression(
        retType,
        SemExpression.FunctionApplication(target.as(updatedTarget), arg.as(argExpr))
      ),
      retType,
      SlotRecord(target, arg, updatedTarget, retType, outcome)
    )

  /** Resolve one spine argument against its parameter domain (Phase A). Lambda-shaped arguments route through the
    * ordinary [[check]] (a lambda is never effect-carrier-headed, so neither deferral nor lift applies — and the
    * immediately-applied-lambda `let` shape needs the expected type pushed down). Everything else is inferred once and
    * then either *deferred* (a bare flex domain receiving an effect-carrier-headed argument — Phase B decides) or run
    * through the shared resolution ladder ([[resolveGuardedLadder]], which folds in the effectful-signatures kind
    * acceptance (W2b) exactly as the return-boundary [[checkAgainst]] does).
    */
  private def checkArgumentSlot(
      arg: Sourced[OperatorResolvedExpression],
      domain: SemValue,
      pinned: Boolean
  ): CheckIO[SlotOutcome] =
    arg.value match {
      case _: OperatorResolvedExpression.FunctionLiteral                                            =>
        check(arg, domain).map(SlotOutcome.Resolved.apply)
      case OperatorResolvedExpression.FunctionApplication(target, _) if isUnannotatedLambda(target.value) =>
        check(arg, domain).map(SlotOutcome.Resolved.apply)
      case _                                                                                        =>
        for {
          (argExpr, argType) <- infer(arg)
          // A **declaration-generic** slot — the callee's declared parameter type is one of its own binders, so the
          // domain is a bare metavariable *before forcing*, whether or not a sibling argument has since solved it — is
          // exactly the position the rows desugar deliberately left undecided (A.8.6 bounded staging). Its mode is an
          // instantiation fact, so on the runtime track a computation there is **suspended** for the post-drain
          // [[ModeResolver]] ([[genericArgSlot]]) rather than elaborated mid-spine.
          outcome            <- if (platform == Platform.Runtime && isDeclarationGeneric(domain))
                                  genericArgSlot(arg, argExpr, argType, domain, pinned)
                                else routeArgumentSlot(arg, argExpr, argType, domain, pinned)
        } yield outcome
    }

  /** Whether the slot's *unforced* domain is one of the callee's own generic binders — a bare metavariable, which is
    * what [[instantiatePolymorphic]] leaves behind when it peels the callee's polytype. Read **before** forcing on
    * purpose: a sibling argument may already have solved that meta (`"Hello, " ++ readLine` solves `++`'s `A := String`
    * from the left operand), and the position is a *declaration*-generic one either way — which is what decides whether
    * the desugar wrote the elaboration or deferred it.
    */
  private def isDeclarationGeneric(domain: SemValue): Boolean = domain match {
    case VMeta(_, Spine.SNil) => true
    case _                    => false
  }

  /** A declaration-generic slot on the runtime track (docs/effects-as-rows.md A.8.7): a carrier-headed computation
    * there **suspends** — no unification into the slot, no mid-spine bind — and the post-drain [[ModeResolver]]
    * classifies it against the solved store (pass / hoist / capture). A *pure* actual carries no mode question at all
    * and routes normally ([[routeArgumentSlot]]), with the instantiation already done (a no-op re-instantiation
    * downstream).
    */
  private def genericArgSlot(
      arg: Sourced[OperatorResolvedExpression],
      argExpr: SemExpression,
      argType: SemValue,
      domain: SemValue,
      pinned: Boolean
  ): CheckIO[SlotOutcome] =
    for {
      // The suspension decision needs the argument's *instantiated* type — a bare ability-method reference (`readLine`)
      // infers as a polytype (`VLam`), whose carrier only appears once the binder is peeled to its (flagged) meta.
      (updatedExpr, instantiated) <- instantiatePolymorphic(argExpr, argType)
      split                       <- lifter.effectCarrierSplit(instantiated)
      outcome                     <- split match {
                                       case Some(_) =>
                                         pure(SlotOutcome.Deferred(updatedExpr, instantiated, domain))
                                       case None    =>
                                         routeArgumentSlot(arg, updatedExpr, instantiated, domain, pinned)
                                     }
    } yield outcome

  /** The three-way slot routing over the *forced* domain: a **plain payload** parameter domain (a concrete value type)
    * and an **effect-carrier** domain (`?G[T]` — the ambient / a callee's `F ~ Effect` binder; a conditional arm
    * `value: {Abort} T`, a discharger's `fallback: G[A]`) route through the uniform ladder; everything else keeps the
    * default Phase-A logic. Restricted to the **runtime** track, exactly as the return boundary is
    * ([[uniformReturnRoutable]]): the §8 boundary keeps the compile-time track (`eliot-compiler/` value bodies, the
    * `Either` guard discharge) entirely on the default path — carrier-free — so it stays byte-identical.
    */
  private def routeArgumentSlot(
      arg: Sourced[OperatorResolvedExpression],
      argExpr: SemExpression,
      argType: SemValue,
      domain: SemValue,
      pinned: Boolean
  ): CheckIO[SlotOutcome] =
    for {
      forcedDomain  <- force(domain)
      uniform        = platform == Platform.Runtime
      plainDomain   <- if (uniform) uniformPlainValueType(forcedDomain) else pure(false)
      carrierDomain <- if (uniform && !plainDomain) lifter.effectCarrierSplit(forcedDomain).map(_.nonEmpty)
                       else pure(false)
      outcome       <- if (plainDomain) {
                         uniformPayloadSlot(arg, argExpr, argType, forcedDomain, pinned)
                       } else if (carrierDomain) {
                         uniformCarrierSlot(arg, argExpr, argType, forcedDomain)
                       } else {
                         defaultArgSlot(arg, argExpr, argType, forcedDomain)
                       }
    } yield outcome

  /** The default (carrier-based) Phase-A argument-slot resolution — kept verbatim as the fallback for every slot the
    * U3a-2b(ii) uniform ladder does not cover (the live path when `uniformCarrier` is off, and under it for a
    * flex/carrier domain or a slot the uniform path declines). A bare flex domain receiving an effect-carrier-headed
    * argument is *deferred* (Phase B decides); everything else runs the shared resolution ladder.
    */
  private def defaultArgSlot(
      arg: Sourced[OperatorResolvedExpression],
      argExpr: SemExpression,
      argType: SemValue,
      forcedDomain: SemValue
  ): CheckIO[SlotOutcome] =
    forcedDomain match {
      case VMeta(_, Spine.SNil) =>
        // The deferral decision needs the argument's *instantiated* type — a bare ability-method reference (`readLine`)
        // infers as a polytype (`VLam`), whose carrier only appears once the binder is peeled to its (flagged) meta.
        // Instantiating here is exactly once either way (the ladder's own instantiation is a no-op on a monotype).
        for {
          (updatedExpr, instantiated) <- instantiatePolymorphic(argExpr, argType)
          out                         <- lifter.effectCarrierSplit(instantiated).flatMap {
                                           case Some(_) =>
                                             pure(SlotOutcome.Deferred(updatedExpr, instantiated, forcedDomain))
                                           case None    =>
                                             resolveGuardedLadder(arg, updatedExpr, instantiated, forcedDomain)
                                         }
        } yield out
      case _                    =>
        resolveGuardedLadder(arg, argExpr, argType, forcedDomain)
    }

  /** Effects-as-channel U3a-2b(ii), spine wiring (docs/effects-as-channel.md §10) — gated on [[uniformCarrier]]: resolve
    * an argument against a **plain payload** parameter domain through the uniform ladder. The argument is instantiated
    * once (peeling a polytype like `readLine`'s `[F ~ Console] F[String]` to its carrier-headed monotype `?F[String]`),
    * then routed through [[uniformArgumentSlot]] when it is a carrier-headed value whose payload fits the domain by pure
    * definitional equality:
    *
    *   - a **pure** actual (a plain `VTopDef` value) ⇒ it passes directly, with no node at all — it already *is* its
    *     payload (A.8.10; a manufactured `Id` head used to be wrapped here and stripped again downstream);
    *   - an **effectful** actual (`?F[String]`, an ambient/role effect carrier) ⇒ the slot **suspends**: sequencing it
    *     is the *hoist*, which the row desugar owns and the post-drain [[ModeResolver]] splices (A.8.9).
    *
    * A function/polytype/type-level or ill-fitting argument (no carrier-headed payload, or a payload that does not fit)
    * falls back to [[defaultArgSlot]] with the already-instantiated argument (a no-op re-instantiation), so flag-off and
    * the un-routed shapes stay byte-identical.
    */
  private def uniformPayloadSlot(
      arg: Sourced[OperatorResolvedExpression],
      argExpr: SemExpression,
      argType: SemValue,
      domain: SemValue,
      pinned: Boolean
  ): CheckIO[SlotOutcome] =
    for {
      (updatedExpr, instantiated) <- instantiatePolymorphic(argExpr, argType)
      // Bind vs capture, decided by whether the actual's **payload** fits the domain (the payload split off the carrier
      // first, so the carrier meta can never be stolen):
      //   - **payload fits** ⇒ **bind** ([[uniformArgumentSlot]]): the effect runs and the payload flows into the slot —
      //     `printLine(readLine)` (`String` fits `String`), and crucially the compound-state case `items : ?F[List[X]]`
      //     into `foldLeft`'s `list : List[A]` (`List[X]` fits `List[A]`, `A := X`), which the default path *rejects*
      //     because the equal-arity unify steals the carrier (`?F := List`, then `Effect[List]` has no instance). A pure
      //     actual whose payload fits passes directly, with no node at all — it already *is* its payload.
      //   - **payload does NOT fit** ⇒ [[uniformCaptureSlot]]: either a **capture** — the whole effectful actual's carrier
      //     partial-applies a carrier-stack / pinned domain (a discharger's `computation: {Abort | G} A` ⤳
      //     `AbortCarrier[G, A]`, `runMain`'s `IO[A]`; the actual's *payload* `Option[?E]` does not fit `AbortCarrier`,
      //     but the *whole* `?G[Option[?E]]` unifies via `?G := AbortCarrier[G']`, storing the computation — the uniform
      //     ladder's arm-1 whole-type pass-through) — or a doomed under-applied bind / an ordinary mismatch, both left
      //     on the default ladder.
      // Checking payload-fit *first* is what distinguishes the two: a carrier-stack domain's inner value never fits its
      // outer carrier, so it captures; a data container's element type does fit, so it binds.
      payload                     <- uniformPayloadOf(instantiated)
      payloadFits                 <- payload match {
                                       case Some(p) => payloadFitsDomain(p, domain, arg.as("Type mismatch."))
                                       case None    => pure(false)
                                     }
      // A carrier-headed actual whose payload fits is the **hoist** shape: the effect runs at the call site and the
      // payload flows into the slot. That is an elaboration decision, and under effects-as-rows the desugar owns it —
      // so the slot **suspends** instead of binding mid-spine, and the post-drain [[ModeResolver]] classifies it
      // (a rigid non-carrier domain with a fitting payload ⟹ hoist) and splices the desugar's own bind chain.
      // The desugar left this position undecided because the *argument* is generic-headed (`printLine(x.field)` —
      // the dot operator's return is its bare binder `B`), never because the slot is: a declaration-decided argument
      // was already hoisted before the checker ever saw it, and arrives here pure.
      actualCarrier               <- lifter.effectCarrierSplit(instantiated)
      outcome                     <- if (payloadFits && actualCarrier.nonEmpty) {
                                       pure(SlotOutcome.Deferred(updatedExpr, instantiated, domain): SlotOutcome)
                                     } else if (payloadFits) {
                                       uniformArgumentSlot(arg, updatedExpr, instantiated, domain)
                                     } else {
                                       uniformCaptureSlot(arg, updatedExpr, instantiated, domain, pinned)
                                     }
    } yield outcome

  /** The no-fit branch of [[uniformPayloadSlot]] (U4-a(ii)): an actual whose payload does not fit the plain domain is
    * either a **capture** — the whole carrier-headed actual pass-through-unifies with a carrier-stack / pinned domain
    * (`{Abort | G} A` ⤳ `AbortCarrier[G, A]`, `runMain`'s `IO[A]`), the uniform ladder's **arm-1 whole-type
    * pass-through** — or a *doomed* under-applied bind / an ordinary mismatch, both left on the default ladder.
    *
    * The doomed shape ([[EffectLifter.mustLiftBeforeUnify]] — a carrier-meta application against an under-applied /
    * equal-arity-non-carrier rigid head, always a **bare-flex payload** here since a concrete-payload fit would have
    * taken the `payloadFits` branch) is checked **first**: it must *bind-lift* (sequence the effect), never capture,
    * so it routes through the uniform **bind** arm ([[uniformArgumentSlot]] → the PayloadSlot bind: the flex payload
    * unifies with the domain, the carrier binds), byte-identical to the default `tryBindLift` (same payload solve, same
    * `$eff$N`/`Bind`). Otherwise the whole-type unify is tried ([[tryUnifyCommitting]]): **success is the capture** — a
    * uniform `Resolved`, byte-identical to the default `resolveFailureLadder`'s arm-1 whole-unify (the same
    * `tryUnifyCommitting`, same solutions, same slot expr); **failure is the mismatch**, committed directly via
    * [[commitMismatch]] — byte-identical to the default (a non-fitting non-doomed actual's bind-lift / pure-wrap arms
    * cannot fire, so the default ladder also bottoms out at exactly this `commitMismatch`; the failed
    * `tryUnifyCommitting` here commits nothing, so no state differs).
    */
  private def uniformCaptureSlot(
      arg: Sourced[OperatorResolvedExpression],
      updatedExpr: SemExpression,
      instantiated: SemValue,
      domain: SemValue,
      pinned: Boolean
  ): CheckIO[SlotOutcome] =
    for {
      // The argument's own carrier meta, read *before* the capture unify solves it — the row-argument type-pinning
      // rules (docs/effects-as-channel.md §7/§10) need its id to look its declared ability constraints up.
      carrierMeta <- lifter.effectCarrierSplit(instantiated).map(_.collect { case (VMeta(id, _), _) => id.value })
      doomed      <- lifter.mustLiftBeforeUnify(instantiated, domain)
      // §7 step 4 (finding 14): route the capture through the JOIN solver — not the eager whole-unify — when the callee's
      // tagged pinned-row parameter (`catch`'s `{Throw[E] | G} A` ⤳ `ThrowCarrier[E, G, A]`) receives an open-row
      // effectful actual. The join splits the domain's carrier stack off first (`Carrier.split`), joins the actual's
      // carrier toward it, and unifies payloads — the same solution the whole-unify reaches by partial-application
      // injectivity, but with the carrier meta split off first so it can never be stolen. Guarded to the **first slice**:
      // a single-layer pinned domain (`{E | G} A`, base a generic meta) + an open-row-carrier actual. Every other capture
      // (multi-layer pinned domains, concrete-payload actuals, doomed shapes) stays on the whole-unify fallback below,
      // and `mustLiftBeforeUnify` doomed shapes take their bind arm first, unchanged.
      joinRoutable <- if (pinned && !doomed && carrierMeta.nonEmpty) singleLayerCarrierDomain(domain) else pure(false)
      outcome     <- if (doomed) {
                       // The doomed shape must *sequence*, never capture — and sequencing is the desugar's rewrite, so
                       // the slot suspends and the post-drain [[ModeResolver]] hoists it (its flex payload fits the
                       // rigid domain, which is the hoist classification).
                       pure(SlotOutcome.Deferred(updatedExpr, instantiated, domain): SlotOutcome)
                     } else if (joinRoutable)
                       for {
                         // The eager row-directed pin (finding 13 §4) runs *inside* the join too — it derives the domain's
                         // error slot from the argument's row constraints, so the join solves `?F := ThrowCarrier[String, ?G]`
                         // with the error slot already `String` rather than junk-grounding.
                         _   <- carrierMeta.traverse_(eagerRowPinIntoDomain(_, domain, arg))
                         out <- uniformArgumentSlot(arg, updatedExpr, instantiated, domain, forcePinnedCarrier = true)
                       } yield out
                     else
                       for {
                         // §7 row-directed-at-elaboration pin (finding 13 §4): pin the *pinned-row domain*'s carrier-layer
                         // ability slots from the argument's own row constraints **before** the capturing whole-unify, so a
                         // free error-slot meta never exists to junk-ground.
                         _   <- carrierMeta.traverse_(eagerRowPinIntoDomain(_, domain, arg))
                         out <- tryUnifyCommitting(instantiated, domain, arg.as("Type mismatch.")).flatMap {
                                  case true  =>
                                    pure(SlotOutcome.Resolved(updatedExpr): SlotOutcome)
                                  case false =>
                                    commitMismatch(instantiated, domain, arg, updatedExpr)
                                }
                       } yield out
    } yield outcome

  /** Whether `domain` is a **single-layer** carrier stack the §7 step-4 join routing admits (finding 14) — always
    * consulted behind the callee's recognition tag ([[calleePinnedParams]]), so it only ever classifies a domain already
    * known to be a carrier capture; it merely tells a single-layer stack (route through the join) from a pre-nested
    * multi-layer one (leave on the whole-unify fallback). Two single-layer shapes qualify:
    *
    *   - a **pinned-row stack** `<Ability>Carrier[…, G, A]` (source (i): `catch`/`runThrow`/`else`/`runStateToPair`/
    *     `provide`'s `{E | G} A`) whose base — the carrier's last stack slot, before the payload — is still a bare
    *     metavariable (the generic tail `G`), not itself a nested carrier;
    *   - a **flat concrete carrier** `IO[A]` (source (ii): the jvm run boundary `runMain`) — a `Con` with an *empty*
    *     stack prefix (no error/state/base slots), so its `split` yields just the payload. This is the empty-`prefix`
    *     case; it is a carrier by the tag, so admitting it routes `?F[Unit] ~ IO[A]` through the pass-join (`?F := IO`)
    *     instead of the eager whole-unify.
    *
    * A pre-nested stack (a multi-layer pinned domain, base a concrete carrier) is left on the whole-unify fallback.
    */
  private def singleLayerCarrierDomain(domain: SemValue): CheckIO[Boolean] =
    force(domain).flatMap {
      case VTopDef(_, _, Spine.SApp(prefix, _)) =>
        prefix.toList.lastOption match {
          case Some(base) => force(base).map {
              case VMeta(_, Spine.SNil) => true
              case _                    => false
            }
          case None       => pure(true) // a flat concrete carrier (`IO[A]`) — empty prefix, so no base slot to nest
        }
      case _                                    => pure(false)
    }

  /** Row-directed discharge pinning **at elaboration** (docs/effects-as-channel.md §7, finding 13 §4): when an open-row
    * argument (`?F ~ Throw[String]`) is captured whole into a pinned-row parameter *domain* (`ThrowCarrier[E, G, A]`),
    * pin the domain's carrier-layer ability slots (`E := String`) *before* the capturing whole-unify, directly from the
    * argument's own row constraints ([[CheckState.metaConstraints]]).
    *
    * Deriving the pin from the argument's row — the single source of truth: the `E` the user means is the one the
    * computation's row declares — at capture time means a free error-slot meta never exists to junk-ground to `Type`
    * and select the `where E1 != E2` lift (the pinned-finding-7 class), **even when the base carrier `G` is concrete**
    * (`IO`). This is the principled form U4-f approximated at the slots, and — since the effects-as-rows slices
    * retired the *deferred* pin-if-still-free backstop that never once applied across the gate — the only pinning
    * mechanism left.
    *
    * Fail-safe: a constraint whose ability has no matching carrier layer in the domain records no pin (the whole-unify
    * runs unchanged), and a domain slot already solved to a value is left untouched (pin-if-still-free) so an
    * explicitly-typed slot is never overwritten — misfiring can only miss the pin, never accept a wrong typing.
    */
  private def eagerRowPinIntoDomain(
      actualCarrierMetaId: Int,
      domain: SemValue,
      at: Sourced[OperatorResolvedExpression]
  ): CheckIO[Unit] =
    inspect(_.metaConstraints.getOrElse(actualCarrierMetaId, Seq.empty)).flatMap {
      _.traverse_ { constraint =>
        val nonCarrierArgs = constraint.args.dropRight(1)
        if (nonCarrierArgs.isEmpty) pure(())
        else
          findCarrierLayerSlots(domain, EffectCarrierNaming.carrierFQN(constraint.abilityFQN)).flatMap {
            case Some(slots) =>
              nonCarrierArgs.zip(slots).traverse_ { case (value, slot) =>
                force(slot).flatMap {
                  case VMeta(_, Spine.SNil) =>
                    doUnify(slot, value, at.as("Row-argument effect pinning (elaboration)."))
                  case _                    => pure(())
                }
              }
            case None        => pure(())
          }
      }
    }

  /** The leading (result-unapplied) spine slots of the outermost `carrierFqn` layer within a canonical carrier stack,
    * descending through each layer's *base* (its last spine element) — `ThrowCarrier`'s slots inside
    * `StateCarrier[S, ThrowCarrier[E, G]]`. [[None]] when no layer of that carrier is present.
    */
  private def findCarrierLayerSlots(carrier: SemValue, carrierFqn: ValueFQN): CheckIO[Option[Seq[SemValue]]] =
    force(carrier).flatMap {
      case VTopDef(fqn, _, spine) if fqn == carrierFqn      => pure(Some(spine.toList))
      case VTopDef(_, _, spine) if spine.toList.nonEmpty    => findCarrierLayerSlots(spine.toList.last, carrierFqn)
      case _                                                => pure(None)
    }

  /** Whether an effectful actual's **payload** `p` *genuinely* fits the parameter `domain` — the bind-vs-capture
    * decision of [[uniformPayloadSlot]]. A **bare flex payload metavariable** (`?A`) is **not** a genuine fit even
    * though it speculatively unifies with anything: a discharger's `raise`/`map` over a still-polymorphic carrier
    * (`raise(err) : ?F[?A]` into `map`'s `fa : F[A]`) has such a payload, and treating it as "fits" would **bind**
    * (sequence) a computation that must be **captured** whole — the flex payload would absorb the domain and strip the
    * carrier (the same flex-payload theft the old capture check guarded against). Only a *headed* payload (`List[X]`,
    * `String`, `Option[..]`) whose head actually matches the domain counts, so the compound-state `List[X]`-into-`List[A]`
    * bind is admitted while the carrier-stack capture is not.
    */
  private def payloadFitsDomain(payload: SemValue, domain: SemValue, context: Sourced[String]): CheckIO[Boolean] =
    force(payload).flatMap {
      case VMeta(_, Spine.SNil) => pure(false)
      case _                    => unifiesDefinitionally(payload, domain, context)
    }

  /** Effects-as-channel U3a-2b(ii), the conditional-arm slice (docs/effects-as-channel.md §3) — gated on
    * [[uniformCarrier]] + `Platform.Runtime`: resolve an argument against an **effect-carrier** parameter domain
    * (`?G[T]` — a conditional arm `if`'s `value: {Abort} T`, a discharger's `fallback: G[A]`, an effect combinator's
    * `fa: F[A]`). The uniform-carrier property the default path lacks is that at such a slot a carrier meta must be
    * solved by the *payload*, never stolen whole by the actual's head. Two arms, telling **pure** apart from
    * **effectful** by the actual's own carrier ([[EffectLifter.effectCarrierSplit]]):
    *
    *   - **pure actual** (a plain `H[X]` value, `Id`-carried) ⇒ **pure-wrap first**
    *     ([[EffectLifter.tryPureWrap]]): the payload `H[X]` unifies with the carrier's payload slot `?T` and the term is
    *     `pure@Effect[?G]`-lifted, `?G` kept a meta the enclosing discharge / ambient solves (never defaulted). This
    *     fires *before* the default ladder's `tryUnifyCommitting`, which — at equal arity (`None : Option[?E]` into
    *     `?G[?T]`, both arity 1) — would **steal** the carrier whole (`?G := Option`) because the pure-wrap pre-arm only
    *     triggers on a strictly *under*-applied actual. That theft is exactly why `if(c, None) else Some(x)` is rejected
    *     on the default path; pure-wrapping first fixes it. Where the default path *does* pure-wrap (a strictly
    *     under-applied `"+"`), this produces the identical node — byte-identical. A pure-wrap that does not fit falls
    *     back to [[defaultArgSlot]] (which commits the mismatch).
    *   - **effectful actual** (`?F[Unit]`, a carrier-headed sibling) ⇒ [[defaultArgSlot]]: its carrier meta unifies with
    *     `?G` correctly (`?G := ?F`), the same as the default path — no theft hazard, so no reshaping needed.
    *
    * Reuses [[EffectLifter.tryPureWrap]] unchanged (reshape, not rebuild) — the clean single `pure@Effect[?G](arg)` node,
    * which is now the only shape there is: with no manufactured `Id` head (A.8.10) the historical double-wrap
    * `pure(runId(pure@Id(arg)))`, whose inner `pure@Id` confused the outer `pure`'s `Effect` instance resolution and
    * mis-erased it, can no longer be built. `if`'s `value` arm is a single slot
    * (no pure/effectful *sibling* in one call — that is `fold`'s `Generic` case), so the full `CarrierJoin` lattice is
    * not needed here.
    */
  private def uniformCarrierSlot(
      arg: Sourced[OperatorResolvedExpression],
      argExpr: SemExpression,
      argType: SemValue,
      domain: SemValue
  ): CheckIO[SlotOutcome] =
    for {
      (updatedExpr, instantiated) <- instantiatePolymorphic(argExpr, argType)
      effectful                   <- lifter.effectCarrierSplit(instantiated).map(_.nonEmpty)
      // Effects-as-channel U4-a(ii) (docs/effects-as-channel.md §10): an **effectful** actual into an effect-carrier
      // slot routes through the uniform CarrierSlot arm ([[uniformArgumentSlot]] → the pass-join: the actual's carrier
      // meta joins the domain's, the payloads unify, and the whole action passes through as `Passed`) rather than
      // handing off to [[defaultArgSlot]]. Byte-identical to the default whole-unify (`?F[Unit] ~ ?G[T]` ⇒ `?G := ?F`,
      // `T := Unit`, slot expr unchanged); exercised by `report`'s `if(flag, printLine("on"))` in the byte-identical gate.
      outcome                     <- if (effectful) {
                                       uniformArgumentSlot(arg, updatedExpr, instantiated, domain)
                                     } else
                                       lifter.tryPureWrap(arg, updatedExpr, instantiated, domain).flatMap {
                                         case Some(wrapped) =>
                                           pure(SlotOutcome.Resolved(wrapped): SlotOutcome)
                                         case None          =>
                                           defaultArgSlot(arg, updatedExpr, instantiated, domain)
                                       }
    } yield outcome

  /** The payload of a carrier-headed argument type, for the [[uniformPayloadSlot]] routing decision: the effect-carrier
    * payload for an effectful actual ([[EffectLifter.effectCarrierSplit]]), the value itself for a pure plain `VTopDef`
    * actual ([[uniformPlainValueType]]), and [[None]] for anything the uniform ladder does not carrier-head (a function
    * `VPi`/polytype `VLam`, `VType`, a bare metavariable).
    */
  private def uniformPayloadOf(tpe: SemValue): CheckIO[Option[SemValue]] =
    for {
      forced <- force(tpe)
      split  <- lifter.effectCarrierSplit(forced)
      plain  <- uniformPlainValueType(forced)
    } yield split.map(_._2).orElse(Option.when(plain)(forced))

  /** Resolve a routable argument through [[UniformCarrierChecker.resolveArgumentSlot]], which now only ever *passes*
    * the slot: an argument the elaboration must sequence never reaches here — it is hoisted by the desugar up front,
    * or suspended and spliced by the post-drain [[ModeResolver]].
    */
  private def uniformArgumentSlot(
      arg: Sourced[OperatorResolvedExpression],
      argExpr: SemExpression,
      argType: SemValue,
      domain: SemValue,
      forcePinnedCarrier: Boolean = false
  ): CheckIO[SlotOutcome] =
    uniformChecker
      .resolveArgumentSlot(arg, argExpr, argType, domain, forcePinnedCarrier)
      .map(SlotOutcome.Resolved(_))

  /** Whether `expr` is an unannotated function literal `(x -> body)`. Its parameter type cannot be inferred from the
    * literal alone; when it is *immediately applied* the type is taken from the argument (see [[typeImmediateLambda]]).
    */
  private def isUnannotatedLambda(expr: OperatorResolvedExpression): Boolean = expr match {
    case OperatorResolvedExpression.FunctionLiteral(_, None, _) => true
    case _                                                      => false
  }

  /** Type an immediately-applied unannotated lambda `(param -> body)(arg)` — a `let` (the shape a block
    * `val`/statement lowers to). The parameter type is taken from the (instantiated) argument; the body is checked
    * against `expected` when known (pushing the type down) and inferred otherwise. Returns the rebuilt application
    * expression and its type.
    *
    * The let-bind rule (docs/effect-lift-in-checker.md): an *effect-carrier-headed* argument bound by an unannotated
    * binder is sequenced — the binder receives the payload type `T'` and the whole `let` becomes
    * `flatMap/map(param -> body, arg)` ([[EffectLifter.bindWrap]]). This is what threads effects through `{ ... }`
    * blocks. An *annotated* carrier-typed binder — deliberate storage — never reaches this method (annotated
    * immediately-applied lambdas go through the ordinary application path, where the annotation unifies with the
    * carrier type).
    */
  private def typeImmediateLambda(
      target: Sourced[OperatorResolvedExpression],
      paramName: Sourced[String],
      body: Sourced[OperatorResolvedExpression],
      arg: Sourced[OperatorResolvedExpression],
      expected: Option[SemValue]
  ): CheckIO[(SemExpression, SemValue)] =
    for {
      (argExpr0, argType0) <- infer(arg)
      (argExpr, argType)   <- instantiatePolymorphic(argExpr0, argType0)
      split                <- lifter.effectCarrierSplit(argType)
      result               <- split match {
                                case Some((carrier, payload)) =>
                                  // The continuation body is *inferred*, never checked against the pushed-down carrier
                                  // expectation: a still-flex tail type (`old : ?S`) would wrongly unify with the whole
                                  // carrier type (`?S := IO[String]`), corrupting the binder's payload. The wrap decides
                                  // `map` (pure tail) vs `flatMap` (carrier-headed tail) from the inferred shape —
                                  // exactly the former desugarer's continuation rule — and the wrap's carrier-headed
                                  // result then resolves against the expected type (with coercion) at the let level.
                                  for {
                                    _                          <- modify(_.bindValueParam(paramName.value, payload))
                                    // Instantiate the inferred continuation's polytype (like the argument at the top of
                                    // this method): a bare polymorphic nullary reference in tail position (`state :
                                    // [S, F] F[S]`) must get its `[?S, ?F]` implicit type args here, or it reaches
                                    // monomorphization with none and its ability resolves at empty arguments. This is
                                    // instantiation, not the pushed-down carrier check the comment above warns against —
                                    // it peels leading polytype binders to fresh metas, leaving a monotype tail
                                    // (`?F[?S]`) the `bindWrap` below then classifies (an effect-carrier-headed tail
                                    // correctly selects `flatMap`; a bound-var / applied monotype tail is a peel no-op).
                                    (bodyExpr0, bodyType0)     <- infer(body)
                                    (bodyExpr, bodyType)       <- instantiatePolymorphic(bodyExpr0, bodyType0)
                                    bind                        = EffectLifter.Bind(paramName.value, arg, argExpr, argType, carrier, payload)
                                    (wrappedExpr, wrappedType) <- lifter.bindWrap(bind, bodyExpr, bodyType)
                                    resolved                   <- expected match {
                                                                    // Definitional equality, with the doomed shape
                                                                    // (`?G[String]` ~ `String`, which unification can
                                                                    // only postpone) skipped so it commits a single
                                                                    // Expected/Actual mismatch rather than the
                                                                    // unifier's per-type-argument spine errors. The
                                                                    // pure-boundary `Id` fallback that used to sit
                                                                    // here retired with the effects-as-rows slices —
                                                                    // a fully-discharged block `val` reaches a pure
                                                                    // return through the uniform return boundary's own
                                                                    // discharge ([[UniformCarrierChecker.checkReturnBoundary]]).
                                                                    case Some(exp) =>
                                                                      lifter
                                                                        .mustLiftBeforeUnify(wrappedType, exp)
                                                                        .flatMap {
                                                                          case true  => pure(false)
                                                                          case false => tryUnifyCommitting(wrappedType, exp, body.as("Type mismatch."))
                                                                        }
                                                                        .flatMap {
                                                                          case true  => pure((wrappedExpr, exp))
                                                                          case false =>
                                                                            modify(st =>
                                                                              st.withUnifier(st.unifier.addMismatch(wrappedType, exp, body.as("Type mismatch.")))
                                                                            ).as((wrappedExpr, exp))
                                                                        }
                                                                    case None      => pure((wrappedExpr, wrappedType))
                                                                  }
                                  } yield resolved
                                case None                     =>
                                  for {
                                    // A.8.7: a bound type still a *bare metavariable* is unclassifiable at build time —
                                    // the binding's bind-vs-plain mode is the bound instantiation's to decide (e.g. a
                                    // `val` over a generic-returning call whose suspended argument adopts a carrier only
                                    // at quiescence). Build the plain `let` and record a let obligation; a bound type
                                    // resolved carrier-headed by quiescence gets the desugar's binding rewrite spliced
                                    // ([[ModeResolver.pendingLetTargets]]) and the mono restarted.
                                    forcedArg            <- force(argType)
                                    _                    <- forcedArg match {
                                                              case VMeta(_, Spine.SNil) if platform == Platform.Runtime =>
                                                                modify(_.recordLetObligation(CheckState.LetObligation(arg, argType)))
                                                              case _                                                    => pure(())
                                                            }
                                    _                    <- modify(_.bindValueParam(paramName.value, argType))
                                    (bodyExpr, bodyType) <- expected match {
                                                              case Some(exp) => check(body, exp).map(e => (e, exp))
                                                              case None      => infer(body)
                                                            }
                                    lamType               = VPi(argType, _ => bodyType)
                                    lamExpr               =
                                      SemExpression(lamType, SemExpression.FunctionLiteral(paramName, argType, body.as(bodyExpr)))
                                  } yield (
                                    SemExpression(bodyType, SemExpression.FunctionApplication(target.as(lamExpr), arg.as(argExpr))),
                                    bodyType
                                  )
                              }
    } yield result

  /** Peel leading `VLam` closures from an inferred type with fresh metas, baking the metas as implicit type arguments
    * onto the expression's [[SemExpression.ValueReference]] and updating its `expressionType`. Returns the updated
    * expression paired with the peeled (monotype) type. Used both by the generic `check` fallback and by
    * [[applyInferred]] — any polytype introduced by referencing a generic value gets instantiated at exactly one place.
    */
  private def instantiatePolymorphic(
      expr: SemExpression,
      tpe: SemValue
  ): CheckIO[(SemExpression, SemValue)] =
    for {
      (peeled, implicitMetas) <- peelLams(tpe)
      _                       <- carriers.recordCarrierMetas(expr, implicitMetas)
      updated                 <- appendTypeArgs(expr, implicitMetas)
    } yield (updated.copy(expressionType = peeled), peeled)

  /** Append implicit-meta type args to a [[SemExpression.ValueReference]] expression. Only a value reference can
    * inherit a polytype (since polymorphism lives on named signatures), so no other shape should ever arrive here with
    * non-empty `extraArgs`. Hitting that branch indicates a compiler bug.
    */
  private def appendTypeArgs(expr: SemExpression, extraArgs: Seq[SemValue]): CheckIO[SemExpression] =
    if (extraArgs.isEmpty) pure(expr)
    else
      expr.expression match {
        case ref: SemExpression.ValueReference =>
          val updatedArgs = ref.typeArguments ++ extraArgs
          pure(expr.copy(expression = ref.copy(typeArguments = updatedArgs)))
        case other                             =>
          throw new IllegalStateException(
            s"Polytype instantiation produced implicit type arguments for a non-reference expression: $other"
          )
      }

  /** Prefetch-only traversal: walks an ORE and calls [[ensureBinding]] at every ValueReference, discarding any resulting
    * SemValue. Used for subtrees whose actual evaluation is deferred to a pure [[Evaluator]] invocation inside a
    * [[VLam]] closure — the closure must find every reachable binding already in the cache. `ensureBinding` itself pulls
    * each value's transitive body dependencies (see [[ensureBodyBindings]]), so this need only visit `ore`'s own
    * references.
    */
  private def prefetchBindings(ore: OperatorResolvedExpression): CheckIO[Unit] =
    OperatorResolvedExpression.foldValueReferences[CheckIO, Unit](ore, ()) { (_, vfqn) =>
      ensureBinding(vfqn.value).void
    }

}
