package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.{BodyValueReferences, GroundValue}
import com.vanillasource.eliot.eliotc.monomorphize.refine.RefinementSolver
import com.vanillasource.eliot.eliotc.monomorphize.unify.UnifyResult
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
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
    track: Track
) {

  /** The track's platform — fact keys read it off the [[track]] rather than threading a bare [[Platform]]. */
  private val platform: Platform = track.platform

  /** The refinement-bounds solver (D4): the directional `Coerce` widening, the `Combine` join, and the deferred
    * upper-bound obligations — the type system's *refinement lattice*, kept out of this checker's *definitional
    * equality* concern. Constructed with the five checker primitives it needs; see [[RefinementSolver]]. Accessible to
    * [[TypeStackLoop]], which routes the post-drain `resolve-combines` / `upper-bounds` passes through it.
    */
  private[check] val solver: RefinementSolver =
    new RefinementSolver(resolveAbility, (tm, env) => evalExpr(tm, env), force, freshMeta, doUnify, platform)

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
    new AbilityResolver(resolveAbility, fetchBinding, platform)

  /** The type-directed effect auto-lift (the fifth collaborator — docs/effect-lift-in-checker.md): the check-mode
    * elaboration arms 3–4 of the resolution ladder (bind-lift at argument positions, pure-wrap against an
    * ambient-carrier-typed expectation) plus the `Effect.flatMap`/`map`/`pure` node assembly. A non-equality
    * *elaboration* concern, kept out of this checker's definitional-equality core. Consulted from the shared
    * resolution ladder ([[resolveLadder]], `allowBindLift` selecting the bind-lift arm at argument positions) and the
    * immediately-applied-lambda `let` rule ([[typeImmediateLambda]]). See [[EffectLifter]].
    */
  private[check] val lifter: EffectLifter = new EffectLifter(force, doUnify)

  /** Ensure a NativeBinding is in the cache, fetching it via CompilerIO if needed.
    *
    * References to abstract associated ability types (`type X` inside `ability ...`, no body) are rewritten to a fresh
    * [[VMeta]] on first access and cached in that form. The meta is solved post-drain by unifying against the concrete
    * impl's corresponding associated-type value. The cache provides per-(fqn, check-session) dedup automatically —
    * subsequent lookups return the same cached meta.
    *
    * Recognition keys on the FQN ([[ValueFQN.isAbstractAbilityType]] — Ability-qualified, upper-case), not on the
    * presence of a body-less binding fact: such a member is body-less, so its binding is none of
    * [[com.vanillasource.eliot.eliotc.monomorphize.processor.UserValueNativesProcessor]]'s business, and `fetchBinding`
    * legitimately returns `None`.
    */
  private def ensureBinding(vfqn: ValueFQN): CheckIO[Option[SemValue]] =
    for {
      cached <- inspect(_.bindingCache.get(vfqn))
      result <- cached match {
                  case Some(value) => pure(value)
                  case None        =>
                    for {
                      opt      <- liftF(fetchBinding(vfqn))
                      replaced <- if (ValueFQN.isAbstractAbilityType(vfqn))
                                    for {
                                      meta <- freshMeta
                                      _    <- modify(_.recordAbstractTypeMeta(vfqn, meta.id))
                                    } yield Some(meta: SemValue)
                                  else pure(opt)
                      _        <- modify(_.cacheBinding(vfqn, replaced)) // cache before recursing so cycles short-circuit
                      _        <- ensureBodyBindings(vfqn)
                    } yield replaced
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
      forced         <- force(expected)
      // Reduce applied abstract associated types in the expected type (`AddResult[Int[0, 1], Int[2, 3]]` ⤳ the Int
      // impl's `Int[2, 4]`) before matching against it: a declared, computable type application must constrain the
      // body. This is also where a codomain-buried application surfaces (the enclosing check applied the codomain), so
      // recursion through here reaches every return position.
      forcedExpected <- abilityResolver.reduceAssocApplications(forced, tm)
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
                                                    pt <- evalExpr(ts.value.signature)
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
                              // Re-force (inference may have solved metas in the expectation) and re-reduce. The
                              // reduced expected replaces the original only when reduction changed it — the ladder's
                              // unify targets must stay the original metas otherwise, so their solutions propagate.
                              forcedExp0       <- force(expected)
                              forcedExp        <- abilityResolver.reduceAssocApplications(forcedExp0, tm)
                              effectiveExp      = if (forcedExp eq forcedExp0) expected else forcedExp
                              checkedResult    <- checkAgainst(tm, expr, inferred, forcedExp, effectiveExp)
                            } yield checkedResult
                        }
    } yield result

  /** Check-mode resolution at a *return boundary* (a lambda body against its codomain, a def body against its declared
    * return): the shared [[resolveGuardedLadder]] with the bind-lift arm *disabled* — stripping an effect carrier at a
    * return boundary would silently drop the effect, so the doomed lift shape commits the exact mismatch instead. The
    * ladder can therefore only ever produce a [[SlotOutcome.Resolved]] here; a [[SlotOutcome.Bound]] is unreachable by
    * construction.
    */
  private def checkAgainst(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      inferred: SemValue,
      forcedExp: SemValue,
      expected: SemValue
  ): CheckIO[SemExpression] =
    resolveGuardedLadder(tm, expr, inferred, forcedExp, expected, allowBindLift = false).map {
      case SlotOutcome.Resolved(e) => e
      case other                   =>
        throw new IllegalStateException(s"Return-boundary resolution produced a non-Resolved outcome: $other")
    }

  /** The check-mode resolution ladder shared by return boundaries ([[checkAgainst]]) and spine argument slots
    * ([[checkArgumentSlot]]), fronted by the W2b guard-kind acceptance. A value whose type is on the compile-time
    * `Throw[String]` carrier types as `Either[..]`, not `Type`; where a `Type` kind is expected, accept it as a
    * *guarded type* (discharged to its payload — or rejected — by the signature/read-site discharge) rather than
    * letting the unifier reject `Either[..]` ≠ `Type`. Otherwise runs the plain [[resolveLadder]].
    *
    * This is the single entry the two fresh-check sites share ([[check]]'s fallback via [[checkAgainst]], and
    * [[checkArgumentSlot]]); the deferred-slot re-entry ([[resolveDeferredSlot]]) calls [[resolveLadder]] directly,
    * where a guard acceptance never applied.
    */
  private def resolveGuardedLadder(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      inferred: SemValue,
      forcedExpected: SemValue,
      expected: SemValue,
      allowBindLift: Boolean
  ): CheckIO[SlotOutcome] =
    for {
      guardKind <- forcedExpected match {
                     case VType => calcReturns.isGuardCarrier(inferred)
                     case _     => pure(false)
                   }
      outcome   <- if (guardKind) modify(_.recordGuardReturn).as(SlotOutcome.Resolved(expr): SlotOutcome)
                   else resolveLadder(tm, expr, inferred, forcedExpected, expected, allowBindLift)
    } yield outcome

  /** The check-mode resolution ladder proper — the algorithm shared verbatim by return boundaries and argument slots
    * (R3-1 dedup): the combinable-meta upper-bound deferral, then polytype instantiation, then the pre-arms for the
    * doomed-postponement shapes, then the failure ladder ([[resolveFailureLadder]]: unify → …lift arms… → `Coerce` →
    * committed mismatch). The single behavioural difference is *position*, carried by `allowBindLift`:
    *
    *   - `true` (argument slot): the **bind-lift arm** (arm 3) is consulted — as a pre-arm and in the failure ladder —
    *     and can produce a [[SlotOutcome.Bound]].
    *   - `false` (return boundary): the bind-lift arm is omitted (stripping a carrier there would silently drop the
    *     effect); the doomed `mustLiftBeforeUnify` shape instead commits the exact mismatch eagerly.
    *
    * The pure-wrap arm (arm 4) and the `Coerce` widening fire on both. `forcedExpected` is the forced form of
    * `expected`, used only for the combinable-meta discrimination; every arm consults the unforced `expected`.
    */
  private def resolveLadder(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      inferred: SemValue,
      forcedExpected: SemValue,
      expected: SemValue,
      allowBindLift: Boolean
  ): CheckIO[SlotOutcome] =
    for {
      (updatedExpr, instantiated) <- instantiatePolymorphic(expr, inferred)
      // Pre-arms: the shapes definitional equality can only *postpone*, never solve — a
      // carrier-meta application against an under-applied rigid head, and its pure-wrap dual.
      // Waiting for a unification failure would mask the lift behind the doomed postponement
      // (surfacing only as the post-drain carrier-kind error); see
      // [[EffectLifter.mustLiftBeforeUnify]]. The bind-lift arm is argument-position only
      // (`allowBindLift`); at a return boundary the doomed shape commits the eager mismatch.
      preBind                     <- if (allowBindLift)
                                       lifter
                                         .mustLiftBeforeUnify(instantiated, expected)
                                         .flatMap(
                                           if (_) lifter.tryBindLift(tm, updatedExpr, instantiated, expected)
                                           else pure(Option.empty[(SemExpression, EffectLifter.Bind)])
                                         )
                                     else pure(Option.empty[(SemExpression, EffectLifter.Bind)])
      prePure                     <- preBind match {
                                       case Some(_) => pure(Option.empty[SemExpression])
                                       case None    =>
                                         lifter
                                           .mustPureWrapBeforeUnify(instantiated, expected)
                                           .flatMap(
                                             if (_) lifter.tryPureWrap(tm, updatedExpr, instantiated, expected)
                                             else pure(Option.empty[SemExpression])
                                           )
                                     }
      // At a return boundary (`!allowBindLift`) a carrier-meta application against an
      // under-applied rigid head has no injective solution — unification could only postpone it
      // into an opaque post-drain carrier-kind error — and the bind-lift arm never fires there
      // (stripping would drop the effect), so commit the exact mismatch immediately.
      doomed                      <- if (allowBindLift) pure(false)
                                     else
                                       prePure match {
                                         case Some(_) => pure(false)
                                         case None    => lifter.mustLiftBeforeUnify(instantiated, expected)
                                       }
      out                         <- (preBind, prePure, doomed) match {
                                       case (Some((slotRef, bind)), _, _) =>
                                         pure(SlotOutcome.Bound(slotRef, bind): SlotOutcome)
                                       case (_, Some(wrapped), _)         =>
                                         pure(SlotOutcome.Resolved(wrapped): SlotOutcome)
                                       case (_, _, true)                  =>
                                         commitMismatch(instantiated, expected, tm, updatedExpr)
                                       case (_, _, false)                 =>
                                         resolveFailureLadder(tm, updatedExpr, instantiated, expected, allowBindLift)
                                     }
    } yield out

  /** The failure ladder consulted when definitional equality (arm 1) does not immediately unify: the lift arms are
    * tried BEFORE the `Coerce` probe (whose ability-fact side effects would fail the build on a shape a lift arm
    * resolves — the arms' guards are disjoint from every coercible shape, so widening behaviour is untouched). The
    * bind-lift arm (arm 3) is consulted only at argument positions (`allowBindLift`).
    */
  private def resolveFailureLadder(
      tm: Sourced[OperatorResolvedExpression],
      updatedExpr: SemExpression,
      instantiated: SemValue,
      expected: SemValue,
      allowBindLift: Boolean
  ): CheckIO[SlotOutcome] =
    tryUnifyCommitting(instantiated, expected, tm.as("Type mismatch.")).flatMap {
      case true  => pure(SlotOutcome.Resolved(updatedExpr): SlotOutcome)
      case false =>
        (if (allowBindLift) lifter.tryBindLift(tm, updatedExpr, instantiated, expected)
         else pure(Option.empty[(SemExpression, EffectLifter.Bind)])).flatMap {
          case Some((slotRef, bind)) => pure(SlotOutcome.Bound(slotRef, bind): SlotOutcome)
          case None                  =>
            lifter.tryPureWrap(tm, updatedExpr, instantiated, expected).flatMap {
              case Some(wrapped) => pure(SlotOutcome.Resolved(wrapped): SlotOutcome)
              case None          =>
                solver.tryCoerce(tm, updatedExpr, instantiated, expected).flatMap {
                  case Some(coerced) => pure(SlotOutcome.Resolved(coerced): SlotOutcome)
                  case None          => commitMismatch(instantiated, expected, tm, updatedExpr)
                }
            }
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
    *   parameter with the same name. `true` is used only by the top-level type-stack walk, where leftover type
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
        // kind of binder: a runtime value parameter's declared type, an erased type-stack parameter's recovered type,
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

    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramTypeStack), body) =>
      for {
        paramType            <- evalExpr(paramTypeStack.value.signature)
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
    * arguments. Shared by [[infer]]'s `ValueReference` case and the associated-type application spine head in
    * [[inferSpine]] (an application spine headed by an abstract associated type IS its explicit instantiation — the
    * member's parameters are the ability's binders).
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
                        // For an ability-method reference, freshen this ability's abstract associated types *per
                        // reference*: evict them from the binding cache so the signature evaluation below re-creates
                        // them as metas unique to this call, and snapshot the pre-existing abstract-assoc metas so the
                        // ones this evaluation adds can be attributed to this reference (recordRefAssocMetas). Without
                        // this, the cache hands every `add` call one shared `AddResult` meta and nested calls collide.
                        isAbilityMethod  <- pure(vfqn.value.name.qualifier.isInstanceOf[Qualifier.Ability])
                        _                <- if (isAbilityMethod) modify(_.evictAbilityAssocs(vfqn.value)) else pure(())
                        assocBefore      <- if (isAbilityMethod) inspect(_.unifier.abstractAssocMetaIds)
                                            else pure(Set.empty[Int])
                        sig              <- evalExpr(sv.value.typeStack.value.signature, env = Some(Env.empty))
                        _                <- if (isAbilityMethod)
                                              inspect(_.unifier.abstractAssocMetaIds)
                                                .flatMap(after => modify(_.recordRefAssocMetas(vfqn, after -- assocBefore)))
                                            else pure(())
                        appliedSig        = explicitTypeArgs.foldLeft(sig)(Evaluator.applyValue)
                        // W4 (deferred W3 item 1): a calculated-return value referenced as a *complete* value — no
                        // parameters left to apply, so its whole type forced to the `Type` placeholder `saturate`
                        // installed — is resolved from its monomorphized return here, so a no-argument producer used by
                        // name (`def y: Int = x`) works instead of leaking `Type` into a mismatch. The applied case
                        // keeps a `VPi` here (resolved by `applyInferred`); a calculated-return *function* passed
                        // unapplied keeps the placeholder inside its codomain (the higher-order limit, out of scope).
                        calcReturn       <- if (sv.value.calculatedReturn)
                                              calcReturns.resolveCompleteCalculatedReturn(vfqn, explicitTypeArgs, appliedSig)
                                            else pure(Option.empty[SemValue])
                        afterCalc         = calcReturn.getOrElse(appliedSig)
                        // Discharge a `{Throw[String]}` guard on a *complete* (fully applied) value read by name (W2b):
                        // `def y: Bar = foo` where `foo`'s return is `Right(Bar)`. A guarded *function* read unapplied
                        // stays a `VPi`/`VLam` (the guard is in its codomain), so it is left untouched here.
                        discharged       <- calcReturns.dischargeGuardedReturn(afterCalc, vfqn).map(_.getOrElse(afterCalc))
                        // Reduce an applied abstract associated type in the reference's (top-level) type — a complete
                        // value declared `AddResult[Int[0, 1], Int[2, 3]]` reads as the concrete `Int[2, 4]`, so the
                        // caller never unifies against the unreduced application.
                        resultType       <- abilityResolver.reduceAssocApplications(discharged, tm)
                      } yield (
                        SemExpression(resultType, SemExpression.ValueReference(vfqn, explicitTypeArgs)),
                        resultType
                      )
                    case None     =>
                      liftF(compilerError(tm.as("Name not defined.")) >> abort)
                  }
      } yield result

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
    head.value match {
      // An application spine headed by an abstract associated ability type IS its explicit instantiation: the
      // member's parameters are the ability's binders, so `AddResult[Int[0, 1], Int[2, 3]]` (a curried type-position
      // application) supplies them directly — never peel-then-apply, which would consume the binders with fresh
      // metavariables and reject the arguments with "Not a function.".
      case OperatorResolvedExpression.ValueReference(vfqn, declaredTypeArgs)
          if ValueFQN.isAbstractAbilityType(vfqn.value) =>
        for {
          declaredArgs <- declaredTypeArgs.traverse(ta => evalExpr(ta.value))
          spineArgs    <- apps.traverse { case (_, arg) => evalExpr(arg.value) }
          result       <- inferValueReference(tm, vfqn, declaredArgs ++ spineArgs)
        } yield result
      case _                                                                                        =>
        inferSpineApplications(tm, head, apps)
    }
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
      (built, records) <- rest.foldLeftM((start, Vector.empty[SlotRecord])) {
                            case (((targetExpr, targetType), recs), (target, arg)) =>
                              applyInferred(target, targetExpr, targetType, arg).map { case (expr, tpe, record) =>
                                ((expr, tpe), recs :+ record)
                              }
                          }
      hadDeferred       = records.exists(_.outcome.isInstanceOf[SlotOutcome.Deferred])
      finalRecords     <- records.traverse(resolveDeferredSlot)
      result           <- assembleSpine(built, finalRecords, hadDeferred)
    } yield result
  }

  /** The outcome of resolving one spine argument slot. */
  private sealed trait SlotOutcome {

    /** The expression this slot contributes to the application chain — final for `Resolved` (the ladder ran) and
      * `Bound` (the fresh `$eff$N` reference), provisional (the uninstantiated argument) for `Deferred`.
      */
    def slotExpr: SemExpression
  }

  private object SlotOutcome {

    /** The ladder resolved the slot in place (unified, coerced, or pure-wrapped). */
    case class Resolved(slotExpr: SemExpression) extends SlotOutcome

    /** The bind-lift arm fired: the slot receives the fresh binder reference and the spine wraps the recorded bind. */
    case class Bound(slotExpr: SemExpression, bind: EffectLifter.Bind) extends SlotOutcome

    /** Phase-A deferral (bare flex domain + effect-carrier-headed argument); decided in Phase B. */
    case class Deferred(slotExpr: SemExpression, argType: SemValue, domain: SemValue) extends SlotOutcome
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

  /** Phase B: decide a deferred slot. A domain rigidified by later arguments runs the full ladder
    * ([[resolveLadder]] — unify / coerce / bind-lift); a still-flex one prefers pass-through: the carrier-headed
    * argument type unifies into the slot, the effectful result propagates upward, and the parent's slot decides (this
    * is how `identity(readLine)` and eliminator branches resolve, with no special case).
    */
  private def resolveDeferredSlot(record: SlotRecord): CheckIO[SlotRecord] = record.outcome match {
    case SlotOutcome.Deferred(argExpr, argType, domain) =>
      for {
        forcedDomain <- force(domain)
        outcome      <- forcedDomain match {
                          case VMeta(id, Spine.SNil) =>
                            for {
                              (updated, instantiated) <- instantiatePolymorphic(argExpr, argType)
                              // Pass-through *adoption* ([[Unifier.solveAdopting]]) lets the effect-carrier-headed
                              // argument ride up as a first-class value — sound ONLY for a *transparent* callee whose
                              // result flows from this domain meta (`identity`, `const`, a data ctor over the slot): the
                              // meta occurs in the node's result, so after `?id := C[T']` the result is carrier-headed
                              // and the enclosing slot decides. Adopting solves the bare meta directly (the reversed
                              // orientation would only postpone — a meta application against a bare meta is not a
                              // pattern) and without recording a `Combine` candidate (a candidate would defer the ridden-
                              // up result to the post-saturation upper-bounds check, starving ability resolution).
                              //
                              // For a *non-transparent* callee whose result carrier is independent of the domain
                              // (`putState[S, F](s: S): F[Unit]` — `S` absent from `F[Unit]`), adoption would strand the
                              // argument's carrier inside the type parameter, where nothing ever grounds it ("contains
                              // unresolved variable" at quote). The effect cannot ride up, so it must be sequenced here:
                              // bind-lift the argument and pass its payload, exactly as a rigid domain would.
                              ridesUp                 <- inspect(_.unifier.occursInValue(id, record.retType))
                              outcome                 <- if (ridesUp)
                                                           modify(s =>
                                                             s.withUnifier(
                                                               s.unifier.solveAdopting(id, instantiated, record.arg.as("Type mismatch."))
                                                             )
                                                           ).as(SlotOutcome.Resolved(updated): SlotOutcome)
                                                         else
                                                           lifter.tryBindLift(record.arg, updated, instantiated, domain).flatMap {
                                                             case Some((slotRef, bind)) =>
                                                               pure(SlotOutcome.Bound(slotRef, bind): SlotOutcome)
                                                             case None                  =>
                                                               modify(s =>
                                                                 s.withUnifier(
                                                                   s.unifier
                                                                     .solveAdopting(id, instantiated, record.arg.as("Type mismatch."))
                                                                 )
                                                               ).as(SlotOutcome.Resolved(updated): SlotOutcome)
                                                           }
                            } yield outcome
                          case rigid                 =>
                            // The bare [[resolveLadder]], not [[resolveGuardedLadder]]: the W2b guard-kind acceptance
                            // never applied on the deferred re-entry (a Deferred slot is always effect-carrier-headed,
                            // never a `Type`-kind guard carrier), so keeping it off here preserves the pre-dedup
                            // behaviour exactly.
                            resolveLadder(record.arg, argExpr, argType, rigid, domain, allowBindLift = true)
                        }
      } yield record.copy(outcome = outcome)
    case _                                              => pure(record)
  }

  /** Assemble the spine result: rebuild the application chain when Phase B changed a deferred slot's expression, then
    * fold the recorded effect-binds around the core — the spine's type becomes the outermost wrap's carrier-headed
    * type, so the lifted effect is never dropped. With no binds and no deferral this is the Phase-A build unchanged.
    */
  private def assembleSpine(
      built: (SemExpression, SemValue),
      records: Vector[SlotRecord],
      hadDeferred: Boolean
  ): CheckIO[(SemExpression, SemValue)] = {
    val (builtExpr, resultType) = built
    val binds                   = records.collect { case SlotRecord(_, _, _, _, SlotOutcome.Bound(_, bind)) => bind }
    val core                    = if (hadDeferred) rebuildChain(records) else builtExpr
    if (binds.isEmpty) pure((core, resultType))
    else lifter.wrapBinds(core, resultType, binds)
  }

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
      arg: Sourced[OperatorResolvedExpression]
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
      outcome                 <- checkArgumentSlot(arg, vpi.domain)
      argExpr                  = outcome.slotExpr
      // For a lifted argument the dependent codomain is applied to the fresh binder's neutral, not the action value —
      // the slot's value is the bound result. (Today all codomains are constant, so this is future-proofing, not a
      // behaviour change; a deferred slot likewise uses the evaluated argument.)
      argSem                  <- outcome match {
                                   case SlotOutcome.Bound(_, bind) => inspect(_.paramNeutral(bind.name))
                                   case _                          => evalExpr(arg.value)
                                 }
      // The codomain may embed a native applied to the target's instantiation metas — e.g. `+`'s result type
      // `Int[add(LMin,RMin), …]`. Those bounds are solved by the argument checks just above, so renormalise the
      // codomain now to re-fire the natives (`add(3,4) ⤳ 7`) before the type reaches unification or quoting. A *bare*
      // metavariable result (a result-position type parameter, e.g. `pick[A](a,b): A`) is left untouched: forcing it to
      // its provisional first candidate would destroy the combinable-meta signal the `check` fallback uses to defer the
      // `Combine` join (see `check` / resolveUpperBounds).
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
    * through the shared resolution ladder ([[resolveGuardedLadder]], the argument-position `allowBindLift = true`
    * entry, which folds in the effectful-signatures kind acceptance (W2b) exactly as the return-boundary
    * [[checkAgainst]] does).
    */
  private def checkArgumentSlot(arg: Sourced[OperatorResolvedExpression], domain: SemValue): CheckIO[SlotOutcome] =
    arg.value match {
      case _: OperatorResolvedExpression.FunctionLiteral                                            =>
        check(arg, domain).map(SlotOutcome.Resolved.apply)
      case OperatorResolvedExpression.FunctionApplication(target, _) if isUnannotatedLambda(target.value) =>
        check(arg, domain).map(SlotOutcome.Resolved.apply)
      case _                                                                                        =>
        for {
          (argExpr, argType) <- infer(arg)
          forcedDomain       <- force(domain)
          outcome            <- forcedDomain match {
                                  case VMeta(_, Spine.SNil) =>
                                    // The deferral decision needs the argument's *instantiated* type — a bare
                                    // ability-method reference (`readLine`) infers as a polytype (`VLam`), whose
                                    // carrier only appears once the binder is peeled to its (flagged) meta.
                                    // Instantiating here is exactly once either way (the ladder's own instantiation is a
                                    // no-op on a monotype).
                                    for {
                                      (updatedExpr, instantiated) <- instantiatePolymorphic(argExpr, argType)
                                      out                         <- lifter.effectCarrierSplit(instantiated).flatMap {
                                                                       case Some(_) =>
                                                                         pure(SlotOutcome.Deferred(updatedExpr, instantiated, domain))
                                                                       case None    =>
                                                                         resolveGuardedLadder(arg, updatedExpr, instantiated, forcedDomain, domain, allowBindLift = true)
                                                                     }
                                    } yield out
                                  case _                    =>
                                    resolveGuardedLadder(arg, argExpr, argType, forcedDomain, domain, allowBindLift = true)
                                }
        } yield outcome
    }

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
                                    (bodyExpr, bodyType)       <- infer(body)
                                    bind                        = EffectLifter.Bind(paramName.value, arg, argExpr, argType, carrier, payload)
                                    (wrappedExpr, wrappedType) <- lifter.bindWrap(bind, bodyExpr, bodyType)
                                    resolved                   <- expected match {
                                                                    case Some(exp) =>
                                                                      solver.unifyOrCoerce(body, wrappedExpr, wrappedType, exp).map((_, exp))
                                                                    case None      => pure((wrappedExpr, wrappedType))
                                                                  }
                                  } yield resolved
                                case None                     =>
                                  for {
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
