package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.monomorphize.refine.RefinementSolver
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
  * to a post-drain pass in [[TypeStackLoop]], using [[com.vanillasource.eliot.eliotc.monomorphize.eval. Quoter]]. This
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
                      _        <- modify(_.cacheBinding(vfqn, replaced))
                    } yield replaced
                }
    } yield result

  /** Evaluate an ORE expression against an env (defaulting to the current state's env). Prefetches every reachable
    * binding into [[CheckState.bindingCache]] first — including rewriting abstract associated-ability-types to fresh
    * metas via [[ensureBinding]] — so that the pure [[Evaluator]] has everything it needs.
    */
  def evalExpr(tm: OperatorResolvedExpression, env: Option[Env] = None): CheckIO[SemValue] =
    for {
      _ <- prefetchBindings(tm)
      s <- get
    } yield s.makeEvaluator.eval(env.getOrElse(s.env), tm)

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
                              paramType <- paramTypeStack match {
                                             case Some(ts) =>
                                               for {
                                                 pt <- evalExpr(ts.value.signature)
                                                 _  <- doUnify(pt, domain, paramName.as("Type mismatch."))
                                               } yield pt
                                             case None     => pure(domain)
                                           }
                              _         <- modify(_.bind(paramName.value, paramType))
                              bodyExpr  <- check(body, codomain(paramType))
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
                              // Defer only when checking, against a *concrete* expected type, a combinable-meta result
                              // that actually accumulated argument contributions (so it could be a `Combine` join). Two
                              // guards matter: (1) if `expected` is itself a (flowing) meta this is an intermediate
                              // contribution — e.g. `id(i)` as an argument — and must unify now to propagate through the
                              // chain; (2) a result meta with *no* candidates (e.g. an ability method's `B` in
                              // `convert(x): B`, or any fresh result) has no join to wait for and must unify now so the
                              // value/ability resolution that depends on its solution can proceed.
                              forcedExp        <- force(expected)
                              // Effectful-signatures kind acceptance (W2b): a return-type expression whose value is on
                              // the compile-time `Throw[String]` carrier types as `Either[..]`, not `Type`. Where a
                              // `Type` kind is expected (the signature/return position), accept it as a *guarded type*
                              // rather than letting the unifier reject `Either[..]` ≠ `Type`; the carrier value is
                              // discharged to its payload type — or rejected — by the signature/read-site discharge.
                              guardKind        <- forcedExp match {
                                                    case VType => calcReturns.isGuardCarrier(inferred)
                                                    case _     => pure(false)
                                                  }
                              checkedResult    <- if (guardKind) modify(_.recordGuardReturn).as(expr)
                                                  else checkAgainst(tm, expr, inferred, forcedExp, expected)
                            } yield checkedResult
                        }
    } yield result

  /** The ordinary (non-guard) check-mode resolution against an expected type: the combinable-meta deferral plus the
    * polytype instantiation and `unifyOrCoerce`. Factored out of [[check]] so the effectful-signatures kind acceptance
    * (W2b) can short-circuit before it.
    */
  private def checkAgainst(
      tm: Sourced[OperatorResolvedExpression],
      expr: SemExpression,
      inferred: SemValue,
      forcedExp: SemValue,
      expected: SemValue
  ): CheckIO[SemExpression] =
    for {
      combinableMeta <- (inferred, forcedExp) match {
                          case (VMeta(id, Spine.SNil), exp) if !exp.isInstanceOf[VMeta] =>
                            inspect(s =>
                              Option.when(
                                s.unifier.isCombinable(id.value) &&
                                  s.unifier.candidatesOf(id.value).nonEmpty
                              )(id)
                            )
                          case _                                                        => pure(None)
                        }
      checked        <- combinableMeta match {
                          // The term's type is a bare combinable meta — the result of a polymorphic call whose result
                          // type is a type parameter. Its final solution (possibly a `Combine` join) is unknown until
                          // drain, so defer the check against `expected` rather than committing it against the meta's
                          // first candidate (which would unsoundly accept a join that overflows a narrower `expected`).
                          // See resolveUpperBounds.
                          case Some(id) =>
                            modify(_.recordUpperBound(id, expected, tm.as("Type mismatch.")))
                              .as(expr)
                          case None     =>
                            for {
                              (updatedExpr, instantiated) <- instantiatePolymorphic(expr, inferred)
                              c                           <- solver.unifyOrCoerce(tm, updatedExpr, instantiated, expected)
                            } yield c
                        }
    } yield checked

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
                        // Implicit type-parameter instantiation metas sit in covariant positions (a match result, a
                        // result-position type param), so they are eligible for `Combine`-based multi-candidate
                        // resolution. The unifier taints any that later flow into a contravariant (VPi domain) position.
                        _    <- modify(s => s.withUnifier(s.unifier.markCombinable(meta.id)))
                        _    <- if (bindInEnv) modify(_.bind(name, meta)) else pure(())
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
        result <- state.env.lookupByName(name.value) match {
                    case Some(sem) =>
                      // A runtime value parameter's env binding *is* its type, so it serves as the inferred type
                      // directly. An erased type-stack value parameter's binding is its concrete *value* instead (e.g.
                      // `A: Person` bound to `Person("Alice", …)`); referenced in value position its type is the value's
                      // type, recovered here. (Type-position references go through evaluation, never this path, so they
                      // still see the concrete value.)
                      val tpe = sem match {
                        case VConst(ground) if state.typeStackValueParams.contains(name.value) =>
                          Evaluator.groundToSem(ground.valueType)
                        case _                                                                 => sem
                      }
                      pure((SemExpression(tpe, SemExpression.ParameterReference(name)), tpe))
                    case None      =>
                      liftF(compilerError(tm.as("Name not defined.")) >> abort)
                  }
      } yield result

    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs) =>
      for {
        _      <- ensureBinding(vfqn.value)
        svOpt  <- liftF(getFact(SaturatedValue.Key(vfqn.value, platform)))
        result <- svOpt match {
                    case Some(sv) =>
                      // Read the *saturated* signature, so a callee's parameter-position bare omittable references
                      // (e.g. bare `Int`) present as ordinary leading generic binders that the instantiation machinery
                      // solves from this call's arguments. Signatures reference only their own parameters or top-level
                      // values, so they evaluate under an empty env — outer-session bindings are not in scope.
                      for {
                        sig              <- evalExpr(sv.value.typeStack.value.signature, env = Some(Env.empty))
                        explicitTypeArgs <- typeArgs.traverse(ta => evalExpr(ta.value))
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
                        resultType       <- calcReturns.dischargeGuardedReturn(afterCalc, vfqn).map(_.getOrElse(afterCalc))
                      } yield (
                        SemExpression(resultType, SemExpression.ValueReference(vfqn, explicitTypeArgs)),
                        resultType
                      )
                    case None     =>
                      liftF(compilerError(tm.as("Name not defined.")) >> abort)
                  }
      } yield result

    case OperatorResolvedExpression.FunctionApplication(target, arg) =>
      target.value match {
        // An immediately-applied unannotated lambda `(x -> body)(arg)` is a `let` (the shape a non-effectful block
        // `val`/statement lowers to). The lambda alone has no inferable parameter type, so infer it from the argument.
        case OperatorResolvedExpression.FunctionLiteral(paramName, None, body) =>
          typeImmediateLambda(target, paramName, body, arg, None)
        case _                                                                 =>
          for {
            (targetExpr, targetType) <- infer(target)
            result                   <- applyInferred(target, targetExpr, targetType, arg)
          } yield result
      }

    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramTypeStack), body) =>
      for {
        paramType            <- evalExpr(paramTypeStack.value.signature)
        _                    <- modify(_.bind(paramName.value, paramType))
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

  /** Handle function application: peel any polytype (`VLam`) layers with fresh metas, then apply one argument to the
    * resulting monotype. If the monotype isn't already `VPi`, it gets unified against a fresh one. The implicit metas
    * introduced by peeling are baked into the target reference.
    */
  private def applyInferred(
      target: Sourced[OperatorResolvedExpression],
      targetExpr: SemExpression,
      targetType: SemValue,
      arg: Sourced[OperatorResolvedExpression]
  ): CheckIO[(SemExpression, SemValue)] =
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
      argExpr                 <- check(arg, vpi.domain)
      argSem                  <- evalExpr(arg.value)
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
      retType
    )

  /** Whether `expr` is an unannotated function literal `(x -> body)`. Its parameter type cannot be inferred from the
    * literal alone; when it is *immediately applied* the type is taken from the argument (see [[typeImmediateLambda]]).
    */
  private def isUnannotatedLambda(expr: OperatorResolvedExpression): Boolean = expr match {
    case OperatorResolvedExpression.FunctionLiteral(_, None, _) => true
    case _                                                      => false
  }

  /** Type an immediately-applied unannotated lambda `(param -> body)(arg)` — a `let` (the shape a non-effectful block
    * `val`/statement lowers to). The parameter type is taken from the (instantiated) argument; the body is checked
    * against `expected` when known (pushing the type down) and inferred otherwise. Returns the rebuilt application
    * expression and its type.
    */
  private def typeImmediateLambda(
      target: Sourced[OperatorResolvedExpression],
      paramName: Sourced[String],
      body: Sourced[OperatorResolvedExpression],
      arg: Sourced[OperatorResolvedExpression],
      expected: Option[SemValue]
  ): CheckIO[(SemExpression, SemValue)] =
    for {
      (argExpr0, argType0)   <- infer(arg)
      (argExpr, argType)     <- instantiatePolymorphic(argExpr0, argType0)
      _                      <- modify(_.bind(paramName.value, argType))
      (bodyExpr, bodyType)   <- expected match {
                                  case Some(exp) => check(body, exp).map(e => (e, exp))
                                  case None      => infer(body)
                                }
      lamType                 = VPi(argType, _ => bodyType)
      lamExpr                 = SemExpression(lamType, SemExpression.FunctionLiteral(paramName, argType, body.as(bodyExpr)))
    } yield (
      SemExpression(bodyType, SemExpression.FunctionApplication(target.as(lamExpr), arg.as(argExpr))),
      bodyType
    )

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

  /** Prefetch-only traversal: walks an ORE and calls [[ensureBinding]] at every ValueReference, discarding any
    * resulting SemValue. Used for subtrees whose actual evaluation is deferred to a pure [[Evaluator]] invocation
    * inside a [[VLam]] closure — the closure must find every reachable binding already in the cache.
    */
  private def prefetchBindings(ore: OperatorResolvedExpression): CheckIO[Unit] =
    OperatorResolvedExpression.foldValueReferences[CheckIO, Unit](ore, ()) { (_, vfqn) =>
      ensureBinding(vfqn.value).void
    }

}
