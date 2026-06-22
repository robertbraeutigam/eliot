package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicValue}
import com.vanillasource.eliot.eliotc.monomorphize.refine.RefinementSolver
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
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
  * to a post-drain pass in [[TypeStackLoop]], using [[com.vanillasource.eliot.eliotc.monomorphize.eval. Quoter]]. This
  * avoids any silent "default to Type" behaviour for unsolved metas — they surface as explicit errors at quoting time.
  */
class Checker(
    fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
    resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]]
) {

  /** The refinement-bounds solver (D4): the directional `Coerce` widening, the `Combine` join, and the deferred
    * upper-bound obligations — the type system's *refinement lattice*, kept out of this checker's *definitional
    * equality* concern. Constructed with the five checker primitives it needs; see [[RefinementSolver]]. Accessible to
    * [[TypeStackLoop]], which routes the post-drain `resolve-combines` / `upper-bounds` passes through it.
    */
  private[check] val solver: RefinementSolver =
    new RefinementSolver(resolveAbility, (tm, env) => evalExpr(tm, env), force, freshMeta, doUnify)

  /** Ensure a NativeBinding is in the cache, fetching it via CompilerIO if needed.
    *
    * References to abstract associated ability types (`type X` inside `ability ...`, no body) are rewritten to a fresh
    * [[VMeta]] on first access and cached in that form. The meta is solved post-drain by unifying against the concrete
    * impl's corresponding associated-type value. The cache provides per-(fqn, check-session) dedup automatically —
    * subsequent lookups return the same cached meta.
    */
  private def ensureBinding(vfqn: ValueFQN): CheckIO[Option[SemValue]] =
    for {
      cached <- inspect(_.bindingCache.get(vfqn))
      result <- cached match {
                  case Some(value) => pure(value)
                  case None        =>
                    for {
                      opt      <- liftF(fetchBinding(vfqn))
                      replaced <- opt match {
                                    case Some(VTopDef(fqn, None, Spine.SNil)) if ValueFQN.isAbstractAbilityType(fqn) =>
                                      for {
                                        meta <- freshMeta
                                        _    <- modify(_.recordAbstractTypeMeta(vfqn, meta.id))
                                      } yield Some(meta: SemValue)
                                    case other                                                                       =>
                                      pure(other)
                                  }
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
                              combinableMeta   <- (inferred, forcedExp) match {
                                                    case (VMeta(id, Spine.SNil), exp) if !exp.isInstanceOf[VMeta] =>
                                                      inspect(s =>
                                                        Option.when(
                                                          s.unifier.isCombinable(id.value) &&
                                                            s.unifier.candidatesOf(id.value).nonEmpty
                                                        )(id)
                                                      )
                                                    case _                                                        => pure(None)
                                                  }
                              checked          <- combinableMeta match {
                                                    // The term's type is a bare combinable meta — the result of a
                                                    // polymorphic call whose result type is a type parameter. Its final
                                                    // solution (possibly a `Combine` join) is unknown until drain, so
                                                    // defer the check against `expected` rather than committing it
                                                    // against the meta's first candidate (which would unsoundly accept a
                                                    // join that overflows a narrower `expected`). See resolveUpperBounds.
                                                    case Some(id) =>
                                                      modify(_.recordUpperBound(id, expected, tm.as("Type mismatch.")))
                                                        .as(expr)
                                                    case None     =>
                                                      for {
                                                        (updatedExpr, instantiated) <-
                                                          instantiatePolymorphic(expr, inferred)
                                                        c                           <- solver.unifyOrCoerce(tm, updatedExpr, instantiated, expected)
                                                      } yield c
                                                  }
                            } yield checked
                        }
    } yield result

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
        svOpt  <- liftF(getFact(SaturatedValue.Key(vfqn.value)))
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
                                              resolveCompleteCalculatedReturn(vfqn, explicitTypeArgs, appliedSig)
                                            else pure(Option.empty[SemValue])
                        resultType        = calcReturn.getOrElse(appliedSig)
                      } yield (
                        SemExpression(resultType, SemExpression.ValueReference(vfqn, explicitTypeArgs)),
                        resultType
                      )
                    case None     =>
                      liftF(compilerError(tm.as("Name not defined.")) >> abort)
                  }
      } yield result

    case OperatorResolvedExpression.FunctionApplication(target, arg) =>
      for {
        (targetExpr, targetType) <- infer(target)
        result                   <- applyInferred(target, targetExpr, targetType, arg)
      } yield result

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
                                     resolveCalculatedReturn(updatedTarget, other).flatMap {
                                       case Some(resolved) => pure(resolved)
                                       case None           => renormalize(other)
                                     }
                                 }
    } yield (
      SemExpression(
        retType,
        SemExpression.FunctionApplication(target.as(updatedTarget), arg.as(argExpr))
      ),
      retType
    )

  /** When a function application has reached the bare omittable return of a *calculated-return* producer
    * (implicit-generics, W3), resolve that return from the callee's monomorphized signature rather than the
    * (under-applied) source return left by `saturate`. This is the architectural "back-edge": the callee's concrete
    * type arguments are read off the (instantiated) target value reference, and the caller reads
    * `MonomorphicValue(callee, args).signature`'s deep return type — the body-checked result the callee already
    * produced when monomorphized at those arguments — re-entering it as a [[SemValue]]. It reuses the monomorphization
    * the compiler performs anyway; no symbolic quoting on this path.
    *
    * Returns [[None]] (so the ordinary codomain stands) when this is not a reached calculated return: the target is not
    * a value reference, the return is still a function (an intermediate `VPi` of a partial application), or the callee
    * is not a calculated-return producer. Also [[None]] when the type arguments are not yet ground or no
    * monomorphization exists — the caller then keeps the bare return, which fails the ordinary check downstream rather
    * than being silently mistyped.
    */
  private def resolveCalculatedReturn(
      targetExpr: SemExpression,
      rawReturn: SemValue
  ): CheckIO[Option[SemValue]] =
    force(rawReturn).flatMap {
      // The calculated-return placeholder evaluates to `VType` (saturate replaced the bare omittable return with the
      // kind-correct `Type`). A bare `VType` return here therefore means either a reached calculated return or an
      // ordinary type-level function — confirm with the callee's `calculatedReturn` flag before reading its
      // monomorphized return. An intermediate `VPi` (partial application) or any concrete return falls through to the
      // ordinary codomain, so the SaturatedValue fact is fetched only at the rare `VType`-return application.
      case VType =>
        innermostValueRef(targetExpr) match {
          case Some((fqn, typeArgs)) =>
            liftF(getFact(SaturatedValue.Key(fqn.value))).flatMap {
              case Some(sv) if sv.value.calculatedReturn => readMonomorphicReturn(fqn, typeArgs)
              case _                                     => pure(None)
            }
          case None                  => pure(None)
        }
      case _     => pure(None)
    }

  /** Resolve the return of a *complete* (fully applied) calculated-return value referenced by name — the read-site twin
    * of the [[applyInferred]] back-edge (W4, deferred W3 item 1). The value is complete iff its type forced to the
    * `Type` placeholder `saturate` installed, i.e. no parameter `VPi` remains to apply; then its body-checked return is
    * read from `MonomorphicValue(value, args)` exactly as in the applied path, sharing [[readMonomorphicReturn]]'s
    * recursion guard. Returns [[None]] when a `VPi` still remains (the value is applied later, or passed higher-order
    * with the placeholder buried in its codomain) so the ordinary signature stands.
    */
  private def resolveCompleteCalculatedReturn(
      vfqn: Sourced[ValueFQN],
      typeArgs: Seq[SemValue],
      appliedSig: SemValue
  ): CheckIO[Option[SemValue]] =
    force(appliedSig).flatMap {
      case VType => readMonomorphicReturn(vfqn, typeArgs)
      case _     => pure(None)
    }

  /** The innermost [[SemExpression.ValueReference]] of a (possibly curried) application target, with its accumulated
    * type arguments — the callee whose calculated return is being resolved.
    */
  private def innermostValueRef(expr: SemExpression): Option[(Sourced[ValueFQN], Seq[SemValue])] =
    expr.expression match {
      case SemExpression.ValueReference(fqn, typeArgs)  => Some((fqn, typeArgs))
      case SemExpression.FunctionApplication(target, _) => innermostValueRef(target.value)
      case _                                            => None
    }

  /** Read the callee's body-checked return at the concrete type arguments: quote the (instantiation-meta) type args to
    * ground and read `MonomorphicValue(callee, args).signature`'s deep return type as a [[SemValue]]. [[None]] when the
    * args are not yet ground (left as a bare return for the ordinary check to reject) or no monomorphization exists.
    */
  private def readMonomorphicReturn(
      fqn: Sourced[ValueFQN],
      typeArgs: Seq[SemValue]
  ): CheckIO[Option[SemValue]] =
    for {
      s          <- get
      groundArgsE = typeArgs.toList.traverse(a => Quoter.quote(0, a, s.unifier.metaStore))
      result     <- groundArgsE match {
                      case Left(_)           => reportUngroundCalculatedReturn(fqn)
                      case Right(groundArgs) => readMonomorphicReturnGround(fqn, groundArgs)
                    }
    } yield result

  /** W4 (Limit 3 / deferred W3 item 2): a calculated return is read off `MonomorphicValue(callee, args)`, so the
    * callee's type arguments must be ground at the call. They are not when an argument's bounds come from a branch join
    * (a `Combine`) that is resolved only later, in the drain loop — `double(pick(a, b))` instantiates `double`'s bounds
    * from `pick`'s combinable result, which is deferred. Reading the return eagerly here would leave the bare `Type`
    * placeholder, which then leaks into a confusing `Coerce` mismatch downstream. Report a specific, actionable error
    * instead. (Resolving such a call by postponing the calculation past the join — making it *compile* — is a
    * completeness improvement deferred to W5; it requires reordering against the combinable-meta machinery.)
    */
  private def reportUngroundCalculatedReturn(fqn: Sourced[ValueFQN]): CheckIO[Option[SemValue]] =
    liftF(
      compilerError(
        fqn.as(
          s"Cannot calculate the return type of '${fqn.value.name.name}' here: its argument bounds are not determined at this call site."
        ),
        Seq(
          "This happens when an argument's bounds come from a branch join (a `Combine`) not yet resolved when the call is checked.",
          "Annotate the argument's type, or give the value an explicit return type."
        )
      ) >> abort[Option[SemValue]]
    )

  /** Read the callee's monomorphized return at ground type arguments, first guarding against a recursive
    * calculated-return chain (Limit 1 of the implicit-generics calculated-return limits). If the callee's FQN is already an ancestor
    * of the fact being checked now, requesting `MonomorphicValue(callee, args)` would re-enter an in-progress
    * computation and dead-lock the fact cache — and, more fundamentally, the callee's return depends (directly,
    * mutually, or through a value-dependent bound) on itself, which monomorphization-by-type cannot ground. A
    * non-recursive program has an acyclic producer call graph, so a repeated FQN on the active chain is exactly the
    * recursion signal; report it as a specific error rather than blocking forever or defaulting to `Type`.
    */
  private def readMonomorphicReturnGround(
      fqn: Sourced[ValueFQN],
      groundArgs: Seq[GroundValue]
  ): CheckIO[Option[SemValue]] =
    liftF(activeFactKeys).flatMap { active =>
      val recursing = active.exists {
        case MonomorphicValue.Key(vfqn, _) => vfqn == fqn.value
        case _                             => false
      }
      if (recursing) liftF(reportRecursiveCalculatedReturn(fqn) >> abort[Option[SemValue]])
      else
        liftF(getFact(MonomorphicValue.Key(fqn.value, groundArgs)))
          .map(_.map(mv => Evaluator.groundToSem(mv.signature.deepReturnType)))
    }

  private def reportRecursiveCalculatedReturn(fqn: Sourced[ValueFQN]): CompilerIO[Unit] =
    compilerError(
      fqn.as(s"Cannot calculate the return type of recursive value '${fqn.value.name.name}'."),
      Seq(
        "Its result type depends on itself — directly, mutually, or through a value-dependent bound — which " +
          "monomorphization cannot ground.",
        "Write an explicit return type."
      )
    )

  /** Replace the return position of a calculated-return signature with a fresh metavariable, returning the rewritten
    * signature and the meta's id (W3 callee side). The signature is a chain of value-parameter `VPi` arrows ending in
    * the bare omittable return left by `saturate`; the meta stands in for that return so that *checking the body*
    * against this signature solves it (by ordinary unification) to the body's inferred type. A no-parameter producer
    * (`def x: Int = 5`) has no arrows, so the signature *is* the return and is replaced directly. The descent reads a
    * snapshot of the metastore to tell a `VPi` arrow from the return; that is stable, because the arrows come from the
    * (meta-independent) `Function` native, not from any meta solution.
    */
  private[check] def installReturnMeta(sig: SemValue): CheckIO[(SemValue, VMeta)] =
    for {
      meta <- freshMeta
      s    <- get
    } yield (substituteReturn(sig, meta, s.unifier.metaStore), meta)

  private def substituteReturn(sig: SemValue, meta: SemValue, metaStore: MetaStore): SemValue =
    Evaluator.force(sig, metaStore) match {
      case VPi(domain, codomain) => VPi(domain, arg => substituteReturn(codomain(arg), meta, metaStore))
      case _                     => meta
    }

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
      _                       <- recordCarrierMetas(expr, implicitMetas)
      updated                 <- appendTypeArgs(expr, implicitMetas)
    } yield (updated.copy(expressionType = peeled), peeled)

  /** Tag the freshly-peeled instantiation metas that stand for *higher-kinded* type parameters (a `[F[_]]` carrier)
    * with their expected kind, so [[verifyCarrierKinds]] can reject a wrong-kind solution post-drain. Only a
    * [[SemExpression.ValueReference]] carries a polytype, so the binders' kinds are read off the referenced value's
    * signature ([[SignatureView]]); the metas align with the binders *after* the explicit type arguments already
    * applied. An ordinary `[A]` binder (kind `Type`) is left untagged — solving it to a proper type is correct. */
  private def recordCarrierMetas(expr: SemExpression, implicitMetas: Seq[SemValue]): CheckIO[Unit] =
    expr.expression match {
      case SemExpression.ValueReference(fqn, explicitArgs) if implicitMetas.nonEmpty =>
        for {
          svOpt <- liftF(getFact(SaturatedValue.Key(fqn.value)))
          _     <- svOpt match {
                     case None     => pure(())
                     case Some(sv) =>
                       val signature = sv.value.typeStack.map(_.signature)
                       val binders   = SignatureView.of(signature).binders.drop(explicitArgs.size)
                       implicitMetas.zip(binders).traverse_ {
                         case (VMeta(id, _), binder) => recordIfHigherKinded(id, binder, fqn)
                         case _                      => pure(())
                       }
                   }
        } yield ()
      case _                                                                         => pure(())
    }

  /** Evaluate a binder's kind annotation; if it forces to a `VPi` (a higher kind such as `Type -> Type`), record the
    * meta as a carrier with that kind. A `Type`-kinded (ordinary) binder, or one with no annotation, is not recorded.
    */
  private def recordIfHigherKinded(
      id: SemValue.MetaId,
      binder: SignatureView.Binder,
      fqn: Sourced[ValueFQN]
  ): CheckIO[Unit] =
    binder.parameterType match {
      case None     => pure(())
      case Some(ts) =>
        for {
          kind   <- evalExpr(ts.value.signature, env = Some(Env.empty))
          forced <- force(kind)
          ctx     = fqn.as("Higher-kinded type parameter mismatch.")
          _      <- forced match {
                      case _: VPi => modify(_.recordCarrierKind(id, forced, ctx))
                      case _      => pure(())
                    }
        } yield ()
    }

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

  /** Post-drain kind check for higher-kinded type-parameter instantiation metas (`[F[_]]` carriers). Each carrier
    * recorded by [[recordCarrierMetas]] is checked two ways:
    *
    *   - **Solved** to a value of the wrong kind — a fully-applied proper type (e.g. `?F := Box[String]`, kind `Type`)
    *     where a `Type -> Type` constructor is required — is reported as a mismatch. The unifier's direct empty-spine
    *     solve does not see the binder's declared kind, so it would otherwise be silently accepted.
    *   - **Unsolved** but with a postponed application `?F[a..] ~ H r..` against a *rigid* head `H` (a type constructor
    *     or bound variable) of a *different* arity is unsatisfiable: no injective type constructor `F` makes
    *     `F[a..] = H r..` (only a non-injective constant/projection lambda would, which a carrier never is). It is
    *     reported here rather than being silently dropped from the postponement queue. A carrier postponed against a
    *     *non-rigid* `VPi` (`?F[A, B] ~ Function[A, B]`) is left alone — that is legitimately higher-order unification
    *     the pattern unifier cannot solve, backstopped when the callee is monomorphized at the defaulted carrier.
    */
  def verifyCarrierKinds: CheckIO[Unit] =
    for {
      s <- get
      _ <- s.unifier.carrierMetas.traverse_ { case (rawId, (expectedKind, context)) =>
             for {
               solution <- force(VMeta(SemValue.MetaId(rawId), Spine.SNil))
               _        <- solution match {
                             case _: VMeta => checkUnsolvedCarrier(SemValue.MetaId(rawId), context)
                             case _        =>
                               kindOfSolution(solution).flatMap {
                                 case None       => pure(())
                                 case Some(kind) => doUnify(kind, expectedKind, context)
                               }
                           }
             } yield ()
           }
    } yield ()

  /** Report an unsolved carrier meta that has a postponed application against a rigid head of a mismatched arity (see
    * [[verifyCarrierKinds]]). The postponement queue is at its fixed point by the time this runs, so such a constraint
    * can never resolve.
    */
  private def checkUnsolvedCarrier(id: SemValue.MetaId, context: Sourced[String]): CheckIO[Unit] =
    for {
      s     <- get
      store  = s.unifier.metaStore
      unsat  = s.unifier.postponed.exists { case (l, r, _) =>
                 unsatisfiableApplication(id, l, r, store) || unsatisfiableApplication(id, r, l, store)
               }
      _     <- if (unsat) modify(st => st.withUnifier(st.unifier.addError(context))) else pure(())
    } yield ()

  /** True when `applied` forces to this carrier meta applied to a non-empty spine, and `other` forces to a rigid head
    * (a body-less [[VTopDef]] type constructor or a [[VNeutral]] bound variable) that is *under-applied* relative to the
    * meta — the unsatisfiable shape `?F[a..] ~ H r..` with `arity(H) < arity(F's spine)`: no injective `F` makes `F`
    * applied to *more* args equal `H` applied to *fewer*. (`arity(H) >= arity(F)` is satisfiable by partial-application
    * injectivity — `?F := H` applied to the leading prefix — and is solved in [[Unifier.decomposeSpines]], so it never
    * reaches here as a postponed constraint.)
    */
  private def unsatisfiableApplication(
      id: SemValue.MetaId,
      applied: SemValue,
      other: SemValue,
      store: MetaStore
  ): Boolean =
    Evaluator.force(applied, store) match {
      case VMeta(mid, spine) if mid.value == id.value && spine.toList.nonEmpty =>
        Evaluator.force(other, store) match {
          case VTopDef(_, None, rhsSpine) => rhsSpine.toList.length < spine.toList.length
          case VNeutral(_, rhsSpine)      => rhsSpine.toList.length < spine.toList.length
          case _                          => false
        }
      case _                                                                   => false
    }

  /** The kind (type) of a carrier meta's solution, or [[None]] when it cannot be determined locally (so no false
    * positive is raised). A type-constructor application `C[a..]` has the constructor's kind with the applied spine
    * removed: `Box` ⟹ `Type -> Type`, `Box[String]` ⟹ `Type`. A function type (`VPi`) or `Type` is itself a `Type`.
    */
  private def kindOfSolution(sv: SemValue): CheckIO[Option[SemValue]] =
    force(sv).flatMap {
      case VConst(g)                => pure(Some(Evaluator.groundToSem(g.valueType)))
      case VType                    => pure(Some(VType))
      case _: VPi                   => pure(Some(VType))
      case VTopDef(fqn, None, spine) =>
        kindOfTypeConstructor(fqn).map(_.map(headKind => spine.toList.foldLeft(headKind)(Evaluator.applyValue)))
      case _                        => pure(None)
    }

  /** The kind of a type constructor, read off its signature (which, for a type constructor, *is* its kind chain —
    * e.g. `data Box[A]` ⟹ `Function[Type, Type]`). [[None]] when the value has no fetchable signature (a native or
    * primitive head), in which case the carrier check is skipped for that solution.
    */
  private def kindOfTypeConstructor(fqn: ValueFQN): CheckIO[Option[SemValue]] =
    liftF(getFact(SaturatedValue.Key(fqn))).flatMap {
      case None     => pure(None)
      case Some(sv) => evalExpr(sv.value.typeStack.value.signature, env = Some(Env.empty)).map(Some(_))
    }

}
