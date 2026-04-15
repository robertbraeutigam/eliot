package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
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
    fetchBinding: ValueFQN => CompilerIO[Option[SemValue]]
) {

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
                                      freshMeta.map(meta => Some(meta: SemValue))
                                    case other                                                                       =>
                                      pure(other)
                                  }
                      _        <- modify(_.cacheBinding(vfqn, replaced))
                    } yield replaced
                }
    } yield result

  /** Evaluate an ORE expression under the current state's env. Fetches bindings lazily as the traversal encounters
    * ValueReferences, so [[ensureBinding]]'s intercept of abstract associated ability types fires naturally.
    */
  def evalExpr(tm: OperatorResolvedExpression): CheckIO[SemValue] =
    for {
      s      <- get
      result <- evalIn(s.env, tm)
    } yield result

  /** Evaluate an ORE expression against an empty env. Used for signatures, which have no outer-session bindings in
    * scope and only reference their own parameters or top-level values.
    */
  private def evalExprInEmptyEnv(tm: OperatorResolvedExpression): CheckIO[SemValue] =
    evalIn(Env.empty, tm)

  /** Fetch a value's signature ORE and evaluate it against an empty env. */
  private def fetchAndEvalSignature(vfqn: ValueFQN): CheckIO[Option[SemValue]] =
    liftF(getFact(OperatorResolvedValue.Key(vfqn))).flatMap {
      case Some(orv) => evalExprInEmptyEnv(orv.typeStack.value.signature).map(Some(_))
      case None      => pure(None)
    }

  /** Force a SemValue through the current meta store. */
  private[check] def force(v: SemValue): CheckIO[SemValue] =
    inspect(s => Evaluator.force(v, s.unifier.metaStore))

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
                          // FunctionLiteral with annotation checked against expected type
                          case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramTypeStack), body) =>
                            for {
                              paramType <- evalExpr(paramTypeStack.value.signature)
                              _         <- modify(_.bind(paramName.value, paramType))
                              retType   <- getReturnType(forcedExpected)
                              bodyExpr  <- check(body, retType)
                            } yield SemExpression(
                              forcedExpected,
                              SemExpression.FunctionLiteral(paramName, paramType, body.as(bodyExpr))
                            )

                          // FunctionLiteral without annotation checked against VPi — use domain from VPi
                          case OperatorResolvedExpression.FunctionLiteral(paramName, None, body)                 =>
                            forcedExpected match {
                              case VPi(domain, codomain) =>
                                for {
                                  _        <- modify(_.bind(paramName.value, domain))
                                  bodyExpr <- check(body, codomain(domain))
                                } yield SemExpression(
                                  forcedExpected,
                                  SemExpression.FunctionLiteral(paramName, domain, body.as(bodyExpr))
                                )
                              case _                     =>
                                liftF(compilerError(tm.as("Cannot infer type of unannotated lambda.")) >> abort)
                            }

                          case _ =>
                            for {
                              (expr, inferred)            <- infer(tm)
                              (updatedExpr, instantiated) <- instantiatePolymorphic(expr, inferred)
                              _                           <- doUnify(instantiated, expected, tm.as("Type mismatch."))
                            } yield updatedExpr
                        }
    } yield result

  /** Peel off leading VLam closures by substituting fresh metas, and return the resulting non-VLam head together with
    * the metas collected in order. The metas are not bound in the env — use [[peelLamsBinding]] if you also need to
    * bind the peeled parameter names.
    */
  private[check] def peelLams(sem: SemValue): CheckIO[(SemValue, Seq[SemValue])] =
    peelLamsImpl(sem, bindInEnv = false)

  /** Like [[peelLams]] but also binds each peeled parameter name to its fresh meta in the env. Used by the top-level
    * type-stack walk so remaining unapplied type parameters become in-scope names.
    */
  private[check] def peelLamsBinding(sem: SemValue): CheckIO[(SemValue, Seq[SemValue])] =
    peelLamsImpl(sem, bindInEnv = true)

  private def peelLamsImpl(
      sem: SemValue,
      bindInEnv: Boolean
  ): CheckIO[(SemValue, Seq[SemValue])] = {
    def loop(s: SemValue, acc: Seq[SemValue]): CheckIO[(SemValue, Seq[SemValue])] =
      for {
        forced <- force(s)
        result <- forced match {
                    case VLam(name, closure) =>
                      for {
                        meta <- freshMeta
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
                      pure((SemExpression(sem, SemExpression.ParameterReference(name)), sem))
                    case None      =>
                      liftF(compilerError(tm.as("Name not defined.")) >> abort)
                  }
      } yield result

    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs) =>
      for {
        _      <- ensureBinding(vfqn.value)
        sigOpt <- fetchAndEvalSignature(vfqn.value)
        result <- sigOpt match {
                    case Some(sig) =>
                      for {
                        explicitTypeArgs <- typeArgs.traverse(ta => evalExpr(ta.value))
                        _                <- recordIfAbility(vfqn, explicitTypeArgs)
                      } yield {
                        val appliedSig = explicitTypeArgs.foldLeft(sig)(Evaluator.applyValue)
                        (
                          SemExpression(appliedSig, SemExpression.ValueReference(vfqn, explicitTypeArgs)),
                          appliedSig
                        )
                      }
                    case None      =>
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
      retType                  = vpi.codomain(argSem)
    } yield (
      SemExpression(
        retType,
        SemExpression.FunctionApplication(target.as(updatedTarget), arg.as(argExpr))
      ),
      retType
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
      updated                 <- addTypeArgs(expr, implicitMetas)
    } yield (updated.copy(expressionType = peeled), peeled)

  /** If the given vfqn has an `Ability` qualifier, record the (sourced-vfqn → type args) entry in state so the
    * drain-and-resolve loop doesn't have to re-walk the output tree.
    */
  private def recordIfAbility(vfqn: Sourced[ValueFQN], typeArgs: Seq[SemValue]): CheckIO[Unit] =
    vfqn.value.name.qualifier match {
      case _: Qualifier.Ability => modify(_.recordAbilityRef(vfqn, typeArgs))
      case _                    => pure(())
    }

  /** Append type args to a [[SemExpression.ValueReference]] expression. Only a value reference can inherit a polytype
    * (since polymorphism lives on named signatures), so no other shape should ever arrive here with non-empty
    * `extraArgs`. Hitting that branch indicates the checker produced a polytype for a non-reference expression —
    * surface it as a compiler bug instead of silently dropping the type args.
    *
    * For ability-qualified refs, the updated type-argument sequence is also recorded in the state so the
    * drain-and-resolve loop sees the final set (explicit surface args + implicit metas).
    */
  private def addTypeArgs(expr: SemExpression, extraArgs: Seq[SemValue]): CheckIO[SemExpression] =
    if (extraArgs.isEmpty) pure(expr)
    else
      expr.expression match {
        case ref: SemExpression.ValueReference =>
          val updatedArgs = ref.typeArguments ++ extraArgs
          val updatedExpr = expr.copy(expression = ref.copy(typeArguments = updatedArgs))
          recordIfAbility(ref.valueName, updatedArgs).as(updatedExpr)
        case other                             =>
          throw new IllegalStateException(
            s"Polytype instantiation produced implicit type arguments for a non-reference expression: $other"
          )
      }

  /** Single-traversal evaluator: walks the ORE in CheckIO, fetching bindings inline at each ValueReference and building
    * the corresponding SemValue. Replaces the old pre-walk-then-pure-eval pattern.
    *
    * `FunctionLiteral` is special: its body is not evaluated here (it becomes a VLam closure that fires later via the
    * pure [[Evaluator]]), so we still prefetch all bindings reachable through the body. Likewise for parameter type
    * annotations, whose type-stack levels the pure evaluator may read when the closure runs.
    */
  private def evalIn(env: Env, tm: OperatorResolvedExpression): CheckIO[SemValue] = tm match {
    case OperatorResolvedExpression.IntegerLiteral(value) =>
      pure(VConst(GroundValue.Direct(value.value, Evaluator.bigIntGroundType)))

    case OperatorResolvedExpression.StringLiteral(value) =>
      pure(VConst(GroundValue.Direct(value.value, Evaluator.stringGroundType)))

    case OperatorResolvedExpression.ParameterReference(name) =>
      pure(
        env
          .lookupByName(name.value)
          .getOrElse(VNeutral(NeutralHead.VVar(env.level, name.value), Spine.SNil))
      )

    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs) =>
      for {
        binding <- ensureBinding(vfqn.value)
        // Prefetch bindings reachable through type arguments; the pure Evaluator doesn't evaluate them, but the
        // checker does evaluate them separately elsewhere, and any nested ValueReferences need their bindings in
        // cache before the pure evaluator fires on enclosing lambda bodies.
        _       <- typeArgs.traverse_(ta => prefetchBindings(ta.value))
      } yield binding.getOrElse(VNeutral(NeutralHead.VVar(env.level, vfqn.value.name.name), Spine.SNil))

    case OperatorResolvedExpression.FunctionApplication(target, arg) =>
      for {
        tv <- evalIn(env, target.value)
        av <- evalIn(env, arg.value)
      } yield Evaluator.applyValue(tv, av)

    case OperatorResolvedExpression.FunctionLiteral(paramName, paramType, body) =>
      for {
        _ <- paramType.traverse_(pt => pt.value.levels.toSeq.traverse_(prefetchBindings))
        _ <- prefetchBindings(body.value)
        s <- get
      } yield {
        val ev = s.makeEvaluator
        VLam(paramName.value, arg => ev.eval(env.bind(paramName.value, arg), body.value))
      }
  }

  /** Prefetch-only traversal: walks an ORE and calls [[ensureBinding]] at every ValueReference, discarding any
    * resulting SemValue. Used for subtrees whose actual evaluation is deferred to a pure [[Evaluator]] invocation
    * inside a [[VLam]] closure — the closure must find every reachable binding already in the cache.
    */
  private def prefetchBindings(ore: OperatorResolvedExpression): CheckIO[Unit] = ore match {
    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs)      =>
      ensureBinding(vfqn.value).void >>
        typeArgs.traverse_(ta => prefetchBindings(ta.value))
    case OperatorResolvedExpression.FunctionApplication(target, arg)    =>
      prefetchBindings(target.value) >> prefetchBindings(arg.value)
    case OperatorResolvedExpression.FunctionLiteral(_, paramType, body) =>
      paramType.traverse_(pt => pt.value.levels.toSeq.traverse_(prefetchBindings)) >>
        prefetchBindings(body.value)
    case _                                                              => pure(())
  }

  private def getReturnType(funcType: SemValue): CheckIO[SemValue] =
    inspect { s =>
      funcType match {
        case VPi(_, codomain) => codomain(VNeutral(NeutralHead.VVar(s.env.level, "$ret"), Spine.SNil))
        case other            => other
      }
    }
}
