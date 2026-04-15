package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
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
    * impl's corresponding associated-type value. One meta per (fqn, check-session).
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
                                    case Some(VTopDef(fqn, None, Spine.SNil)) if Checker.isAbstractAbilityType(fqn) =>
                                      allocateAssociatedTypeMeta(fqn).map(Some(_))
                                    case other                                                                      =>
                                      pure(other)
                                  }
                      _        <- modify(_.cacheBinding(vfqn, replaced))
                    } yield replaced
                }
    } yield result

  /** Allocate or reuse the meta standing in for an abstract associated ability type. */
  private def allocateAssociatedTypeMeta(fqn: ValueFQN): CheckIO[SemValue] =
    for {
      existing <- inspect(_.associatedTypeMetas.get(fqn))
      result   <- existing match {
                    case Some(id) => pure(VMeta(id, Spine.SNil))
                    case None     =>
                      for {
                        meta <- freshMeta
                        _    <- modify(_.recordAssociatedTypeMeta(fqn, meta.id))
                      } yield (meta: SemValue)
                  }
    } yield result

  /** Evaluate an ORE expression under the current state's env. Pre-fetches every referenced binding (so
    * [[ensureBinding]]'s intercept of abstract associated ability types fires before the pure [[Evaluator]] runs).
    */
  def evalExpr(tm: OperatorResolvedExpression): CheckIO[SemValue] =
    evalExprIn(tm, useCurrentEnv = true)

  /** Fetch a value's signature ORE and evaluate it against an empty env (signatures have no local bindings in scope).
    * Pre-fetches referenced bindings the same way [[evalExpr]] does.
    */
  private def fetchAndEvalSignature(vfqn: ValueFQN): CheckIO[Option[SemValue]] =
    liftF(getFact(OperatorResolvedValue.Key(vfqn))).flatMap {
      case Some(orv) => evalExprIn(orv.typeStack.value.signature, useCurrentEnv = false).map(Some(_))
      case None      => pure(None)
    }

  private def evalExprIn(tm: OperatorResolvedExpression, useCurrentEnv: Boolean): CheckIO[SemValue] =
    for {
      _ <- fetchBindings(tm)
      s <- get
    } yield s.makeEvaluator.eval(if (useCurrentEnv) s.env else Env.empty, tm)

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
                                for {
                                  (expr, inferred) <- infer(tm)
                                  _                <- doUnify(inferred, expected, tm.as("Type mismatch."))
                                } yield expr
                            }

                          case _ =>
                            for {
                              (expr, inferred)              <- infer(tm)
                              (instantiated, implicitMetas) <- peelLams(inferred, (_, _) => pure(()))
                              _                             <- doUnify(instantiated, expected, tm.as("Type mismatch."))
                              updatedExpr                    =
                                addTypeArgs(expr, implicitMetas).copy(expressionType = instantiated)
                            } yield updatedExpr
                        }
    } yield result

  /** Peel off leading VLam closures by substituting fresh metas, and return the resulting non-VLam head together with
    * the metas collected in order. `onPeel` runs for each VLam peeled, receiving the parameter name and the fresh meta
    * — used by the top-level type-stack walk to bind param names in the env.
    */
  private[check] def peelLams(
      sem: SemValue,
      onPeel: (String, SemValue) => CheckIO[Unit]
  ): CheckIO[(SemValue, Seq[SemValue])] = {
    def loop(s: SemValue, acc: Seq[SemValue]): CheckIO[(SemValue, Seq[SemValue])] =
      for {
        forced <- force(s)
        result <- forced match {
                    case VLam(name, closure) =>
                      for {
                        meta <- freshMeta
                        _    <- onPeel(name, meta)
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
        result <- state.nameLevels.get(name.value) match {
                    case Some(level) =>
                      val sem = state.env.lookupByLevel(level)
                      pure((SemExpression(sem, SemExpression.ParameterReference(name)), sem))
                    case None        =>
                      liftF(compilerError(tm.as("Name not defined.")) >> abort)
                  }
      } yield result

    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs) =>
      for {
        _      <- ensureBinding(vfqn.value)
        sigOpt <- fetchAndEvalSignature(vfqn.value)
        result <- sigOpt match {
                    case Some(sig) =>
                      typeArgs.traverse(ta => evalExpr(ta.value)).map { explicitTypeArgs =>
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
      (peeled, implicitMetas) <- peelLams(targetType, (_, _) => pure(()))
      updatedTarget            = addTypeArgs(targetExpr, implicitMetas).copy(expressionType = peeled)
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

  /** Append type args to a [[SemExpression.ValueReference]] expression. No-op for other shapes. */
  private def addTypeArgs(expr: SemExpression, extraArgs: Seq[SemValue]): SemExpression =
    if (extraArgs.isEmpty) expr
    else
      expr.expression match {
        case ref: SemExpression.ValueReference =>
          expr.copy(expression = ref.copy(typeArguments = ref.typeArguments ++ extraArgs))
        case _                                 => expr
      }

  /** Fetch all NativeBindings referenced by ValueReferences in an ORE into the cache. Called automatically by evalExpr
    * before evaluation.
    */
  private def fetchBindings(ore: OperatorResolvedExpression): CheckIO[Unit] = ore match {
    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs)      =>
      ensureBinding(vfqn.value).void >>
        typeArgs.traverse_(ta => fetchBindings(ta.value))
    case OperatorResolvedExpression.FunctionApplication(target, arg)    =>
      fetchBindings(target.value) >> fetchBindings(arg.value)
    case OperatorResolvedExpression.FunctionLiteral(_, paramType, body) =>
      paramType.traverse_(pt => pt.value.levels.toSeq.traverse_(fetchBindings)) >>
        fetchBindings(body.value)
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

object Checker {

  /** True when the FQN refers to a declaration inside an ability block whose name starts with an uppercase letter,
    * indicating an associated type rather than an abstract method. Abstract associated types have no runtime body; the
    * concrete value comes from the ability impl and is resolved post-drain.
    */
  def isAbstractAbilityType(fqn: ValueFQN): Boolean =
    fqn.name.qualifier match {
      case _: Qualifier.Ability => fqn.name.name.headOption.exists(_.isUpper)
      case _                    => false
    }
}
