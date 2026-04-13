package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{Qualifier => CoreQualifier}
import com.vanillasource.eliot.eliotc.eval.fact.{Types, Value}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.*
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerError, compilerAbort}

/** Bidirectional type checker for the NbE pipeline. All state is threaded via the CheckIO state monad.
  *
  *   - `check(tm, expected)` checks a term against a known type.
  *   - `infer(tm)` infers a term's type.
  */
class Checker(
    fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
    fetchValueType: ValueFQN => CompilerIO[Option[SemValue]],
    paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]] = Map.empty,
    resolveAbility: (ValueFQN, Seq[Value]) => CompilerIO[Option[ValueFQN]] = (_, _) => None.pure[CompilerIO]
) {

  /** Ensure a NativeBinding is in the cache, fetching it via CompilerIO if needed. */
  private def ensureBinding(vfqn: ValueFQN): CheckIO[Option[SemValue]] =
    for {
      cached <- inspect(_.bindingCache.get(vfqn))
      result <- cached match {
                  case Some(value) => pure(value)
                  case None        =>
                    for {
                      opt <- liftF(fetchBinding(vfqn))
                      _   <- modify(_.cacheBinding(vfqn, opt))
                    } yield opt
                }
    } yield result

  /** Create an evaluator from the current state. Pure — only reads cache and nameLevels. */
  private def makeEvaluator(state: CheckState): Evaluator =
    new Evaluator(
      vfqn => state.bindingCache.getOrElse(vfqn, None),
      state.nameLevels
    )

  /** Evaluate an ORE expression. Fetches any referenced bindings on demand before evaluating. */
  def evalExpr(tm: OperatorResolvedExpression): CheckIO[SemValue] =
    for {
      _ <- fetchBindings(tm)
      s <- get
    } yield makeEvaluator(s).eval(s.env, tm)

  /** Force a SemValue through the current meta store. */
  private[check] def force(v: SemValue): CheckIO[SemValue] =
    inspect(s => Evaluator.force(v, s.unifier.metaStore))

  /** Unify two semantic values, updating the unifier in the state. */
  private def doUnify(l: SemValue, r: SemValue, context: Sourced[String]): CheckIO[Unit] =
    modify(s => s.withUnifier(s.unifier.unify(l, r, context)))

  /** Allocate a fresh metavariable. */
  private[check] def freshMeta: CheckIO[VMeta] =
    for {
      s                    <- get
      (metaId, freshStore)  = s.unifier.metaStore.fresh
      _                    <- modify(_.withUnifier(s.unifier.copy(metaStore = freshStore)))
    } yield VMeta(metaId, Spine.SNil, VType)

  /** Check a term against a known expected type. */
  def check(
      tm: Sourced[OperatorResolvedExpression],
      expected: SemValue
  ): CheckIO[MonomorphicExpression] =
    for {
      forcedExpected <- force(expected)
      result         <- tm.value match {
                          // FunctionLiteral with annotation checked against expected type
                          case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramTypeStack), body) =>
                            for {
                              paramType   <- evalExpr(paramTypeStack.value.signature)
                              _           <- modify(_.bind(paramName.value, paramType))
                              retType     <- getReturnType(forcedExpected)
                              bodyExpr    <- check(body, retType)
                              paramGround <- forceAndConst(paramType)
                              exprType    <- forceAndConst(forcedExpected)
                            } yield MonomorphicExpression(
                              exprType,
                              MonomorphicExpression.FunctionLiteral(paramName, paramGround, body.as(bodyExpr))
                            )

                          // FunctionLiteral without annotation checked against VPi — use domain from VPi
                          case OperatorResolvedExpression.FunctionLiteral(paramName, None, body)                 =>
                            forcedExpected match {
                              case VPi(domain, codomain) =>
                                for {
                                  _           <- modify(_.bind(paramName.value, domain))
                                  bodyExpr    <- check(body, codomain(domain))
                                  paramGround <- forceAndConst(domain)
                                  exprType    <- forceAndConst(forcedExpected)
                                } yield MonomorphicExpression(
                                  exprType,
                                  MonomorphicExpression.FunctionLiteral(paramName, paramGround, body.as(bodyExpr))
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
                              (instantiated, implicitMetas) <- instantiateCollecting(inferred)
                              _                             <- doUnify(instantiated, expected, tm.as("Type mismatch."))
                              resolvedImplicits             <- implicitMetas.traverse(forceAndConst)
                              updatedExpr                    = addImplicitTypeArgs(expr, resolvedImplicits)
                            } yield updatedExpr
                        }
    } yield result

  /** Peel off leading VLam closures by instantiating them with fresh metas, collecting the metas for later resolution.
    */
  private def instantiateCollecting(
      sem: SemValue,
      metas: Seq[SemValue] = Seq.empty
  ): CheckIO[(SemValue, Seq[SemValue])] =
    for {
      forced <- force(sem)
      result <- forced match {
                  case VLam(_, closure) =>
                    for {
                      meta   <- freshMeta
                      result <- instantiateCollecting(closure(meta), metas :+ meta)
                    } yield result
                  case other            => pure((other, metas))
                }
    } yield result

  /** Infer the type of a term. */
  def infer(
      tm: Sourced[OperatorResolvedExpression]
  ): CheckIO[(MonomorphicExpression, SemValue)] = tm.value match {
    case OperatorResolvedExpression.IntegerLiteral(value) =>
      val tpe = VConst(Evaluator.bigIntGroundType)
      pure((MonomorphicExpression(Evaluator.bigIntGroundType, MonomorphicExpression.IntegerLiteral(value)), tpe))

    case OperatorResolvedExpression.StringLiteral(value) =>
      val tpe = VConst(Evaluator.stringGroundType)
      pure((MonomorphicExpression(Evaluator.stringGroundType, MonomorphicExpression.StringLiteral(value)), tpe))

    case OperatorResolvedExpression.ParameterReference(name) =>
      for {
        state  <- get
        result <- state.nameLevels.get(name.value) match {
                    case Some(level) =>
                      val sem = state.env.lookupByLevel(level)
                      for {
                        ground <- forceAndConst(sem)
                      } yield (MonomorphicExpression(ground, MonomorphicExpression.ParameterReference(name)), sem)
                    case None        =>
                      liftF(compilerError(tm.as("Name not defined.")) >> abort)
                  }
      } yield result

    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs) =>
      for {
        _      <- ensureBinding(vfqn.value)
        sigOpt <- liftF(fetchValueType(vfqn.value))
        result <- sigOpt match {
                    case Some(sig) =>
                      for {
                        resolvedVfqn                    <- tryResolveAbility(vfqn)
                        (appliedSig, explicitGroundArgs) <- typeArgs.foldLeftM((sig, Seq.empty[GroundValue])) {
                                                              case ((s, grounds), typeArg) =>
                                                                for {
                                                                  argVal <- evalExpr(typeArg.value)
                                                                  ground <- forceAndConst(argVal)
                                                                } yield (Evaluator.applyValue(s, argVal), grounds :+ ground)
                                                            }
                        ground                           <- forceAndConst(appliedSig)
                      } yield (
                        MonomorphicExpression(
                          ground,
                          MonomorphicExpression.MonomorphicValueReference(resolvedVfqn, explicitGroundArgs)
                        ),
                        appliedSig
                      )
                    case None      =>
                      liftF(compilerError(tm.as("Name not defined.")) >> abort)
                  }
      } yield result

    case OperatorResolvedExpression.FunctionApplication(target, arg) =>
      for {
        (targetExpr, targetType) <- infer(target)
        result                   <- applyInferred(target, targetExpr, targetType, arg, tm)
      } yield result

    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramTypeStack), body) =>
      for {
        paramType            <- evalExpr(paramTypeStack.value.signature)
        _                    <- modify(_.bind(paramName.value, paramType))
        (bodyExpr, bodyType) <- infer(body)
        tpe                   = VPi(paramType, _ => bodyType)
        typeGround           <- forceAndConst(tpe)
        paramGround          <- forceAndConst(paramType)
      } yield (
        MonomorphicExpression(
          typeGround,
          MonomorphicExpression.FunctionLiteral(paramName, paramGround, body.as(bodyExpr))
        ),
        tpe
      )

    case OperatorResolvedExpression.FunctionLiteral(_, None, _) =>
      liftF(compilerError(tm.as("Cannot infer type of unannotated lambda.")) >> abort)
  }

  /** Handle function application: infer target, then apply argument. Tracks implicit type args from VLam instantiation.
    */
  private def applyInferred(
      target: Sourced[OperatorResolvedExpression],
      targetExpr: MonomorphicExpression,
      targetType: SemValue,
      arg: Sourced[OperatorResolvedExpression],
      whole: Sourced[OperatorResolvedExpression],
      implicitTypeArgs: Seq[SemValue] = Seq.empty
  ): CheckIO[(MonomorphicExpression, SemValue)] =
    for {
      forced <- force(targetType)
      result <- forced match {
                  case VPi(domain, codomain) =>
                    for {
                      argExpr           <- check(arg, domain)
                      argSem            <- evalExpr(arg.value)
                      retType            = codomain(argSem)
                      ground            <- forceAndConst(retType)
                      resolvedImplicits <- implicitTypeArgs.traverse(forceAndConst)
                      updatedTarget      = addImplicitTypeArgs(targetExpr, resolvedImplicits)
                    } yield (
                      MonomorphicExpression(
                        ground,
                        MonomorphicExpression.FunctionApplication(target.as(updatedTarget), arg.as(argExpr))
                      ),
                      retType
                    )

                  case VLam(_, closure) =>
                    // Polytype at term level: instantiate with fresh meta, then recurse.
                    // This handles implicit type arg instantiation for generic values like `id(42)`.
                    for {
                      meta   <- freshMeta
                      result <- applyInferred(target, targetExpr, closure(meta), arg, whole, implicitTypeArgs :+ meta)
                    } yield result

                  case _ =>
                    // Try to unify with a fresh function type
                    for {
                      domMeta <- freshMeta
                      codMeta <- freshMeta
                      _       <- doUnify(forced, VPi(domMeta, _ => codMeta), target.as("Not a function."))
                      domain  <- force(domMeta)
                      argExpr <- check(arg, domain)
                      retType <- force(codMeta)
                      ground  <- forceAndConst(retType)
                    } yield (
                      MonomorphicExpression(
                        ground,
                        MonomorphicExpression.FunctionApplication(target.as(targetExpr), arg.as(argExpr))
                      ),
                      retType
                    )
                }
    } yield result

  /** Add resolved implicit type args to a MonomorphicValueReference expression. */
  private def addImplicitTypeArgs(expr: MonomorphicExpression, extraArgs: Seq[GroundValue]): MonomorphicExpression =
    if (extraArgs.isEmpty) expr
    else
      expr.expression match {
        case ref: MonomorphicExpression.MonomorphicValueReference =>
          expr.copy(expression = ref.copy(typeArguments = ref.typeArguments ++ extraArgs))
        case _                                                    => expr
      }

  /** Resolve an ability method reference to its concrete implementation using constraint information. When a
    * ValueReference has an Ability qualifier, the constraint parameter's current binding provides the type arguments
    * for looking up the AbilityImplementation fact.
    */
  private def tryResolveAbility(vfqn: Sourced[ValueFQN]): CheckIO[Sourced[ValueFQN]] =
    vfqn.value.name.qualifier match {
      case CoreQualifier.Ability(abilityName) =>
        findConstraintParam(abilityName) match {
          case Some((_, constraintTypeArgs)) =>
            for {
              abilityTypeArgs <- constraintTypeArgs.traverse { arg =>
                                   for {
                                     sem    <- evalExpr(arg)
                                     ground <- forceAndConst(sem)
                                   } yield GroundValue.toEvalValue(ground)
                                 }
              resolved        <- liftF(resolveAbility(vfqn.value, abilityTypeArgs))
            } yield resolved match {
              case Some(implFqn) => vfqn.as(implFqn)
              case None          => vfqn
            }
          case None                          => pure(vfqn)
        }
      case _                                  => pure(vfqn)
    }

  private def findConstraintParam(abilityName: String): Option[(String, Seq[OperatorResolvedExpression])] =
    paramConstraints.collectFirst {
      Function.unlift { (paramName, constraints) =>
        constraints.find(_.abilityFQN.abilityName == abilityName).map(c => (paramName, c.typeArgs))
      }
    }

  /** Fetch all NativeBindings referenced by ValueReferences in an ORE into the cache. Called automatically by
    * evalExpr before evaluation.
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
        case VPi(_, codomain) => codomain(VNeutral(NeutralHead.VVar(s.env.level, "$ret"), Spine.SNil, VType))
        case other            => other
      }
    }

  private[check] def forceAndConst(v: SemValue): CheckIO[GroundValue] = inspect { state =>
    def go(v: SemValue): GroundValue = {
      val forced = Evaluator.force(v, state.unifier.metaStore)
      forced match {
        case VConst(g)             => g
        case VType                 => GroundValue.Type
        case VPi(domain, codomain) =>
          GroundValue.Structure(
            Map(
              "$typeName" -> GroundValue.Direct(Types.functionDataTypeFQN, GroundValue.Type),
              "A"         -> go(domain),
              "B"         -> go(codomain(VNeutral(NeutralHead.VVar(state.env.level, "$quote"), Spine.SNil, VType)))
            ),
            GroundValue.Type
          )
        case _                     => GroundValue.Type // fallback
      }
    }
    go(v)
  }
}
