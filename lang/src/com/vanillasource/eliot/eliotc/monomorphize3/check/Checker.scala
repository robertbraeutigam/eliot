package com.vanillasource.eliot.eliotc.monomorphize3.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{Qualifier => CoreQualifier}
import com.vanillasource.eliot.eliotc.eval.fact.{Types, Value}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize3.domain.*
import com.vanillasource.eliot.eliotc.monomorphize3.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize3.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize3.fact.*
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerError, compilerAbort}

/** Bidirectional type checker for the NbE pipeline. All state is threaded explicitly through CheckState — no mutable
  * fields.
  *
  *   - `check(state, tm, expected)` checks a term against a known type, returning updated state.
  *   - `infer(state, tm)` infers a term's type, returning updated state.
  */
class Checker(
    fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
    fetchValueType: ValueFQN => CompilerIO[Option[SemValue]],
    paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]] = Map.empty,
    resolveAbility: (ValueFQN, Seq[Value]) => CompilerIO[Option[ValueFQN]] = (_, _) => None.pure[CompilerIO]
) {

  /** Ensure a NativeBinding is in the cache, fetching it via CompilerIO if needed. */
  private def ensureBinding(state: CheckState, vfqn: ValueFQN): CompilerIO[(Option[SemValue], CheckState)] =
    state.bindingCache.get(vfqn) match {
      case Some(cached) => (cached, state).pure[CompilerIO]
      case None         =>
        fetchBinding(vfqn).map { opt =>
          (opt, state.cacheBinding(vfqn, opt))
        }
    }

  /** Create an evaluator that can resolve ValueReferences from the binding cache. */
  private def makeEvaluator(state: CheckState): Evaluator =
    new Evaluator(
      vfqn => state.bindingCache.getOrElse(vfqn, None),
      state.nameLevels
    )

  /** Evaluate an ORE expression using the given state. */
  def evalExpr(state: CheckState, tm: OperatorResolvedExpression): SemValue =
    makeEvaluator(state).eval(state.env, tm)

  /** Check a term against a known expected type. Returns the checked expression and updated state. */
  def check(
      state: CheckState,
      tm: Sourced[OperatorResolvedExpression],
      expected: SemValue
  ): CompilerIO[(Monomorphic3Expression, CheckState)] = {
    val forcedExpected = Evaluator.force(expected, state.unifier.metaStore)
    tm.value match {
      // FunctionLiteral with annotation checked against expected type
      case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramTypeStack), body) =>
        for {
          state1             <- prefetchBindings(state, paramTypeStack.value.signature)
          paramType           = evalExpr(state1, paramTypeStack.value.signature)
          state2              = state1.bind(paramName.value, paramType)
          (bodyExpr, state3) <- check(state2, body, getReturnType(state2, forcedExpected))
          paramGround         = forceAndConst(state3, paramType)
        } yield (
          Monomorphic3Expression(
            forceAndConst(state3, forcedExpected),
            Monomorphic3Expression.FunctionLiteral(paramName, paramGround, body.as(bodyExpr))
          ),
          state3
        )

      // FunctionLiteral without annotation checked against VPi — use domain from VPi
      case OperatorResolvedExpression.FunctionLiteral(paramName, None, body)                 =>
        forcedExpected match {
          case VPi(domain, codomain) =>
            val state1 = state.bind(paramName.value, domain)
            for {
              (bodyExpr, state2) <- check(state1, body, codomain(domain))
              paramGround         = forceAndConst(state2, domain)
            } yield (
              Monomorphic3Expression(
                forceAndConst(state2, forcedExpected),
                Monomorphic3Expression.FunctionLiteral(paramName, paramGround, body.as(bodyExpr))
              ),
              state2
            )
          case _                     =>
            for {
              ((expr, inferred), state1) <- infer(state, tm)
              state2 = state1.withUnifier(state1.unifier.unify(inferred, expected, tm.as("Type mismatch.")))
            } yield (expr, state2)
        }

      case _ =>
        for {
          ((expr, inferred), state1) <- infer(state, tm)
          (instantiated, state2)      = instantiatePolymorphic(state1, inferred)
          state3                      = unifyWithContext(state2, instantiated, expected, tm)
        } yield (expr, state3)
    }
  }

  /** Peel off leading VLam closures by instantiating them with fresh metas. This handles implicit type arg
    * instantiation when a polymorphic value is checked against a concrete/meta expected type.
    */
  private def instantiatePolymorphic(state: CheckState, sem: SemValue): (SemValue, CheckState) = {
    val forced = Evaluator.force(sem, state.unifier.metaStore)
    forced match {
      case VLam(_, closure) =>
        val (metaId, freshStore) = state.unifier.metaStore.fresh
        val freshMeta            = VMeta(metaId, Spine.SNil, VType)
        instantiatePolymorphic(state.withUnifier(state.unifier.copy(metaStore = freshStore)), closure(freshMeta))
      case other            => (other, state)
    }
  }

  /** Infer the type of a term. Returns the expression, its type, and updated state. */
  def infer(
      state: CheckState,
      tm: Sourced[OperatorResolvedExpression]
  ): CompilerIO[((Monomorphic3Expression, SemValue), CheckState)] = tm.value match {
    case OperatorResolvedExpression.IntegerLiteral(value) =>
      val tpe = VConst(Evaluator.bigIntGroundType)
      (
        (Monomorphic3Expression(Evaluator.bigIntGroundType, Monomorphic3Expression.IntegerLiteral(value)), tpe),
        state
      ).pure[CompilerIO]

    case OperatorResolvedExpression.StringLiteral(value) =>
      val tpe = VConst(Evaluator.stringGroundType)
      (
        (Monomorphic3Expression(Evaluator.stringGroundType, Monomorphic3Expression.StringLiteral(value)), tpe),
        state
      ).pure[CompilerIO]

    case OperatorResolvedExpression.ParameterReference(name) =>
      state.nameLevels.get(name.value) match {
        case Some(level) =>
          val sem  = state.env.lookupByLevel(level)
          val expr = Monomorphic3Expression(
            forceAndConst(state, sem),
            Monomorphic3Expression.ParameterReference(name)
          )
          ((expr, sem), state).pure[CompilerIO]
        case None        =>
          compilerError(tm.as("Name not defined.")) >> abort
      }

    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs) =>
      for {
        (_, state1) <- ensureBinding(state, vfqn.value)
        sigOpt      <- fetchValueType(vfqn.value)
        result      <- sigOpt match {
                         case Some(sig) =>
                           for {
                             state2       <- typeArgs.foldLeftM(state1) { (s, ta) =>
                                               prefetchBindings(s, ta.value)
                                             }
                             resolvedVfqn <- tryResolveAbility(state2, vfqn)
                           } yield {
                             // Apply explicit type args
                             val appliedSig = typeArgs.foldLeft(sig) { (s, typeArg) =>
                               Evaluator.applyValue(s, evalExpr(state2, typeArg.value))
                             }
                             val expr       = Monomorphic3Expression(
                               forceAndConst(state2, appliedSig),
                               Monomorphic3Expression.MonomorphicValueReference(resolvedVfqn, Seq.empty)
                             )
                             ((expr, appliedSig), state2)
                           }
                         case None      =>
                           compilerError(tm.as("Name not defined.")) >> abort
                       }
      } yield result

    case OperatorResolvedExpression.FunctionApplication(target, arg) =>
      for {
        ((targetExpr, targetType), state1) <- infer(state, target)
        result                             <- applyInferred(state1, target, targetExpr, targetType, arg, tm)
      } yield result

    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramTypeStack), body) =>
      for {
        state1                        <- prefetchBindings(state, paramTypeStack.value.signature)
        paramType                      = evalExpr(state1, paramTypeStack.value.signature)
        state2                         = state1.bind(paramName.value, paramType)
        ((bodyExpr, bodyType), state3) <- infer(state2, body)
      } yield {
        val tpe = VPi(paramType, _ => bodyType)
        (
          (
            Monomorphic3Expression(
              forceAndConst(state3, tpe),
              Monomorphic3Expression.FunctionLiteral(paramName, forceAndConst(state3, paramType), body.as(bodyExpr))
            ),
            tpe
          ),
          state3
        )
      }

    case OperatorResolvedExpression.FunctionLiteral(_, None, _) =>
      compilerError(tm.as("Cannot infer type of unannotated lambda.")) >> abort
  }

  /** Handle function application: infer target, then apply argument. */
  private def applyInferred(
      state: CheckState,
      target: Sourced[OperatorResolvedExpression],
      targetExpr: Monomorphic3Expression,
      targetType: SemValue,
      arg: Sourced[OperatorResolvedExpression],
      whole: Sourced[OperatorResolvedExpression]
  ): CompilerIO[((Monomorphic3Expression, SemValue), CheckState)] = {
    val forced = Evaluator.force(targetType, state.unifier.metaStore)
    forced match {
      case VPi(domain, codomain) =>
        for {
          (argExpr, state1) <- check(state, arg, domain)
          argSem              = evalExpr(state1, arg.value)
          retType             = codomain(argSem)
        } yield {
          val expr = Monomorphic3Expression(
            forceAndConst(state1, retType),
            Monomorphic3Expression.FunctionApplication(target.as(targetExpr), arg.as(argExpr))
          )
          ((expr, retType), state1)
        }

      case VLam(_, closure) =>
        // Polytype at term level: instantiate with fresh meta, then recurse.
        // This handles implicit type arg instantiation for generic values like `id(42)`.
        val (metaId, freshStore) = state.unifier.metaStore.fresh
        val freshMeta            = VMeta(metaId, Spine.SNil, VType)
        val state1               = state.withUnifier(state.unifier.copy(metaStore = freshStore))
        applyInferred(state1, target, targetExpr, closure(freshMeta), arg, whole)

      case _ =>
        // Try to unify with a fresh function type
        val (domId, store1) = state.unifier.metaStore.fresh
        val (codId, store2) = store1.fresh
        val domain          = VMeta(domId, Spine.SNil, VType)
        val codomain        = VMeta(codId, Spine.SNil, VType)
        val unifier1        = state.unifier.copy(metaStore = store2)
          .unify(forced, VPi(domain, _ => codomain), target.as("Not a function."))
        val state1          = state.withUnifier(unifier1)
        val forcedDomain    = Evaluator.force(domain, state1.unifier.metaStore)
        for {
          (argExpr, state2) <- check(state1, arg, forcedDomain)
          retType            = Evaluator.force(codomain, state2.unifier.metaStore)
        } yield {
          val expr = Monomorphic3Expression(
            forceAndConst(state2, retType),
            Monomorphic3Expression.FunctionApplication(target.as(targetExpr), arg.as(argExpr))
          )
          ((expr, retType), state2)
        }
    }
  }

  /** Resolve an ability method reference to its concrete implementation using constraint information. When a
    * ValueReference has an Ability qualifier, the constraint parameter's current binding provides the type arguments
    * for looking up the AbilityImplementation fact.
    */
  private def tryResolveAbility(state: CheckState, vfqn: Sourced[ValueFQN]): CompilerIO[Sourced[ValueFQN]] =
    vfqn.value.name.qualifier match {
      case CoreQualifier.Ability(abilityName) =>
        findConstraintParam(abilityName) match {
          case Some((_, constraintTypeArgs)) =>
            // The constraint's typeArgs already include the constrained parameter
            // (e.g., `A ~ Show` has typeArgs=[ParameterReference("A")])
            val abilityTypeArgs =
              constraintTypeArgs.map(arg => GroundValue.toEvalValue(forceAndConst(state, evalExpr(state, arg))))
            resolveAbility(vfqn.value, abilityTypeArgs).map {
              case Some(implFqn) => vfqn.as(implFqn)
              case None          => vfqn
            }
          case None                          => vfqn.pure[CompilerIO]
        }
      case _                                  => vfqn.pure[CompilerIO]
    }

  private def findConstraintParam(abilityName: String): Option[(String, Seq[OperatorResolvedExpression])] =
    paramConstraints.collectFirst {
      case (paramName, constraints) if constraints.exists(_.abilityFQN.abilityName == abilityName) =>
        val constraint = constraints.find(_.abilityFQN.abilityName == abilityName).get
        (paramName, constraint.typeArgs)
    }

  /** Pre-fetch all NativeBindings referenced by ValueReferences in an ORE. Returns updated state with populated
    * cache.
    */
  private[check] def prefetchBindings(state: CheckState, ore: OperatorResolvedExpression): CompilerIO[CheckState] =
    ore match {
      case OperatorResolvedExpression.ValueReference(vfqn, typeArgs)      =>
        for {
          (_, state1) <- ensureBinding(state, vfqn.value)
          state2      <- typeArgs.foldLeftM(state1) { (s, ta) => prefetchBindings(s, ta.value) }
        } yield state2
      case OperatorResolvedExpression.FunctionApplication(target, arg)    =>
        for {
          state1 <- prefetchBindings(state, target.value)
          state2 <- prefetchBindings(state1, arg.value)
        } yield state2
      case OperatorResolvedExpression.FunctionLiteral(_, paramType, body) =>
        for {
          state1 <- paramType match {
                      case Some(pt) =>
                        pt.value.levels.toSeq.foldLeftM(state) { (s, level) => prefetchBindings(s, level) }
                      case None     => state.pure[CompilerIO]
                    }
          state2 <- prefetchBindings(state1, body.value)
        } yield state2
      case _                                                              => state.pure[CompilerIO]
    }

  private def unifyWithContext(
      state: CheckState,
      inferred: SemValue,
      expected: SemValue,
      tm: Sourced[OperatorResolvedExpression]
  ): CheckState =
    state.withUnifier(state.unifier.unify(inferred, expected, tm.as("Type mismatch.")))

  private def getReturnType(state: CheckState, funcType: SemValue): SemValue = funcType match {
    case VPi(_, codomain) => codomain(VNeutral(NeutralHead.VVar(state.env.level, "$ret"), Spine.SNil, VType))
    case other            => other
  }

  private[check] def forceAndConst(state: CheckState, v: SemValue): GroundValue = {
    val forced = Evaluator.force(v, state.unifier.metaStore)
    forced match {
      case VConst(g)             => g
      case VType                 => GroundValue.Type
      case VPi(domain, codomain) =>
        val domGround = forceAndConst(state, domain)
        val codGround = forceAndConst(
          state,
          codomain(VNeutral(NeutralHead.VVar(state.env.level, "$quote"), Spine.SNil, VType))
        )
        GroundValue.Structure(
          Map(
            "$typeName" -> GroundValue.Direct(Types.functionDataTypeFQN, GroundValue.Type),
            "A"         -> domGround,
            "B"         -> codGround
          ),
          GroundValue.Type
        )
      case _                     => GroundValue.Type // fallback
    }
  }
}
