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

import scala.collection.mutable

/** Bidirectional type checker for the NbE pipeline.
  *
  *   - `check(tm, expected)` checks a term against a known type.
  *   - `infer(tm)` infers a term's type.
  *
  * Uses a mutable binding cache to allow the synchronous NbE evaluator to resolve ValueReferences via NativeBindings
  * that were fetched through CompilerIO.
  */
class Checker(
    var state: CheckState,
    fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
    fetchValueType: ValueFQN => CompilerIO[Option[SemValue]],
    paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]] = Map.empty,
    resolveAbility: (ValueFQN, Seq[Value]) => CompilerIO[Option[ValueFQN]] = (_, _) => None.pure[CompilerIO]
) {
  // Cache of already-fetched NativeBinding SemValues, keyed by ValueFQN
  private val bindingCache: mutable.Map[ValueFQN, Option[SemValue]] = mutable.Map.empty

  /** Ensure a NativeBinding is in the cache, fetching it via CompilerIO if needed. */
  private def ensureBinding(vfqn: ValueFQN): CompilerIO[Option[SemValue]] =
    bindingCache.get(vfqn) match {
      case Some(cached) => cached.pure[CompilerIO]
      case None         =>
        fetchBinding(vfqn).map { opt =>
          bindingCache(vfqn) = opt
          opt
        }
    }

  /** Create an evaluator that can resolve ValueReferences from the binding cache. */
  def makeEvaluator: Evaluator =
    new Evaluator(
      vfqn => bindingCache.getOrElse(vfqn, None),
      state.nameLevels
    )

  /** Evaluate an ORE expression using the current state and binding cache. Must be called after all relevant bindings
    * have been cached via ensureBinding.
    */
  def evalExpr(env: Env, tm: OperatorResolvedExpression): SemValue =
    makeEvaluator.eval(env, tm)

  /** Check a term against a known expected type. */
  def check(
      tm: Sourced[OperatorResolvedExpression],
      expected: SemValue
  ): CompilerIO[Monomorphic3Expression] = {
    val forcedExpected = Evaluator.force(expected, state.unifier.metaStore)
    tm.value match {
      // FunctionLiteral with annotation checked against expected type
      case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramTypeStack), body) =>
        for {
          _          <- prefetchBindings(paramTypeStack.value.signature)
          paramType   = evalExpr(state.env, paramTypeStack.value.signature)
          newState    = state.bind(paramName.value, paramType)
          _           = state = newState
          bodyExpr   <- check(body, getReturnType(forcedExpected))
          paramGround = forceAndConst(paramType)
        } yield Monomorphic3Expression(
          forceAndConst(forcedExpected),
          Monomorphic3Expression.FunctionLiteral(paramName, paramGround, body.as(bodyExpr))
        )

      // FunctionLiteral without annotation checked against VPi — use domain from VPi
      case OperatorResolvedExpression.FunctionLiteral(paramName, None, body)                 =>
        forcedExpected match {
          case VPi(domain, codomain) =>
            val newState = state.bind(paramName.value, domain)
            state = newState
            for {
              bodyExpr   <- check(body, codomain(domain))
              paramGround = forceAndConst(domain)
            } yield Monomorphic3Expression(
              forceAndConst(forcedExpected),
              Monomorphic3Expression.FunctionLiteral(paramName, paramGround, body.as(bodyExpr))
            )
          case _                     =>
            for {
              (expr, inferred) <- infer(tm)
              _                 = state.unifier.unify(inferred, expected, tm.as("Type mismatch."))
            } yield expr
        }

      case _ =>
        for {
          (expr, inferred) <- infer(tm)
          instantiated      = instantiatePolymorphic(inferred)
          _                 = unifyWithContext(instantiated, expected, tm)
        } yield expr
    }
  }

  /** Peel off leading VLam closures by instantiating them with fresh metas. This handles implicit type arg
    * instantiation when a polymorphic value is checked against a concrete/meta expected type.
    */
  private def instantiatePolymorphic(sem: SemValue): SemValue = {
    val forced = Evaluator.force(sem, state.unifier.metaStore)
    forced match {
      case VLam(_, closure) =>
        val (metaId, freshStore) = state.unifier.metaStore.fresh
        state.unifier.metaStore = freshStore
        val freshMeta            = VMeta(metaId, Spine.SNil, VType)
        instantiatePolymorphic(closure(freshMeta))
      case other            => other
    }
  }

  /** Infer the type of a term. */
  def infer(
      tm: Sourced[OperatorResolvedExpression]
  ): CompilerIO[(Monomorphic3Expression, SemValue)] = tm.value match {
    case OperatorResolvedExpression.IntegerLiteral(value) =>
      val tpe = VConst(Evaluator.bigIntGroundType)
      (Monomorphic3Expression(Evaluator.bigIntGroundType, Monomorphic3Expression.IntegerLiteral(value)), tpe)
        .pure[CompilerIO]

    case OperatorResolvedExpression.StringLiteral(value) =>
      val tpe = VConst(Evaluator.stringGroundType)
      (Monomorphic3Expression(Evaluator.stringGroundType, Monomorphic3Expression.StringLiteral(value)), tpe)
        .pure[CompilerIO]

    case OperatorResolvedExpression.ParameterReference(name) =>
      state.nameLevels.get(name.value) match {
        case Some(level) =>
          val sem  = state.env.lookupByLevel(level)
          val expr = Monomorphic3Expression(
            forceAndConst(sem),
            Monomorphic3Expression.ParameterReference(name)
          )
          (expr, sem).pure[CompilerIO]
        case None        =>
          compilerError(tm.as("Name not defined.")) >> abort
      }

    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs) =>
      for {
        _      <- ensureBinding(vfqn.value)
        sigOpt <- fetchValueType(vfqn.value)
        result <- sigOpt match {
                    case Some(sig) =>
                      for {
                        _            <- typeArgs.traverse_(ta => prefetchBindings(ta.value))
                        resolvedVfqn <- tryResolveAbility(vfqn)
                      } yield {
                        // Apply explicit type args
                        val appliedSig = typeArgs.foldLeft(sig) { (s, typeArg) =>
                          Evaluator.applyValue(s, evalExpr(state.env, typeArg.value))
                        }
                        val expr       = Monomorphic3Expression(
                          forceAndConst(appliedSig),
                          Monomorphic3Expression.MonomorphicValueReference(resolvedVfqn, Seq.empty)
                        )
                        (expr, appliedSig)
                      }
                    case None      =>
                      compilerError(tm.as("Name not defined.")) >> abort
                  }
      } yield result

    case OperatorResolvedExpression.FunctionApplication(target, arg) =>
      for {
        (targetExpr, targetType) <- infer(target)
        result                   <- applyInferred(target, targetExpr, targetType, arg, tm)
      } yield result

    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramTypeStack), body) =>
      for {
        _                    <- prefetchBindings(paramTypeStack.value.signature)
        paramType             = evalExpr(state.env, paramTypeStack.value.signature)
        newState              = state.bind(paramName.value, paramType)
        _                     = state = newState
        (bodyExpr, bodyType) <- infer(body)
      } yield {
        val tpe = VPi(paramType, _ => bodyType)
        (
          Monomorphic3Expression(
            forceAndConst(tpe),
            Monomorphic3Expression.FunctionLiteral(paramName, forceAndConst(paramType), body.as(bodyExpr))
          ),
          tpe
        )
      }

    case OperatorResolvedExpression.FunctionLiteral(_, None, _) =>
      compilerError(tm.as("Cannot infer type of unannotated lambda.")) >> abort
  }

  /** Handle function application: infer target, then apply argument. */
  private def applyInferred(
      target: Sourced[OperatorResolvedExpression],
      targetExpr: Monomorphic3Expression,
      targetType: SemValue,
      arg: Sourced[OperatorResolvedExpression],
      whole: Sourced[OperatorResolvedExpression]
  ): CompilerIO[(Monomorphic3Expression, SemValue)] = {
    val forced = Evaluator.force(targetType, state.unifier.metaStore)
    forced match {
      case VPi(domain, codomain) =>
        for {
          argExpr <- check(arg, domain)
          argSem   = evalExpr(state.env, arg.value)
          retType  = codomain(argSem)
        } yield {
          val expr = Monomorphic3Expression(
            forceAndConst(retType),
            Monomorphic3Expression.FunctionApplication(target.as(targetExpr), arg.as(argExpr))
          )
          (expr, retType)
        }

      case VLam(_, closure) =>
        // Polytype at term level: instantiate with fresh meta, then recurse.
        // This handles implicit type arg instantiation for generic values like `id(42)`.
        val (metaId, freshStore) = state.unifier.metaStore.fresh
        state.unifier.metaStore = freshStore
        val freshMeta            = VMeta(metaId, Spine.SNil, VType)
        val instantiated         = closure(freshMeta)
        applyInferred(target, targetExpr, instantiated, arg, whole)

      case _ =>
        // Try to unify with a fresh function type
        val (domId, store1) = state.unifier.metaStore.fresh
        state.unifier.metaStore = store1
        val (codId, store2) = state.unifier.metaStore.fresh
        state.unifier.metaStore = store2
        val domain          = VMeta(domId, Spine.SNil, VType)
        val codomain        = VMeta(codId, Spine.SNil, VType)
        state.unifier.unify(forced, VPi(domain, _ => codomain), target.as("Not a function."))
        val forcedDomain    = Evaluator.force(domain, state.unifier.metaStore)
        for {
          argExpr <- check(arg, forcedDomain)
          retType  = Evaluator.force(codomain, state.unifier.metaStore)
        } yield {
          val expr = Monomorphic3Expression(
            forceAndConst(retType),
            Monomorphic3Expression.FunctionApplication(target.as(targetExpr), arg.as(argExpr))
          )
          (expr, retType)
        }
    }
  }

  /** Resolve an ability method reference to its concrete implementation using constraint information. When a
    * ValueReference has an Ability qualifier, the constraint parameter's current binding provides the type arguments
    * for looking up the AbilityImplementation fact.
    */
  private def tryResolveAbility(vfqn: Sourced[ValueFQN]): CompilerIO[Sourced[ValueFQN]] =
    vfqn.value.name.qualifier match {
      case CoreQualifier.Ability(abilityName) =>
        findConstraintParam(abilityName) match {
          case Some((_, constraintTypeArgs)) =>
            // The constraint's typeArgs already include the constrained parameter
            // (e.g., `A ~ Show` has typeArgs=[ParameterReference("A")])
            val abilityTypeArgs =
              constraintTypeArgs.map(arg => GroundValue.toEvalValue(forceAndConst(evalExpr(state.env, arg))))
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

  /** Pre-fetch all NativeBindings referenced by ValueReferences in an ORE. */
  private[check] def prefetchBindings(ore: OperatorResolvedExpression): CompilerIO[Unit] = ore match {
    case OperatorResolvedExpression.ValueReference(vfqn, typeArgs)      =>
      ensureBinding(vfqn.value).void >>
        typeArgs.traverse_(ta => prefetchBindings(ta.value))
    case OperatorResolvedExpression.FunctionApplication(target, arg)    =>
      prefetchBindings(target.value) >> prefetchBindings(arg.value)
    case OperatorResolvedExpression.FunctionLiteral(_, paramType, body) =>
      paramType.traverse_(pt => pt.value.levels.toSeq.traverse_(prefetchBindings)) >>
        prefetchBindings(body.value)
    case _                                                              => ().pure[CompilerIO]
  }

  private def unifyWithContext(
      inferred: SemValue,
      expected: SemValue,
      tm: Sourced[OperatorResolvedExpression]
  ): Unit =
    state.unifier.unify(inferred, expected, tm.as("Type mismatch."))

  private def getReturnType(funcType: SemValue): SemValue = funcType match {
    case VPi(_, codomain) => codomain(VNeutral(NeutralHead.VVar(state.env.level, "$ret"), Spine.SNil, VType))
    case other            => other
  }

  private[check] def forceAndConst(v: SemValue): GroundValue = {
    val forced = Evaluator.force(v, state.unifier.metaStore)
    forced match {
      case VConst(g)             => g
      case VType                 => GroundValue.Type
      case VPi(domain, codomain) =>
        val domGround = forceAndConst(domain)
        val codGround = forceAndConst(
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
