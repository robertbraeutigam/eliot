package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.{
  ConcreteValue,
  FunctionApplication as EVFunctionApplication,
  FunctionLiteral as EVFunctionLiteral,
  NativeFunction,
  ParameterReference as EVParameterReference,
  fromValue
}
import com.vanillasource.eliot.eliotc.eval.fact.Types.typeFQN
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.{Constraints, ShortUniqueIdentifiers}
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints.Constraint
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import SolverState.*

/** Solves ORE constraints by working through the [[Evaluator]]: every constraint side is evaluated, and the resulting
  * [[ExpressionValue]] forms (which the evaluator has produced by actually running the code) drive unification. The
  * solver makes no structural assumptions about the original ORE — only the evaluator can interpret it. When the
  * evaluator reveals a function abstraction (a generic), the solver instantiates it on the fly with a fresh unification
  * variable.
  */
object ConstraintSolver extends Logging {

  def solve(constraints: Constraints, initialShortIds: ShortUniqueIdentifiers): CompilerIO[Solution] =
    propagate.runA(SolverState(pending = constraints.constraints, shortIds = initialShortIds))

  private def propagate: SolverIO[Solution] =
    for {
      _            <- debug[SolverIO]("Starting a solver cycle...")
      constraints  <- takePending
      anyResolved  <- constraints.foldLeftM(false) { (progress, constraint) =>
                        processConstraint(constraint).map(_ || progress)
                      }
      stillPending <- takePending
      solution     <- if (stillPending.isEmpty)
                        extractSolution
                      else if (anyResolved)
                        stillPending.traverse_(defer) >> propagate
                      else
                        StateT.liftF(reportUnresolved(stillPending)) >> extractSolution
    } yield solution

  /** Returns true if the constraint made progress (resolved, decomposed, bound, or errored), false if deferred. */
  private def processConstraint(constraint: Constraint): SolverIO[Boolean] =
    for {
      bindings  <- currentBindings
      leftSub    = substituteAll(constraint.left, bindings)
      rightSub   = constraint.right.map(substituteAll(_, bindings))
      _         <- debug[SolverIO](s"Checking ${leftSub.show} vs. ${rightSub.value.show}")
      // Hand the constraint over to the evaluator: only the evaluator can interpret what each side actually is.
      leftEval  <- StateT.liftF(Evaluator.evaluate(rightSub.as(leftSub)))
      rightEval <- StateT.liftF(Evaluator.evaluate(rightSub))
      _         <- debug[SolverIO](s"Reduced ${leftEval.show} vs. ${rightEval.show}")
      resolved  <- unify(leftEval, rightEval, Constraint(leftSub, rightSub, constraint.errorMessage))
    } yield resolved

  private def substituteAll(
      expr: OperatorResolvedExpression,
      bindings: Map[String, Sourced[OperatorResolvedExpression]]
  ): OperatorResolvedExpression =
    bindings.foldLeft(expr) { case (e, (name, term)) =>
      OperatorResolvedExpression.substitute(e, name, term.value)
    }

  /** Unify two evaluated expressions. Pattern-matches on [[ExpressionValue]] (the evaluator's output, which has known
    * shape regardless of the original ORE structure).
    */
  private def unify(
      left: ExpressionValue,
      right: ExpressionValue,
      constraint: Constraint
  ): SolverIO[Boolean] =
    (left, right) match {
      // Trivial: same parameter reference on both sides
      case (EVParameterReference(ln), EVParameterReference(rn)) if ln == rn =>
        true.pure[SolverIO]

      // Open a function abstraction by applying a fresh unification variable through the evaluator. This relies on
      // [[Evaluator.reduce]] — only it knows how to "run" the abstraction.
      case (_: EVFunctionLiteral, _)                                        =>
        for {
          freshVar <- generateFreshVar
          newLeft  <- StateT.liftF(applyFreshVar(left, freshVar, constraint.right))
          result   <- unify(newLeft, right, constraint)
        } yield result
      case (_, _: EVFunctionLiteral)                                        =>
        for {
          freshVar <- generateFreshVar
          newRight <- StateT.liftF(applyFreshVar(right, freshVar, constraint.right))
          result   <- unify(left, newRight, constraint)
        } yield result

      // A concrete data type structure on one side and an unreduced application chain on the other (still containing
      // unification vars) — expand the structure into the same application form via [[fromValue]] so the two sides
      // align.
      case (ConcreteValue(v @ Value.Structure(fields, Value.Type)), _: EVFunctionApplication)
          if fields.size > 1 && fields.contains("$typeName") =>
        unify(fromValue(v, constraint.right), right, constraint)
      case (_: EVFunctionApplication, ConcreteValue(v @ Value.Structure(fields, Value.Type)))
          if fields.size > 1 && fields.contains("$typeName") =>
        unify(left, fromValue(v, constraint.right), constraint)

      // Variable binding: one side is a unification variable
      case (EVParameterReference(name), _)                                  =>
        bindEvalToOre(name, right, constraint)
      case (_, EVParameterReference(name))                                  =>
        bindEvalToOre(name, left, constraint)

      // Structural decomposition: both sides are application chains
      case (EVFunctionApplication(lt, la), EVFunctionApplication(rt, ra))   =>
        for {
          r1 <- unify(lt.value, rt.value, constraint)
          r2 <- unify(la.value, ra.value, constraint)
        } yield r1 && r2

      // Both sides are the same native function (cached, so reference-equal). Trivially satisfied.
      case (NativeFunction(_, _), NativeFunction(_, _)) if left == right    =>
        true.pure[SolverIO]

      // Both sides reduce to identical concrete values
      case (ConcreteValue(v1), ConcreteValue(v2)) if v1 == v2               =>
        true.pure[SolverIO]

      // Concrete-vs-concrete mismatch — actual type error
      case (ConcreteValue(_), ConcreteValue(_))                             =>
        StateT.liftF(issueValueError(constraint, left, right)).as(true)

      // Anything else (e.g. one side still has free variables that aren't bindable as ORE) — defer for later cycles
      case _                                                                =>
        defer(constraint).as(false)
    }

  /** Apply a fresh unification variable to an ExpressionValue function abstraction by going through
    * [[Evaluator.reduce]]. The Evaluator handles the beta reduction; we don't pattern-match on the abstraction's body.
    */
  private def applyFreshVar(
      eval: ExpressionValue,
      freshVar: String,
      source: Sourced[?]
  ): CompilerIO[ExpressionValue] = {
    val applied = EVFunctionApplication(
      source.as(eval),
      source.as(EVParameterReference(freshVar))
    )
    Evaluator.reduce(applied, source)
  }

  /** Bind an ORE-level unification variable to an evaluated expression value. Converts the [[ExpressionValue]] back to
    * an ORE term so the existing ORE-level binding store can carry it.
    */
  private def bindEvalToOre(
      name: String,
      ev: ExpressionValue,
      constraint: Constraint
  ): SolverIO[Boolean] =
    expressionValueToOre(ev, constraint.right) match {
      case Some(ore) =>
        if (OperatorResolvedExpression.containsVar(ore, name))
          StateT.liftF(issueOreError(constraint, Some("Infinite type."))).as(true)
        else
          bind(name, constraint.right.as(ore)).as(true)
      case None      =>
        // Can't represent the value as ORE (e.g. NativeFunction or unreduced FL); defer and try again later.
        defer(constraint).as(false)
    }

  /** Best-effort conversion from [[ExpressionValue]] to [[OperatorResolvedExpression]] for use as a binding RHS. */
  private def expressionValueToOre(
      ev: ExpressionValue,
      source: Sourced[?]
  ): Option[OperatorResolvedExpression] =
    ev match {
      case EVParameterReference(name)               =>
        Some(OperatorResolvedExpression.ParameterReference(source.as(name)))
      case EVFunctionApplication(t, a)              =>
        for {
          tOre <- expressionValueToOre(t.value, source)
          aOre <- expressionValueToOre(a.value, source)
        } yield OperatorResolvedExpression.FunctionApplication(source.as(tOre), source.as(aOre))
      case ConcreteValue(v)                         =>
        valueToOre(v, source)
      case _: NativeFunction | _: EVFunctionLiteral =>
        // Cannot represent at ORE level; caller should defer the constraint.
        None
    }

  /** Convert a fully-evaluated [[Value]] back into an [[OperatorResolvedExpression]] form, expanding type structures
    * into application chains so they can participate in further structural unification.
    */
  private def valueToOre(
      v: Value,
      source: Sourced[?]
  ): Option[OperatorResolvedExpression] =
    v match {
      case Value.Type                      =>
        Some(OperatorResolvedExpression.ValueReference(source.as(typeFQN)))
      case Value.Direct(vfqn: ValueFQN, _) =>
        Some(OperatorResolvedExpression.ValueReference(source.as(vfqn)))
      case Value.Structure(fields, _)      =>
        fields.get("$typeName") match {
          case Some(Value.Direct(typeFqn: ValueFQN, _)) =>
            val typeArgs = fields.removed("$typeName").toSeq.sortBy(_._1).map(_._2)
            typeArgs.foldLeft[Option[OperatorResolvedExpression]](
              Some(OperatorResolvedExpression.ValueReference(source.as(typeFqn)))
            ) { case (accOpt, arg) =>
              for {
                acc    <- accOpt
                argOre <- valueToOre(arg, source)
              } yield OperatorResolvedExpression.FunctionApplication(source.as(acc), source.as(argOre))
            }
          case _                                        => None
        }
      case _                               => None
    }

  private def extractSolution: SolverIO[Solution] =
    for {
      bindings <- currentBindings
      resolved <- StateT.liftF(
                    bindings.toList.traverse { case (name, term) =>
                      Evaluator.evaluate(term).map { expr =>
                        ExpressionValue.concreteValueOf(expr).map(name -> _)
                      }
                    }
                  )
    } yield Solution(resolved.flatten.toMap)

  private def issueOreError(constraint: Constraint, hint: Option[String]): CompilerIO[Unit] =
    debug[CompilerIO](
      s"Type error (${constraint.errorMessage}): ${constraint.left.show} vs. ${constraint.right.value.show}"
    ) >>
      compilerError(
        constraint.right.as(constraint.errorMessage),
        Seq(
          s"Expected: ${constraint.left.show}",
          s"Found:    ${constraint.right.value.show}"
        ) ++ hint.toSeq
      )

  private def issueValueError(
      constraint: Constraint,
      leftReduced: ExpressionValue,
      rightReduced: ExpressionValue
  ): CompilerIO[Unit] =
    debug[CompilerIO](s"Type error (${constraint.errorMessage}): ${leftReduced.show} vs. ${rightReduced.show}") >>
      compilerError(
        constraint.right.as(constraint.errorMessage),
        Seq(
          s"Expected: ${leftReduced.show}",
          s"Found:    ${rightReduced.show}"
        )
      )

  private def reportUnresolved(constraints: Seq[Constraint]): CompilerIO[Unit] =
    constraints.traverse_ { constraint =>
      debug[CompilerIO](
        s"Constraint unresolved: ${constraint.left.show} vs ${constraint.right.value.show} (${constraint.errorMessage})"
      ) >>
        compilerError(
          constraint.right.as("Could not resolve type."),
          Seq(
            s"Left:  ${constraint.left.show}",
            s"Right: ${constraint.right.value.show}"
          )
        )
    }
}
