package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.ConcreteValue
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints.Constraint
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import SolverState.*

/** Solves ORE constraints by iteratively substituting current bindings, decomposing constraints with matching
  * structural shape (function applications and value references), and binding unification variables to (possibly
  * partial) ORE terms. Falls back to evaluation via [[Evaluator]] when neither decomposition nor binding applies.
  */
object ConstraintSolver extends Logging {

  def solve(constraints: Constraints): CompilerIO[Solution] =
    propagate.runA(SolverState(pending = constraints.constraints))

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
      bindings <- currentBindings
      leftSub   = substituteAll(constraint.left, bindings)
      rightSub  = constraint.right.map(substituteAll(_, bindings))
      _        <- debug[SolverIO](s"Checking ${leftSub.show} vs. ${rightSub.value.show}")
      resolved <- tryResolve(Constraint(leftSub, rightSub, constraint.errorMessage))
    } yield resolved

  private def substituteAll(
      expr: OperatorResolvedExpression,
      bindings: Map[String, Sourced[OperatorResolvedExpression]]
  ): OperatorResolvedExpression =
    bindings.foldLeft(expr) { case (e, (name, term)) =>
      OperatorResolvedExpression.substitute(e, name, term.value)
    }

  private def tryResolve(constraint: Constraint): SolverIO[Boolean] =
    (constraint.left, constraint.right.value) match {
      // Trivially satisfied: same unification variable on both sides
      case (ParameterReference(ln), ParameterReference(rn)) if ln.value == rn.value =>
        true.pure[SolverIO]

      // Beta-reduction: left side is a beta-redex (function literal applied to an argument).
      // Reduce before any structural decomposition, otherwise we'd decompose pieces of a redex
      // against pieces of an unrelated function application.
      case (FunctionApplication(Sourced(_, _, FunctionLiteral(paramName, _, body)), arg), _) =>
        val reduced = OperatorResolvedExpression.substitute(body.value, paramName.value, arg.value)
        enqueue(Seq(Constraint(reduced, constraint.right, constraint.errorMessage))).as(true)

      // Beta-reduction: right side is a beta-redex.
      case (_, FunctionApplication(Sourced(_, _, FunctionLiteral(paramName, _, body)), arg)) =>
        val reduced = OperatorResolvedExpression.substitute(body.value, paramName.value, arg.value)
        enqueue(Seq(Constraint(constraint.left, constraint.right.as(reduced), constraint.errorMessage))).as(true)

      // Structural decomposition: both sides are function applications
      case (FunctionApplication(lt, la), FunctionApplication(rt, ra))               =>
        val subs = Seq(
          Constraint(lt.value, rt, constraint.errorMessage),
          Constraint(la.value, ra, constraint.errorMessage)
        )
        enqueue(subs).as(true)

      // Structural decomposition: both sides are value references with the same name and arity
      case (ValueReference(ln, largs), ValueReference(rn, rargs))
          if ln.value == rn.value && largs.length == rargs.length =>
        val subs = largs.zip(rargs).map { case (la, ra) =>
          Constraint(la.value, ra, constraint.errorMessage)
        }
        enqueue(subs).as(true)

      // Variable binding: left is a unification variable
      case (ParameterReference(name), _)                                            =>
        if (OperatorResolvedExpression.containsVar(constraint.right.value, name.value))
          StateT.liftF(issueOreError(constraint, Some("Infinite type."))).as(true)
        else
          bind(name.value, constraint.right).as(true)

      // Variable binding: right is a unification variable
      case (_, ParameterReference(name))                                            =>
        if (OperatorResolvedExpression.containsVar(constraint.left, name.value))
          StateT.liftF(issueOreError(constraint, Some("Infinite type."))).as(true)
        else
          bind(name.value, constraint.right.as(constraint.left)).as(true)

      // Fallback: evaluate both sides and compare as concrete values
      case _                                                                        =>
        evalAndCompare(constraint)
    }

  private def evalAndCompare(constraint: Constraint): SolverIO[Boolean] =
    for {
      leftReduced  <- StateT.liftF(Evaluator.evaluate(constraint.right.as(constraint.left)))
      rightReduced <- StateT.liftF(Evaluator.evaluate(constraint.right))
      _            <- debug[SolverIO](s"Reduced ${leftReduced.show} vs. ${rightReduced.show}")
      resolved     <- (leftReduced, rightReduced) match {
                        case (ConcreteValue(v1), ConcreteValue(v2)) if v1 == v2 =>
                          true.pure[SolverIO]
                        case (ConcreteValue(_), ConcreteValue(_))               =>
                          StateT.liftF(issueValueError(constraint, leftReduced, rightReduced)).as(true)
                        case _                                                  =>
                          defer(constraint).as(false)
                      }
    } yield resolved

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
