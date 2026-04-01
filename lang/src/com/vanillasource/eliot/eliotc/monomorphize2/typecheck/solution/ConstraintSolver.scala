package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints.Constraint
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import SolverState.*

/** Solves constraints by propagation and evaluation. Binds unification variables to concrete Values, substitutes
  * bindings into expressions, evaluates via Evaluator.reduce, and iterates until all constraints are resolved.
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
                        currentBindings.map(Solution(_))
                      else if (anyResolved)
                        stillPending.traverse_(defer) >> propagate
                      else
                        StateT.liftF(reportUnresolved(stillPending)) >> currentBindings.map(Solution(_))
    } yield solution

  /** Returns true if the constraint was resolved, false if deferred. */
  private def processConstraint(constraint: Constraint): SolverIO[Boolean] =
    for {
      leftReduced  <- substituteAndReduce(constraint.left, constraint.right)
      rightReduced <- substituteAndReduce(constraint.right.value, constraint.right)
      _            <- debug[SolverIO](s"Checking ${leftReduced.show} vs. ${rightReduced.show}")
      resolved     <- (leftReduced, rightReduced) match {
                        case (ConcreteValue(v1), ConcreteValue(v2)) if v1 == v2 =>
                          true.pure[SolverIO]
                        case (ConcreteValue(_), ConcreteValue(_))               =>
                          StateT.liftF(issueError(constraint, leftReduced, rightReduced)).as(true)
                        case (ParameterReference(name), ConcreteValue(v))       =>
                          bind(name, v).as(true)
                        case (ConcreteValue(v), ParameterReference(name))       =>
                          bind(name, v).as(true)
                        case _                                                  =>
                          defer(constraint).as(false)
                      }
    } yield resolved

  /** Substitute all known bindings into an expression and reduce it. */
  // TODO: move this into ExpressionValue, there should not be non-reduced ExpressionValues
  private def substituteAndReduce(expr: ExpressionValue, source: Sourced[?]): SolverIO[ExpressionValue] =
    for {
      bindings   <- currentBindings
      substituted = bindings.foldLeft(expr) { case (e, (name, value)) =>
                      ExpressionValue.substitute(e, name, ConcreteValue(value))
                    }
      reduced    <- StateT.liftF(Evaluator.reduce(substituted, source))
    } yield reduced

  private def issueError(
      constraint: Constraint,
      leftReduced: ExpressionValue,
      rightReduced: ExpressionValue
  ): CompilerIO[Unit] = {
    debug[CompilerIO](s"Type error (${constraint.errorMessage}): ${leftReduced.show} vs. ${rightReduced.show}") >>
      compilerError(
        constraint.right.as(constraint.errorMessage),
        Seq(
          s"Expected: ${leftReduced.show}",
          s"Found:    ${rightReduced.show}"
        )
      )
  }

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
