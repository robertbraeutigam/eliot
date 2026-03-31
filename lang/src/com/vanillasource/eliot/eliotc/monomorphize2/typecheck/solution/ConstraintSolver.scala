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
      constraints  <- getPending
      _            <- resetPending
      prevSize     <- currentBindings.map(_.size)
      _            <- constraints.traverse_(processConstraint)
      stillPending <- getPending
      newSize      <- currentBindings.map(_.size)
      solution     <- if (stillPending.isEmpty)
                        currentBindings.map(Solution(_))
                      else if (newSize > prevSize)
                        propagate
                      else
                        StateT.liftF(reportUnresolved(stillPending)) >> currentBindings.map(Solution(_))
    } yield solution

  private def processConstraint(constraint: Constraint): SolverIO[Unit] =
    for {
      leftReduced  <- substituteAndReduce(constraint.left)
      rightReduced <- substituteAndReduce(constraint.right.value)
      _            <- (leftReduced, rightReduced) match {
                        case (ConcreteValue(v1), ConcreteValue(v2)) if v1 == v2 =>
                          ().pure[SolverIO]
                        case (ConcreteValue(_), ConcreteValue(_))               =>
                          StateT.liftF(issueError(constraint, leftReduced, rightReduced))
                        case (ParameterReference(name), ConcreteValue(v))       =>
                          bind(name, v)
                        case (ConcreteValue(v), ParameterReference(name))       =>
                          bind(name, v)
                        case _                                                  =>
                          defer(constraint)
                      }
    } yield ()

  /** Substitute all known bindings into an expression and reduce it. */
  // TODO: move this into ExpressionValue, there should not be non-reduced ExpressionValues
  private def substituteAndReduce(expr: ExpressionValue): SolverIO[ExpressionValue] =
    for {
      bindings <- currentBindings
      substituted = bindings.foldLeft(expr) { case (e, (name, value)) =>
                      ExpressionValue.substitute(e, name, ConcreteValue(value))
                    }
      reduced  <- StateT.liftF(Evaluator.reduce(substituted, ExpressionValue.unsourced(substituted)))
    } yield reduced

  private def issueError(
      constraint: Constraint,
      leftReduced: ExpressionValue,
      rightReduced: ExpressionValue
  ): CompilerIO[Unit] =
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
