package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints.Constraint
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Solves constraints by propagation and evaluation. Binds unification variables to concrete Values, substitutes
  * bindings into expressions, evaluates via Evaluator.reduce, and iterates until all constraints are resolved.
  */
object ConstraintSolver extends Logging {

  def solve(constraints: Constraints): CompilerIO[Solution] =
    propagate(constraints.constraints, Map.empty)

  private def propagate(pending: Seq[Constraint], bindings: Map[String, Value]): CompilerIO[Solution] =
    for {
      result                     <- pending.foldLeftM[CompilerIO, (Seq[Constraint], Map[String, Value])]((Seq.empty, bindings)) {
                                      case ((deferred, currentBindings), constraint) =>
                                        processConstraint(constraint, currentBindings).map {
                                          case (None, newBindings)            => (deferred, currentBindings ++ newBindings)
                                          case (Some(remaining), newBindings) => (deferred :+ remaining, currentBindings ++ newBindings)
                                        }
                                    }
      (stillPending, newBindings) = result
      solution                   <- if (stillPending.isEmpty)
                                      Solution(newBindings).pure[CompilerIO]
                                    else if (newBindings.size > bindings.size)
                                      propagate(stillPending, newBindings)
                                    else
                                      reportUnresolved(stillPending) >> Solution(newBindings).pure[CompilerIO]
    } yield solution

  /** Process a single constraint. Returns (None, newBindings) if fully resolved, or (Some(constraint), newBindings) if
    * deferred.
    */
  private def processConstraint(
      constraint: Constraint,
      bindings: Map[String, Value]
  ): CompilerIO[(Option[Constraint], Map[String, Value])] =
    for {
      leftReduced  <- substituteAndReduce(constraint.left, bindings)
      rightReduced <- substituteAndReduce(constraint.right.value, bindings)
      result       <- (leftReduced, rightReduced) match {
                        // Both fully evaluated: check equality
                        case (ConcreteValue(v1), ConcreteValue(v2)) if v1 == v2 =>
                          (None, Map.empty[String, Value]).pure[CompilerIO]
                        case (ConcreteValue(_), ConcreteValue(_))               =>
                          issueError(constraint, leftReduced, rightReduced) >>
                            (None, Map.empty[String, Value]).pure[CompilerIO]

                        // Unification variable vs concrete value: bind
                        case (ParameterReference(name), ConcreteValue(v))       =>
                          (None, Map(name -> v)).pure[CompilerIO]
                        case (ConcreteValue(v), ParameterReference(name))       =>
                          (None, Map(name -> v)).pure[CompilerIO]

                        // Both unification variables: defer
                        case (ParameterReference(_), ParameterReference(_))     =>
                          (Some(constraint), Map.empty[String, Value]).pure[CompilerIO]

                        // One side concrete, other has free vars: try matchTypes to extract bindings
                        case (_, ConcreteValue(_))                              =>
                          extractBindingsOrDefer(constraint, leftReduced, rightReduced)
                        case (ConcreteValue(_), _)                              =>
                          extractBindingsOrDefer(constraint, rightReduced, leftReduced)

                        // Both sides have free vars: defer
                        case _                                                  =>
                          (Some(constraint), Map.empty[String, Value]).pure[CompilerIO]
                      }
    } yield result

  /** Try to extract bindings using matchTypes. If bindings found, return them. Otherwise defer. */
  private def extractBindingsOrDefer(
      constraint: Constraint,
      withVars: ExpressionValue,
      concrete: ExpressionValue
  ): CompilerIO[(Option[Constraint], Map[String, Value])] = {
    val matched      = ExpressionValue.matchTypes(withVars, concrete)
    val valueMatches = matched.flatMap { case (name, expr) =>
      ExpressionValue.concreteValueOf(expr).map(name -> _)
    }
    if (valueMatches.nonEmpty)
      (None, valueMatches).pure[CompilerIO]
    else
      (Some(constraint), Map.empty[String, Value]).pure[CompilerIO]
  }

  /** Substitute all known bindings into an expression and reduce it. */
  private def substituteAndReduce(expr: ExpressionValue, bindings: Map[String, Value]): CompilerIO[ExpressionValue] = {
    val substituted = bindings.foldLeft(expr) { case (e, (name, value)) =>
      ExpressionValue.substitute(e, name, ConcreteValue(value))
    }
    Evaluator.reduce(substituted, ExpressionValue.unsourced(substituted))
  }

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
