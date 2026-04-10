package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.ConcreteValue
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.{Constraints, ShortUniqueIdentifiers}
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.normalize.OreNormalizer
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints.Constraint
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import SolverState.*

/** Solves ORE constraints by iteratively substituting current bindings, normalizing both sides to WHNF via
  * [[OreNormalizer]], decomposing constraints with matching structural shape (function applications and value
  * references), and binding unification variables to (possibly partial) ORE terms. Falls back to evaluation via
  * [[Evaluator]] when neither decomposition nor binding applies.
  */
object ConstraintSolver extends Logging {

  def solve(constraints: Constraints, shortIds: ShortUniqueIdentifiers): CompilerIO[Solution] =
    propagate.runA(SolverState(pending = constraints.constraints, shortIds = shortIds))

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
      leftSub   = constraint.left.map(l => OreNormalizer.normalize(substituteAll(l, bindings)))
      rightSub  = constraint.right.map(r => OreNormalizer.normalize(substituteAll(r, bindings)))
      _        <- debug[SolverIO](s"Checking ${leftSub.value.show} vs. ${rightSub.value.show}")
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
    (constraint.left.value, constraint.right.value) match {
      // Trivially satisfied: same unification variable on both sides
      case (ParameterReference(ln), ParameterReference(rn)) if ln.value == rn.value =>
        true.pure[SolverIO]

      // Instantiation: top-level FunctionLiteral on the left (an unapplied polytype, e.g. the
      // signature of a referenced generic value). Specialize it by substituting the bound
      // parameter with a fresh unification variable. This is HM-style polytype instantiation
      // and lets the resulting structure unify with whatever is on the right.
      case (FunctionLiteral(paramName, _, body), _)                                 =>
        for {
          fresh   <- generateUnificationVar
          freshRef = ParameterReference(paramName.as(fresh))
          reduced  = OperatorResolvedExpression.substitute(body.value, paramName.value, freshRef)
          _       <- enqueue(Seq(Constraint(constraint.left.as(reduced), constraint.right, constraint.errorMessage)))
        } yield true

      // Instantiation: top-level FunctionLiteral on the right.
      // Re-source the reduced body so that all inner Sourced positions point to the call site
      // (constraint.right) rather than the definition site. This ensures that structural
      // decomposition preserves call-site positions for error reporting.
      case (_, FunctionLiteral(paramName, _, body))                                 =>
        for {
          fresh   <- generateUnificationVar
          freshRef = ParameterReference(constraint.right.as(fresh))
          reduced  = OreNormalizer.reSource(
                       OperatorResolvedExpression.substitute(body.value, paramName.value, freshRef),
                       constraint.right
                     )
          _       <- enqueue(Seq(Constraint(constraint.left, constraint.right.as(reduced), constraint.errorMessage)))
        } yield true

      // Structural decomposition: both sides are function applications.
      // Target sub-constraints are re-sourced to the parent's position to prevent
      // definition-site positions from leaking through signature tree nodes.
      // Argument sub-constraints preserve inner positions so that errors point to
      // the specific body expression or call-site argument.
      case (FunctionApplication(lt, la), FunctionApplication(rt, ra))               =>
        val subs = Seq(
          Constraint(constraint.left.as(lt.value), constraint.right.as(rt.value), constraint.errorMessage),
          Constraint(la, ra, constraint.errorMessage)
        )
        enqueue(subs).as(true)

      // Structural decomposition: both sides are value references with the same name and arity
      case (ValueReference(ln, largs), ValueReference(rn, rargs))
          if ln.value == rn.value && largs.length == rargs.length =>
        val subs = largs.zip(rargs).map { case (la, ra) =>
          Constraint(constraint.left.as(la.value), constraint.right.as(ra.value), constraint.errorMessage)
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
        if (OperatorResolvedExpression.containsVar(constraint.left.value, name.value))
          StateT.liftF(issueOreError(constraint, Some("Infinite type."))).as(true)
        else
          bind(name.value, constraint.right.as(constraint.left.value)).as(true)

      // Fallback: evaluate both sides and compare as concrete values
      case _                                                                        =>
        evalAndCompare(constraint)
    }

  private def evalAndCompare(constraint: Constraint): SolverIO[Boolean] =
    for {
      leftReduced  <- StateT.liftF(Evaluator.evaluate(constraint.left))
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
      s"Type error (${constraint.errorMessage}): ${constraint.left.value.show} vs. ${constraint.right.value.show}"
    ) >>
      compilerError(
        constraint.right.as(constraint.errorMessage),
        Seq(
          s"Expected: ${constraint.left.value.show}",
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
        s"Constraint unresolved: ${constraint.left.value.show} vs ${constraint.right.value.show} (${constraint.errorMessage})"
      ) >>
        compilerError(
          constraint.right.as("Could not resolve type."),
          Seq(
            s"Left:  ${constraint.left.value.show}",
            s"Right: ${constraint.right.value.show}"
          )
        )
    }
}
