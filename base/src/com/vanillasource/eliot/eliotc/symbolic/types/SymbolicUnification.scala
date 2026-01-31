package com.vanillasource.eliot.eliotc.symbolic.types

import cats.Show
import cats.data.StateT
import cats.kernel.Monoid
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicUnification.Constraint

/** Collects unification constraints between expression values. Variable sets (universal vs unification) are passed to
  * solve() from the caller's state.
  */
case class SymbolicUnification(constraints: Seq[Constraint]) {

  def solve(universalVars: Set[String], unificationVars: Set[String]): CompilerIO[UnificationState] =
    constraints
      .traverse(solveConstraint(universalVars, unificationVars))
      .runS(UnificationState())

  private def solveConstraint(universalVars: Set[String], unificationVars: Set[String])(
      constraint: Constraint
  ): StateT[CompilerIO, UnificationState, Unit] =
    for {
      state        <- StateT.get[CompilerIO, UnificationState]
      leftResolved  = state.substitute(constraint.left)
      rightResolved = state.substitute(constraint.right.value)
      _            <- unify(universalVars, unificationVars)(
                        constraint.copy(left = leftResolved, right = constraint.right.as(rightResolved))
                      )
    } yield ()

  private def unify(universalVars: Set[String], unificationVars: Set[String])(
      constraint: Constraint
  ): StateT[CompilerIO, UnificationState, Unit] = {
    val left  = constraint.left
    val right = constraint.right.value

    def isUnificationVar(name: String): Boolean = unificationVars.contains(name)
    def isUniversalVar(name: String): Boolean   = universalVars.contains(name)

    (left, right) match {
      // Unification variable on left: bind it (if occurs check passes)
      case (ParameterReference(name, _), _) if isUnificationVar(name) && !isOccursCheck(name, right) =>
        StateT.modify[CompilerIO, UnificationState](_.bind(name, right))

      // Unification variable on right: bind it
      case (_, ParameterReference(name, _)) if isUnificationVar(name) && !isOccursCheck(name, left)  =>
        StateT.modify[CompilerIO, UnificationState](_.bind(name, left))

      // Occurs check failure
      case (ParameterReference(name, _), _) if isUnificationVar(name)                                =>
        issueError(constraint, "Infinite type detected.")

      case (_, ParameterReference(name, _)) if isUnificationVar(name)                               =>
        issueError(constraint, "Infinite type detected.")

      // Universal variables: must match exactly
      case (ParameterReference(n1, _), ParameterReference(n2, _)) if isUniversalVar(n1) && n1 == n2 =>
        StateT.pure(())

      case (ParameterReference(n1, _), _) if isUniversalVar(n1) =>
        issueError(constraint, constraint.errorMessage)

      case (_, ParameterReference(n2, _)) if isUniversalVar(n2)               =>
        issueError(constraint, constraint.errorMessage)

      // Regular parameter references: must be same parameter
      case (ParameterReference(n1, _), ParameterReference(n2, _)) if n1 == n2 =>
        StateT.pure(())

      case (ParameterReference(_, _), _) | (_, ParameterReference(_, _)) =>
        issueError(constraint, constraint.errorMessage)

      // Concrete values: must be equal
      case (ConcreteValue(v1), ConcreteValue(v2)) if v1 == v2            =>
        StateT.pure(())

      case (ConcreteValue(_), ConcreteValue(_))                       =>
        issueError(constraint, constraint.errorMessage)

      // Function types: unify parameter and return types separately
      case (FunctionType(p1, r1), FunctionType(p2, r2))               =>
        for {
          _ <-
            unify(universalVars, unificationVars)(Constraint(p1, constraint.right.as(p2), "Parameter type mismatch."))
          _ <- unify(universalVars, unificationVars)(Constraint(r1, constraint.right.as(r2), "Return type mismatch."))
        } yield ()

      // Function literals: structural comparison (alpha-equivalence ignoring param name)
      case (FunctionLiteral(_, t1, b1), FunctionLiteral(_, t2, b2))   =>
        for {
          _ <- unify(universalVars, unificationVars)(
                 Constraint(ConcreteValue(t1), constraint.right.as(ConcreteValue(t2)), "Parameter type mismatch.")
               )
          _ <- unify(universalVars, unificationVars)(Constraint(b1, constraint.right.as(b2), "Return type mismatch."))
        } yield ()

      // Function applications: structural comparison
      case (FunctionApplication(t1, a1), FunctionApplication(t2, a2)) =>
        for {
          _ <- unify(universalVars, unificationVars)(
                 Constraint(t1, constraint.right.as(t2), "Type constructor mismatch.")
               )
          _ <- unify(universalVars, unificationVars)(
                 Constraint(a1, constraint.right.as(a2), "Type argument mismatch.")
               )
        } yield ()

      // Native functions should match by their type (rare in type checking)
      case (NativeFunction(t1, _), NativeFunction(t2, _)) if t1 == t2 =>
        StateT.pure(())

      // Anything else is a type error
      case _                                                          =>
        issueError(constraint, constraint.errorMessage)
    }
  }

  private def isOccursCheck(varName: String, expr: ExpressionValue): Boolean =
    ExpressionValue.containsVar(expr, varName)

  private def issueError(constraint: Constraint, message: String): StateT[CompilerIO, UnificationState, Unit] =
    StateT.liftF(
      compilerError(
        constraint.right.as(message),
        Seq(
          s"Expected: ${constraint.left.show}",
          s"Found:    ${constraint.right.value.show}"
        )
      )
    )
}

object SymbolicUnification {
  val empty: SymbolicUnification = SymbolicUnification(Seq.empty)

  case class Constraint(
      left: ExpressionValue,
      right: Sourced[ExpressionValue],
      errorMessage: String
  )

  def constraint(
      left: ExpressionValue,
      right: Sourced[ExpressionValue],
      errorMessage: String
  ): SymbolicUnification =
    SymbolicUnification(Seq(Constraint(left, right, errorMessage)))

  given Monoid[SymbolicUnification] = new Monoid[SymbolicUnification] {
    override def empty: SymbolicUnification = SymbolicUnification.empty

    override def combine(x: SymbolicUnification, y: SymbolicUnification): SymbolicUnification =
      SymbolicUnification(x.constraints ++ y.constraints)
  }

  given Show[SymbolicUnification] = (unification: SymbolicUnification) =>
    unification.constraints
      .map(c => s"${c.left.show} := ${c.right.value.show}")
      .mkString(" ∧ ")
}
