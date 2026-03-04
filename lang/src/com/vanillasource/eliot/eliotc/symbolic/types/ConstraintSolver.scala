package com.vanillasource.eliot.eliotc.symbolic.types

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicUnification.Constraint
import com.vanillasource.eliot.eliotc.symbolic.types.UnificationState.UnificationCompilerIO

/** Solves a set of unification constraints using Robinson's algorithm. Keeps constraint solving separate from
  * constraint accumulation (SymbolicUnification).
  */
object ConstraintSolver {

  def solve(
      constraints: SymbolicUnification,
      universalVars: Set[String]
  ): CompilerIO[UnificationState] =
    constraints.constraints
      .traverse(solveConstraint(universalVars))
      .runEmptyS

  private def solveConstraint(universalVars: Set[String])(constraint: Constraint): UnificationCompilerIO[Unit] =
    for {
      state        <- StateT.get[CompilerIO, UnificationState]
      leftResolved  = state.substitute(constraint.left)
      rightResolved = state.substitute(constraint.right.value)
      _            <- unify(universalVars)(constraint.copy(left = leftResolved, right = constraint.right.as(rightResolved)))
    } yield ()

  private def unify(universalVars: Set[String])(constraint: Constraint): UnificationCompilerIO[Unit] = {
    val left  = constraint.left
    val right = constraint.right.value

    def isUnificationVar(name: String): Boolean = !universalVars.contains(name)
    def isUniversalVar(name: String): Boolean   = universalVars.contains(name)

    (left, right) match {
      // Unification variable on left: bind it (if not recursive)
      case (ParameterReference(name, _), _) if isUnificationVar(name) && !isRecursiveCheck(name, right) =>
        StateT.modify[CompilerIO, UnificationState](_.bind(name, right))

      // Unification variable on right: bind it (if not recursive)
      case (_, ParameterReference(name, _)) if isUnificationVar(name) && !isRecursiveCheck(name, left)  =>
        StateT.modify[CompilerIO, UnificationState](_.bind(name, left))

      // Recursion detected
      case (ParameterReference(name, _), _) if isUnificationVar(name)                                   =>
        issueError(constraint, "Infinite type detected.")

      case (_, ParameterReference(name, _)) if isUnificationVar(name)                               =>
        issueError(constraint, "Infinite type detected.")

      // Universal variables: must match exactly
      case (ParameterReference(n1, _), ParameterReference(n2, _)) if isUniversalVar(n1) && n1 == n2 =>
        StateT.pure(())

      case (ParameterReference(n1, _), _) if isUniversalVar(n1) =>
        issueError(constraint, constraint.errorMessage)

      case (_, ParameterReference(n2, _)) if isUniversalVar(n2) =>
        issueError(constraint, constraint.errorMessage)

      // Concrete values: must be equal
      case (ConcreteValue(v1), ConcreteValue(v2)) if v1 == v2   =>
        StateT.pure(())

      case (ConcreteValue(_), ConcreteValue(_))                       =>
        issueError(constraint, constraint.errorMessage)

      // Function types (A -> B): unify parameter and return types separately with specific error messages
      // Note: this is just a special case of "Function Application" just below to issue special message
      case (FunctionType(p1, r1), FunctionType(p2, r2))               =>
        for {
          _ <-
            unify(universalVars)(
              Constraint(p1, constraint.right.as(p2), "Parameter type mismatch.")
            )
          _ <- unify(universalVars)(
                 Constraint(r1, constraint.right.as(r2), "Return type mismatch.")
               )
        } yield ()

      // Function applications: structural comparison
      case (FunctionApplication(t1, a1), FunctionApplication(t2, a2)) =>
        for {
          _ <- unify(universalVars)(
                 Constraint(t1.value, constraint.right.as(t2.value), "Type constructor mismatch.")
               )
          _ <- unify(universalVars)(
                 Constraint(a1.value, a2.withFallback(constraint.right), "Type argument mismatch.")
               )
        } yield ()

      // Anything else is a type error
      // Note: we intentionally don't handle function literals (it means nothing now)
      // Note: we don't handle native functions either (skipped before it gets here)
      case _                                                          =>
        issueError(constraint, constraint.errorMessage)
    }
  }

  private def isRecursiveCheck(varName: String, expr: ExpressionValue): Boolean =
    ExpressionValue.containsVar(expr, varName)

  private def issueError(constraint: Constraint, message: String): UnificationCompilerIO[Unit] =
    StateT.liftF(
      compilerError(
        constraint.right.as(message),
        Seq(
          s"Expected: ${expressionValueUserDisplay.show(constraint.left)}",
          s"Found:    ${expressionValueUserDisplay.show(constraint.right.value)}"
        )
      )
    )
}
