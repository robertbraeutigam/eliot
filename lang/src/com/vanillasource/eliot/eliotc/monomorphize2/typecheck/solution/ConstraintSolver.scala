package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints.Constraint
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Solves unification constraints using Robinson's algorithm. Adapted from the deleted symbolic ConstraintSolver but
  * works with ExpressionValue instead of SymbolicType, and has no universal variables.
  */
object ConstraintSolver extends Logging {

  private case class SolverState(substitutions: Map[String, Sourced[ExpressionValue]] = Map.empty) {

    def resolveHead(expr: ExpressionValue): ExpressionValue =
      expr match {
        case ParameterReference(name) =>
          substitutions.get(name).map(s => resolveHead(s.value)).getOrElse(expr)
        case other                    => other
      }

    def resolveHeadSourced(sourced: Sourced[ExpressionValue]): Sourced[ExpressionValue] =
      sourced.value match {
        case ParameterReference(name) =>
          substitutions.get(name).map(resolveHeadSourced).getOrElse(sourced)
        case _                        => sourced
      }

    def containsVar(expr: ExpressionValue, varName: String): Boolean =
      expr match {
        case ParameterReference(name) if name == varName => true
        case ParameterReference(name)                    =>
          substitutions.get(name).exists(s => containsVar(s.value, varName))
        case FunctionApplication(target, arg)            =>
          containsVar(target.value, varName) || containsVar(arg.value, varName)
        case _                                           => false
      }

    def bind(varName: String, value: Sourced[ExpressionValue]): SolverState =
      copy(substitutions = substitutions + (varName -> value))

    def substitute(expr: ExpressionValue): ExpressionValue =
      ExpressionValue.fold(
        onConcrete = v => ConcreteValue(v),
        onNative = pt => NativeFunction(pt, _ => ConcreteValue(Value.Type)),
        onParamRef =
          (name) => substitutions.get(name).map(s => substitute(s.value)).getOrElse(ParameterReference(name)),
        onFunApp =
          (target, arg) => FunctionApplication(ExpressionValue.unsourced(target), ExpressionValue.unsourced(arg)),
        onFunLit = (name, pt, body) => throw IllegalStateException("FunctionLiteral should not appear in constraints")
      )(expr)
  }

  private type SolverIO[T] = StateT[CompilerIO, SolverState, T]

  def solve(constraints: Constraints): CompilerIO[Solution] =
    for {
      endState <- constraints.constraints.traverse(solveConstraint).runS(SolverState())
      solution <- extractSolution(endState)
    } yield solution

  private def solveConstraint(constraint: Constraint): SolverIO[Unit] =
    for {
      state        <- StateT.get[CompilerIO, SolverState]
      leftResolved  = state.resolveHead(constraint.left)
      rightResolved = state.resolveHeadSourced(constraint.right)
      _            <- unify(constraint.copy(left = leftResolved, right = rightResolved))
    } yield ()

  private def unify(constraint: Constraint): SolverIO[Unit] = {
    val left  = constraint.left
    val right = constraint.right.value

    (left, right) match {
      // Unification variable on left: bind (with occurs check)
      case (ParameterReference(name), _)                      =>
        for {
          state <- StateT.get[CompilerIO, SolverState]
          _     <- if (state.containsVar(right, name))
                     issueError(constraint, "Infinite type detected.")
                   else
                     StateT.modify[CompilerIO, SolverState](_.bind(name, constraint.right))
        } yield ()

      // Unification variable on right: bind (with occurs check)
      case (_, ParameterReference(name))                      =>
        for {
          state <- StateT.get[CompilerIO, SolverState]
          _     <- if (state.containsVar(left, name))
                     issueError(constraint, "Infinite type detected.")
                   else
                     StateT.modify[CompilerIO, SolverState](_.bind(name, constraint.right.as(left)))
        } yield ()

      // Both concrete values: check equality
      case (ConcreteValue(v1), ConcreteValue(v2)) if v1 == v2 =>
        StateT.pure(())

      case (ConcreteValue(_), ConcreteValue(_))                       =>
        issueError(constraint, constraint.errorMessage)

      // Function types: decompose into parameter and return type constraints
      case (FunctionType(p1, r1), FunctionType(p2, r2))               =>
        for {
          _ <- solveConstraint(Constraint(p1, constraint.right.as(p2), "Parameter type mismatch."))
          _ <- solveConstraint(Constraint(r1, constraint.right.as(r2), "Return type mismatch."))
        } yield ()

      // Generic FunctionApplication: structural comparison
      case (FunctionApplication(t1, a1), FunctionApplication(t2, a2)) =>
        for {
          _ <- solveConstraint(Constraint(t1.value, constraint.right.as(t2.value), "Type constructor mismatch."))
          _ <- solveConstraint(Constraint(a1.value, constraint.right.as(a2.value), "Type argument mismatch."))
        } yield ()

      // ConcreteValue with type args vs FunctionApplication: normalize and retry
      case (_: FunctionApplication, ConcreteValue(v @ Value.Structure(fields, Value.Type)))
          if fields.size > 1 && fields.contains("$typeName") =>
        unify(constraint.copy(right = constraint.right.as(ExpressionValue.fromValue(v))))

      case (ConcreteValue(v @ Value.Structure(fields, Value.Type)), _: FunctionApplication)
          if fields.size > 1 && fields.contains("$typeName") =>
        unify(constraint.copy(left = ExpressionValue.fromValue(v)))

      // NativeFunction: match parameter types
      case (NativeFunction(pt1, _), NativeFunction(pt2, _)) if pt1 == pt2 =>
        StateT.pure(())

      // Anything else is a type error
      case _                                                              =>
        debug[SolverIO](
          s"Constraint failed, expected ${constraint.left.show}, found: ${constraint.right.value.show}"
        ) >> issueError(constraint, constraint.errorMessage)
    }
  }

  private def issueError(constraint: Constraint, message: String): SolverIO[Unit] =
    StateT.liftF(
      compilerError(
        constraint.right.as(message),
        Seq(
          s"Expected: ${constraint.left.show}",
          s"Found:    ${constraint.right.value.show}"
        )
      )
    )

  /** Extract the final solution by fully substituting all unification vars and verifying they resolve to concrete
    * values.
    */
  private def extractSolution(state: SolverState): CompilerIO[Solution] = {
    val resolved    = state.substitutions.map { case (name, sourced) =>
      val fullyResolved = state.substitute(sourced.value)
      name -> fullyResolved
    }
    val concreteMap = resolved.flatMap { case (name, expr) =>
      ExpressionValue.concreteValueOf(expr).map(name -> _)
    }
    Solution(concreteMap).pure[CompilerIO]
  }
}
