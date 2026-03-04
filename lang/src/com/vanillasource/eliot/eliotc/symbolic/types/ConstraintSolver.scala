package com.vanillasource.eliot.eliotc.symbolic.types

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.Qualifier
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.processor.TypeExpressionEvaluator
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicUnification.Constraint

/** Solves a set of unification constraints using Robinson's algorithm. Keeps constraint solving separate from
  * constraint accumulation (SymbolicUnification).
  */
object ConstraintSolver {

  def solve(
      constraints: SymbolicUnification,
      universalVars: Set[String],
      unificationVars: Set[String]
  ): CompilerIO[UnificationState] =
    constraints.constraints
      .traverse(solveConstraint(universalVars, unificationVars))
      .runS(UnificationState())

  private def solveConstraint(
      universalVars: Set[String],
      unificationVars: Set[String]
  )(
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

  private def unify(
      universalVars: Set[String],
      unificationVars: Set[String]
  )(
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

      // Non-constructor FunctionApplication containing universal vars: try to resolve the return type
      // from the function's type signature. If the return type can be determined, continue unification
      // with it. Otherwise, defer to monomorphization.
      // Constructor applications (Qualifier.Type, e.g. List(I)) are compared structurally.
      // When the other side also contains universal vars, the resolved return type is at the type
      // level (e.g. BigInteger) while the other side is at the value level (e.g. I: BigInteger).
      // In that case, defer to monomorphization instead of comparing across levels.
      case (fa @ FunctionApplication(_, _), _)
          if !isConstructorApplication(fa) && containsUniversalVar(left, isUniversalVar) =>
        resolveNonConstructorReturn(fa).flatMap {
          case Some(returnType) if !containsUniversalVar(right, isUniversalVar) =>
            unify(universalVars, unificationVars)(constraint.copy(left = returnType))
          case _                                                                => StateT.pure(())
        }

      case (_, fa @ FunctionApplication(_, _))
          if !isConstructorApplication(fa) && containsUniversalVar(right, isUniversalVar) =>
        resolveNonConstructorReturn(fa).flatMap {
          case Some(returnType) if !containsUniversalVar(left, isUniversalVar) =>
            unify(universalVars, unificationVars)(
              constraint.copy(right = constraint.right.as(returnType))
            )
          case _                                                               => StateT.pure(())
        }

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

      // Function types (A -> B): unify parameter and return types separately with specific error messages
      case (FunctionType(p1, r1), FunctionType(p2, r2))               =>
        for {
          _ <-
            unify(universalVars, unificationVars)(
              Constraint(p1, constraint.right.as(p2), "Parameter type mismatch.")
            )
          _ <- unify(universalVars, unificationVars)(
                 Constraint(r1, constraint.right.as(r2), "Return type mismatch.")
               )
        } yield ()

      // Function literals: structural comparison (alpha-equivalence ignoring param name)
      case (FunctionLiteral(_, t1, b1), FunctionLiteral(_, t2, b2))   =>
        for {
          _ <- unify(universalVars, unificationVars)(
                 Constraint(ConcreteValue(t1), constraint.right.as(ConcreteValue(t2)), "Parameter type mismatch.")
               )
          _ <- unify(universalVars, unificationVars)(
                 Constraint(b1.value, constraint.right.as(b2.value), "Return type mismatch.")
               )
        } yield ()

      // Function applications: structural comparison
      case (FunctionApplication(t1, a1), FunctionApplication(t2, a2)) =>
        for {
          _ <- unify(universalVars, unificationVars)(
                 Constraint(t1.value, constraint.right.as(t2.value), "Type constructor mismatch.")
               )
          _ <- unify(universalVars, unificationVars)(
                 Constraint(a1.value, a2.withFallback(constraint.right), "Type argument mismatch.")
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

  private def containsUniversalVar(expr: ExpressionValue, isUniversalVar: String => Boolean): Boolean =
    ExpressionValue.fold[Boolean](_ => false, _ => false, (name, _) => isUniversalVar(name), _ || _, (_, _, b) => b)(
      expr
    )

  private def isConstructorApplication(expr: ExpressionValue): Boolean =
    ExpressionValue.stripLeadingFunctionApplications(expr) match {
      case ConcreteValue(Value.Structure(fields, _)) =>
        fields.get("$typeName").exists {
          case Value.Direct(vfqn: ValueFQN, _) => vfqn.name.qualifier == Qualifier.Type
          case _                                => false
        }
      case ParameterReference(_, _)                  => true
      case _                                         => false
    }

  /** Try to resolve the return type of a non-constructor FunctionApplication by looking up the base function's type
    * signature from the fact system. Returns None if the return type cannot be determined (e.g., no FQN extractable, no
    * OperatorResolvedValue available, or insufficient FunctionType layers).
    */
  private def resolveNonConstructorReturn(
      fa: ExpressionValue
  ): StateT[CompilerIO, UnificationState, Option[ExpressionValue]] = {
    val (base, argCount) = extractBaseAndArgCount(fa)
    extractFQN(base) match {
      case Some(vfqn) =>
        StateT.liftF(
          getFact(OperatorResolvedValue.Key(vfqn)).flatMap {
            case Some(resolved) =>
              TypeExpressionEvaluator
                .processStackForDeclaration(resolved.typeStack)
                .runA(TypeCheckState())
                .map { case (signatureType, _) =>
                  val stripped = ExpressionValue.stripUniversalTypeIntros(signatureType)
                  stripFunctionTypes(stripped, argCount)
                }
            case None           => Option.empty[ExpressionValue].pure[CompilerIO]
          }
        )
      case None       => StateT.pure(Option.empty[ExpressionValue])
    }
  }

  private def extractBaseAndArgCount(expr: ExpressionValue): (ExpressionValue, Int) =
    expr match {
      case FunctionApplication(target, _) =>
        val (base, count) = extractBaseAndArgCount(target.value)
        (base, count + 1)
      case other                          => (other, 0)
    }

  private def extractFQN(expr: ExpressionValue): Option[ValueFQN] =
    expr match {
      case ConcreteValue(Value.Structure(fields, _)) =>
        fields.get("$typeName").collect { case Value.Direct(vfqn: ValueFQN, _) => vfqn }
      case _                                         => None
    }

  @scala.annotation.tailrec
  private def stripFunctionTypes(expr: ExpressionValue, n: Int): Option[ExpressionValue] =
    if (n <= 0) Some(expr)
    else
      expr match {
        case FunctionType(_, returnType) => stripFunctionTypes(returnType, n - 1)
        case _                           => None
      }

  private def issueError(constraint: Constraint, message: String): StateT[CompilerIO, UnificationState, Unit] =
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
