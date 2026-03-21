package com.vanillasource.eliot.eliotc.symbolic.types

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.Qualifier
import com.vanillasource.eliot.eliotc.eval.fact.Types.{typeFQN, typeFQNType}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType.*
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicUnification.Constraint
import com.vanillasource.eliot.eliotc.symbolic.types.UnificationState.UnificationCompilerIO

/** Solves a set of unification constraints using Robinson's algorithm. Keeps constraint solving separate from
  * constraint accumulation (SymbolicUnification).
  *
  * Uses lazy head resolution: only the top-level variable is resolved before pattern matching, and sub-types are left
  * for recursive calls to resolve. This preserves source location provenance through variable binding chains.
  */
object ConstraintSolver extends Logging {

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
      leftResolved  = state.resolveHead(constraint.left)
      rightResolved = state.resolveHeadSourced(constraint.right)
      _            <- unify(universalVars, state)(constraint.copy(left = leftResolved, right = rightResolved))
    } yield ()

  private def unify(
      universalVars: Set[String],
      state: UnificationState
  )(constraint: Constraint): UnificationCompilerIO[Unit] = {
    val left  = constraint.left
    val right = constraint.right.value

    def isUnificationVar(name: String): Boolean = !universalVars.contains(name)
    def isUniversalVar(name: String): Boolean   = universalVars.contains(name)

    (left, right) match {
      // Unification variable on left: bind it (if not recursive)
      case (TypeVariable(name), _) if isUnificationVar(name) && !state.containsVar(right, name) =>
        StateT.modify[CompilerIO, UnificationState](_.bind(name, constraint.right))

      // Unification variable on right: bind it (if not recursive)
      case (_, TypeVariable(name)) if isUnificationVar(name) && !state.containsVar(left, name)  =>
        StateT.modify[CompilerIO, UnificationState](_.bind(name, constraint.right.as(left)))

      // Recursion detected
      case (TypeVariable(name), _) if isUnificationVar(name)                                    =>
        issueError(constraint, "Infinite type detected.")

      case (_, TypeVariable(name)) if isUnificationVar(name)                       =>
        issueError(constraint, "Infinite type detected.")

      // Universal variables: must match exactly
      case (TypeVariable(n1), TypeVariable(n2)) if isUniversalVar(n1) && n1 === n2 =>
        StateT.pure(())

      case (TypeVariable(n1), _) if isUniversalVar(n1) =>
        issueError(constraint, constraint.errorMessage)

      case (_, TypeVariable(n2)) if isUniversalVar(n2)                                        =>
        issueError(constraint, constraint.errorMessage)

      // Opaque function applications in type positions: defer to monomorphize
      case _ if isOpaqueApplication(left) || isOpaqueApplication(right)                       =>
        StateT.pure(())

      // At the kind level, fully-applied type constructor applications have kind Type
      // TODO: this seems fishy
      case (TypeReference(fqn), _: TypeApplication) if fqn === typeFQN || fqn === typeFQNType =>
        StateT.pure(())
      case (_: TypeApplication, TypeReference(fqn)) if fqn === typeFQN || fqn === typeFQNType =>
        StateT.pure(())

      // Type references: must refer to the same FQN
      case (TypeReference(fqn1), TypeReference(fqn2)) if fqn1 === fqn2                        =>
        StateT.pure(())

      // Type's runtime and type equals, so this needs special handling
      case (TypeReference(fqn1), TypeReference(fqn2))
          if (fqn1 === typeFQN || fqn1 === typeFQNType) && (fqn2 === typeFQN || fqn2 === typeFQNType) =>
        StateT.pure(())

      case (TypeReference(_), TypeReference(_))                                =>
        debug[UnificationCompilerIO](
          s"Constraint failed comparing type references, expected ${symbolicTypeUserDisplay
              .show(constraint.left)}, found: ${symbolicTypeUserDisplay.show(constraint.right.value)} "
        ) >> issueError(constraint, constraint.errorMessage)

      // Literal types: must be equal
      case (LiteralType(v1, t1), LiteralType(v2, t2)) if v1 == v2 && t1 === t2 =>
        StateT.pure(())

      case (LiteralType(_, _), LiteralType(_, _))             =>
        debug[UnificationCompilerIO](
          s"Constraint failed comparing literal types, expected ${symbolicTypeUserDisplay
              .show(constraint.left)}, found: ${symbolicTypeUserDisplay.show(constraint.right.value)} "
        ) >> issueError(constraint, constraint.errorMessage)

      // Function types (A -> B): unify parameter and return types separately with specific error messages
      // Note: this is just a special case of "Type Application" just below to issue special message
      case (FunctionType(p1, r1), FunctionType(p2, r2))       =>
        for {
          _ <-
            solveConstraint(universalVars)(
              Constraint(p1, constraint.right.as(p2), "Parameter type mismatch.")
            )
          _ <- solveConstraint(universalVars)(
                 Constraint(r1, constraint.right.as(r2), "Return type mismatch.")
               )
        } yield ()

      // Type applications: structural comparison
      case (TypeApplication(t1, a1), TypeApplication(t2, a2)) =>
        for {
          _ <- solveConstraint(universalVars)(
                 Constraint(t1.value, constraint.right.as(t2.value), "Type constructor mismatch.")
               )
          _ <- solveConstraint(universalVars)(
                 Constraint(a1.value, constraint.right.as(a2.value), "Type argument mismatch.")
               )
        } yield ()

      // Anything else is a type error
      case _                                                  =>
        debug[UnificationCompilerIO](s"Constraint failed in else branch, expected ${symbolicTypeUserDisplay
            .show(constraint.left)}, found: ${symbolicTypeUserDisplay.show(constraint.right.value)} ") >>
          issueError(constraint, constraint.errorMessage)
    }
  }

  private def isOpaqueApplication(st: SymbolicType): Boolean =
    st match {
      case _: TypeApplication =>
        SymbolicType.stripLeadingApplications(st) match {
          case TypeReference(vfqn) => vfqn.name.qualifier != Qualifier.Type && vfqn != typeFQN
          case _                   => false
        }
      case _                  => false
    }

  private def issueError(constraint: Constraint, message: String): UnificationCompilerIO[Unit] =
    StateT.liftF(
      compilerError(
        constraint.right.as(message),
        Seq(
          s"Expected: ${symbolicTypeUserDisplay.show(constraint.left)}",
          s"Found:    ${symbolicTypeUserDisplay.show(constraint.right.value)}"
        )
      )
    )
}
