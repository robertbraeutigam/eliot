package com.vanillasource.eliot.eliotc.typesystem2.types

import cats.Show
import cats.data.StateT
import cats.kernel.Monoid
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.typesystem2.types.NormalizedExpression.*
import com.vanillasource.eliot.eliotc.typesystem2.types.SymbolicUnification.Constraint

/** Collects unification constraints between normalized expressions. Unlike the old TypeUnification which worked on
  * TypeReference, this works on NormalizedExpression (symbolic forms).
  */
case class SymbolicUnification(
    universalVars: Set[String],
    constraints: Seq[Constraint]
) {

  def solve(): CompilerIO[UnificationState] =
    constraints
      .traverse(solveConstraint)
      .runS(UnificationState())

  private def solveConstraint(constraint: Constraint): StateT[CompilerIO, UnificationState, Unit] =
    for {
      state         <- StateT.get[CompilerIO, UnificationState]
      leftResolved   = state.substitute(constraint.left)
      rightResolved  = state.substitute(constraint.right.value)
      _             <- unify(constraint.copy(left = leftResolved, right = constraint.right.as(rightResolved)))
    } yield ()

  private def unify(constraint: Constraint): StateT[CompilerIO, UnificationState, Unit] = {
    val left  = constraint.left
    val right = constraint.right.value

    (left, right) match {
      // Unification variable on left: bind it
      case (UnificationVar(id, _), _) if !isOccursCheck(id, right) =>
        StateT.modify[CompilerIO, UnificationState](_.bind(id, right))

      // Unification variable on right: bind it
      case (_, UnificationVar(id, _)) if !isOccursCheck(id, left) =>
        StateT.modify[CompilerIO, UnificationState](_.bind(id, left))

      // Occurs check failure
      case (UnificationVar(id, _), _) =>
        issueError(constraint, "Infinite type detected.")

      case (_, UnificationVar(id, _)) =>
        issueError(constraint, "Infinite type detected.")

      // Value references: must have same VFQN and recursively unify arguments
      case (ValueRef(vfqn1, args1), ValueRef(vfqn2, args2)) if vfqn1.value === vfqn2.value =>
        if (args1.length === args2.length) {
          (args1 zip args2).traverse_ { case (a1, a2) =>
            unify(Constraint(a1, constraint.right.as(a2), "Type argument mismatch."))
          }
        } else {
          issueError(constraint, "Different number of type arguments.")
        }

      case (ValueRef(_, _), ValueRef(_, _)) =>
        issueError(constraint, constraint.errorMessage)

      // Function types: unify param and return types
      case (FunctionType(p1, r1, _), FunctionType(p2, r2, _)) =>
        for {
          _ <- unify(Constraint(p1, constraint.right.as(p2), "Parameter type mismatch."))
          _ <- unify(Constraint(r1, constraint.right.as(r2), "Return type mismatch."))
        } yield ()

      // Universal variables: must match exactly
      case (UniversalVar(n1), UniversalVar(n2)) if n1.value === n2.value =>
        StateT.pure(())

      case (UniversalVar(n1), _) if universalVars.contains(n1.value) =>
        issueError(
          constraint,
          s"Expression with type ${right.show} cannot be assigned to universal type ${n1.value}."
        )

      case (_, UniversalVar(n2)) if universalVars.contains(n2.value) =>
        issueError(
          constraint,
          s"Expression with universal type ${n2.value} cannot be assigned to type ${left.show}."
        )

      // Parameter references: must be same parameter
      case (ParameterRef(n1), ParameterRef(n2)) if n1.value === n2.value =>
        StateT.pure(())

      case (ParameterRef(_), _) | (_, ParameterRef(_)) =>
        issueError(constraint, constraint.errorMessage)

      // Literals: must be equal
      case (IntLiteral(v1), IntLiteral(v2)) if v1.value === v2.value =>
        StateT.pure(())

      case (StringLiteral(v1), StringLiteral(v2)) if v1.value === v2.value =>
        StateT.pure(())

      // Symbolic applications: structural match
      case (SymbolicApplication(t1, a1, _), SymbolicApplication(t2, a2, _)) =>
        for {
          _ <- unify(Constraint(t1, constraint.right.as(t2), "Symbolic application target mismatch."))
          _ <- unify(Constraint(a1, constraint.right.as(a2), "Symbolic application argument mismatch."))
        } yield ()

      // Anything else is a type error
      case _ =>
        issueError(constraint, constraint.errorMessage)
    }
  }

  private def isOccursCheck(varId: String, expr: NormalizedExpression): Boolean =
    expr match {
      case UnificationVar(id, _)               => id === varId
      case ValueRef(_, args)                   => args.exists(isOccursCheck(varId, _))
      case FunctionType(param, ret, _)         => isOccursCheck(varId, param) || isOccursCheck(varId, ret)
      case SymbolicApplication(target, arg, _) => isOccursCheck(varId, target) || isOccursCheck(varId, arg)
      case _                                   => false
    }

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
  val empty: SymbolicUnification = SymbolicUnification(Set.empty, Seq.empty)

  case class Constraint(
      left: NormalizedExpression,
      right: Sourced[NormalizedExpression],
      errorMessage: String
  )

  def constraint(
      left: NormalizedExpression,
      right: Sourced[NormalizedExpression],
      errorMessage: String
  ): SymbolicUnification =
    SymbolicUnification(Set.empty, Seq(Constraint(left, right, errorMessage)))

  def universalVar(name: String): SymbolicUnification =
    SymbolicUnification(Set(name), Seq.empty)

  def universalVars(names: Set[String]): SymbolicUnification =
    SymbolicUnification(names, Seq.empty)

  given Monoid[SymbolicUnification] = new Monoid[SymbolicUnification] {
    override def empty: SymbolicUnification = SymbolicUnification.empty

    override def combine(x: SymbolicUnification, y: SymbolicUnification): SymbolicUnification =
      SymbolicUnification(x.universalVars ++ y.universalVars, x.constraints ++ y.constraints)
  }

  given Show[SymbolicUnification] = (unification: SymbolicUnification) =>
    (if (unification.universalVars.nonEmpty) unification.universalVars.map("∀" + _).mkString(", ") + ": "
     else "") +
      unification.constraints
        .map(c => s"${c.left.show} := ${c.right.value.show}")
        .mkString(" ∧ ")
}
