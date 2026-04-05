package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints

import cats.Show
import cats.kernel.Monoid
import cats.syntax.all.*
import Constraints.Constraint
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class Constraints(constraints: Seq[Constraint])

object Constraints {
  case class Constraint(
      left: OperatorResolvedExpression,
      right: Sourced[OperatorResolvedExpression],
      errorMessage: String
  )

  def empty: Constraints = Constraints(Seq.empty)

  def constraint(
      left: OperatorResolvedExpression,
      right: Sourced[OperatorResolvedExpression],
      errorMessage: String
  ): Constraints =
    Constraints(Seq(Constraint(left, right, errorMessage)))

  given Monoid[Constraints] = new Monoid[Constraints] {
    override def empty: Constraints = Constraints.empty

    override def combine(x: Constraints, y: Constraints): Constraints =
      Constraints(x.constraints ++ y.constraints)
  }

  given Show[Constraints] = (unification: Constraints) =>
    unification.constraints
      .map(c => s"${c.left.show} := ${c.right.value.show}")
      .mkString(" ∧ ")
}
