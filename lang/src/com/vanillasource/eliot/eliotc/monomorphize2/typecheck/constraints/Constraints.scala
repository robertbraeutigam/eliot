package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints

import cats.Show
import cats.kernel.Monoid
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import Constraints.Constraint
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class Constraints(constraints: Seq[Constraint])

object Constraints {
  case class Constraint(
      left: ExpressionValue,
      right: Sourced[ExpressionValue],
      errorMessage: String
  )

  def empty: Constraints = Constraints(Seq.empty)

  def constraint(
      left: ExpressionValue,
      right: Sourced[ExpressionValue],
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
