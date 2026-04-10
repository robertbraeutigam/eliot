package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints

import cats.Show
import cats.kernel.Monoid
import cats.syntax.all.*
import Constraints.Constraint
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class Constraints(constraints: Seq[Constraint])

object Constraints extends Logging {
  case class Constraint(
      left: Sourced[OperatorResolvedExpression],
      right: Sourced[OperatorResolvedExpression],
      errorMessage: String
  )

  def empty: Constraints = Constraints(Seq.empty)

  def constraint(
      left: Sourced[OperatorResolvedExpression],
      right: Sourced[OperatorResolvedExpression],
      errorMessage: String
  ): Constraints =
    Constraints(Seq(Constraint(left, right, errorMessage)))

  given Monoid[Constraints] = new Monoid[Constraints] {
    override def empty: Constraints = Constraints.empty

    override def combine(x: Constraints, y: Constraints): Constraints =
      Constraints(x.constraints ++ y.constraints)
  }

  def debugConstraints(constraints: Constraints): CompilerIO[Unit] =
    constraints.constraints.traverse_ { c =>
      for {
        debugString <- Sourced.displaySnippet(c.right)
        _           <- debug[CompilerIO](s"Constraint: ${c.left.value.show} := ${c.right.value.show}, at: $debugString")
      } yield ()
    }
}
