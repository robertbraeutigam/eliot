package com.vanillasource.eliot.eliotc.symbolic.types

import cats.Show
import cats.kernel.Monoid
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicType.symbolicTypeUserDisplay
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicUnification.Constraint

/** Pure container for unification constraints between symbolic types. Constraints are accumulated during the
  * TypeGraphIO state pass and solved afterwards by ConstraintSolver.
  */
case class SymbolicUnification(constraints: Seq[Constraint])

object SymbolicUnification {
  val empty: SymbolicUnification = SymbolicUnification(Seq.empty)

  case class Constraint(
      left: SymbolicType,
      right: Sourced[SymbolicType],
      errorMessage: String
  )

  def constraint(
      left: SymbolicType,
      right: Sourced[SymbolicType],
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
      .map(c => s"${symbolicTypeUserDisplay.show(c.left)} := ${symbolicTypeUserDisplay.show(c.right.value)}")
      .mkString(" ∧ ")
}
