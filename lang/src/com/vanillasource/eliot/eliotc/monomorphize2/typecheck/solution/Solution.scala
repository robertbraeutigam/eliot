package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution

import cats.Show
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.ConcreteValue
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Value.valueUserDisplay

case class Solution(substitutions: Map[String, Value] = Map.empty) {

  /** Apply all substitutions to an ExpressionValue, replacing unification vars with their concrete values. */
  def resolveExpressionValue(expr: ExpressionValue): ExpressionValue =
    substitutions.foldLeft(expr) { case (e, (name, value)) =>
      ExpressionValue.substitute(e, name, ConcreteValue(value))
    }
}

object Solution {
  given Show[Solution] = solution =>
    solution.substitutions
      .map { case (name, value) => s"?$name -> ${valueUserDisplay.show(value)}" }
      .mkString(", ")
}
