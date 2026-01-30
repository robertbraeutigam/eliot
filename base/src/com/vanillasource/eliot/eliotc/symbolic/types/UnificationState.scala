package com.vanillasource.eliot.eliotc.symbolic.types

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*

/** Tracks the current state of unification. Maps unification variable names to their resolved expressions.
  */
case class UnificationState(substitutions: Map[String, ExpressionValue] = Map.empty) {

  /** Apply all known substitutions to an expression recursively. */
  def substitute(expr: ExpressionValue): ExpressionValue =
    ExpressionValue.transform(expr, {
      case ref @ ParameterReference(name, _) => substitutions.get(name).map(substitute).getOrElse(ref)
      case other                             => other
    })

  /** Bind a unification variable to an expression. */
  def bind(varName: String, expr: ExpressionValue): UnificationState =
    copy(substitutions = substitutions + (varName -> expr))
}

object UnificationState {
  given Show[UnificationState] = state =>
    state.substitutions
      .map { case (name, expr) => s"?$name -> ${expr.show}" }
      .mkString(", ")
}
