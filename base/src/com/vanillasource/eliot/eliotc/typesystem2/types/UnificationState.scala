package com.vanillasource.eliot.eliotc.typesystem2.types

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.typesystem2.fact.NormalizedExpression
import com.vanillasource.eliot.eliotc.typesystem2.fact.NormalizedExpression.*

/** Tracks the current state of unification. Maps unification variable IDs to their resolved expressions.
  */
case class UnificationState(substitutions: Map[String, NormalizedExpression] = Map.empty) {

  /** Apply all known substitutions to an expression recursively. */
  def substitute(expr: NormalizedExpression): NormalizedExpression =
    expr.transform {
      case uvar @ UnificationVar(id, _) => substitutions.get(id).map(substitute).getOrElse(uvar)
      case other                        => other
    }

  /** Bind a unification variable to an expression. */
  def bind(varId: String, expr: NormalizedExpression): UnificationState =
    copy(substitutions = substitutions + (varId -> expr))
}

object UnificationState {
  given Show[UnificationState] = state =>
    state.substitutions
      .map { case (id, expr) => s"?$id -> ${expr.show}" }
      .mkString(", ")
}
