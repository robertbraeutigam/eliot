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
    expr match {
      case UnificationVar(id, _)                  =>
        substitutions.get(id).map(substitute).getOrElse(expr)
      case ValueRef(vfqn, args)                   =>
        ValueRef(vfqn, args.map(substitute))
      case FunctionType(param, ret, source)       =>
        FunctionType(substitute(param), substitute(ret), source)
      case SymbolicApplication(target, arg, src)  =>
        SymbolicApplication(substitute(target), substitute(arg), src)
      case ParameterRef(_) | IntLiteral(_) | StringLiteral(_) | UniversalVar(_) =>
        expr
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
