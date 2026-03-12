package com.vanillasource.eliot.eliotc.symbolic.types

import cats.Show
import cats.data.StateT
import cats.kernel.Monoid
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicType.*

/** Tracks the current state of unification. Maps unification variable names to their resolved symbolic types.
  */
case class UnificationState(substitutions: Map[String, SymbolicType] = Map.empty) {

  /** Apply all known substitutions to a symbolic type recursively. */
  def substitute(st: SymbolicType): SymbolicType =
    SymbolicType.transform(
      st,
      {
        case ref @ TypeVariable(name) => substitutions.get(name).map(substitute).getOrElse(ref)
        case other                    => other
      }
    )

  /** Bind a unification variable to a symbolic type. */
  def bind(varName: String, st: SymbolicType): UnificationState =
    copy(substitutions = substitutions + (varName -> st))
}

object UnificationState {
  type UnificationCompilerIO[T] = StateT[CompilerIO, UnificationState, T]

  given Monoid[UnificationState] with {
    override def empty: UnificationState = UnificationState()

    override def combine(x: UnificationState, y: UnificationState): UnificationState =
      UnificationState(x.substitutions ++ y.substitutions)
  }

  given Show[UnificationState] = state =>
    state.substitutions
      .map { case (name, st) => s"?$name -> ${symbolicTypeUserDisplay.show(st)}" }
      .mkString(", ")
}
