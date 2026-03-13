package com.vanillasource.eliot.eliotc.symbolic.types

import cats.Show
import cats.data.StateT
import cats.kernel.Monoid
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType.*

/** Tracks the current state of unification. Maps unification variable names to their resolved symbolic types with
  * source location provenance.
  */
case class UnificationState(substitutions: Map[String, Sourced[SymbolicType]] = Map.empty) {

  /** Apply all known substitutions to a symbolic type recursively. */
  def substitute(st: SymbolicType): SymbolicType =
    SymbolicType.transform(
      st,
      {
        case ref @ TypeVariable(name) => substitutions.get(name).map(s => substitute(s.value)).getOrElse(ref)
        case other                    => other
      }
    )

  /** Resolve only the top-level variable chain (does not recurse into sub-types). */
  def resolveHead(st: SymbolicType): SymbolicType =
    st match {
      case TypeVariable(name) => substitutions.get(name).map(s => resolveHead(s.value)).getOrElse(st)
      case other              => other
    }

  /** Resolve the top-level variable chain, updating the source from bindings. */
  def resolveHeadSourced(sourced: Sourced[SymbolicType]): Sourced[SymbolicType] =
    sourced.value match {
      case TypeVariable(name) => substitutions.get(name).map(resolveHeadSourced).getOrElse(sourced)
      case _                  => sourced
    }

  /** Check if a variable appears in a type, following bindings. */
  def containsVar(st: SymbolicType, varName: String): Boolean =
    st match {
      case TypeVariable(name) if name == varName => true
      case TypeVariable(name)                    =>
        substitutions.get(name).exists(s => containsVar(s.value, varName))
      case TypeApplication(target, arg)          =>
        containsVar(target.value, varName) || containsVar(arg.value, varName)
      case TypeLambda(_, body)                   => containsVar(body.value, varName)
      case _                                     => false
    }

  /** Bind a unification variable to a sourced symbolic type. */
  def bind(varName: String, st: Sourced[SymbolicType]): UnificationState =
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
      .map { case (name, st) => s"?$name -> ${symbolicTypeUserDisplay.show(st.value)}" }
      .mkString(", ")
}
