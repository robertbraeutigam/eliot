package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints.Constraint
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class SolverState(
    pending: Seq[Constraint] = Seq.empty,
    bindings: Map[String, Sourced[OperatorResolvedExpression]] = Map.empty
)

object SolverState {
  type SolverIO[T] = StateT[CompilerIO, SolverState, T]

  /** Insert a binding `name -> term`. Caller must ensure `term` is already fully substituted with the current bindings
    * (so we don't need to re-substitute existing bindings into it). To preserve the "fully substituted" invariant for
    * existing bindings, this also rewrites every existing binding's RHS by replacing free occurrences of `name` with
    * `term`.
    */
  def bind(name: String, term: Sourced[OperatorResolvedExpression]): SolverIO[Unit] =
    StateT.modify { s =>
      val updatedBindings = s.bindings.map { case (k, v) =>
        k -> v.map(OperatorResolvedExpression.substitute(_, name, term.value))
      }
      s.copy(bindings = updatedBindings + (name -> term))
    }

  def defer(constraint: Constraint): SolverIO[Unit] =
    StateT.modify(s => s.copy(pending = s.pending :+ constraint))

  def enqueue(constraints: Seq[Constraint]): SolverIO[Unit] =
    StateT.modify(s => s.copy(pending = s.pending ++ constraints))

  def currentBindings: SolverIO[Map[String, Sourced[OperatorResolvedExpression]]] =
    StateT.inspect(_.bindings)

  def takePending: SolverIO[Seq[Constraint]] =
    StateT(s => (s.copy(pending = Seq.empty), s.pending).pure[CompilerIO])
}
