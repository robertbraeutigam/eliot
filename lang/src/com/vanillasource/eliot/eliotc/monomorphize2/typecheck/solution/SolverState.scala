package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints.Constraint
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO

case class SolverState(
    pending: Seq[Constraint] = Seq.empty,
    bindings: Map[String, Value] = Map.empty
)

object SolverState {
  type SolverIO[T] = StateT[CompilerIO, SolverState, T]

  def bind(name: String, value: Value): SolverIO[Unit] =
    StateT.modify(s => s.copy(bindings = s.bindings + (name -> value)))

  def defer(constraint: Constraint): SolverIO[Unit] =
    StateT.modify(s => s.copy(pending = s.pending :+ constraint))

  def currentBindings: SolverIO[Map[String, Value]] =
    StateT.inspect(_.bindings)

  def takePending: SolverIO[Seq[Constraint]] =
    StateT(s => (s.copy(pending = Seq.empty), s.pending).pure[CompilerIO])
}
