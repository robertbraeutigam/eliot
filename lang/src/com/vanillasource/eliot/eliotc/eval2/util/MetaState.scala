package com.vanillasource.eliot.eliotc.eval2.util

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO

/** State threaded through NbE evaluation. Tracks in-progress top-level definitions
  * to detect recursion.
  */
case class MetaState(
    inProgress: Set[ValueFQN] = Set.empty
)

object MetaState {
  type EvalIO[A] = StateT[CompilerIO, MetaState, A]

  def withInProgress[A](vfqn: ValueFQN)(action: EvalIO[A]): EvalIO[A] =
    for {
      _      <- StateT.modify[CompilerIO, MetaState](s => s.copy(inProgress = s.inProgress + vfqn))
      result <- action
      _      <- StateT.modify[CompilerIO, MetaState](s => s.copy(inProgress = s.inProgress - vfqn))
    } yield result

  def isInProgress(vfqn: ValueFQN): EvalIO[Boolean] =
    StateT.inspect[CompilerIO, MetaState, Boolean](_.inProgress.contains(vfqn))

  def lift[A](fa: CompilerIO[A]): EvalIO[A] = StateT.liftF(fa)
}
