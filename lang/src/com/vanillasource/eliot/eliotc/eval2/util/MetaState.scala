package com.vanillasource.eliot.eliotc.eval2.util

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO

/** Metacontext for NbE evaluation. In Phase 1, this only tracks the set of top-level definitions currently being
  * evaluated, to detect and break recursive cycles.
  */
case class MetaState(
    inProgress: Set[ValueFQN] = Set.empty
)

object MetaState {

  /** The effect type for NbE evaluation: CompilerIO with MetaState threaded through. */
  type EvalIO[A] = StateT[CompilerIO, MetaState, A]

  def isInProgress(vfqn: ValueFQN): EvalIO[Boolean] =
    StateT.inspect[CompilerIO, MetaState, Boolean](_.inProgress.contains(vfqn))

  /** Run an action with the given vfqn marked as in-progress. On completion (or short-circuit), the vfqn is removed
    * from the in-progress set.
    */
  def withInProgress[A](vfqn: ValueFQN)(action: EvalIO[A]): EvalIO[A] =
    for {
      _ <- StateT.modify[CompilerIO, MetaState](s => s.copy(inProgress = s.inProgress + vfqn))
      a <- action
      _ <- StateT.modify[CompilerIO, MetaState](s => s.copy(inProgress = s.inProgress - vfqn))
    } yield a

  def liftCompilerIO[A](io: CompilerIO[A]): EvalIO[A] =
    StateT.liftF(io)
}
