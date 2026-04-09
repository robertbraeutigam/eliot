package com.vanillasource.eliot.eliotc.eval2.util

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval2.fact.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

/** Information about a metavariable. */
case class MetaInfo(
    name: String,
    expectedType: Sem,
    solution: Option[Sem] = None
)

/** The metacontext tracks metavariables and the set of top-level definitions currently being evaluated (for recursion
  * guarding).
  */
case class MetaState(
    metas: Map[MetaId, MetaInfo] = Map.empty,
    nextId: Int = 0,
    inProgress: Set[ValueFQN] = Set.empty
)

object MetaState {

  /** The evaluation IO monad: CompilerIO with MetaState threaded through. */
  type EvalIO[A] = StateT[CompilerIO, MetaState, A]

  def liftCompilerIO[A](io: CompilerIO[A]): EvalIO[A] = StateT.liftF(io)

  /** Create a fresh metavariable and return its id. */
  def freshMeta(name: String, expectedType: Sem): EvalIO[Sem] =
    StateT { state =>
      val id       = MetaId(state.nextId)
      val info     = MetaInfo(name, expectedType)
      val newState = state.copy(
        metas = state.metas + (id -> info),
        nextId = state.nextId + 1
      )
      (newState, Sem.Neut(Head.Meta(id))).pure[CompilerIO]
    }

  /** Look up a metavariable's info. */
  def lookupMeta(id: MetaId): EvalIO[MetaInfo] =
    StateT.inspect[CompilerIO, MetaState, MetaInfo](_.metas(id))

  /** Solve a metavariable. Overwrites any previous solution. */
  def solveMeta(id: MetaId, sem: Sem): EvalIO[Unit] =
    StateT.modify[CompilerIO, MetaState] { state =>
      val info = state.metas(id)
      state.copy(metas = state.metas + (id -> info.copy(solution = Some(sem))))
    }

  /** Check if a top-level definition is currently being evaluated. */
  def isInProgress(vfqn: ValueFQN): EvalIO[Boolean] =
    StateT.inspect[CompilerIO, MetaState, Boolean](_.inProgress.contains(vfqn))

  /** Run an action with a top-level definition marked as in-progress, cleaning up after. */
  def withInProgress[A](vfqn: ValueFQN)(action: EvalIO[A]): EvalIO[A] =
    for {
      _ <- StateT.modify[CompilerIO, MetaState](s => s.copy(inProgress = s.inProgress + vfqn))
      a <- action
      _ <- StateT.modify[CompilerIO, MetaState](s => s.copy(inProgress = s.inProgress - vfqn))
    } yield a

  /** Force (chase) meta solutions to head normal form. If a meta is solved, apply its spine and re-force.
    */
  def force(s: Sem): EvalIO[Sem] = s match {
    case Sem.Neut(Head.Meta(id), spine) =>
      lookupMeta(id).flatMap { info =>
        info.solution match {
          case Some(solution) =>
            spine.foldLeftM(solution)(Evaluator2.apply).flatMap(force)
          case None           => s.pure[EvalIO]
        }
      }
    case other                          => other.pure[EvalIO]
  }
}
