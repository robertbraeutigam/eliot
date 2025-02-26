package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.stm.*
import com.vanillasource.stm.STM.*
import com.vanillasource.util.MultiSet

private[engine] case class RunningFactEngineStatus[K](
    runningCount: STMVar[Int],
    waitingCount: STMVar[Int],
    waitingKeys: STMVar[MultiSet[K]],
    crash: STMVar[Option[Throwable]]
) {
  def wrapProcessingStart(logic: IO[Unit])(using stmRuntime: STMRuntime): IO[Unit] =
    runningCount.incCommit() >>
      logic
        .handleErrorWith(t => crash.set(Some(t)).commit)
        .flatTap(_ => runningCount.decCommit())
        .start
        .void

  def wrapLookup[T](key: K, logic: IO[T])(using stmRuntime: STMRuntime): IO[T] =
    (waitingCount.update(_ + 1) >> waitingKeys.update(_.added(key))).commit
      .bracket(_ => logic)(_ => (waitingCount.update(_ - 1) >> waitingKeys.update(_.removed(key))).commit)

  /** The engine is stalled if all running processors are in a lookup currently, _and_ none of the keys being looked up
    * is actually present.
    */
  def stalled(existingKeys: Set[K]): STM[Boolean] = for {
    rc          <- runningCount.get()
    wc          <- waitingCount.get()
    waitingKeys <- waitingKeys.get()
  } yield rc === wc && existingKeys.intersect(waitingKeys.keySet).isEmpty

  def waitForTermination()(using stmRuntime: STMRuntime): IO[Unit] = (for {
    terminated <- terminated()
    _          <- if terminated
                  then exitWithPotentialThrowable()
                  else retry()
  } yield ()).commit

  private def exitWithPotentialThrowable(): STM[Unit] = for {
    crashCause <- crash.get()
    _          <- crashCause match
                    case Some(t) => raiseError(t)
                    case None    => ().pure[STM]
  } yield ()

  private def terminated(): STM[Boolean] = for {
    rc <- runningCount.get()
  } yield rc === 0
}

object RunningFactEngineStatus {
  def initialStatus[K](using stmRuntime: STMRuntime): IO[RunningFactEngineStatus[K]] = for {
    runningCount <- createSTMVar(0).commit
    waitingCount <- createSTMVar(0).commit
    waitingKeys  <- createSTMVar(MultiSet.empty[K]).commit
    crashedCount <- createSTMVar(Option.empty).commit
  } yield RunningFactEngineStatus(runningCount, waitingCount, waitingKeys, crashedCount)
}
