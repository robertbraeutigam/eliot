package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO
import cats.syntax.all._
import com.vanillasource.stm.*
import com.vanillasource.stm.STM.*

private[engine] case class RunningFactEngineStatus(
    runningCount: STMVar[Int],
    waitingCount: STMVar[Int],
    crash: STMVar[Option[Throwable]]
) {
  def wrapProcessingStart(logic: IO[Unit])(using stmRuntime: STMRuntime): IO[Unit] =
    runningCount.incCommit() >>
      logic
        .handleErrorWith(t => crash.set(Some(t)).commit)
        .flatTap(_ => runningCount.decCommit())
        .start
        .void

  def wrapLookup[T](logic: IO[T])(using stmRuntime: STMRuntime): IO[T] =
    waitingCount
      .incCommit()
      .bracket(_ => logic)(_ => waitingCount.decCommit())

  def stalled(): STM[Boolean] = for {
    rc <- runningCount.get()
    wc <- waitingCount.get()
  } yield rc === wc

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
  def initialStatus(using stmRuntime: STMRuntime): IO[RunningFactEngineStatus] = for {
    runningCount <- createSTMVar(0).commit
    waitingCount <- createSTMVar(0).commit
    crashedCount <- createSTMVar(Option.empty).commit
  } yield RunningFactEngineStatus(runningCount, waitingCount, crashedCount)
}
