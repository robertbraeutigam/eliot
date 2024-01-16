package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO
import com.vanillasource.stm.*
import com.vanillasource.stm.STM.*

private[engine] case class RunningFactEngineStatus(
    runningCount: STMVar[Int],
    waitingCount: STMVar[Int],
    crash: STMVar[Option[Throwable]]
) {
  def wrapProcessing(logic: IO[Unit])(using stmRuntime: STMRuntime): IO[Unit] =
    runningCount
      .update(_ + 1)
      .commit
      .bracket(_ => logic.handleErrorWith(t => crash.set(Some(t)).commit))(_ =>
        runningCount
          .update(_ - 1)
          .commit
      )

  def waitForTermination()(using stmRuntime: STMRuntime): IO[Unit] = ???
}

object RunningFactEngineStatus {
  def initialStatus(using stmRuntime: STMRuntime): IO[RunningFactEngineStatus] = for {
    runningCount <- createSTMVar(0).commit
    waitingCount <- createSTMVar(0).commit
    crashedCount <- createSTMVar(Option.empty).commit
  } yield RunningFactEngineStatus(runningCount, waitingCount, crashedCount)
}
