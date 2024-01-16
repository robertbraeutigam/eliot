package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO
import com.vanillasource.stm.*
import com.vanillasource.stm.STM.*

private[engine] case class RunningFactEngineStatus(
    runningCount: STMVar[Int],
    waitingCount: STMVar[Int],
    crashedCount: STMVar[Int]
) {}

object RunningFactEngineStatus {
  def initialStatus(using stmRuntime: STMRuntime): IO[RunningFactEngineStatus] = for {
    runningCount <- createSTMVar(0).commit
    waitingCount <- createSTMVar(0).commit
    crashedCount <- createSTMVar(0).commit
  } yield RunningFactEngineStatus(runningCount, waitingCount, crashedCount)
}
