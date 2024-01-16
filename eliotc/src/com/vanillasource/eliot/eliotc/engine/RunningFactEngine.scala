package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.stm.STM.*
import com.vanillasource.stm.{STMMap, STMRuntime}

case class RunningFactEngine[K, V] private (
    processors: Seq[FactProcessor[K, V]],
    status: RunningFactEngineStatus,
    facts: STMMap[K, V]
)(using
    stmRuntime: STMRuntime
) {
  def registerFact(key: K, value: V): IO[Unit] = for {
    oldValue <- facts.putIfAbsent(key, value).commit
    _        <- oldValue match
                  case Some(value) =>
                    IO.raiseError(new IllegalArgumentException(s"key $key was already present in the engine"))
                  case None        => startProcessorsFor(value)
  } yield ()

  def registerFacts(facts: Iterable[(K, V)]): IO[Unit] = facts.map(registerFact).toSeq.sequence_

  def getFact(k: K): IO[Option[V]] = ???

  private def startProcessorsFor(value: V): IO[Unit] =
    processors.map(p => status.wrapProcessing(p.process(value)(using this))).sequence_

  private[engine] def waitForTermination(): IO[Map[K, V]] = for {
    _       <- status.waitForTermination()
    results <- facts.toMap().commit
  } yield results
}

object RunningFactEngine {
  private[engine] def create[K, V](processors: Seq[FactProcessor[K, V]]): IO[RunningFactEngine[K, V]] =
    for {
      stmRuntime <- createRuntime()
      status     <- RunningFactEngineStatus.initialStatus(using stmRuntime)
      facts      <- STMMap.empty[K, V]().commit(using stmRuntime)
    } yield new RunningFactEngine[K, V](processors, status, facts)(using stmRuntime)
}
