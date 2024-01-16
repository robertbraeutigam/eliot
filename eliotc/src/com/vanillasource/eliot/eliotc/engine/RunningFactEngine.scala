package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.stm.STM.createRuntime
import com.vanillasource.stm.STMRuntime

case class RunningFactEngine[K, V] private (processors: FactProcessor[K, V])(using stmRuntime: STMRuntime) {
  def registerFact(k: K, v: V): IO[Unit] = ???

  def registerFacts(facts: Iterable[(K, V)]): IO[Unit] = facts.map(registerFact).toSeq.sequence_

  def getFact(k: K): IO[Option[V]] = ???

  private[engine] def waitForTermination(): IO[Map[K, V]] = ???
}

object RunningFactEngine {
  private[engine] def create[K, V](processors: FactProcessor[K, V]): IO[RunningFactEngine[K, V]] =
    for {
      stmRuntime <- createRuntime()
    } yield new RunningFactEngine[K, V](processors)(using stmRuntime)
}
