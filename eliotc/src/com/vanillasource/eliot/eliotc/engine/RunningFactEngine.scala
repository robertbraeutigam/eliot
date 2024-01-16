package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO
import cats.syntax.all.*
import io.github.timwspence.cats.stm.STM as CatsSTM

case class RunningFactEngine[K, V] private (catsSTM: CatsSTM[IO], processors: FactProcessor[K, V]) {
  def registerFact(k: K, v: V): IO[Unit] = ???

  def registerFacts(facts: Iterable[(K, V)]): IO[Unit] = facts.map(registerFact).toSeq.sequence_

  def getFact(k: K): IO[Option[V]] = ???

  private[engine] def waitForTermination(): IO[Map[K, V]] = ???
}

object RunningFactEngine {
  private[engine] def create[K, V](processors: FactProcessor[K, V]): IO[RunningFactEngine[K, V]] =
    for {
      catsSTM <- CatsSTM.runtime[IO]
    } yield new RunningFactEngine[K, V](catsSTM, processors)
}
