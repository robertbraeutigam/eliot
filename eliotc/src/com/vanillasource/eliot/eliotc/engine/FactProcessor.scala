package com.vanillasource.eliot.eliotc.engine

import cats.data.ReaderT
import cats.effect.IO
import cats.syntax.all._
import com.vanillasource.eliot.eliotc.engine.FactProcessor.FactsIO

trait FactProcessor[K, V] {
  def process(v: V): FactsIO[K, V, Unit]
}

object FactProcessor {
  type FactsIO[K, V, A] = ReaderT[IO, RunningFactEngine[K, V], A]

  def registerFact[K, V](k: K, v: V): FactsIO[K, V, Unit] = ReaderT(_.registerFact(k, v))

  def registerFacts[K, V](facts: Iterable[(K, V)]): FactsIO[K, V, Unit] = facts.map(registerFact).toSeq.sequence_

  def getFact[K, V](k: K): FactsIO[K, V, Option[V]] = ReaderT(_.getFact(k))
}
