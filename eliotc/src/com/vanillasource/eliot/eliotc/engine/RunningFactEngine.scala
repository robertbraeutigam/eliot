package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO
import cats.syntax.all._

trait RunningFactEngine[K, V] {
  def registerFact(k: K, v: V): IO[Unit]

  def registerFacts(facts: Iterable[(K, V)]): IO[Unit] = facts.map(registerFact).toSeq.sequence_

  def getFact(k: K): IO[Option[V]]
}
