package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.Logging

final class FactEngine[K, V] private (processors: Seq[FactProcessor[K, V]]) extends Logging {
  def resolve(facts: Map[K, V]): IO[Map[K, V]] =
    for {
      _      <- info(s"fact engine starting resolve with ${processors.size} processors and ${facts.size} initial facts")
      engine <- RunningFactEngine.create(processors)
      _      <- engine.registerFacts(facts)
      result <- engine.waitForTermination()
    } yield result
}

object FactEngine {
  def apply[K, V](processors: Seq[FactProcessor[K, V]]): FactEngine[K, V] = new FactEngine[K, V](processors)
}
