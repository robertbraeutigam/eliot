package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO

final class FactEngine[K, V] private (processors: Seq[FactProcessor[K, V]]) {
  def resolve(facts: Map[K, V]): IO[Map[K, V]] =
    for {
      engine <- RunningFactEngine.create(processors)
      _      <- engine.registerFacts(facts)
      result <- engine.waitForTermination()
    } yield result
}

object FactEngine {
  def apply[K, V](processors: Seq[FactProcessor[K, V]]): FactEngine[K, V] = new FactEngine[K, V](processors)
}
