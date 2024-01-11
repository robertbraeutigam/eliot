package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO

final class FactEngine[K, V] private (processors: FactProcessor[K, V]) {
  def resolve(facts: Map[K, V]): IO[Map[K, V]] =
    for {
      engine <- STMRunningFactEngine.create(processors)
      _      <- engine.registerFacts(facts)
      result <- engine.waitForTermination()
    } yield result
}

object FactEngine {
  def apply[K, V](processors: FactProcessor[K, V]): FactEngine[K, V] = new FactEngine[K, V](processors)
}
