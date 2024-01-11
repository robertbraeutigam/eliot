package com.vanillasource.eliot.eliotc.engine

import cats.data.ReaderT
import cats.effect.IO
import com.vanillasource.eliot.eliotc.engine.FactProcessor.FactsIO

trait FactProcessor[K, V] {
  def process(v: V): FactsIO[K, V, Unit]
}

object FactProcessor {
  type FactsIO[K, V, A] = ReaderT[IO, RunningFactEngine[K, V], A]
}
