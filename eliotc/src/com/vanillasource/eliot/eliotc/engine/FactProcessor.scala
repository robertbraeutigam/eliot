package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO

trait FactProcessor[K, V] {
  def process(v: V)(using runningFactEngine: RunningFactEngine[K, V]): IO[Unit]
}
