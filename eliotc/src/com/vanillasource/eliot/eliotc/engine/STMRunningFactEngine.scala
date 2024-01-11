package com.vanillasource.eliot.eliotc.engine
import cats.effect.IO

private[engine] final class STMRunningFactEngine[K, V] private (processors: FactProcessor[K, V]) extends RunningFactEngine[K, V] {
  override def registerFact(k: K, v: V): IO[Unit] = ???

  override def getFact(k: K): IO[Option[V]] = ???

  def waitForTermination(): IO[Map[K, V]] = ???
}

object STMRunningFactEngine {
  def create[K, V](processors: FactProcessor[K, V]): IO[STMRunningFactEngine[K, V]] = IO.pure(new STMRunningFactEngine[K, V](processors))
}
