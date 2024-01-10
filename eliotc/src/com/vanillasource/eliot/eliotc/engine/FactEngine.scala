package com.vanillasource.eliot.eliotc.engine

import cats.data.ReaderT
import cats.effect.IO

import scala.collection.concurrent.TrieMap

private case class FactEngine[K, V](facts: TrieMap[K, V])

object FactEngine {
  type FactsIO[K, V] = [A] =>> ReaderT[IO, FactEngine[K,V], A]

  trait FactProcessor[K, V] {
    def process(v: V): FactsIO[K, V][Unit]
  }

  def resolveFacts[K, V](processors: FactProcessor[K, V], facts: Map[K, V]): IO[Option[Map[K, V]]] = ???
}

