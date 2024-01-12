package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO
import io.github.timwspence.cats.stm.STM
import cats.syntax.all._

import scala.collection.concurrent.TrieMap

private[engine] class TMap[K, V] private (val stm: STM[IO], val values: TrieMap[K, stm.TVar[Option[V]]]) {
  import stm.*

  def lookup(key: K): Txn[Option[V]] = getOrCreateTVar(key).flatMap(_.get)

  def insert(key: K, value: V): Txn[Unit] = getOrCreateTVar(key).flatMap(_.set(Some(value)))

  private def getOrCreateTVar(key: K): Txn[TVar[Option[V]]] = for {
    newTVar <- TVar.of(Option.empty[V])
  } yield values.putIfAbsent(key, newTVar).getOrElse(newTVar)
}

object TMap {
  def create[K, V](stm: STM[IO]): IO[TMap[K, V]] = new TMap(stm, TrieMap.empty).pure
}
