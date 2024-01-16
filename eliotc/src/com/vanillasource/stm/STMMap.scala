package com.vanillasource.stm

import cats.free.Free
import cats.syntax.all.*
import com.vanillasource.stm.STM.*

import scala.collection.concurrent.TrieMap

class STMMap[K, V] private (values: TrieMap[K, STMVar[Option[V]]]) {
  def lookup(key: K): STM[Option[V]] = getOrCreateTVar(key).flatMap(_.get())

  def insert(key: K, value: V): STM[Unit] = getOrCreateTVar(key).flatMap(_.set(Some(value)))

  def putIfAbsent(key: K, value: V): STM[Option[V]] = for {
    oldValue <- lookup(key)
    _        <- oldValue match
                  case Some(value) => ().pure[STM]
                  case None        => insert(key, value)
  } yield oldValue

  private def getOrCreateTVar(key: K): STM[STMVar[Option[V]]] = for {
    newTVar <- createSTMVar(Option.empty[V])
  } yield values.putIfAbsent(key, newTVar).getOrElse(newTVar)
}

object STMMap {
  def empty[K, V](): STM[STMMap[K, V]] = Free.pure(new STMMap(TrieMap.empty))
}
