package com.vanillasource.stm

import cats.free.Free
import cats.syntax.all.*
import com.vanillasource.stm.STM.*

import scala.collection.concurrent.TrieMap

case class STMMap[K, V] private (stmMap: STMVar[Map[K, V]]) {
  def lookup(key: K): STM[Option[V]] = stmMap.get().map(_.get(key))

  def insert(key: K, value: V): STM[Unit] = stmMap.update(_ + (key -> value))

  def putIfAbsent(key: K, value: V): STM[Option[V]] = for {
    oldValue <- lookup(key)
    _        <- oldValue match
                  case Some(_) => ().pure[STM]
                  case None    => insert(key, value)
  } yield oldValue

  def toMap: STM[Map[K, V]] = stmMap.get()
}

object STMMap {
  def empty[K, V](): STM[STMMap[K, V]] = createSTMVar(Map.empty[K, V]).map(STMMap.apply)
}
