package com.vanillasource.util

import cats.syntax.all.*

case class MultiSet[A](elements: Map[A, Int] = Map.empty) {
  def added(a: A): MultiSet[A] =
    copy(elements = elements.updatedWith(a)(_.map(_ + 1).getOrElse(1).pure[Option]))

  def removed(a: A): MultiSet[A] =
    copy(elements = elements.updatedWith(a)(_.map(_ - 1).filter(_ > 0)))

  def keySet: Set[A] = elements.keySet
}

object MultiSet {
  def empty[A]: MultiSet[A] = MultiSet[A](Map.empty)
}
