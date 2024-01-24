package com.vanillasource.collections

/** Immutable multi-node, sorting-preserving tree.
  */
sealed trait Tree[+T]

object Tree {
  case class Empty private[Tree] ()                      extends Tree[Nothing]
  case class Leaf[T] private[Tree] (value: T)            extends Tree[T]
  case class Node[T] private[Tree] (nodes: Seq[Tree[T]]) extends Tree[T]

  def apply[T](value: T): Tree[T] = Leaf(value)

  def apply[T](nodes: Seq[Tree[T]]): Tree[T] = Node(nodes)

  def empty[T](): Tree[T] = Empty()
}
