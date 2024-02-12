package com.vanillasource.collections

/** Immutable multi-node, sorting-preserving tree.
  */
sealed trait Tree[+T]

object Tree {
  case class Empty private[Tree] ()                                extends Tree[Nothing]
  case class Node[T] private[Tree] (value: T, nodes: Seq[Tree[T]]) extends Tree[T]

  def apply[T](value: T, nodes: Seq[Tree[T]] = Seq.empty): Tree[T] = Node(value, nodes)

  def empty[T](): Tree[T] = Empty()
}
