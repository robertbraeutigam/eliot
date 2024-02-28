package com.vanillasource.collections

import cats.implicits.*
import cats.{Applicative, Eval, Functor, Monoid, Traverse}

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}

/** Immutable multi-node, sorting-preserving tree.
  */
sealed trait Tree[T]

object Tree {
  case class Empty[T] private[Tree] ()                             extends Tree[T]
  case class Node[T] private[Tree] (value: T, nodes: Seq[Tree[T]]) extends Tree[T]

  def apply[T](value: T, nodes: Seq[Tree[T]] = Seq.empty[Tree[T]]): Tree[T] = Node(value, nodes)

  def empty[T](): Tree[T] = Empty()

  @tailrec
  private def toSeqBreadthFirstInternal[B](fas: Seq[Tree[B]], acc: Seq[B]): Seq[B] = fas match
    case Seq()        => acc
    case head +: tail =>
      head match
        case Empty()            => toSeqBreadthFirstInternal(tail, acc)
        case Node(value, nodes) => toSeqBreadthFirstInternal(tail ++ nodes, acc :+ value)

  private def foldDepthFirstMonoidInternal[B](fa: Tree[B])(using bm: Monoid[B]): B = fa match
    case Empty()            => bm.empty
    case Node(value, nodes) => nodes.map(fn => foldDepthFirstMonoidInternal(fn)).fold(bm.empty)(bm.combine) |+| value

  extension [A](t: Tree[A])(using Monoid[A]) {
    def foldDepthFirstMonoid(): A = foldDepthFirstMonoidInternal(t)

  }

  extension [A](t: Tree[A]) {
    def toSeqBreadthFirst: Seq[A] = toSeqBreadthFirstInternal(Seq(t), Seq.empty[A])
  }

  given Functor[Tree] = new Functor[Tree]:
    // Note: this might be a little stack-heavy if the tree is big, use trampoline?
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match
      case Empty()            => Empty()
      case Node(value, nodes) => Node(f(value), nodes.map(map(_)(f)))

  given Traverse[Tree] = new Traverse[Tree]:
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(using gapp: Applicative[G]): G[Tree[B]] = fa match
      case Empty()            => gapp.pure(Empty())
      case Node(value, nodes) =>
        val traversedNodes   = nodes.toList.traverse(traverse(_)(f))
        val transformedValue = f(value)
        gapp.map2(transformedValue, traversedNodes)(Node(_, _))

    override def foldLeft[A, B](fa: Tree[A], b: B)(f: (B, A) => B): B = fa match
      case Empty()            => b
      case Node(value, nodes) => nodes.foldLeft(f(b, value))((acc, node) => foldLeft(node, acc)(f))

    override def foldRight[A, B](fa: Tree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match
      case Empty()            => lb
      case Node(value, nodes) => nodes.foldRight(f(value, lb))((node, acc) => foldRight(node, acc)(f))
}
