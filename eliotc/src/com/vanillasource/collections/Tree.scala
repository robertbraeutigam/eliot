package com.vanillasource.collections

import cats.implicits.*
import cats.{Applicative, Eval, Functor, Monad, Monoid, Traverse}

import scala.annotation.tailrec

/*
sealed trait Tree[T] {
  def children(self: T): Seq[Tree[T]]

  def transform[R](f: T => R): Tree[R]
}

object Tree {
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

  extension [A](tree: Tree[A]) {
    def toSeqBreadthFirst: Seq[A] = toSeqBreadthFirstInternal(Seq(tree), Seq.empty[A])

    def head: Option[A] = tree match
      case Empty()        => None
      case Node(value, _) => Some(value)

    // Note: this might be a little stack-heavy if the tree is big, use trampoline?
    def foreachWithChildrenF[F[_]](f: (A, Seq[Option[A]]) => F[Unit])(using fm: Monad[F]): F[Unit] =
      tree match
        case Empty()            => fm.pure(())
        case Node(value, nodes) =>
          nodes.map(_.foreachWithChildrenF(f)).sequence_ >> f(
            value,
            nodes.map {
              case Empty()        => None
              case Node(value, _) => Some(value)
            }
          )
  }

  given Functor[Tree] = new Functor[Tree]:
    // Note: this might be a little stack-heavy if the tree is big, use trampoline?
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match
      case Empty()            => Empty()
      case Node(value, nodes) => Node(f(value), nodes.map(map(_)(f)))

  given Traverse[Tree] = new Traverse[Tree]() {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(using Applicative[G]): G[Tree[B]] =


    override def foldLeft[A, B](fa: Tree[A], b: B)(f: (B, A) => B): B = fa match
      case Empty()            => b
      case Node(value, nodes) => nodes.foldLeft(f(b, value))((acc, node) => foldLeft(node, acc)(f))

    override def foldRight[A, B](fa: Tree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match
      case Empty()            => lb
      case Node(value, nodes) => nodes.foldRight(f(value, lb))((node, acc) => foldRight(node, acc)(f))
  }

}
 */
