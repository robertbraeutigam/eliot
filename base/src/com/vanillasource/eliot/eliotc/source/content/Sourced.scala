package com.vanillasource.eliot.eliotc.source.content

import cats.implicits.*
import cats.{Functor, Show}
import com.vanillasource.eliot.eliotc.pos.{PositionRange, Sourced}

import java.io.File

/** A value of generic type transformed from a given snippet of code inside a given source code file.
  */
case class Sourced[+T](file: File, range: PositionRange, value: T) {
  def reFocus(newRange: PositionRange): Sourced[T] = copy(range = newRange)
}

object Sourced {
  given Functor[Sourced] = new Functor[Sourced] {
    override def map[A, B](fa: Sourced[A])(f: A => B): Sourced[B] = Sourced(fa.file, fa.range, f(fa.value))
  }

  given [T]: Show[Sourced[T]] = (t: Sourced[T]) => s"${t.value.toString} (${t.range.show})"

  def outline(ss: Seq[Sourced[?]]): Sourced[Unit] = ss match
    case head :: _ => Sourced(head.file, PositionRange(ss.map(_.range.from).min, ss.map(_.range.to).max), ())
    case _         => throw IllegalArgumentException("can't produce an outline of empty sourced values")
}
