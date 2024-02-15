package com.vanillasource.eliot.eliotc.source

import cats.Functor
import cats.implicits.*

import java.io.File
import scala.collection.immutable.{AbstractSeq, LinearSeq}

/** A value of generic type transformed from a given snippet of code inside a given source code file.
  */
case class Sourced[+T](file: File, range: PositionRange, value: T)

object Sourced {
  given Functor[Sourced] = new Functor[Sourced] {
    override def map[A, B](fa: Sourced[A])(f: A => B): Sourced[B] = Sourced(fa.file, fa.range, f(fa.value))
  }

  def outline(ss: Seq[Sourced[_]]): Sourced[Unit] = ss match
    case head :: _ => Sourced(head.file, PositionRange(ss.map(_.range.from).min, ss.map(_.range.to).max), ())
    case _         => throw IllegalArgumentException("can't produce an outline of empty sourced values")

}
