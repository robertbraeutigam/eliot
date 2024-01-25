package com.vanillasource.eliot.eliotc.source

import cats.Functor

import java.io.File

/** A value of generic type transformed from a given snippet of code inside a given source code file.
  */
case class Sourced[+T](range: PositionRange, value: T)

object Sourced {
  implicit val sourcedFunctor: Functor[Sourced] = new Functor[Sourced]:
    override def map[A, B](fa: Sourced[A])(f: A => B): Sourced[B] = Sourced(fa.range, f(fa.value))
}
