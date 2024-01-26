package com.vanillasource.parser

import cats.{Applicative, Functor}

sealed trait ParserResult[O] {
  def map[O2](f: O => O2): ParserResult[O2]
}

object ParserResult {

  /** Parser was successful and produces a value.
    */
  case class Success[O](o: O) extends ParserResult[O]

  /** Parser wants to be skipped. Normally this means "failed without consuming input", but its meaning might extend to
    * other cases depending on the combinator used.
    */
  case class Skip[O](expected: String) extends ParserResult[O]

  /** Means that the parser failed in mid-parsing, and there's no safe way to continue parsing.
    */
  case class Error[O](expected: String) extends ParserResult[O]

  given Functor[ParserResult] = new Functor[ParserResult]:
    override def map[A, B](fa: ParserResult[A])(f: A => B): ParserResult[B] = fa.map(f)
}
