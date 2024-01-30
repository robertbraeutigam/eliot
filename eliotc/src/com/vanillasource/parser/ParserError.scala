package com.vanillasource.parser

import cats.kernel.Semigroup

case class ParserError(pos: Int, expected: Seq[String])

object ParserError {
  def noError: ParserError = ParserError(0, Seq.empty)

  given Semigroup[ParserError] = (x: ParserError, y: ParserError) => ParserError(y.pos, x.expected ++ y.expected)
}
