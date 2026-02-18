package com.vanillasource.eliot.eliotc.ast.parser

import cats.kernel.Semigroup

case class ParserError(pos: Int, expected: Set[String])

object ParserError {
  def noError: ParserError = ParserError(0, Set.empty)

  given Semigroup[ParserError] = (x: ParserError, y: ParserError) => ParserError(y.pos, x.expected ++ y.expected)
}
