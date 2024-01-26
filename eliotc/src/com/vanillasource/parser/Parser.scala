package com.vanillasource.parser

import cats.data.ReaderT
import cats.{Eq, Show}
import cats.syntax.all.*
import com.vanillasource.parser.ParserResult.*

/** A parser combinator that consumes items of type [[I]] and produces results of some type [[O]].
  */
type Parser[I, O] = ReaderT[ParserResult, Stream[I], O]

object Parser {

  /** A parser that will consume exactly the given item, or fail without consuming input.
    */
  def literal[I](i: I)(using Eq[I], Show[I]): Parser[I, I] = acceptIf(_ === i, i.show)

  /** Accept if the given predicate holds.
    */
  def acceptIf[I](predicate: I => Boolean, expected: String): Parser[I, I] = ReaderT { input =>
    input.head match {
      case Some(nextI) if predicate(nextI) => Success(nextI)
      case _                               => Skip(expected)
    }
  }

  def acceptIfAll[I](predicates: (I => Boolean)*)(expected: String): Parser[I, I] =
    acceptIf(i => predicates.forall(_.apply(i)), expected)
}
