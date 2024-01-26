package com.vanillasource.parser

import cats.data.StateT
import cats.syntax.all.*
import cats.{Eq, Show}
import com.vanillasource.parser.ParserResult.*

/** A parser combinator that consumes items of type [[I]] and produces results of some type [[O]].
  */
type Parser[I, O] = StateT[ParserResult, Stream[I], O]

object Parser {

  /** A parser that will consume exactly the given item, or fail without consuming input.
    */
  def literal[I](i: I)(using Eq[I], Show[I]): Parser[I, I] = acceptIf(_ === i, i.show)

  /** Accept if the given predicate holds.
    */
  def acceptIf[I](predicate: I => Boolean, expected: String): Parser[I, I] = StateT { input =>
    input.head match {
      case Some(nextI) if predicate(nextI) => Success((input.tail, nextI))
      case _                               => Skip(expected)
    }
  }

  /** Fully read the input with the given parser. This means after the parser completes, the input should be empty.
    */
  def fully[I, O](p: Parser[I, O]): Parser[I, O] = p <* endOfInput()

  /** Match the given parser optionally. The parser returns None, if the given parser can be skipped, i.e. it consumes
    * no input.
    */
  def option[I, O](p: Parser[I, O]): Parser[I, Option[O]] = StateT { input =>
    p.run(input) match
      case Success((restInput, o)) => Success((restInput, Some(o)))
      case Skip(expected)          => Success((input, None))
      case Error(expected)         => Error(expected)
  }

  /** Match the given parser zero or more times. */
  def anyTimes[I, O](p: Parser[I, O]): Parser[I, Seq[O]] =
    Seq.empty[O].tailRecM { acc =>
      option(p).map {
        case Some(value) => Left(acc.appended(value))
        case None        => Right(acc)
      }
    }

  /** A parser that matches the end of input.
    */
  def endOfInput[I](): Parser[I, Unit] = StateT { input =>
    input.head match {
      case None => Success((input, ()))
      case _    => Error("end of input")
    }
  }

  def acceptIfAll[I](predicates: (I => Boolean)*)(expected: String): Parser[I, I] =
    acceptIf(i => predicates.forall(_.apply(i)), expected)
}
