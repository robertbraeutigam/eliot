package com.vanillasource.parser

import cats.data.StateT
import cats.syntax.all.*
import cats.{Eq, Show}
import com.vanillasource.parser.ParserResult.*

/** A parser combinator that consumes items of type [[I]] and produces results of some type [[O]].
  */
type Parser[I, O] = StateT[ParserResult, Seq[I], O]

object Parser {
  extension [I, O](p: Parser[I, O]) {

    /** Fully read the input with the given parser. This means after the parser completes, the input should be empty.
      */
    def fully(): Parser[I, O] = p <* endOfInput()

    /** Match the given parser optionally. The parser returns None, if the given parser can be skipped, i.e. it consumes
      * no input.
      */
    def optional(): Parser[I, Option[O]] = StateT { input =>
      p.run(input) match
        case Success(consumed, expected, (restInput, o)) => Success(consumed, expected, (restInput, Some(o)))
        case Failure(false, expected)                    => Success(consumed = false, expected, (input, None))
        case Failure(true, expected)                     => Failure(consumed = true, expected)
    }

    /** Match the given parser zero or more times. */
    def anyTimes(): Parser[I, Seq[O]] =
      Seq.empty[O].tailRecM { acc =>
        optional().map {
          case Some(value) => Left(acc.appended(value))
          case None        => Right(acc)
        }
      }
  }

  /** A parser that will consume exactly the given item, or fail without consuming input. TODO: remove literal
    */
  def literal[I](i: I)(using Eq[I], Show[I]): Parser[I, I] = acceptIf(_ === i, s"literal '${i.show}'")

  /** Accept if the given predicate holds.
    */
  def acceptIf[I](predicate: I => Boolean, expected: String): Parser[I, I] = StateT { input =>
    input.headOption match {
      case Some(nextI) if predicate(nextI) => Success(consumed = true, Seq.empty, (input.tail, nextI))
      case _                               => Failure(consumed = false, Seq(expected))
    }
  }

  /** A parser that matches the end of input.
    */
  def endOfInput[I](): Parser[I, Unit] = StateT { input =>
    input.headOption match {
      case None => Success(consumed = false, Seq.empty, (input, ()))
      case _    => Failure(consumed = false, Seq("end of input"))
    }
  }

  def acceptIfAll[I](predicates: (I => Boolean)*)(expected: String): Parser[I, I] =
    acceptIf(i => predicates.forall(_.apply(i)), expected)
}
