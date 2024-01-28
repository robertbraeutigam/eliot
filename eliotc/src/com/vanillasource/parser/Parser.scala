package com.vanillasource.parser

import cats.data.StateT
import cats.syntax.all.*
import cats.{Eq, Show}
import com.vanillasource.parser.ParserResult.*

/** A parser combinator that consumes items of type [[I]] and produces results of some type [[O]].
  */
type Parser[I, O] = StateT[ParserResult, InputStream[I], O]

object Parser {
  extension [I, O](p: Parser[I, O]) {

    /** Parse the given input elements with this parser.
      */
    def runParser(input: Seq[I]): ParserResult[O] = p.runA(InputStream.of(input))

    /** Parse the given input element with this parser, and return a user-friendly representation of the results.
      */
    def parse(input: Seq[I]): Either[ParserError[I], O] =
      runParser(input) match
        case Success(consumed, expectedPos, expected, a) => Right(a)
        case Failure(consumed, expectedPos, expected)    => Left(ParserError(input.drop(expectedPos), expected))

    /** Fully read the input with the given parser. This means after the parser completes, the input should be empty.
      */
    def fully(): Parser[I, O] = p <* endOfInput()

    /** Match the given parser optionally. The parser returns None, if the given parser can be skipped, i.e. it consumes
      * no input.
      */
    def optional(): Parser[I, Option[O]] = StateT { input =>
      p.run(input) match
        case Success(consumed, expectedPos, expected, (restInput, o)) =>
          Success(consumed, expectedPos, expected, (restInput, Some(o)))
        case Failure(false, expectedPos, expected)                    => Success(consumed = false, expectedPos, expected, (input, None))
        case Failure(true, expectedPos, expected)                     => Failure(consumed = true, expectedPos, expected)
    }

    /** Match the given parser zero or more times. */
    def anyTimes(): Parser[I, Seq[O]] =
      Seq.empty[O].tailRecM { acc =>
        optional().map {
          case Some(value) => Left(acc.appended(value))
          case None        => Right(acc)
        }
      }

    /** Make the whole parser a single transaction. Which means that if it fails, it will always fail without consuming
      * any input.
      */
    def atomic(): Parser[I, O] = StateT { input =>
      p.run(input) match
        case Success(consumed, expectedPos, expected, a) => Success(consumed, expectedPos, expected, a)
        case Failure(_, expectedPos, expected)           => Failure(false, expectedPos, expected)
    }

    /** Returns the result of the first parser, if it succeeds, or the second one if the first one fails without
      * consuming any input.
      */
    def or(p2: Parser[I, O]): Parser[I, O] =
      p.optional().flatMap {
        case Some(a) => a.pure
        case None    => p2
      }
  }

  /** A parser that will consume exactly the given item, or fail without consuming input.
    */
  def literal[I](i: I)(using Eq[I], Show[I]): Parser[I, I] = acceptIf(_ === i, i.show)

  /** Accept if the given predicate holds.
    */
  def acceptIf[I](predicate: I => Boolean, expected: String): Parser[I, I] = StateT { input =>
    input.headOption match {
      case Some(nextI) if predicate(nextI) => Success(consumed = true, 0, Seq.empty, (input.tail, nextI))
      case _                               => Failure(consumed = false, input.pos, Seq(expected))
    }
  }

  /** A parser that matches the end of input.
    */
  def endOfInput[I](): Parser[I, Unit] = StateT { input =>
    input.headOption match {
      case None => Success(consumed = false, 0, Seq.empty, (input, ()))
      case _    => Failure(consumed = false, input.pos, Seq("end of input"))
    }
  }

  def acceptIfAll[I](predicates: (I => Boolean)*)(expected: String): Parser[I, I] =
    acceptIf(i => predicates.forall(_.apply(i)), expected)
}
