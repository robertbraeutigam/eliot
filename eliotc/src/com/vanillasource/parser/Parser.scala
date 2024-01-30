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
    def parse(input: Seq[I]): Either[ParserError, O] =
      runParser(input) match
        case Success(consumed, expected, a) => Right(a)
        case Failure(consumed, expected)    => Left(expected)

    def debugError(): Parser[I, O] = StateT { input =>
      p.run(input) match
        case Success(consumed, expected, a) => Success(consumed, expected, a)
        case Failure(false, expected)       => Failure(false, expected)
        case Failure(true, expected)        =>
          // TODO: this is just for debugging for now
          input.remainder.drop(expected.pos - input.pos).headOption match
            case Some(token) => println(s"Couldn't match $token, expected: $expected")
            case None        => println(s"Reached end of stream, expected: $expected")
          Failure(true, expected)
    }

    /** Fully read the input with the given parser. This means after the parser completes, the input should be empty.
      */
    def fully(): Parser[I, O] = p <* endOfInput()

    /** Match the given parser optionally. The parser returns None, if the given parser can be skipped, i.e. it consumes
      * no input.
      */
    def optional(): Parser[I, Option[O]] = StateT { input =>
      p.run(input) match
        case Success(consumed, expected, (restInput, o)) =>
          Success(consumed, expected, (restInput, Some(o)))
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

    /** Make the whole parser a single transaction. Which means that if it fails, it will always fail without consuming
      * any input.
      */
    def atomic(): Parser[I, O] = StateT { input =>
      p.run(input) match
        case Success(consumed, expected, a) => Success(consumed, expected, a)
        case Failure(_, expected)           => Failure(false, expected) // TODO: is this right? Not input.pos?
    }

    /** Find the given parser in the stream. The resulting parser will advance the stream until the parser can be
      * matched at some position.
      *
      * Note: when such a parser is made [[atomic]], that would mean that it will either find the given input which can
      * be parsed, or it will not consume any input.
      *
      * Note: when such a parser is combined with [[anyTimes]], it will always consume the full input searching for the
      * given parser.
      *
      * Note: when such a parser is both [[atomic]] and [[anyTimes]] (in that order), then the parser will be matched as
      * many times as possible, but input will only be consumed to the end of the last match.
      */
    def find(): Parser[I, O] = StateT { input =>
      p.run(input) match
        case Success(consumed, expected, a)                          => Success(consumed, expected, a)
        case Failure(consumed, expected) if input.headOption.isEmpty =>
          Failure(consumed, expected)
        case Failure(true, expected)                                 => find().run(input.tail).setConsumed()
        case Failure(false, expected)                                => find().run(input.tail)
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
      case Some(nextI) if predicate(nextI) => Success(consumed = true, ParserError.noError, (input.tail, nextI))
      case _                               => Failure(consumed = false, ParserError(input.pos, Seq(expected)))
    }
  }

  /** A parser that matches the end of input.
    */
  def endOfInput[I](): Parser[I, Unit] = StateT { input =>
    input.headOption match {
      case None => Success(consumed = false, ParserError.noError, (input, ()))
      case _    => Failure(consumed = false, ParserError(input.pos, Seq("end of input")))
    }
  }

  def acceptIfAll[I](predicates: (I => Boolean)*)(expected: String): Parser[I, I] =
    acceptIf(i => predicates.forall(_.apply(i)), expected)
}
