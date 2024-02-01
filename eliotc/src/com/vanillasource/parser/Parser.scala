package com.vanillasource.parser

import cats.data.StateT
import cats.syntax.all.*
import cats.{Eq, Show}
import com.vanillasource.parser.ParserResult.*
import com.vanillasource.parser.ParserResult.Consume.*

/** A parser combinator that consumes items of type [[I]] and produces results of some type [[O]].
  */
type Parser[I, O] = StateT[ParserResult, InputStream[I], O]

object Parser {
  extension [I, O](p: Parser[I, O]) {

    /** Parse the given input elements with this parser.
      */
    def parse(input: Seq[I]): ParserResult[O] = p.runA(InputStream.of(input))

    /** Save the error into the errors list. This does not alter the result in any other way, the parser will still fail
      * on error.
      */
    def saveError(): Parser[I, O] = StateT { input =>
      p.run(input) match
        case ParserResult(consume, expected, allErrors, None) =>
          ParserResult(consume, expected, allErrors :+ expected, None)
        case other                                            => other
    }

    /** Fully read the input with the given parser. This means after the parser completes, the input should be empty.
      */
    def fully(): Parser[I, O] = p <* endOfInput()

    /** Match the given parser optionally. The parser returns None, if the given parser can be skipped, i.e. it consumes
      * no input.
      */
    def optional(): Parser[I, Option[O]] = StateT { input =>
      p.run(input) match
        case ParserResult(NotConsumed, expected, allErrors, None) =>
          ParserResult(NotConsumed, expected, allErrors, Some((input, None)))
        case other                                                => other.map((input, a) => (input, Some(a)))
    }

    /** Match the given parser zero or more times. */
    def anyTimes(): Parser[I, Seq[O]] = // TODO: iterateWhile?
      Seq.empty[O].tailRecM { acc =>
        optional().map {
          case Some(value) => Left(acc.appended(value))
          case None        => Right(acc)
        }
      }

    def anyTimesWhen(n: Parser[I, _]): Parser[I, Seq[O]] =
      Seq.empty[O].tailRecM { acc =>
        (n.lookahead() *> p).optional().map {
          case Some(value) => Left(acc.appended(value))
          case None        => Right(acc)
        }
      }

    /** Parses if this parser is followed by the given parser. No input is consumed on the given parser.
      */
    def followedBy(n: Parser[I, _]): Parser[I, O] = p <* n.lookahead()

    /** Make the whole parser a single transaction. Which means that if it fails, it will always fail without consuming
      * any input.
      */
    def atomic(): Parser[I, O] = StateT { input =>
      p.run(input) match
        case ParserResult(_, currentError, allErrors, None) => ParserResult(NotConsumed, currentError, allErrors, None)
        case other                                          => other
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
    def find(): Parser[I, O] = findAt(any())

    /** Find this input which matches this parser, but only in positions where the "at" parser matches.
      */
    def findAt(at: Parser[I, _]): Parser[I, O] =
      recoverWith(skipTo(at))
        .iterateUntil(_.nonEmpty)
        .map(_.get)

    def recoverWith(skip: Parser[I, _]): Parser[I, Option[O]] = p.atomic().map(Some.apply) or (any() >> skip.as(None))

    /** Will match if this parser matches the input, but will not consume any input regardless of success or failure.
      */
    def lookahead(): Parser[I, Unit] = StateT { input =>
      p.run(input).copy(consume = NotConsumed).as((input, ()))
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
      case Some(nextI) if predicate(nextI) =>
        ParserResult(Consumed, ParserError.noError, Seq.empty, Some((input.tail, nextI)))
      case _                               =>
        ParserResult(
          NotConsumed,
          ParserError(input.pos, Seq(expected)),
          Seq.empty,
          None
        )
    }
  }

  /** Skip to a given input, but do not consume it.
    */
  def skipTo[I](p: Parser[I, _]): Parser[I, Unit] =
    (p.lookahead().as(true) or any().as(false)).iterateUntil(identity).void

  /** Match any input item.
    */
  def any[I](): Parser[I, I] = acceptIf(_ => true, "input")

  /** A parser that matches the end of input.
    */
  def endOfInput[I](): Parser[I, Unit] = StateT { input =>
    input.headOption match {
      case None => ParserResult(NotConsumed, ParserError.noError, Seq.empty, Some((input, ())))
      case _    => ParserResult(NotConsumed, ParserError(input.pos, Seq("end of input")), Seq.empty, None)
    }
  }

  def acceptIfAll[I](predicates: (I => Boolean)*)(expected: String): Parser[I, I] =
    acceptIf(i => predicates.forall(_.apply(i)), expected)
}
