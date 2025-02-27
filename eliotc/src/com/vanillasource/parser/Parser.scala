package com.vanillasource.parser

import cats.data.StateT
import cats.syntax.all.*
import cats.{Eq, Show}
import com.vanillasource.parser.ParserResult.*
import com.vanillasource.parser.ParserResult.Consume.*

/** A parser combinator that consumes items of type [[I]] and produces results of some type [[O]].
  *
  * Foundational parsers are:
  *   - [[acceptIf]], which consumed a single token if the supplied predicate holds
  *
  * Foundational combinators are:
  *   - [[lookahead]], which is successful if the given parser can be matched, but does not consume any input
  *   - [[atomic]], which makes the parser fail without consuming any input
  *   - [[optional]], which makes a parser return None, if it failed without consuming input
  *
  * All other parsers are based on these foundational parsers or are combinations thereof, based on the foundational
  * combinators.
  */
type Parser[I, O] = StateT[ParserResult, InputStream[I], O]

object Parser {
  extension [I, O](p: Parser[I, O]) {

    /** Parse the given input elements with this parser.
      */
    def parse(input: Seq[I]): ParserResult[O] = p.runA(InputStream.of(input))

    /** Save the error into the error list. This does not alter the result in any other way, the parser will still fail
      * on error.
      */
    private def saveError(): Parser[I, O] = StateT { input =>
      p.run(input) match
        case ParserResult(consume, expected, allErrors, None) =>
          ParserResult(consume, expected, allErrors :+ expected, None)
        case other                                            => other
    }

    /** Fully read the input with the given parser. This means after the parser completes, the input should be empty.
      */
    def fully(): Parser[I, O] = p <* endOfInput()

    /** Match the given parser optionally. The parser returns None, if the given parser can be skipped, i.e., it
      * consumes no input.
      */
    def optional(): Parser[I, Option[O]] = StateT { input =>
      p.run(input) match
        case ParserResult(NotConsumed, expected, allErrors, None) =>
          ParserResult(NotConsumed, expected, allErrors, Some((input, None)))
        case other                                                => other.map((input, a) => (input, Some(a)))
    }

    /** Attempt this parser for the input until the given other parser. If the parser fails, the error will be saved and
      * the input will skip to the given delimiter.
      */
    def attemptPhraseTo(delimiter: Parser[I, _]): Parser[I, Option[O]] = p
      .followedBy(delimiter)
      .saveError()
      .recoverWith(delimiter.skipTo())

    /** Match the given parser zero or more times. */
    def anyTimes(): Parser[I, Seq[O]] = anyTimesWhile(().pure)

    /** Match this parser any times while the given parser matches.
      * @param n
      *   The parser that should match before applying this parser.
      */
    def anyTimesWhile(n: Parser[I, _]): Parser[I, Seq[O]] =
      Seq.empty[O].tailRecM { acc =>
        (n.lookahead() *> p).optional().map {
          case Some(value) => Left(acc.appended(value))
          case None        => Right(acc)
        }
      }

    def atLeastOnce(): Parser[I, Seq[O]] = for {
      head <- p
      tail <- anyTimes()
    } yield head +: tail

    /** Accept this parser at least once separated by another parser. If this parser only matches once, the separator is
      * not used. Otherwise, it is.
      */
    def atLeastOnceSeparatedBy(sep: Parser[I, _]): Parser[I, Seq[O]] = for {
      head <- p
      tail <- (sep *> p).anyTimes()
    } yield head +: tail

    /** Accept this parser when between the given parsers.
      */
    def between(begin: Parser[I, _], end: Parser[I, _]): Parser[I, O] = for {
      _      <- begin
      result <- p
      _      <- end
    } yield result

    /** Parses if this parser is followed by the given parser. No input is consumed on the given parser.
      */
    def followedBy(n: Parser[I, _]): Parser[I, O] = p <* n.lookahead()

    /** Make the whole parser a single transaction. Which means that if it fails, it will always fail without consuming
      * any input.
      */
    def atomic(): Parser[I, O] = StateT { input =>
      p.run(input) match
        case ParserResult(_, _, allErrors, None) =>
          ParserResult(NotConsumed, ParserError.noError, allErrors, None)
        case other                               => other
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
      recoverWith(at.skipTo())
        .iterateUntil(_.nonEmpty)
        .map(_.get)

    /** Skip to this parser.
      */
    def skipTo(): Parser[I, O] =
      (p.lookahead().map(Some.apply) or any().as(None)).iterateUntil(_.isDefined).map(_.get)

    def recoverWith(skip: Parser[I, _]): Parser[I, Option[O]] = p.atomic().map(Some.apply) or (any() >> skip.as(None))

    /** Will match if this parser matches the input, but will not consume any input regardless of success or failure.
      */
    def lookahead(): Parser[I, O] = StateT { input =>
      p.run(input).copy(consume = NotConsumed).map((_, o) => (input, o))
    }

    /** Returns the result of the first parser if it succeeds, or the second one if the first one fails without
      * consuming any input.
      */
    def or(p2: Parser[I, O]): Parser[I, O] =
      p.optional().flatMap {
        case Some(a) => a.pure
        case None    => p2
      }

    /** Parses either this or the given parser if this fails. This is like "or", but the two parsers may differ in type,
      * therefore, a pair is returned.
      */
    def xor[P](p2: Parser[I, P]): Parser[I, Either[O, P]] = p.map(Left(_)) or p2.map(Right(_))
  }

  /** Accept if the given predicate holds.
    */
  def acceptIf[I](predicate: I => Boolean, expected: String = ""): Parser[I, I] = StateT { input =>
    input.headOption match {
      case Some(nextI) if predicate(nextI) =>
        ParserResult(Consumed, ParserError.noError, Seq.empty, Some((input.tail, nextI)))
      case _                               =>
        ParserResult(
          NotConsumed,
          ParserError(input.pos, if (expected.isBlank) Set.empty else Set(expected)),
          Seq.empty,
          None
        )
    }
  }

  /** A parser that will consume exactly the given item, or fail without consuming input.
    */
  def literal[I](i: I)(using Eq[I], Show[I]): Parser[I, I] = acceptIf(_ === i, i.show)

  /** A parser that fails with the given "expected" message and does not consume any input.
    */
  def error[I](expected: String): Parser[I, Unit] = acceptIf[I](_ => false, expected).as(())

  /** A parser that matches the end of input. This does not consume input.
    */
  def endOfInput[I](): Parser[I, Unit] =
    acceptIf[I](_ => true)
      .optional()
      .lookahead()
      .flatMap {
        case Some(_) => error("end of input")
        case None    => ().pure
      }
      .void

  /** Match any input item. This will always succeed, except if there is no more input.
    */
  def any[I](): Parser[I, I] = acceptIf(_ => true, "input")

  /** Accept a single token if all the given predicates hold.
    */
  def acceptIfAll[I](predicates: (I => Boolean)*)(expected: String): Parser[I, I] =
    acceptIf(i => predicates.forall(_.apply(i)), expected)
}
