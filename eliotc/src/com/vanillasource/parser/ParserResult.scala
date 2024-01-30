package com.vanillasource.parser

import cats.Monad
import cats.syntax.all.*
import com.vanillasource.parser.ParserError.given
import com.vanillasource.parser.ParserResult.Consume.{Consumed, NotConsumed}

sealed trait ParserResult[A] {
  def prependExpected(before: ParserError): ParserResult[A]

  def setConsumed(): ParserResult[A]
}

object ParserResult {
  enum Consume {
    case Consumed, NotConsumed
  }

  /** Parser successfully got a value.
    *
    * @param consume
    *   Whether the parser consumed any input.
    * @param currentError
    *   The sequence of expected item parsers that were tried and/or possible at this given position. Note that even if
    *   the parser successfully parsed a value it may have tried some other things, which must be listed here.
    * @param a
    *   The successfully parsed value.
    */
  case class Success[A](consume: Consume, currentError: ParserError, a: A) extends ParserResult[A] {
    override def prependExpected(before: ParserError): ParserResult[A] =
      consume match
        case Consumed    => Success(Consumed, currentError, a)
        case NotConsumed => Success(NotConsumed, before |+| currentError, a)

    override def setConsumed(): ParserResult[A] = Success(Consumed, currentError, a)
  }

  /** Parser failed to get a valid value.
    *
    * @param consume
    *   Whether the parser consumed any input.
    * @param currentError
    *   The sequence of expected item parsers that were tried and/or possible at this given position. Note that even if
    *   the parser successfully parsed a value it may have tried some other things, which must be listed here.
    */
  case class Failure[A](consume: Consume, currentError: ParserError) extends ParserResult[A] {
    override def prependExpected(before: ParserError): ParserResult[A] =
      Failure(consume, before |+| currentError)

    override def setConsumed(): ParserResult[A] = Failure(Consumed, currentError)
  }

  given Monad[ParserResult] = new Monad[ParserResult]:
    override def pure[A](a: A): ParserResult[A] = Success(NotConsumed, ParserError(0, Seq.empty), a)

    override def flatMap[A, B](fa: ParserResult[A])(f: A => ParserResult[B]): ParserResult[B] = fa match
      case Success(NotConsumed, currentError, a) => f(a).prependExpected(currentError)
      case Success(Consumed, currentError, a)    => f(a).prependExpected(currentError).setConsumed()
      case Failure(NotConsumed, currentError)    => Failure(NotConsumed, currentError)
      case Failure(Consumed, currentError)       => Failure(Consumed, currentError)

    // TODO: this is not stack-safe
    override def tailRecM[A, B](a: A)(f: A => ParserResult[Either[A, B]]): ParserResult[B] =
      flatMap(f(a)) {
        case Left(newA) => tailRecM(newA)(f)
        case Right(b)   => pure(b)
      }
}
