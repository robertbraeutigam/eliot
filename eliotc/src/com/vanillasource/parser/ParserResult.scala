package com.vanillasource.parser

import cats.Monad
import cats.syntax.all._
import com.vanillasource.parser.ParserError.given

sealed trait ParserResult[A] {
  def prependExpected(before: ParserError): ParserResult[A]

  def setConsumed(): ParserResult[A]
}

object ParserResult {

  /** Parser successfully got a value.
    *
    * @param consumed
    *   Whether the parser consumed any input.
    * @param expected
    *   The sequence of expected item parsers that were tried and/or possible at this given position. Note that even if
    *   the parser successfully parsed a value it may have tried some other things, which must be listed here.
    * @param a
    *   The successfully parsed value.
    */
  case class Success[A](consumed: Boolean, currentExpected: ParserError, a: A) extends ParserResult[A] {
    override def prependExpected(before: ParserError): ParserResult[A] =
      if (consumed) {
        Success(true, currentExpected, a)
      } else {
        Success(false, before |+| currentExpected, a)
      }

    override def setConsumed(): ParserResult[A] = Success(true, currentExpected, a)
  }

  /** Parser failed to get a valid value.
    *
    * @param consumed
    *   Whether the parser consumed any input.
    * @param expected
    *   The sequence of expected item parsers that were tried and/or possible at this given position. Note that even if
    *   the parser successfully parsed a value it may have tried some other things, which must be listed here.
    */
  case class Failure[A](consumed: Boolean, currentExpected: ParserError) extends ParserResult[A] {
    override def prependExpected(before: ParserError): ParserResult[A] =
      Failure(consumed, before |+| currentExpected)

    override def setConsumed(): ParserResult[A] = Failure(true, currentExpected)
  }

  given Monad[ParserResult] = new Monad[ParserResult]:
    override def pure[A](a: A): ParserResult[A] = Success(consumed = false, ParserError(0, Seq.empty), a)

    override def flatMap[A, B](fa: ParserResult[A])(f: A => ParserResult[B]): ParserResult[B] = fa match
      case Success(false, expected, a) => f(a).prependExpected(expected)
      case Success(true, expected, a)  => f(a).prependExpected(expected).setConsumed()
      case Failure(false, expected)    => Failure(false, expected)
      case Failure(true, expected)     => Failure(true, expected)

    // TODO: this is not stack-safe
    override def tailRecM[A, B](a: A)(f: A => ParserResult[Either[A, B]]): ParserResult[B] =
      flatMap(f(a)) {
        case Left(newA) => tailRecM(newA)(f)
        case Right(b)   => pure(b)
      }
}
