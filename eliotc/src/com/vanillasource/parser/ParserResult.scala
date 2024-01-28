package com.vanillasource.parser

import cats.Monad

sealed trait ParserResult[A] {
  def prependExpected(es: Seq[String]): ParserResult[A]

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
  case class Success[A](consumed: Boolean, expectedPos: Int, expected: Seq[String], a: A) extends ParserResult[A] {
    override def prependExpected(es: Seq[String]): ParserResult[A] =
      if (consumed) {
        Success(true, expectedPos, expected, a)
      } else {
        Success(false, expectedPos, es ++ expected, a)
      }

    override def setConsumed(): ParserResult[A] = Success(true, expectedPos, expected, a)
  }

  /** Parser failed to get a valid value.
    *
    * @param consumed
    *   Whether the parser consumed any input.
    * @param expected
    *   The sequence of expected item parsers that were tried and/or possible at this given position. Note that even if
    *   the parser successfully parsed a value it may have tried some other things, which must be listed here.
    */
  case class Failure[A](consumed: Boolean, expectedPos: Int, expected: Seq[String]) extends ParserResult[A] {
    override def prependExpected(es: Seq[String]): ParserResult[A] = Failure(consumed, expectedPos, es ++ expected)

    override def setConsumed(): ParserResult[A] = Failure(true, expectedPos, expected)
  }

  given Monad[ParserResult] = new Monad[ParserResult]:
    override def pure[A](a: A): ParserResult[A] = Success(consumed = false, 0, Seq.empty, a)

    override def flatMap[A, B](fa: ParserResult[A])(f: A => ParserResult[B]): ParserResult[B] = fa match
      case Success(false, _, expected, a)        => f(a).prependExpected(expected)
      case Success(true, _, expected, a)         => f(a).prependExpected(expected).setConsumed()
      case Failure(false, expectedPos, expected) => Failure(false, expectedPos, expected)
      case Failure(true, expectedPos, expected)  => Failure(true, expectedPos, expected)

    // TODO: this is not stack-safe
    override def tailRecM[A, B](a: A)(f: A => ParserResult[Either[A, B]]): ParserResult[B] =
      flatMap(f(a)) {
        case Left(newA) => tailRecM(newA)(f)
        case Right(b)   => pure(b)
      }
}
