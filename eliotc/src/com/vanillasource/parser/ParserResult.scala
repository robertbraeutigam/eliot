package com.vanillasource.parser

import cats.{Functor, Monad}

sealed trait ParserResult[A] {
  def fold[B](ifEmpty: => B)(f: A => B): B
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
  case class Success[A](consumed: Boolean, expected: Seq[String], a: A) extends ParserResult[A] {
    override def fold[B](ifEmpty: => B)(f: A => B): B = f(a)
  }

  /** Parser failed to get a valid value.
    *
    * @param consumed
    *   Whether the parser consumed any input.
    * @param expected
    *   The sequence of expected item parsers that were tried and/or possible at this given position. Note that even if
    *   the parser successfully parsed a value it may have tried some other things, which must be listed here.
    */
  case class Failure[A](consumed: Boolean, expected: Seq[String]) extends ParserResult[A] {
    override def fold[B](ifEmpty: => B)(f: A => B): B = ifEmpty
  }

  given Functor[ParserResult] = new Functor[ParserResult]:
    override def map[A, B](fa: ParserResult[A])(f: A => B): ParserResult[B] = ???

  given Monad[ParserResult] = new Monad[ParserResult]:
    override def pure[A](a: A): ParserResult[A] = Success(consumed = false, Seq.empty, a)

    override def flatMap[A, B](fa: ParserResult[A])(f: A => ParserResult[B]): ParserResult[B] = fa match
      case Success(false, expected, a) =>
        // Previous parser did not consume input, so if the followup didn't either, then
        // add their expected items
        f(a) match
          case Success(false, expected2, a2) => Success(false, expected ++ expected2, a2)
          case Success(true, expected2, a2)  => Success(true, expected2, a2)
          case Failure(false, expected2)     => Failure(false, expected ++ expected2)
          case Failure(true, expected2)      => Failure(true, expected2)
      case Success(true, _, a)         =>
        // Previous parser consumed input, so forget all expected items
        // Also, the combined parser will then have consumed items
        f(a) match
          case Success(_, expected, a2) => Success(true, expected, a2)
          case Failure(_, expected)     => Failure(true, expected)
      // Failures are short-circuit, follow-up parser is not executed
      case Failure(false, expected)    => Failure(false, expected)
      case Failure(true, expected)     => Failure(true, expected)

    // TODO: this is not stack-safe
    override def tailRecM[A, B](a: A)(f: A => ParserResult[Either[A, B]]): ParserResult[B] =
      flatMap(f(a)) {
        case Left(newA) => tailRecM(newA)(f)
        case Right(b)   => pure(b)
      }
}
