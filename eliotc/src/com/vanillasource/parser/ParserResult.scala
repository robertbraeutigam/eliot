package com.vanillasource.parser

import cats.{Functor, Monad}

sealed trait ParserResult[A] {
  def fold[B](ifEmpty: => B)(f: A => B): B
}

object ParserResult {

  /** Parser successfully got a value without parsing any input. */
  case class SuccessWithoutConsuming[A](a: A) extends ParserResult[A] {
    override def fold[B](ifEmpty: => B)(f: A => B): B = f(a)
  }

  /** Parser successfully parsed a value by consuming some input.
    */
  case class SuccessWithConsuming[A](a: A) extends ParserResult[A] {
    override def fold[B](ifEmpty: => B)(f: A => B): B = f(a)
  }

  /** Parser wants to be skipped. Normally this means "failed without consuming input", but its meaning might extend to
    * other cases depending on the combinator used.
    */
  case class FailedWithoutConsuming[A](expected: String) extends ParserResult[A] {
    override def fold[B](ifEmpty: => B)(f: A => B): B = ifEmpty
  }

  /** Means that the parser failed in mid-parsing, and there's no safe way to continue parsing.
    */
  case class FailedWithConsuming[A](expected: String) extends ParserResult[A] {
    override def fold[B](ifEmpty: => B)(f: A => B): B = ifEmpty
  }

  given Functor[ParserResult] = new Functor[ParserResult]:
    override def map[A, B](fa: ParserResult[A])(f: A => B): ParserResult[B] = ???

  given Monad[ParserResult] = new Monad[ParserResult]:
    override def pure[A](a: A): ParserResult[A] = SuccessWithoutConsuming(a)

    override def flatMap[A, B](fa: ParserResult[A])(f: A => ParserResult[B]): ParserResult[B] = fa match
      case SuccessWithoutConsuming(a)       => f(a)
      case SuccessWithConsuming(a)          =>
        f(a) match
          case SuccessWithoutConsuming(a)       => SuccessWithConsuming(a)
          case SuccessWithConsuming(a)          => SuccessWithConsuming(a)
          case FailedWithoutConsuming(expected) => FailedWithConsuming(expected)
          case FailedWithConsuming(expected)    => FailedWithConsuming(expected)
      case FailedWithoutConsuming(expected) => FailedWithoutConsuming(expected)
      case FailedWithConsuming(expected)    => FailedWithConsuming(expected)

    override def tailRecM[A, B](a: A)(f: A => ParserResult[Either[A, B]]): ParserResult[B] = ???
}
