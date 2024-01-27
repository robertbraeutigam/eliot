package com.vanillasource.parser

import cats.{Functor, Monad}

sealed trait ParserResult[A] {
  def fold[B](ifEmpty: => B)(f: A => B): B

  // TODO: make a proper API function to parse()
  def toEither: Either[String, A]
}

object ParserResult {

  /** Parser successfully got a value. */
  case class Success[A](consumed: Boolean, a: A) extends ParserResult[A] {
    override def fold[B](ifEmpty: => B)(f: A => B): B = f(a)

    override def toEither: Either[String, A] = Right(a)
  }

  /** Parser failed to get a valid value.
    */
  case class Failure[A](consumed: Boolean, expected: String) extends ParserResult[A] {
    override def fold[B](ifEmpty: => B)(f: A => B): B = ifEmpty

    override def toEither: Either[String, A] = Left(expected)
  }

  given Functor[ParserResult] = new Functor[ParserResult]:
    override def map[A, B](fa: ParserResult[A])(f: A => B): ParserResult[B] = ???

  given Monad[ParserResult] = new Monad[ParserResult]:
    override def pure[A](a: A): ParserResult[A] = Success(consumed = false, a)

    override def flatMap[A, B](fa: ParserResult[A])(f: A => ParserResult[B]): ParserResult[B] = fa match
      case Success(false, a)        => f(a)
      case Success(true, a)         =>
        f(a) match
          case Success(_, a2)       => Success(true, a2)
          case Failure(_, expected) => Failure(true, expected)
      case Failure(false, expected) => Failure(false, expected)
      case Failure(true, expected)  => Failure(true, expected)

    // TODO: this is not stack-safe
    override def tailRecM[A, B](a: A)(f: A => ParserResult[Either[A, B]]): ParserResult[B] =
      flatMap(f(a)) {
        case Left(newA) => tailRecM(newA)(f)
        case Right(b)   => pure(b)
      }
}
