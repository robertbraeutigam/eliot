package com.vanillasource.parser

import cats.{Functor, Monad}

sealed trait ParserResult[A] {
  def fold[B](ifEmpty: => B)(f: A => B): B
}

object ParserResult {

  /** Parser was successful and produces a value.
    */
  case class Success[A](a: A) extends ParserResult[A] {
    override def fold[B](ifEmpty: => B)(f: A => B): B = f(a)
  }

  /** Parser wants to be skipped. Normally this means "failed without consuming input", but its meaning might extend to
    * other cases depending on the combinator used.
    */
  case class Skip[A](expected: String) extends ParserResult[A] {
    override def fold[B](ifEmpty: => B)(f: A => B): B = ifEmpty
  }

  /** Means that the parser failed in mid-parsing, and there's no safe way to continue parsing.
    */
  case class Error[A](expected: String) extends ParserResult[A] {
    override def fold[B](ifEmpty: => B)(f: A => B): B = ifEmpty
  }

  given Functor[ParserResult] = new Functor[ParserResult]:
    override def map[A, B](fa: ParserResult[A])(f: A => B): ParserResult[B] = ???

  given Monad[ParserResult] = new Monad[ParserResult]:
    override def pure[A](a: A): ParserResult[A] = Success(a)

    override def flatMap[A, B](fa: ParserResult[A])(f: A => ParserResult[B]): ParserResult[B] = fa match
      case Success(a)      =>
        f(a) match
          case Success(b)      => Success(b)
          case Skip(expected)  => Error(expected)
          case Error(expected) => Error(expected)
      case Skip(expected)  => Skip(expected)
      case Error(expected) => Error(expected)

    override def tailRecM[A, B](a: A)(f: A => ParserResult[Either[A, B]]): ParserResult[B] = ???
}
