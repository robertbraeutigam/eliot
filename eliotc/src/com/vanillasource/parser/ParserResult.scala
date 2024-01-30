package com.vanillasource.parser

import cats.Monad
import cats.syntax.all.*
import com.vanillasource.parser.ParserError.given
import com.vanillasource.parser.ParserResult.Consume
import com.vanillasource.parser.ParserResult.Consume.{Consumed, NotConsumed}

/** A result of a parser parsing some input.
  * @param consume
  *   Whether any input items were consumed or not. Note that if input was consumed it means that some prefix of a valid
  *   pattern was consumed.
  * @param currentError
  *   The current error, if there was any.
  * @param value
  *   The value that could be parsed, None if parsing failed. Note that the availability of content is completely
  *   independent of whether any input was consumed. All combinations are valid states of parsing.
  */
sealed case class ParserResult[A](consume: Consume, currentError: ParserError, value: Option[A])

object ParserResult {
  enum Consume {
    case Consumed, NotConsumed
  }

  given Monad[ParserResult] = new Monad[ParserResult]:
    override def pure[A](a: A): ParserResult[A] = ParserResult(NotConsumed, ParserError.noError, Some(a))

    override def flatMap[A, B](fa: ParserResult[A])(f: A => ParserResult[B]): ParserResult[B] = fa match
      case ParserResult(consume, currentError, Some(a)) =>
        f(a) match
          case ParserResult(NotConsumed, error, value) => ParserResult(consume, currentError |+| error, value)
          case ParserResult(Consumed, error, value)    => ParserResult(Consumed, error, value)
      case err                                          => err.asInstanceOf[ParserResult[B]]

    // TODO: this is not stack-safe
    override def tailRecM[A, B](a: A)(f: A => ParserResult[Either[A, B]]): ParserResult[B] =
      flatMap(f(a)) {
        case Left(newA) => tailRecM(newA)(f)
        case Right(b)   => pure(b)
      }
}
