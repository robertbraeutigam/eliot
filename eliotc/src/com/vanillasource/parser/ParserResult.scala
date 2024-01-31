package com.vanillasource.parser

import cats.Monad
import cats.syntax.all.*
import com.vanillasource.parser.ParserError.given
import com.vanillasource.parser.ParserResult.Consume
import com.vanillasource.parser.ParserResult.Consume.{Consumed, NotConsumed}

import scala.util.control.TailCalls.{TailRec, done, tailcall}

/** A result of a parser parsing some input.
  * @param consume
  *   Whether any input items were consumed or not. Note that if input was consumed it means that some prefix of a valid
  *   pattern was consumed.
  * @param currentError
  *   The current error, if there was any.
  * @param allErrors
  *   All the errors collected during parsing, if any.
  * @param value
  *   The value that could be parsed, None if parsing failed. Note that the availability of content is completely
  *   independent of whether any input was consumed. All combinations are valid states of parsing.
  */
sealed case class ParserResult[A](
    consume: Consume,
    currentError: ParserError,
    allErrors: Seq[ParserError],
    value: Option[A]
)

object ParserResult {
  enum Consume {
    case Consumed, NotConsumed
  }

  given Monad[ParserResult] = new Monad[ParserResult]:
    override def pure[A](a: A): ParserResult[A] = ParserResult(NotConsumed, ParserError.noError, Seq.empty, Some(a))

    override def flatMap[A, B](fa: ParserResult[A])(f: A => ParserResult[B]): ParserResult[B] = fa match
      case ParserResult(consume, currentError, allErrors, Some(a)) =>
        f(a) match
          case ParserResult(NotConsumed, error, allNextErrors, value) =>
            ParserResult(consume, currentError |+| error, allErrors ++ allNextErrors, value)
          case ParserResult(Consumed, error, allNextErrors, value)    =>
            ParserResult(Consumed, error, allErrors ++ allNextErrors, value)
      case err                                                     => err.asInstanceOf[ParserResult[B]]

    override def tailRecM[A, B](a: A)(f: A => ParserResult[Either[A, B]]): ParserResult[B] = {
      def trampoline(current: ParserResult[Either[A, B]]): TailRec[ParserResult[B]] = current.value match
        case None              => done(current.asInstanceOf[ParserResult[B]])
        case Some(Right(b))    => done(flatMap(current)(_ => pure(b)))
        case Some(Left(nextA)) => tailcall(trampoline(flatMap(current)(_ => f(nextA))))

      trampoline(f(a)).result
    }
}
