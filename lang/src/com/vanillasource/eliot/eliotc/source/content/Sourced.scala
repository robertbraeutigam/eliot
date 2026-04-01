package com.vanillasource.eliot.eliotc.source.content

import cats.effect.IO
import cats.implicits.*
import cats.{Functor, Show}
import com.vanillasource.eliot.eliotc.feedback.{CompilerError, Logging}
import com.vanillasource.eliot.eliotc.feedback.User.*
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

import java.net.URI

/** A value of generic type transformed from a given snippet of code inside a given source code file.
  */
case class Sourced[+T](uri: URI, range: PositionRange, value: T) {
  def reFocus(newRange: PositionRange): Sourced[T] = copy(range = newRange)
}

object Sourced extends Logging {

  /** Issue a compiler error based on sourced content.
    */
  def compilerError(message: Sourced[String], description: Seq[String] = Seq.empty): CompilerIO[Unit] =
    for {
      sourceContent <- getFact(SourceContent.Key(message.uri))
      _             <- sourceContent match {
                         case Some(sc) => compilerErrorWithContent(message, sc.content.value, description)
                         case None     =>
                           error[CompilerIO](s"Source content not found for URI: ${message.uri}") >>
                             compilerGlobalError(message.value).to[CompilerIO] >>
                             registerCompilerError(
                               CompilerError(
                                 message.value,
                                 description,
                                 Option(message.uri.getPath).getOrElse("<unknown>"),
                                 "",
                                 PositionRange.zero
                               )
                             )
                       }
    } yield ()

  def compilerErrorWithContent(
      message: Sourced[String],
      content: String,
      description: Seq[String] = Seq.empty
  ): CompilerIO[Unit] =
    registerCompilerError(
      CompilerError(
        message.value,
        description,
        message.uri.getPath,
        content,
        message.range
      )
    )

  def compilerAbort[T](message: Sourced[String], description: Seq[String] = Seq.empty): CompilerIO[T] =
    compilerError(message, description) *> abort[T]

  given Functor[Sourced] = new Functor[Sourced] {
    override def map[A, B](fa: Sourced[A])(f: A => B): Sourced[B] = Sourced(fa.uri, fa.range, f(fa.value))
  }

  given [T]: Show[Sourced[T]] = (t: Sourced[T]) => s"${t.value.toString} (${t.range.show})"

  def outline(ss: Seq[Sourced[?]]): Sourced[Unit] = ss match
    case head :: _ => Sourced(head.uri, PositionRange(ss.map(_.range.from).min, ss.map(_.range.to).max), ())
    case _         => throw IllegalArgumentException("can't produce an outline of empty sourced values")
}
