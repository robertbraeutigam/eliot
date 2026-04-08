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

  def displaySnippet(sourced: Sourced[?]): CompilerIO[String] =
    getFact(SourceContent.Key(sourced.uri)).map(_.map { fact =>
      displaySnippet(
        fact.content.value,
        sourced.range.from.line - 1,
        sourced.range.from.col - 1,
        sourced.range.to.line - 1,
        sourced.range.to.col - 1,
        40
      )
    }.getOrElse("<unknown source>"))

  private def displaySnippet(
      source: String,
      startRow: Int,
      startCol: Int,
      endRow: Int,
      endCol: Int,
      maxLength: Int
  ): String =
    require(maxLength >= 3, "maxLength must be at least 3 to fit ellipsis")

    val lines    = source.split("\n", -1)
    val ellipsis = "..."

    /** Trim `text` to `limit` chars from the left, adding ellipsis on the right side. */
    def trimRight(text: String, limit: Int): String =
      if text.length <= limit then text
      else text.take(limit - ellipsis.length) + ellipsis

    /** Trim `text` to `limit` chars from the right, adding ellipsis on the left side. */
    def trimLeft(text: String, limit: Int): String =
      if text.length <= limit then text
      else ellipsis + text.takeRight(limit - ellipsis.length)

    /** Combine two parts with a middle ellipsis, trimming proportionally if needed. */
    def joinWithEllipsis(before: String, after: String): String =
      val candidate = before + ellipsis + after
      if candidate.length <= maxLength then candidate
      else
        val available   = maxLength - ellipsis.length
        val total       = before.length + after.length
        // allocate budget proportionally to each side's original length
        val beforeAlloc =
          if total == 0 then available / 2
          else math.round(available.toDouble * before.length / total).toInt
        val afterAlloc  = available - beforeAlloc
        before.take(beforeAlloc) + ellipsis + after.takeRight(afterAlloc)

    if startRow == endRow then
      // ── Single-line snippet ──────────────────────────────────────────────────
      val snippet = lines(startRow).substring(startCol, endCol)
      if snippet.length <= maxLength then snippet
      else
        // keep as much of both ends as possible, cut the middle
        val available = maxLength - ellipsis.length
        val prefixLen = available / 2
        val suffixLen = available - prefixLen
        snippet.take(prefixLen) + ellipsis + snippet.takeRight(suffixLen)
    else
      // ── Multi-line snippet ───────────────────────────────────────────────────
      // "before" = from startCol to end of startRow
      // "after"  = from beginning of endRow to endCol
      val before = lines(startRow).substring(startCol)
      val after  = lines(endRow).substring(0, endCol)
      joinWithEllipsis(before, after)

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
