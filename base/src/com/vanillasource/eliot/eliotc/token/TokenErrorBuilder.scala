package com.vanillasource.eliot.eliotc.token

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source
import com.vanillasource.eliot.eliotc.source.{pos => posPackage}
import com.vanillasource.eliot.eliotc.source.pos.{PositionRange, Sourced}
import parsley.errors
import parsley.errors.{DefaultErrorBuilder, ErrorBuilder}
import parsley.errors.tokenextractors.SingleChar

import java.io.File

class TokenErrorBuilder(val sourced: Sourced[?]) extends ErrorBuilder[Sourced[String]] {
  def format(pos: Position, source: Source, lines: ErrorInfoLines): Sourced[String] =
    sourced.as(s"Parser error, ${lines.mkString(", ")}.").reFocus(PositionRange(pos, pos.next))

  type Position = posPackage.Position
  type Source   = Option[String]

  def pos(line: Int, col: Int): Position = posPackage.Position(line, col)

  def source(sourceName: Option[String]): Source = sourceName

  type ErrorInfoLines = Seq[String]

  def vanillaError(
      unexpected: UnexpectedLine,
      expected: ExpectedLine,
      reasons: Messages,
      line: LineInfo
  ): ErrorInfoLines = DefaultErrorBuilder.vanillaError(unexpected, expected, reasons, line)

  def specialisedError(msgs: Messages, line: LineInfo): ErrorInfoLines = ???

  type ExpectedItems = Option[String]
  type Messages      = Seq[Message]

  def combineExpectedItems(alts: Set[Item]): ExpectedItems = DefaultErrorBuilder.disjunct(alts)
  def combineMessages(alts: Seq[Message]): Messages        = DefaultErrorBuilder.combineMessages(alts)

  type UnexpectedLine = Option[String]
  type ExpectedLine   = Option[String]
  type Message        = String
  type LineInfo       = Seq[String]

  def unexpected(item: Option[Item]): UnexpectedLine = DefaultErrorBuilder.unexpected(item)

  def expected(alts: ExpectedItems): ExpectedLine = DefaultErrorBuilder.expected(alts)

  def reason(reason: String): Message = ???

  def message(msg: String): Message = ???

  def lineInfo(
      line: String,
      linesBefore: Seq[String],
      linesAfter: Seq[String],
      errorPointsAt: Int,
      errorWidth: Int
  ): LineInfo = Seq.empty

  val numLinesBefore: Int = 1
  val numLinesAfter: Int  = 1

  type Item       = String
  type Raw        = String
  type Named      = String
  type EndOfInput = String

  def raw(item: String): Raw     = DefaultErrorBuilder.raw(item)
  def named(item: String): Named = DefaultErrorBuilder.named(item)
  val endOfInput: EndOfInput     = "end of input"

  def unexpectedToken(
      cs: Iterable[Char],
      amountOfInputParserWanted: Int,
      lexicalError: Boolean
  ): parsley.errors.Token = SingleChar.unexpectedToken(cs)

}
