package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.eliot.eliotc.token.Token.{Identifier, Keyword, Symbol}
import Parser.*

object Primitives {
  def optionalArgumentListOf[A](item: Parser[Sourced[Token], A]): Parser[Sourced[Token], Seq[A]] =
    optionalBracketedCommaSeparatedItems("(", item, ")")

  def bracketedCommaSeparatedItems[A](
      bracketStartSymbol: String,
      item: Parser[Sourced[Token], A],
      bracketEndSymbol: String
  ): Parser[Sourced[Token], Seq[A]] =
    item
      .atLeastOnceSeparatedBy(symbol(","))
      .between(symbol(bracketStartSymbol), symbol(bracketEndSymbol))

  def optionalBracketedCommaSeparatedItems[A](
      bracketStartSymbol: String,
      item: Parser[Sourced[Token], A],
      bracketEndSymbol: String
  ): Parser[Sourced[Token], Seq[A]] =
    item
      .atLeastOnceSeparatedBy(symbol(","))
      .between(symbol(bracketStartSymbol), symbol(bracketEndSymbol))
      .optional()
      .map(_.getOrElse(Seq.empty))

  def sourced[A](p: Parser[Sourced[Token], A]): Parser[Sourced[Token], Sourced[A]] =
    p.withBounds.map { case (result, first, last) =>
      Sourced(first.uri, PositionRange(first.range.from, last.range.to), result)
    }

  def symbol(s: String) = acceptIfAll(isSymbol, hasContent(s))(s"symbol '$s'")

  def keyword(word: String) =
    acceptIfAll(isKeyword, hasContent(word))(s"keyword '$word'")

  def hasContent(content: String)(st: Sourced[Token]) = st.value.content === content

  def isKeyword(st: Sourced[Token]): Boolean = st match {
    case Sourced(_, _, Keyword(_)) => true
    case _                         => false
  }

  def isUpperCase(st: Sourced[Token]) = st.value.content.charAt(0).isUpper

  def isLowerCase(st: Sourced[Token]) = st.value.content.charAt(0).isLower

  def isIdentifier(st: Sourced[Token]): Boolean = st match {
    case Sourced(_, _, Identifier(_)) => true
    case _                            => false
  }

  def isIntegerLiteral(st: Sourced[Token]): Boolean = st match {
    case Sourced(_, _, Token.IntegerLiteral(_)) => true
    case _                                      => false
  }

  def isStringLiteral(st: Sourced[Token]): Boolean = st match {
    case Sourced(_, _, Token.StringLiteral(_)) => true
    case _                                     => false
  }

  def isSymbol(st: Sourced[Token]): Boolean = st match {
    case Sourced(_, _, Symbol(_)) => true
    case _                        => false
  }

  private val reservedSymbols = Set("(", ")", "[", "]", "{", "}", ",", "->", "_", "::", ":", "~", "&", "=")

  def isUserOperator(st: Sourced[Token]): Boolean = isSymbol(st) && !reservedSymbols.contains(st.value.content)

  def isIdentifierOrSymbol(st: Sourced[Token]): Boolean = isIdentifier(st) || isUserOperator(st)

  def identifierWith(name: String) = acceptIfAll(isIdentifier, hasContent(name))(s"identifier '$name'")
}
