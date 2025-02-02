package com.vanillasource.eliot.eliotc.ast

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.eliot.eliotc.token.Token.{Identifier, Keyword, Symbol}
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.*

object Primitives {
  def argumentListOf[A](item: Parser[Sourced[Token], A]): Parser[Sourced[Token], Seq[A]] =
    bracketedCommaSeparatedItems("(", item, ")")

  def bracketedCommaSeparatedItems[A](
      bracketStartSymbol: String,
      item: Parser[Sourced[Token], A],
      bracketEndSymbol: String
  ): Parser[Sourced[Token], Seq[A]] =
    item
      .atLeastOnceSeparatedBy(symbol(","))
      .between(symbol(bracketStartSymbol), symbol(bracketEndSymbol))
      .optional()
      .map(_.getOrElse(Seq.empty))

  def symbol(s: String) = acceptIfAll(isSymbol, hasContent(s))(s"symbol '$s'")

  def topLevelKeyword(word: String) =
    acceptIfAll(isTopLevel, isKeyword, hasContent(word))(s"top level keyword '$word'")

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

  def isSymbol(st: Sourced[Token]): Boolean = st match {
    case Sourced(_, _, Symbol(_)) => true
    case _                        => false
  }

  def isTopLevel(st: Sourced[Token]): Boolean = st.range.from.col === 1

}
