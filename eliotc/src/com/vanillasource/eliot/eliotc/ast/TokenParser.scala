package com.vanillasource.eliot.eliotc.ast

import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

import scala.util.parsing.combinator.Parsers
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.token.Token.{Identifier, Keyword, Symbol}

object TokenParser extends Parsers {
  override type Elem = Sourced[Token]

  lazy val astParser = for {
    importStatements <- importStatement.*
  } yield AST(importStatements)

  lazy val importStatement = for {
    keyword      <- topLevelKeyword("import")
    packageNames <- (packageNameOnSameLineAs(keyword) <~ symbol(".")).*
    moduleName   <- moduleNameOnSameLineAs(keyword)
  } yield ImportStatement(keyword, packageNames, moduleName)

  private def moduleNameOnSameLineAs(keyword: Sourced[Token]) =
    acceptIfAll(Seq(isIdentifier, isUpperCase, isOnSameLineAs(keyword)), "module name on same line as import")

  private def packageNameOnSameLineAs(keyword: Sourced[Token]) =
    acceptIfAll(Seq(isIdentifier, isLowerCase, isOnSameLineAs(keyword)), "package name on same line as import")

  private def isOnSameLineAs(sample: Sourced[Token])(st: Sourced[Token]) = sample.range.to.line === st.range.from.line

  private def isUpperCase(st: Sourced[Token]) = st.value.content.charAt(0).isUpper

  private def isLowerCase(st: Sourced[Token]) = st.value.content.charAt(0).isUpper

  private def topLevelKeyword(word: String) =
    acceptIfAll(Seq(isTopLevel, isKeyword, hasContent(word)), s"top level keyword '$word'")

  private def acceptIfAll(ps: Seq[Elem => Boolean], errorMessage: String): Parser[Sourced[Token]] =
    acceptIf(e => ps.forall(_.apply(e)))(_ => errorMessage)

  private def hasContent(content: String)(st: Sourced[Token]) = st.value.content === content

  private def symbol(s: String) = acceptIfAll(Seq(isSymbol, hasContent(s)), s"symbol '$s'")

  private def isKeyword(st: Sourced[Token]): Boolean = st match {
    case Sourced(range, Keyword(_)) => true
    case _                          => false
  }

  private def isIdentifier(st: Sourced[Token]): Boolean = st match {
    case Sourced(range, Identifier(_)) => true
    case _                             => false
  }

  private def isSymbol(st: Sourced[Token]): Boolean = st match {
    case Sourced(range, Symbol(_)) => true
    case _                         => false
  }

  private def isTopLevel(st: Sourced[Token]): Boolean = st.range.from.col === 1
}
