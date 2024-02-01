package com.vanillasource.eliot.eliotc.ast

import cats.Show
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.token.Token.{Identifier, Keyword, Symbol}
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.*

object TokenParser {
  lazy val astParser: Parser[Sourced[Token], AST] = {
    for {
      importStatements <- importStatement.saveError().findAt(acceptIf(isTopLevel)).atomic().anyTimes()
    } yield AST(importStatements)
  }

  private lazy val importStatement = for {
    keyword      <- topLevelKeyword("import")
    packageNames <- (packageNameOnSameLineAs(keyword) <* symbol(".")).anyTimes()
    moduleName   <- moduleNameOnSameLineAs(keyword)
  } yield ImportStatement(keyword, packageNames, moduleName)

  // TODO: expected package name OR module name
  private def moduleNameOnSameLineAs(keyword: Sourced[Token]) =
    acceptIfAll(isIdentifier, isUpperCase, isOnSameLineAs(keyword))("module name")

  private def packageNameOnSameLineAs(keyword: Sourced[Token]) =
    acceptIfAll(isIdentifier, isLowerCase, isOnSameLineAs(keyword))("package name")

  private def isOnSameLineAs(sample: Sourced[Token])(st: Sourced[Token]) = sample.range.to.line === st.range.from.line

  private def isUpperCase(st: Sourced[Token]) = st.value.content.charAt(0).isUpper

  private def isLowerCase(st: Sourced[Token]) = st.value.content.charAt(0).isLower

  private def topLevelKeyword(word: String) =
    acceptIfAll(isTopLevel, isKeyword, hasContent(word))(s"top level keyword '$word'")

  private def hasContent(content: String)(st: Sourced[Token]) = st.value.content === content

  private def symbol(s: String) = acceptIfAll(isSymbol, hasContent(s))(s"symbol '$s'")

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

  given Show[Token] = {
    case Identifier(content)          => s"identifier '$content'"
    case Symbol(content)              => s"symbol '$content'"
    case Keyword(content)             => s"keyword '$content'"
    case Token.NumberLiteral(content) => s"number literal '$content'"
  }
}
