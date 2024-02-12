package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.data.IndexedStateT
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import cats.syntax.all.*
import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.ast.Expression.FunctionApplication
import com.vanillasource.eliot.eliotc.ast.FunctionBody.{Native, NonNative}
import com.vanillasource.eliot.eliotc.token.Token.{Identifier, Keyword, Symbol}
import com.vanillasource.parser.{InputStream, Parser, ParserResult}
import com.vanillasource.parser.Parser.*

object TokenParser {
  lazy val astParser: Parser[Sourced[Token], AST] = {
    for {
      importStatements    <-
        importStatement
          .attemptPhraseTo(topLevel.void or endOfInput())
          .anyTimesWhile(topLevelKeyword("import").find())
      functionDefinitions <-
        functionDefinition
          .attemptPhraseTo(topLevel.void or endOfInput())
          .anyTimesWhile(any())
    } yield AST(importStatements.flatten, functionDefinitions.flatten)
  }.fully()

  private lazy val importStatement = for {
    keyword      <- topLevelKeyword("import")
    packageNames <- (packageNameOnSameLineAs(keyword) <* symbol(".")).anyTimes()
    moduleName   <- moduleNameOnSameLineAs(keyword)
  } yield ImportStatement(keyword, packageNames, moduleName)

  private lazy val functionDefinition = for {
    name         <- acceptIfAll(isTopLevel, isIdentifier, isLowerCase)("function name")
    args         <- argumentListOf(acceptIf(isIdentifier, "argument name"))
    _            <- symbol("=")
    functionBody <- nativeFunctionBody(args) or nonNativeFunctionBody(args)
  } yield FunctionDefinition(name, functionBody)

  private def nativeFunctionBody(args: Seq[Sourced[Token]]): Parser[Sourced[Token], FunctionBody] =
    keyword("native").map(keyword => Native(keyword, args))

  private def nonNativeFunctionBody(args: Seq[Sourced[Token]]): Parser[Sourced[Token], FunctionBody] =
    expression.map(body => NonNative(args, body))

  private lazy val expression: Parser[Sourced[Token], Tree[Expression]] = functionApplication

  private lazy val functionApplication: Parser[Sourced[Token], Tree[Expression]] = for {
    name <- acceptIf(isIdentifier, "function name")
    args <- argumentListOf(expression)
  } yield Tree(FunctionApplication(name), args)

  private def argumentListOf[A](item: Parser[Sourced[Token], A]): Parser[Sourced[Token], Seq[A]] = {
    val nonEmpty = for {
      _     <- symbol("(")
      items <- (item <* symbol(",")).atLeastOnce()
      _     <- symbol(")")
    } yield items

    nonEmpty.optional().map(_.getOrElse(Seq.empty))
  }

  private def moduleNameOnSameLineAs(keyword: Sourced[Token]) =
    acceptIfAll(isIdentifier, isUpperCase, isOnSameLineAs(keyword))("module name")

  private def packageNameOnSameLineAs(keyword: Sourced[Token]) =
    acceptIfAll(isIdentifier, isLowerCase, isOnSameLineAs(keyword))("package name")

  private def isOnSameLineAs(sample: Sourced[Token])(st: Sourced[Token]) = sample.range.to.line === st.range.from.line

  private def topLevel = acceptIf(isTopLevel, "top level definition")

  private def isUpperCase(st: Sourced[Token]) = st.value.content.charAt(0).isUpper

  private def isLowerCase(st: Sourced[Token]) = st.value.content.charAt(0).isLower

  private def topLevelKeyword(word: String) =
    acceptIfAll(isTopLevel, isKeyword, hasContent(word))(s"top level keyword '$word'")

  private def hasContent(content: String)(st: Sourced[Token]) = st.value.content === content

  private def keyword(s: String) = acceptIfAll(isKeyword, hasContent(s))(s"keyword '$s'")

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
