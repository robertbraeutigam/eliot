package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.data.IndexedStateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.Primitives.*
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.Expression.{FunctionApplication, IntegerLiteral}
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.eliot.eliotc.token.Token.{Identifier, Keyword, Symbol}
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.{Parser, ParserResult}

object TokenParser {
  lazy val astParser: Parser[Sourced[Token], AST] = {
    for {
      importStatements <-
        component[ImportStatement]
          .attemptPhraseTo(topLevel.void or endOfInput())
          .anyTimesWhile(topLevelKeyword("import").find())
          .map(_.flatten)
      definitions      <-
        (functionDefinition xor component[DataDefinition])
          .attemptPhraseTo(topLevel.void or endOfInput())
          .anyTimesWhile(any())
          .map(_.flatten)
    } yield AST(importStatements, definitions.flatMap(_.left.toSeq), definitions.flatMap(_.toSeq))
  }.fully()

  private lazy val functionDefinition = for {
    name          <- acceptIfAll(isTopLevel, isIdentifier, isLowerCase)("function name")
    args          <- argumentListOf(argument())
    typeReference <- component[TypeReference]
    functionBody  <- functionBody()
  } yield FunctionDefinition(name, args, typeReference, functionBody)

  private def argument(): Parser[Sourced[Token], ArgumentDefinition] =
    for {
      name          <- acceptIf(isIdentifier, "argument name")
      typeReference <- component[TypeReference]
    } yield ArgumentDefinition(name, typeReference)

  private def functionBody(): Parser[Sourced[Token], Option[Expression]] =
    (symbol("=") *> component[Expression]).optional()

  private def topLevel = acceptIf(isTopLevel, "top level definition")

  given Show[Token] = {
    case Identifier(content)           => s"identifier '$content'"
    case Symbol(content)               => s"symbol '$content'"
    case Keyword(content)              => s"keyword '$content'"
    case Token.IntegerLiteral(content) => s"number literal '$content'"
  }
}
