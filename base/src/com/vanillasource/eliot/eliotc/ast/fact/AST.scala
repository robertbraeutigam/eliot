package com.vanillasource.eliot.eliotc.ast.fact

import cats.Show
import cats.syntax.all.*
import ASTComponent.component
import Primitives.*
import com.vanillasource.eliot.eliotc.ast.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import Parser.*

case class AST(
    importStatements: Seq[ImportStatement],
    functionDefinitions: Seq[FunctionDefinition],
    typeDefinitions: Seq[DataDefinition]
)

object AST {
  given Show[AST] = (ast: AST) =>
    s"import statements: ${ast.importStatements
        .map(_.show)
        .mkString(", ")}, function definitions: ${ast.functionDefinitions
        .map(_.show)
        .mkString(", ")}, type definitions: ${ast.typeDefinitions.map(_.show).mkString(", ")}"

  given ASTComponent[AST] = new ASTComponent[AST] {
    override def parser: Parser[Sourced[Token], AST] = for {
      importStatements <-
        component[ImportStatement]
          .attemptPhraseTo(anyKeyword.void or endOfInput())
          .anyTimesWhile(keyword("import").find())
          .map(_.flatten)
      definitions      <-
        (component[FunctionDefinition] xor component[DataDefinition])
          .attemptPhraseTo(anyKeyword.void or endOfInput())
          .anyTimesWhile(any())
          .map(_.flatten)
    } yield AST(importStatements, definitions.flatMap(_.left.toSeq), definitions.flatMap(_.toSeq))
  }

  private def anyKeyword = acceptIf(isKeyword)
}
