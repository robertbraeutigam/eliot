package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.Primitives.*
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.*

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
          .attemptPhraseTo(topLevel.void `or` endOfInput())
          .anyTimesWhile(topLevelKeyword("import").find())
          .map(_.flatten)
      definitions      <-
        (component[FunctionDefinition] `xor` component[DataDefinition])
          .attemptPhraseTo(topLevel.void `or` endOfInput())
          .anyTimesWhile(any())
          .map(_.flatten)
    } yield AST(importStatements, definitions.flatMap(_.left.toSeq), definitions.flatMap(_.toSeq))

    private def topLevel = acceptIf(isTopLevel, "top level definition")
  }
}
