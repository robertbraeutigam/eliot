package com.vanillasource.eliot.eliotc.ast.fact

import cats.Show
import cats.syntax.all.*
import ASTComponent.component
import Primitives.*
import com.vanillasource.eliot.eliotc.ast.*
import com.vanillasource.eliot.eliotc.ast.parser.{Parser, ParserError}
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

  def parse(tokens: Seq[Sourced[Token]]): (Seq[ParserError], AST) = {
    val topLevel =
      (component[ImportStatement] xor (component[FunctionDefinition] xor component[DataDefinition]))
        .recoveringAnyTimes(isKeyword)

    val result = topLevel.fully().parse(tokens)
    result.value match
      case Some((errors, items)) =>
        val importStatements    = items.flatMap(_.left.toOption)
        val functionDefinitions = items.flatMap(_.toOption).flatMap(_.left.toOption)
        val dataDefinitions     = items.flatMap(_.toOption).flatMap(_.toOption)
        (errors, AST(importStatements, functionDefinitions, dataDefinitions))
      case None                  =>
        (Seq(result.currentError), AST(Seq.empty, Seq.empty, Seq.empty))
  }
}
