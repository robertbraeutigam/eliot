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

  given ASTComponent[(Seq[ParserError], AST)] = new ASTComponent[(Seq[ParserError], AST)] {
    override def parser: Parser[Sourced[Token], (Seq[ParserError], AST)] =
      for {
        (errors, items) <-
          (component[ImportStatement] xor
            (component[FunctionDefinition] xor
              (component[DataDefinition] xor
                (TypeAliasDefinition.typeAliasDefinition.parser xor
                  (AbilityBlock.abilityBlock.parser xor
                    ImplementBlock.implementBlock.parser)))))
            .recoveringAnyTimes(isKeyword)
      } yield {
        val importStatements     = items.flatMap(_.left.toOption)
        val functionDefinitions  = items.flatMap(_.toOption).flatMap(_.left.toOption)
        val dataDefinitions      = items.flatMap(_.toOption).flatMap(_.toOption).flatMap(_.left.toOption)
        val typeAliasFunctions   = items.flatMap(_.toOption).flatMap(_.toOption).flatMap(_.toOption).flatMap(_.left.toOption)
        val abilities            = items.flatMap(_.toOption).flatMap(_.toOption).flatMap(_.toOption).flatMap(_.toOption).flatMap(_.left.toOption)
        val abilityErrors        = abilities.flatMap(_._1)
        val abilityFunctions     = abilities.flatMap(_._2)
        val abilitiesImpl        = items.flatMap(_.toOption).flatMap(_.toOption).flatMap(_.toOption).flatMap(_.toOption).flatMap(_.toOption)
        val abilityImplErrors    = abilitiesImpl.flatMap(_._1)
        val abilityImplFunctions = abilitiesImpl.flatMap(_._2)
        (
          errors ++ abilityErrors ++ abilityImplErrors,
          AST(
            importStatements,
            functionDefinitions ++ typeAliasFunctions ++ abilityFunctions ++ abilityImplFunctions,
            dataDefinitions
          )
        )
      }
  }
}
