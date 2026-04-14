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
        val typeAliasFunctions   =
          items.flatMap(_.toOption).flatMap(_.toOption).flatMap(_.toOption).flatMap(_.left.toOption)
        val abilities            =
          items.flatMap(_.toOption).flatMap(_.toOption).flatMap(_.toOption).flatMap(_.toOption).flatMap(_.left.toOption)
        val abilityErrors        = abilities.flatMap(_._1)
        val abilityFunctions     = abilities.flatMap(_._2)
        val abilitiesImpl        =
          items.flatMap(_.toOption).flatMap(_.toOption).flatMap(_.toOption).flatMap(_.toOption).flatMap(_.toOption)
        val abilityImplErrors    = abilitiesImpl.flatMap(_._1)
        // Assign a deterministic per-ability index to each implement block, in source order.
        // All functions emitted from the same block share the same index (they were parsed
        // together as one Seq[FunctionDefinition]). The index disambiguates between multiple
        // implementations of the same ability within this module.
        val counters             = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
        val abilityImplFunctions = abilitiesImpl.flatMap { case (_, blockFunctions) =>
          blockFunctions.headOption.flatMap(_.name.value.qualifier match {
            case Qualifier.AbilityImplementation(abilityName, _) => Some(abilityName)
            case _                                               => None
          }) match {
            case Some(abilityName) =>
              val idx = counters(abilityName.value)
              counters.update(abilityName.value, idx + 1)
              blockFunctions.map { f =>
                val newQualifier = f.name.value.qualifier match {
                  case Qualifier.AbilityImplementation(n, _) => Qualifier.AbilityImplementation(n, idx)
                  case other                                 => other
                }
                f.copy(name = f.name.map(qn => QualifiedName(qn.name, newQualifier)))
              }
            case None              => blockFunctions
          }
        }
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
