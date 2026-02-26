package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import Parser.{acceptIfAll, optional, atLeastOnceSeparatedBy, xor}

case class DataDefinition(
    name: Sourced[String],
    genericParameters: Seq[GenericParameter],
    constructors: Option[Seq[DataConstructor]]
)

object DataDefinition {
  val signatureEquality: Eq[DataDefinition] = (x: DataDefinition, y: DataDefinition) =>
    x.genericParameters.length === y.genericParameters.length &&
      (x.genericParameters zip y.genericParameters).forall(GenericParameter.signatureEquality.eqv)

  given Show[DataDefinition] = (fd: DataDefinition) => s"${fd.name.show}"

  given ASTComponent[DataDefinition] = new ASTComponent[DataDefinition] {
    override val parser: Parser[Sourced[Token], DataDefinition] = for {
      _                 <- keyword("data")
      name              <- acceptIfAll(isIdentifier, isUpperCase)("type name")
      genericParameters <- component[Seq[GenericParameter]]
      constructorsOrFields <-
        ((symbol("=") >> component[DataConstructor].atLeastOnceSeparatedBy(symbol("|"))) xor
          bracketedCommaSeparatedItems("(", component[ArgumentDefinition], ")")).optional()
    } yield {
      val constructors = constructorsOrFields.map {
        case Left(ctors)   => ctors
        case Right(fields) => Seq(DataConstructor(name.map(_.content), fields))
      }
      DataDefinition(name.map(_.content), genericParameters, constructors)
    }
  }
}
