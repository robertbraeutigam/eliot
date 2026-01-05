package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import Primitives.*
import com.vanillasource.eliot.eliotc.ast.fact.{ASTComponent, ArgumentDefinition, DataDefinition}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.{acceptIfAll, optional}

case class DataDefinition(
    name: Sourced[String],
    genericParameters: Seq[GenericParameter],
    fields: Option[Seq[ArgumentDefinition]]
)

object DataDefinition {
  val signatureEquality: Eq[DataDefinition] = (x: DataDefinition, y: DataDefinition) =>
    x.genericParameters.length === y.genericParameters.length &&
      (x.genericParameters zip y.genericParameters).forall(GenericParameter.signatureEquality.eqv)

  given Show[DataDefinition] = (fd: DataDefinition) => s"${fd.name.show}"

  given ASTComponent[DataDefinition] = new ASTComponent[DataDefinition] {
    override val parser: Parser[Sourced[Token], DataDefinition] = for {
      _                 <- topLevelKeyword("data")
      name              <- acceptIfAll(isIdentifier, isUpperCase)("type name")
      genericParameters <- component[Seq[GenericParameter]]
      fields            <- bracketedCommaSeparatedItems("(", component[ArgumentDefinition], ")").optional()
    } yield DataDefinition(name.map(_.content), genericParameters, fields)
  }
}
