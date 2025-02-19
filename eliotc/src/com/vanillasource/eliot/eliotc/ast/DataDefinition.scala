package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.Primitives.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.acceptIfAll

case class DataDefinition(
    name: Sourced[Token],
    genericParameters: Seq[GenericParameter]
)

object DataDefinition {
  given Show[DataDefinition] = (fd: DataDefinition) => s"${fd.name.show}"

  given ASTComponent[DataDefinition] = new ASTComponent[DataDefinition] {
    override val parser: Parser[Sourced[Token], DataDefinition] = for {
      _                 <- topLevelKeyword("data")
      name              <- acceptIfAll(isIdentifier, isUpperCase)("type name")
      genericParameters <- component[Seq[GenericParameter]]
    } yield DataDefinition(name, genericParameters)
  }
}
