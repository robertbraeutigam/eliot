package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.Primitives.{isIdentifier, isUpperCase, topLevelKeyword}
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.acceptIfAll

case class DataDefinition(
    name: Sourced[Token]
)

object DataDefinition {
  given Show[DataDefinition] = (fd: DataDefinition) => s"${fd.name.show}"

  given ASTComponent[DataDefinition] = new ASTComponent[DataDefinition] {
    override val parser: Parser[Sourced[Token], DataDefinition] = for {
      _    <- topLevelKeyword("data")
      name <- acceptIfAll(isIdentifier, isUpperCase)("type name")
    } yield DataDefinition(name)
  }
}
