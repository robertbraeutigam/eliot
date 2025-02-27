package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.Primitives.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.acceptIfAll

case class TypeReference(typeName: Sourced[Token], genericParameters: Seq[TypeReference])

object TypeReference {
  given Show[TypeReference] = _.typeName.value.content

  given ASTComponent[TypeReference] = new ASTComponent[TypeReference] {
    override val parser: Parser[Sourced[Token], TypeReference] = for {
      name              <- acceptIfAll(isIdentifier, isUpperCase)("type name")
      genericParameters <- optionalBracketedCommaSeparatedItems("[", parser, "]")
    } yield TypeReference(name, genericParameters)
  }
}
