package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.Primitives.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.acceptIfAll

case class TypeReference(typeName: Sourced[Token])

object TypeReference {
  given Show[TypeReference] = _.typeName.value.content

  given ASTComponent[TypeReference] = new ASTComponent[TypeReference] {
    override val parser: Parser[Sourced[Token], TypeReference] =
      symbol(":") *> acceptIfAll(isIdentifier, isUpperCase)("type name").map(TypeReference(_))
  }
}
