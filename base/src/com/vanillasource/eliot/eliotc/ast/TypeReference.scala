package com.vanillasource.eliot.eliotc.ast

import cats.{Eq, Show}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.Primitives.*
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.acceptIfAll

case class TypeReference(typeName: Sourced[String], genericParameters: Seq[TypeReference])

object TypeReference {
  val signatureEquality: Eq[TypeReference] = (x: TypeReference, y: TypeReference) =>
    x.typeName.value === y.typeName.value &&
      x.genericParameters.length === y.genericParameters.length &&
      (x.genericParameters zip y.genericParameters).forall(signatureEquality.eqv)

  given Show[TypeReference] = _.typeName.value

  given ASTComponent[TypeReference] = new ASTComponent[TypeReference] {
    override val parser: Parser[Sourced[Token], TypeReference] = for {
      name              <- acceptIfAll(isIdentifier, isUpperCase)("type name")
      genericParameters <- optionalBracketedCommaSeparatedItems("[", parser, "]")
    } yield TypeReference(name.map(_.content), genericParameters)
  }
}
