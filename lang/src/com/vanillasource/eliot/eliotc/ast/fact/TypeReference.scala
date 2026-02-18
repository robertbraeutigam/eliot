package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import Primitives.*
import com.vanillasource.eliot.eliotc.ast.fact.{ASTComponent, TypeReference}
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import Parser.acceptIfAll

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
