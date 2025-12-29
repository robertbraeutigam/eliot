package com.vanillasource.eliot.eliotc.ast

import cats.{Eq, Show}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.Primitives.{isIdentifier, symbol}
import com.vanillasource.eliot.eliotc.pos.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.acceptIf

case class ArgumentDefinition(name: Sourced[String], typeReference: TypeReference)

object ArgumentDefinition {
  val signatureEquality: Eq[ArgumentDefinition] = (x: ArgumentDefinition, y: ArgumentDefinition) =>
    x.name.value === y.name.value && TypeReference.signatureEquality.eqv(x.typeReference, y.typeReference)

  given Show[ArgumentDefinition] = _.name.show

  given ASTComponent[ArgumentDefinition] = new ASTComponent[ArgumentDefinition] {
    override def parser: Parser[Sourced[Token], ArgumentDefinition] = for {
      name          <- acceptIf(isIdentifier, "argument name")
      _             <- symbol(":")
      typeReference <- component[TypeReference]
    } yield ArgumentDefinition(name.map(_.content), typeReference)
  }
}
