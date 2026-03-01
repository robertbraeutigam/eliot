package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import Primitives.{isIdentifier, sourced, symbol}
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import Parser.acceptIf

case class ArgumentDefinition(name: Sourced[String], typeExpression: Sourced[Expression])

object ArgumentDefinition {
  val signatureEquality: Eq[ArgumentDefinition] = (x: ArgumentDefinition, y: ArgumentDefinition) =>
    x.name.value === y.name.value && x.typeExpression.value.show === y.typeExpression.value.show

  given Show[ArgumentDefinition] = _.name.show

  given ASTComponent[ArgumentDefinition] = new ASTComponent[ArgumentDefinition] {
    override def parser: Parser[Sourced[Token], ArgumentDefinition] = for {
      name           <- acceptIf(isIdentifier, "argument name")
      _              <- symbol(":")
      typeExpression <- sourced(Expression.typeParser)
    } yield ArgumentDefinition(name.map(_.content), typeExpression)
  }
}
