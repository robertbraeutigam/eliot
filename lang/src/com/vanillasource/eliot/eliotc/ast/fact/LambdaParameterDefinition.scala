package com.vanillasource.eliot.eliotc.ast.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.{isIdentifier, sourced, symbol}
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{acceptIf, optional, or}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class LambdaParameterDefinition(name: Sourced[String], typeExpression: Option[Sourced[Expression]])

object LambdaParameterDefinition {
  given Show[LambdaParameterDefinition] = _.name.show

  given ASTComponent[LambdaParameterDefinition] = new ASTComponent[LambdaParameterDefinition] {
    override def parser: Parser[Sourced[Token], LambdaParameterDefinition] = for {
      name               <- acceptIf(isIdentifier, "lambda parameter name") or symbol("_")
      typeExpressionMaybe <- (symbol(":") >> sourced(Expression.typeParser)).optional()
    } yield LambdaParameterDefinition(name.map(_.content), typeExpressionMaybe)
  }
}
