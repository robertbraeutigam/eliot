package com.vanillasource.eliot.eliotc.ast.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.{isIdentifier, symbol}
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{acceptIf, optional}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class LambdaParameterDefinition(name: Sourced[String], typeReference: Option[TypeReference])

object LambdaParameterDefinition {
  given Show[LambdaParameterDefinition] = _.name.show

  given ASTComponent[LambdaParameterDefinition] = new ASTComponent[LambdaParameterDefinition] {
    override def parser: Parser[Sourced[Token], LambdaParameterDefinition] = for {
      name               <- acceptIf(isIdentifier, "lambda parameter name")
      typeReferenceMaybe <- (symbol(":") >> component[TypeReference]).optional()
    } yield LambdaParameterDefinition(name.map(_.content), typeReferenceMaybe)
  }
}
