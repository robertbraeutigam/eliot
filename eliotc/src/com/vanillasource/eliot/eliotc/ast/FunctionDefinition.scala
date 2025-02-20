package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.Primitives.{
  argumentListOf,
  bracketedCommaSeparatedItems,
  isIdentifier,
  isLowerCase,
  isTopLevel,
  isUpperCase,
  symbol
}
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.*

case class FunctionDefinition(
    name: Sourced[Token],
    genericParameters: Seq[GenericParameter],
    args: Seq[ArgumentDefinition],
    typeDefinition: TypeReference,
    body: Option[Expression] // Can be empty for abstract functions
)

object FunctionDefinition {
  given Show[FunctionDefinition] = (fd: FunctionDefinition) =>
    s"${fd.name.show}(${fd.args.map(_.show).mkString(", ")}): ${fd.body.show}"

  given ASTComponent[FunctionDefinition] = new ASTComponent[FunctionDefinition] {
    override val parser: Parser[Sourced[Token], FunctionDefinition] = for {
      name              <- acceptIfAll(isTopLevel, isIdentifier, isLowerCase)("function name")
      genericParameters <- component[Seq[GenericParameter]]
      args              <- argumentListOf(component[ArgumentDefinition])
      _                 <- symbol(":")
      typeReference     <- component[TypeReference]
      functionBody      <- functionBody
    } yield FunctionDefinition(name, genericParameters, args, typeReference, functionBody)

    private val functionBody =
      (symbol("=") *> component[Expression]).optional()
  }
}
