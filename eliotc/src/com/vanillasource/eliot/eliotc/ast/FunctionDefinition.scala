package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.Primitives.{
  isIdentifier,
  isLowerCase,
  isTopLevel,
  isUpperCase,
  optionalArgumentListOf,
  optionalBracketedCommaSeparatedItems,
  sourced,
  symbol
}
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.*

case class FunctionDefinition(
    name: Sourced[String],
    genericParameters: Seq[GenericParameter],
    args: Seq[ArgumentDefinition],
    typeDefinition: TypeReference,
    body: Option[Sourced[Expression]] // Can be empty for abstract functions
)

object FunctionDefinition {
  given Show[FunctionDefinition] = (fd: FunctionDefinition) =>
    s"${fd.name.show}(${fd.args.map(_.show).mkString(", ")}): ${fd.body.show}"

  given ASTComponent[FunctionDefinition] = new ASTComponent[FunctionDefinition] {
    override val parser: Parser[Sourced[Token], FunctionDefinition] = for {
      name              <- acceptIfAll(isTopLevel, isIdentifier, isLowerCase)("function name")
      genericParameters <- component[Seq[GenericParameter]]
      args              <- optionalArgumentListOf(component[ArgumentDefinition])
      _                 <- symbol(":")
      typeReference     <- component[TypeReference]
      functionBody      <- functionBody
    } yield FunctionDefinition(name.map(_.content), genericParameters, args, typeReference, functionBody)

    private val functionBody =
      (symbol("=") *> sourced(component[Expression])).optional()
  }
}
