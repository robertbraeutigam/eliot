package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.Primitives.{argumentListOf, isIdentifier, isLowerCase, isTopLevel, symbol}
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.*

case class FunctionDefinition(
    name: Sourced[Token],
    args: Seq[ArgumentDefinition],
    typeDefinition: TypeReference,
    body: Option[Expression] // Can be empty for abstract functions
)

object FunctionDefinition {
  given Show[FunctionDefinition] = (fd: FunctionDefinition) =>
    s"${fd.name.show}(${fd.args.map(_.show).mkString(", ")}): ${fd.body.show}"

  given ASTComponent[FunctionDefinition] = new ASTComponent[FunctionDefinition] {
    override def parser: Parser[Sourced[Token], FunctionDefinition] = for {
      name          <- acceptIfAll(isTopLevel, isIdentifier, isLowerCase)("function name")
      args          <- argumentListOf(component[ArgumentDefinition])
      typeReference <- component[TypeReference]
      functionBody  <- functionBody()
    } yield FunctionDefinition(name, args, typeReference, functionBody)

    private def functionBody(): Parser[Sourced[Token], Option[Expression]] =
      (symbol("=") *> component[Expression]).optional()
  }
}
