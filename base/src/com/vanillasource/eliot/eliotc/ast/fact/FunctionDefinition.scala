package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import ASTComponent.component
import Primitives.*
import com.vanillasource.eliot.eliotc.ast.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
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
  val signatureEquality: Eq[FunctionDefinition] = (x: FunctionDefinition, y: FunctionDefinition) =>
    x.genericParameters.length === y.genericParameters.length &&
      x.args.length === y.args.length &&
      (x.genericParameters zip y.genericParameters).forall(GenericParameter.signatureEquality.eqv) &&
      (x.args zip y.args).forall(ArgumentDefinition.signatureEquality.eqv) &&
      TypeReference.signatureEquality.eqv(x.typeDefinition, y.typeDefinition)

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
