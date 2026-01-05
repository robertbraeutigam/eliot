package com.vanillasource.eliot.eliotc.ast.fact

import cats.Eq
import cats.syntax.all.*
import ASTComponent.component
import Primitives.*
import com.vanillasource.eliot.eliotc.ast.fact.{ASTComponent, GenericParameter, TypeReference}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.acceptIfAll

case class GenericParameter(name: Sourced[String], genericParameters: Seq[TypeReference])

object GenericParameter {
  val signatureEquality: Eq[GenericParameter] = (x: GenericParameter, y: GenericParameter) =>
    x.name.value === y.name.value &&
      x.genericParameters.length === y.genericParameters.length &&
      (x.genericParameters zip y.genericParameters).forall(TypeReference.signatureEquality.eqv)

  given ASTComponent[Seq[GenericParameter]] = new ASTComponent[Seq[GenericParameter]] {
    override def parser: Parser[Sourced[Token], Seq[GenericParameter]] =
      optionalBracketedCommaSeparatedItems("[", genericParameter, "]")

    private val genericParameter = for {
      name              <- acceptIfAll(isUpperCase, isIdentifier)("generic type parameter")
      genericParameters <- optionalBracketedCommaSeparatedItems("[", component[TypeReference], "]")
    } yield GenericParameter(name.map(_.content), genericParameters)
  }
}
