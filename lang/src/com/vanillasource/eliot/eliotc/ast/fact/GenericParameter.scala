package com.vanillasource.eliot.eliotc.ast.fact

import cats.Eq
import cats.syntax.all.*
import ASTComponent.component
import Primitives.*
import com.vanillasource.eliot.eliotc.ast.fact.{ASTComponent, GenericParameter}
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import Parser.acceptIfAll
import com.vanillasource.eliot.eliotc.ast.fact.GenericParameter.Arity

case class GenericParameter(
    name: Sourced[String],
    arity: Arity
)

object GenericParameter {
  case class Arity(parameters: Seq[Arity])

  given Eq[Arity] = Eq.fromUniversalEquals

  val signatureEquality: Eq[GenericParameter] = (x: GenericParameter, y: GenericParameter) =>
    x.name.value === y.name.value && x.arity === y.arity

  given ASTComponent[Seq[GenericParameter]] = new ASTComponent[Seq[GenericParameter]] {
    override def parser: Parser[Sourced[Token], Seq[GenericParameter]] =
      optionalBracketedCommaSeparatedItems("[", genericParameter, "]")

    private val genericParameter = for {
      name  <- acceptIfAll(isUpperCase, isIdentifier)("generic type parameter")
      arity <- component[Arity]
    } yield GenericParameter(name.map(_.content), arity)
  }

  given ASTComponent[Arity] = new ASTComponent[Arity] {
    override def parser: Parser[Sourced[Token], Arity] =
      for {
        arities <- optionalBracketedCommaSeparatedItems("[", symbol("_") >> parser, "]")
      } yield Arity(arities)
  }

}
