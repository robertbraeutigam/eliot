package com.vanillasource.eliot.eliotc.ast.fact

import cats.Eq
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.GenericParameter.{AbilityConstraint, Arity}
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{acceptIfAll, atLeastOnceSeparatedBy, optional}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class GenericParameter(
    name: Sourced[String],
    arity: Arity,
    abilityConstraints: Seq[AbilityConstraint]
)

object GenericParameter {
  case class Arity(parameters: Seq[Arity])

  given Eq[Arity] = Eq.fromUniversalEquals

  case class AbilityConstraint(abilityName: Sourced[String], typeParameters: Seq[TypeReference])

  val signatureEquality: Eq[GenericParameter] = (x: GenericParameter, y: GenericParameter) =>
    x.name.value === y.name.value && x.arity === y.arity

  given ASTComponent[Seq[GenericParameter]] = new ASTComponent[Seq[GenericParameter]] {
    override def parser: Parser[Sourced[Token], Seq[GenericParameter]] =
      optionalBracketedCommaSeparatedItems("[", genericParameter, "]")

    private val abilityConstraintsParser =
      for {
        _                  <- symbol("~")
        abilityConstraints <- component[AbilityConstraint]
                                .atLeastOnceSeparatedBy(symbol("&"))
      } yield abilityConstraints

    private val genericParameter = for {
      name               <- acceptIfAll(isUpperCase, isIdentifier)("generic type parameter")
      arity              <- component[Arity]
      abilityConstraints <- abilityConstraintsParser.optional().map(_.getOrElse(Seq.empty))
    } yield GenericParameter(name.map(_.content), arity, abilityConstraints)
  }

  given ASTComponent[Arity] = new ASTComponent[Arity] {
    override def parser: Parser[Sourced[Token], Arity] =
      for {
        arities <- optionalBracketedCommaSeparatedItems("[", symbol("_") >> parser, "]")
      } yield Arity(arities)
  }

  given ASTComponent[AbilityConstraint] = new ASTComponent[AbilityConstraint] {
    override def parser: Parser[Sourced[Token], AbilityConstraint] =
      for {
        name           <- acceptIfAll(isUpperCase, isIdentifier)("ability name")
        typeParameters <- optionalBracketedCommaSeparatedItems("[", component[TypeReference], "]")
      } yield AbilityConstraint(name.map(_.content), typeParameters)
  }
}
