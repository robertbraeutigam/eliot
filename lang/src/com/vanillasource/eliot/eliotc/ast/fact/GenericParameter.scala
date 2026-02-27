package com.vanillasource.eliot.eliotc.ast.fact

import cats.Eq
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.GenericParameter.AbilityConstraint
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{acceptIfAll, atLeastOnceSeparatedBy, optional, or}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class GenericParameter(
    name: Sourced[String],
    typeRestriction: TypeReference,
    abilityConstraints: Seq[AbilityConstraint]
)

object GenericParameter {
  case class AbilityConstraint(abilityName: Sourced[String], typeParameters: Seq[TypeReference])

  val signatureEquality: Eq[GenericParameter] = (x: GenericParameter, y: GenericParameter) =>
    x.name.value === y.name.value && TypeReference.signatureEquality.eqv(x.typeRestriction, y.typeRestriction)

  given ASTComponent[Seq[GenericParameter]] = new ASTComponent[Seq[GenericParameter]] {
    override def parser: Parser[Sourced[Token], Seq[GenericParameter]] =
      optionalBracketedCommaSeparatedItems("[", component[GenericParameter], "]")
  }

  given ASTComponent[GenericParameter] = new ASTComponent[GenericParameter] {
    private val abilityConstraintsParser =
      for {
        _                  <- symbol("~")
        abilityConstraints <- component[AbilityConstraint]
                                .atLeastOnceSeparatedBy(symbol("&"))
      } yield abilityConstraints

    override def parser: Parser[Sourced[Token], GenericParameter] =
      for {
        name               <- acceptIfAll(isUpperCase, isIdentifier)("generic type parameter")
        typeRestriction    <- (arityAsTypeRestriction(name.map(_.content)) or explicitTypeRestriction)
                                .optional()
                                .map(_.getOrElse(TypeReference(name.map(_ => "Type"), Seq.empty)))
        abilityConstraints <- abilityConstraintsParser.optional().map(_.getOrElse(Seq.empty))
      } yield GenericParameter(
        name.map(_.content),
        typeRestriction,
        abilityConstraints.map(ac => extendWithDefault(ac, name.map(_.content)))
      )

    /** When an ability constraint is defined [A ~ Show] (with no parameters), we add the generic parameter its declared
      * on as default.
      */
    private def extendWithDefault(
        abilityConstraint: AbilityConstraint,
        defaultGeneric: Sourced[String]
    ): AbilityConstraint =
      if (abilityConstraint.typeParameters.isEmpty) {
        AbilityConstraint(abilityConstraint.abilityName, Seq(TypeReference(defaultGeneric, Seq.empty)))
      } else {
        abilityConstraint
      }
  }

  /** Parses bracketed arity notation [_, _[_], ...] and converts to a TypeReference. Only succeeds when brackets are
    * present (consumed). This is the syntactic sugar form: [M[_]] is sugar for [M: Function[Type, Type]].
    */
  private def arityAsTypeRestriction(name: Sourced[String]): Parser[Sourced[Token], TypeReference] =
    bracketedCommaSeparatedItems("[", symbol("_") >> subArityParser(name), "]")
      .map(subArities => arityParamsToTypeReference(name, subArities))

  /** Recursive parser for nested arity parameters: handles [_[_]] by parsing the part after each _. */
  private def subArityParser(name: Sourced[String]): Parser[Sourced[Token], TypeReference] =
    optionalBracketedCommaSeparatedItems("[", symbol("_") >> subArityParser(name), "]")
      .map(subArities => arityParamsToTypeReference(name, subArities))

  /** Converts parsed arity parameters into a TypeReference. Empty params → Type. Non-empty → nested
    * Function[paramKind, ...Function[paramKind, Type]].
    */
  private def arityParamsToTypeReference(name: Sourced[String], params: Seq[TypeReference]): TypeReference =
    if (params.isEmpty) TypeReference(name.as("Type"), Seq.empty)
    else
      params.foldRight(TypeReference(name.as("Type"), Seq.empty)) { (param, acc) =>
        TypeReference(name.as("Function"), Seq(param, acc))
      }

  /** Parses explicit type restriction syntax: `: TypeReference`. */
  private val explicitTypeRestriction: Parser[Sourced[Token], TypeReference] =
    symbol(":") >> component[TypeReference]

  given ASTComponent[AbilityConstraint] = new ASTComponent[AbilityConstraint] {
    override def parser: Parser[Sourced[Token], AbilityConstraint] =
      for {
        name           <- acceptIfAll(isUpperCase, isIdentifier)("ability name")
        typeParameters <- optionalBracketedCommaSeparatedItems("[", component[TypeReference], "]")
      } yield AbilityConstraint(name.map(_.content), typeParameters)
  }
}
