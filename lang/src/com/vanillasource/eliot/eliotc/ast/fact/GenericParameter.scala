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

/** A generic-parameter binder, e.g. `MIN: BigInteger` in `type Int[MIN: BigInteger, MAX: BigInteger]`.
  *
  * @param inferable
  *   True when the binder is marked `auto` (`type Int[auto MIN, auto MAX]`), declaring it *omittable* at use sites: the
  *   compiler may supply it rather than the user spelling it. Surface keyword for the implicit/inferred-generics
  *   feature. Purely informational for now — saturation is a later work item; a bare
  *   under-applied constructor still errors exactly as before.
  */
case class GenericParameter(
    name: Sourced[String],
    typeRestriction: Sourced[Expression],
    abilityConstraints: Seq[AbilityConstraint],
    inferable: Boolean = false
)

object GenericParameter {
  case class AbilityConstraint(abilityName: Sourced[String], typeParameters: Seq[Sourced[Expression]])

  val signatureEquality: Eq[GenericParameter] = (x: GenericParameter, y: GenericParameter) =>
    x.name.value === y.name.value && x.typeRestriction.value.show === y.typeRestriction.value.show

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
        // `auto` is a soft keyword (a plain lowercase identifier, like `opaque`/`left`), unambiguous here because a
        // generic-parameter name is always upper-case: a lowercase `auto` can only be the marker.
        inferable          <- identifierWith("auto").as(true).optional().map(_.getOrElse(false))
        name               <- acceptIfAll(isUpperCase, isIdentifier)("generic type parameter")
        typeRestriction    <- (arityAsTypeRestriction(name.map(_.content)) or explicitTypeRestriction)
                                .optional()
                                .map(_.getOrElse(name.as(typeExpr(name.map(_ => "Type")))))
        abilityConstraints <- abilityConstraintsParser.optional().map(_.getOrElse(Seq.empty))
      } yield GenericParameter(
        name.map(_.content),
        typeRestriction,
        abilityConstraints.map(ac => extendWithDefault(ac, name.map(_.content))),
        inferable
      )

    /** When an ability constraint is defined [A ~ Show] (with no parameters), we add the generic parameter its declared
      * on as default.
      */
    private def extendWithDefault(
        abilityConstraint: AbilityConstraint,
        defaultGeneric: Sourced[String]
    ): AbilityConstraint =
      if (abilityConstraint.typeParameters.isEmpty) {
        AbilityConstraint(abilityConstraint.abilityName, Seq(defaultGeneric.as(typeExpr(defaultGeneric))))
      } else {
        abilityConstraint
      }
  }

  /** Parses bracketed arity notation [_, _[_], ...] and converts to an Expression. Only succeeds when brackets are
    * present (consumed). This is the syntactic sugar form: [M[_]] is sugar for [M: Function[Type, Type]].
    */
  private def arityAsTypeRestriction(name: Sourced[String]): Parser[Sourced[Token], Sourced[Expression]] =
    bracketedCommaSeparatedItems("[", symbol("_") >> subArityParser(name), "]")
      .map(subArities => arityParamsToExpression(name, subArities))

  /** Recursive parser for nested arity parameters: handles [_[_]] by parsing the part after each _. */
  private def subArityParser(name: Sourced[String]): Parser[Sourced[Token], Sourced[Expression]] =
    optionalBracketedCommaSeparatedItems("[", symbol("_") >> subArityParser(name), "]")
      .map(subArities => arityParamsToExpression(name, subArities))

  /** Converts parsed arity parameters into an Expression. Empty params -> Type. Non-empty -> nested Function[paramKind,
    * ...Function[paramKind, Type]].
    */
  private def arityParamsToExpression(name: Sourced[String], params: Seq[Sourced[Expression]]): Sourced[Expression] =
    if (params.isEmpty) name.as(typeExpr(name.as("Type")))
    else
      params.foldRight(name.as(typeExpr(name.as("Type")))) { (param, acc) =>
        name.as(typeExpr(name.as("Function"), Seq(param, acc)))
      }

  /** Parses explicit type restriction syntax: `: Expression`. The bound uses `typeRunParser`, so it admits a bare type
    * operator (the run stops at the `~` ability-constraint marker, the `,` separator, and the closing `]`).
    */
  private val explicitTypeRestriction: Parser[Sourced[Token], Sourced[Expression]] =
    symbol(":") >> sourced(Expression.typeRunParser)

  /** Helper to create a type expression from a name and optional generic arguments. */
  private def typeExpr(name: Sourced[String], genericArgs: Seq[Sourced[Expression]] = Seq.empty): Expression =
    Expression.FunctionApplication(None, name, Option.when(genericArgs.nonEmpty)(genericArgs), Seq.empty)

  given ASTComponent[AbilityConstraint] = new ASTComponent[AbilityConstraint] {
    override def parser: Parser[Sourced[Token], AbilityConstraint] =
      for {
        name           <- acceptIfAll(isUpperCase, isIdentifier)("ability name")
        typeParameters <- optionalBracketedCommaSeparatedItems("[", sourced(Expression.typeRunParser), "]")
      } yield AbilityConstraint(name.map(_.content), typeParameters)
  }
}
