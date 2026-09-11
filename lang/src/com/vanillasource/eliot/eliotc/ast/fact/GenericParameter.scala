package com.vanillasource.eliot.eliotc.ast.fact

import cats.Eq
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{acceptIf, acceptIfAll, anyTimes, optional, or}
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

/** A generic-parameter binder, e.g. `MIN: BigInteger` in `type Int[MIN: BigInteger, MAX: BigInteger]`.
  *
  * @param inferable
  *   True when the binder is *omittable* at use sites: the compiler supplies it rather than the caller spelling it.
  *   There is no user surface for this — it is set only internally, by
  *   [[com.vanillasource.eliot.eliotc.core.processor.EffectSugarDesugarer]], to mark the **phantom binders** effects v6
  *   mints for a row entry and a `~` constraint (`docs/effects.md` §9.4 step 2), which the `row` phase writes at every
  *   reference so a caller never spells one. It is also what tells a minted binder from a user's, which is how the
  *   desugar stays idempotent. The former `auto` keyword (for a user-written implicit-generics feature) was retired
  *   together with the saturation machinery it fed.
  * @param abilityLevel
  *   True for every binder an `ability` or `effect` block contributes to each of its members ([[AbilityMembers]]): the
  *   implementation binding slot it mints, and its own generic parameters. The run of them is the member reference's
  *   **ability-level prefix**, which
  *   [[com.vanillasource.eliot.eliotc.monomorphize.check.AbilityResolver]] slices off and
  *   [[com.vanillasource.eliot.eliotc.monomorphize.check.ImplementationBinding]] reads the binding from. Marking the
  *   whole run is what tells the desugar where a member's *own* minted binders go — after it, so the prefix stays
  *   exactly the ability's.
  */
case class GenericParameter(
    name: Sourced[String],
    typeRestriction: Sourced[Expression],
    abilityConstraints: Seq[UnresolvedAbilityConstraint[Sourced[Expression]]],
    inferable: Boolean = false,
    abilityLevel: Boolean = false
)

object GenericParameter {

  /** The **mark** every minted binding binder carries as its declared type — `Impl: Implementation[Console]`
    * (`docs/effects.md` §9.2).
    *
    * It is the one place the fact "this binder is a binding" is written down, and it names the ability the binding is
    * for; [[com.vanillasource.eliot.eliotc.row.BindingWriter]] reads it instead of re-deriving a binder's role from
    * the shape of a signature. Minted in two places, for the same three kinds of binding: an effect-row entry and a
    * `~` constraint ([[com.vanillasource.eliot.eliotc.core.processor.EffectSugarDesugarer]]), and an `ability` block's
    * implementation slot ([[AbilityMembers]]).
    *
    * The head is module-qualified because the mark is compiler-written and must not depend on the file's imports; the
    * ability is named bare, exactly as the constraint beside it names it, so it resolves in the scope that constraint
    * resolves in. [[WellKnownTypes.implementationTypeFQN]] is an alias for `Type`, so a marked binder is an ordinary
    * binder of kind `Type` to everything that does not look for the mark.
    */
  def implementationMark(anchor: Sourced[?], abilityName: Sourced[String]): Sourced[Expression] =
    anchor.as(
      Expression.FunctionApplication(
        Some(anchor.as(WellKnownTypes.implementationTypeFQN.moduleName.show)),
        anchor.as(WellKnownTypes.implementationTypeFQN.name.name),
        Some(Seq(abilityName.as(Expression.FunctionApplication(None, abilityName, None, Seq.empty)))),
        Seq.empty
      )
    )

  val signatureEquality: Eq[GenericParameter] = (x: GenericParameter, y: GenericParameter) =>
    x.name.value === y.name.value && x.typeRestriction.value.render === y.typeRestriction.value.render

  given ASTComponent[Seq[GenericParameter]] = new ASTComponent[Seq[GenericParameter]] {
    override def parser: Parser[Sourced[Token], Seq[GenericParameter]] =
      optionalBracketedCommaSeparatedItems("[", component[GenericParameter], "]")
  }

  given ASTComponent[GenericParameter] = new ASTComponent[GenericParameter] {
    /** The operator joining two ability constraints. Any user operator parses here — which one is legal is a *name*
      * question, answered at resolution against the standard library's `&`, not a symbol the parser recognises. The
      * follow set inside a generic-parameter list is `,` and `]`, both reserved and so never user operators, so this
      * stays unambiguous.
      */
    private val constraintCombinator: Parser[Sourced[Token], Sourced[String]] =
      acceptIf(isUserOperator, "'&' between ability constraints").map(_.map(_.content))

    private val abilityConstraintsParser =
      for {
        _     <- symbol("~")
        first <- component[UnresolvedAbilityConstraint[Sourced[Expression]]]
        rest  <- (for {
                   combinedBy <- constraintCombinator
                   constraint <- component[UnresolvedAbilityConstraint[Sourced[Expression]]]
                 } yield constraint.copy(combinedBy = Some(combinedBy))).anyTimes()
      } yield first +: rest

    override def parser: Parser[Sourced[Token], GenericParameter] =
      for {
        name               <- acceptIfAll(isUpperCase, isIdentifier)("generic type parameter")
        typeRestriction    <- (arityAsTypeRestriction(name.map(_.content)) or explicitTypeRestriction)
                                .optional()
                                .map(_.getOrElse(name.as(typeExpr(name.map(_ => "Type")))))
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
        abilityConstraint: UnresolvedAbilityConstraint[Sourced[Expression]],
        defaultGeneric: Sourced[String]
    ): UnresolvedAbilityConstraint[Sourced[Expression]] =
      if (abilityConstraint.typeArgs.isEmpty) {
        abilityConstraint.copy(typeArgs = Seq(defaultGeneric.as(typeExpr(defaultGeneric))))
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
}
