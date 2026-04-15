package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier => CoreQualifier}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.{MetaStore, SemValue}
import com.vanillasource.eliot.eliotc.monomorphize.eval.Quoter
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Post-drain conversion of a [[SemExpression]] tree into a [[MonomorphicExpression]] tree.
  *
  * Every [[SemValue]] slot is quoted via [[Quoter.quote]] against the final metastore. Unresolved metas, neutrals,
  * lambdas, native applications, and unapplied top-level definitions surface as explicit `Cannot resolve type.`
  * compiler errors (with source positions taken from the enclosing expression / name) — there is no silent
  * `GroundValue.Type` fallback.
  *
  * Ability references (value refs whose qualifier is [[CoreQualifier.Ability]]) are rewritten using the pre-computed
  * [[abilityResolutions]] map: refs resolved during the drain loop emit the chosen impl's FQN and type args; refs
  * absent from the map (constraint-covered calls where the concrete impl lives at the caller's level) emit their
  * original ability FQN so downstream dispatch stays abstract.
  */
class PostDrainQuoter(
    metaStore: MetaStore,
    abilityResolutions: Map[Sourced[ValueFQN], (ValueFQN, Seq[GroundValue])]
) {

  /** Quote a [[SemValue]] to a [[GroundValue]]. Raises a sourced compiler error on failure. */
  def quoteSem(v: SemValue, at: Sourced[?]): CompilerIO[GroundValue] =
    Quoter.quote(0, v, metaStore) match {
      case Right(g)  => g.pure[CompilerIO]
      case Left(msg) => compilerAbort(at.as("Cannot resolve type."), Seq(msg))
    }

  /** Quote a sourced [[SemExpression]] tree to a sourced [[MonomorphicExpression]] tree. */
  def quoteSourced(expr: Sourced[SemExpression]): CompilerIO[Sourced[MonomorphicExpression]] =
    for {
      exprType <- quoteSem(expr.value.expressionType, expr)
      inner    <- quoteExpression(expr.value.expression, expr)
    } yield expr.as(MonomorphicExpression(exprType, inner))

  private def quoteExpression(
      expression: SemExpression.Expression,
      at: Sourced[?]
  ): CompilerIO[MonomorphicExpression.Expression] = expression match {
    case SemExpression.IntegerLiteral(v) =>
      (MonomorphicExpression.IntegerLiteral(v): MonomorphicExpression.Expression).pure[CompilerIO]

    case SemExpression.StringLiteral(v) =>
      (MonomorphicExpression.StringLiteral(v): MonomorphicExpression.Expression).pure[CompilerIO]

    case SemExpression.ParameterReference(name) =>
      (MonomorphicExpression.ParameterReference(name): MonomorphicExpression.Expression).pure[CompilerIO]

    case SemExpression.FunctionApplication(target, argument) =>
      for {
        t <- quoteSourced(target)
        a <- quoteSourced(argument)
      } yield MonomorphicExpression.FunctionApplication(t, a)

    case SemExpression.FunctionLiteral(paramName, paramType, body) =>
      for {
        pt <- quoteSem(paramType, paramName)
        b  <- quoteSourced(body)
      } yield MonomorphicExpression.FunctionLiteral(paramName, pt, b)

    case SemExpression.ValueReference(vfqn, typeArgs) =>
      for {
        args <- typeArgs.traverse(a => quoteSem(a, vfqn))
      } yield resolveIfAbility(vfqn, args)
  }

  private def resolveIfAbility(
      vfqn: Sourced[ValueFQN],
      typeArgs: Seq[GroundValue]
  ): MonomorphicExpression.Expression =
    vfqn.value.name.qualifier match {
      case _: CoreQualifier.Ability =>
        abilityResolutions.get(vfqn) match {
          case Some((implFqn, implTypeArgs)) =>
            MonomorphicExpression.MonomorphicValueReference(vfqn.as(implFqn), implTypeArgs)
          case None                          =>
            MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs)
        }
      case _                        =>
        MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs)
    }
}
