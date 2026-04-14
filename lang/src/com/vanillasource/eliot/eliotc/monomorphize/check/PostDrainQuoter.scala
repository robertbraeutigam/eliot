package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier => CoreQualifier}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.{Env, MetaStore, SemValue}
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
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
  * Ability references (value refs whose qualifier is [[CoreQualifier.Ability]]) are also resolved here: the implicit
  * type args (or the constraint-derived type args when a matching constraint exists on an outer type parameter) are
  * quoted to ground values, then fed to `resolveAbility` to swap the ref's FQN and type arguments for the chosen
  * implementation.
  */
class PostDrainQuoter(
    metaStore: MetaStore,
    paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]],
    resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
    bindingCache: Map[ValueFQN, Option[SemValue]],
    env: Env,
    nameLevels: Map[String, Int]
) {

  private val evaluator: Evaluator = new Evaluator(vfqn => bindingCache.getOrElse(vfqn, None), nameLevels)

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

    case SemExpression.ValueReference(vfqn, explicitArgs, implicitArgs) =>
      for {
        explicit  <- explicitArgs.traverse(a => quoteSem(a, vfqn))
        implicits <- implicitArgs.traverse(a => quoteSem(a, vfqn))
        resolved  <- resolveIfAbility(vfqn, explicit, implicits)
      } yield resolved

  }

  private def resolveIfAbility(
      vfqn: Sourced[ValueFQN],
      explicitArgs: Seq[GroundValue],
      implicitArgs: Seq[GroundValue]
  ): CompilerIO[MonomorphicExpression.Expression] =
    vfqn.value.name.qualifier match {
      case CoreQualifier.Ability(abilityName) =>
        for {
          abilityTypeArgs <- findConstraintParam(abilityName) match {
                               case Some((_, constraintTypeArgs)) =>
                                 // Evaluate constraint-provided type args (ORE expressions) through the
                                 // monomorphized binding cache and the checker's final env/nameLevels, then quote
                                 // each result.
                                 constraintTypeArgs.traverse { arg =>
                                   val sem = evaluator.eval(env, arg)
                                   quoteSem(sem, vfqn)
                                 }
                               case None                          =>
                                 implicitArgs.pure[CompilerIO]
                             }
          resolved        <- resolveAbility(vfqn.value, abilityTypeArgs)
        } yield resolved match {
          case Some((implFqn, implTypeArgs)) =>
            MonomorphicExpression.MonomorphicValueReference(vfqn.as(implFqn), implTypeArgs)
          case None                          =>
            MonomorphicExpression.MonomorphicValueReference(vfqn, explicitArgs ++ implicitArgs)
        }
      case _                                  =>
        (MonomorphicExpression.MonomorphicValueReference(
          vfqn,
          explicitArgs ++ implicitArgs
        ): MonomorphicExpression.Expression).pure[CompilerIO]
    }

  private def findConstraintParam(abilityName: String): Option[(String, Seq[OperatorResolvedExpression])] =
    paramConstraints.collectFirst {
      Function.unlift { (paramName, constraints) =>
        constraints.find(_.abilityFQN.abilityName == abilityName).map(c => (paramName, c.typeArgs))
      }
    }
}
