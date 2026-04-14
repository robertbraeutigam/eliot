package com.vanillasource.eliot.eliotc.monomorphize.check

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The checker's output expression form. Type slots hold [[SemValue]], not [[com.vanillasource.eliot.eliotc.
  * monomorphize.fact.GroundValue]] — unsolved metas therefore remain visible until post-drain quoting converts this
  * tree into a [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression]]. This removes the need for
  * any silent `Type` fallback in the checker.
  */
case class SemExpression(
    expressionType: SemValue,
    expression: SemExpression.Expression
)

object SemExpression {
  sealed trait Expression

  case class FunctionApplication(
      target: Sourced[SemExpression],
      argument: Sourced[SemExpression]
  ) extends Expression

  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: SemValue,
      body: Sourced[SemExpression]
  ) extends Expression

  case class IntegerLiteral(value: Sourced[BigInt]) extends Expression

  case class StringLiteral(value: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  /** A value reference with its type arguments tracked in two groups. `explicitTypeArguments` correspond to the
    * surface-syntax `[...]` brackets; `implicitTypeArguments` are the fresh metas introduced during VLam
    * instantiation (polytype → monotype). Keeping them separate matters for post-drain ability resolution, which
    * takes just the implicit ones (or the constraint-derived ones) as the ability's type-arg list.
    */
  case class ValueReference(
      valueName: Sourced[ValueFQN],
      explicitTypeArguments: Seq[SemValue],
      implicitTypeArguments: Seq[SemValue]
  ) extends Expression
}
