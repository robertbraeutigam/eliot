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

  /** A value reference with its type arguments. These include both the ones from the surface-syntax `[...]` brackets
    * and the fresh metas introduced during VLam instantiation (polytype → monotype); they are concatenated in that
    * order as they accumulate during inference.
    */
  case class ValueReference(
      valueName: Sourced[ValueFQN],
      typeArguments: Seq[SemValue]
  ) extends Expression
}
