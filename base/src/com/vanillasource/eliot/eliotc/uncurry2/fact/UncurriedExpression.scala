package com.vanillasource.eliot.eliotc.uncurry2.fact

import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Expression tree after uncurrying.
  */
case class UncurriedExpression(
    expressionType: ExpressionValue,
    expression: UncurriedExpression.Expression
)

object UncurriedExpression {
  sealed trait Expression

  /** Multi-argument function application. Flattened from nested curried applications based on target arity.
    */
  case class FunctionApplication(
      target: Sourced[UncurriedExpression],
      arguments: Seq[Sourced[UncurriedExpression]]
  ) extends Expression

  /** Multi-parameter function literal. Flattened from nested lambdas.
    */
  case class FunctionLiteral(
      parameters: Seq[ParameterDefinition],
      body: Sourced[UncurriedExpression]
  ) extends Expression

  case class IntegerLiteral(value: Sourced[BigInt]) extends Expression

  case class StringLiteral(value: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  /** Reference to a value.
    */
  case class ValueReference(valueName: Sourced[ValueFQN]) extends Expression
}
