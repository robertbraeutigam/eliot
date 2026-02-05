package com.vanillasource.eliot.eliotc.symbolic.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression.Expression

/** An expression annotated with its type (as an ExpressionValue). */
case class TypedExpression(
    expressionType: ExpressionValue,
    expression: Expression
)

object TypedExpression {
  sealed trait Expression

  case class FunctionApplication(target: Sourced[TypedExpression], argument: Sourced[TypedExpression])
      extends Expression

  case class IntegerLiteral(integerLiteral: Sourced[BigInt]) extends Expression

  case class StringLiteral(stringLiteral: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  case class ValueReference(valueName: Sourced[ValueFQN]) extends Expression

  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: Sourced[ExpressionValue],
      body: Sourced[TypedExpression]
  ) extends Expression
}
