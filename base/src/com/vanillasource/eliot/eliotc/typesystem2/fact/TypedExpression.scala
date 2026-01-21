package com.vanillasource.eliot.eliotc.typesystem2.fact

import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem2.fact.TypedExpression.Expression

/** An expression annotated with its type (as a NormalizedExpression). */
case class TypedExpression(
    expressionType: NormalizedExpression,
    expression: Expression
)

object TypedExpression {
  sealed trait Expression

  case class FunctionApplication(target: Sourced[ExpressionStack[TypedExpression]], argument: Sourced[ExpressionStack[TypedExpression]])
      extends Expression

  case class IntegerLiteral(integerLiteral: Sourced[BigInt]) extends Expression

  case class StringLiteral(stringLiteral: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  case class ValueReference(valueName: Sourced[ValueFQN]) extends Expression

  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: Sourced[ExpressionStack[TypedExpression]],
      body: Sourced[ExpressionStack[TypedExpression]]
  ) extends Expression
}
