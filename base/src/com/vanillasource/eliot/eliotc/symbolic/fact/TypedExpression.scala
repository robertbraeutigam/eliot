package com.vanillasource.eliot.eliotc.symbolic.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression.Expression

/** An expression annotated with its type (as an ExpressionValue). */
case class TypedExpression(
    expressionType: ExpressionValue,
    expression: Expression
) {

  /** Apply a transformation function to all ExpressionValue fields recursively. */
  def transformTypes(f: ExpressionValue => ExpressionValue): TypedExpression =
    TypedExpression(
      f(expressionType),
      expression match {
        case TypedExpression.FunctionApplication(target, arg) =>
          TypedExpression.FunctionApplication(
            target.map(_.transformTypes(f)),
            arg.map(_.transformTypes(f))
          )
        case TypedExpression.FunctionLiteral(paramName, paramType, body) =>
          TypedExpression.FunctionLiteral(
            paramName,
            paramType.map(f),
            body.map(_.transformTypes(f))
          )
        case other => other
      }
    )
}

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
