package com.vanillasource.eliot.eliotc.symbolic.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression.Expression

/** An expression annotated with its type (as an ExpressionValue). */
case class TypedExpression(
    expressionType: ExpressionValue,
    expression: Expression
) {

  /** Transform all types in this expression tree using the given function. */
  def transformTypes(f: ExpressionValue => ExpressionValue): TypedExpression =
    TypedExpression(
      f(expressionType),
      expression match {
        case TypedExpression.FunctionApplication(target, arg) =>
          TypedExpression.FunctionApplication(
            TypedExpression.transformStack(target, f),
            TypedExpression.transformStack(arg, f)
          )
        case TypedExpression.FunctionLiteral(name, paramType, body) =>
          TypedExpression.FunctionLiteral(
            name,
            TypedExpression.transformStack(paramType, f),
            TypedExpression.transformStack(body, f)
          )
        case other => other
      }
    )
}

object TypedExpression {

  def transformStack(
      stack: Sourced[TypeStack[TypedExpression]],
      f: ExpressionValue => ExpressionValue
  ): Sourced[TypeStack[TypedExpression]] =
    stack.map(s => TypeStack(s.levels.map(_.transformTypes(f))))

  sealed trait Expression

  case class FunctionApplication(target: Sourced[TypeStack[TypedExpression]], argument: Sourced[TypeStack[TypedExpression]])
      extends Expression

  case class IntegerLiteral(integerLiteral: Sourced[BigInt]) extends Expression

  case class StringLiteral(stringLiteral: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  case class ValueReference(valueName: Sourced[ValueFQN]) extends Expression

  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: Sourced[TypeStack[TypedExpression]],
      body: Sourced[TypeStack[TypedExpression]]
  ) extends Expression
}
