package com.vanillasource.eliot.eliotc.resolve2.fact

import cats.syntax.all.*
import cats.Show
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

sealed trait Expression

object Expression {
  case class FunctionApplication(
      target: Sourced[ExpressionStack[Expression]],
      argument: Sourced[ExpressionStack[Expression]]
  ) extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[BigInt])    extends Expression
  case class StringLiteral(stringLiteral: Sourced[String])      extends Expression
  case class ParameterReference(parameterName: Sourced[String]) extends Expression
  case class ValueReference(valueName: Sourced[ValueFQN])       extends Expression
  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: Sourced[ExpressionStack[Expression]],
      body: Sourced[ExpressionStack[Expression]]
  ) extends Expression

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                                          => value.toString()
    case StringLiteral(Sourced(_, _, value))                                           => s"\"$value\""
    case FunctionApplication(Sourced(_, _, targetValue), Sourced(_, _, argumentValue)) =>
      s"${targetValue.show}(${argumentValue.show})"
    case FunctionLiteral(param, _, body)                                               => s"${param.value} -> ${body.value.show}"
    case ParameterReference(name)                                                      => name.value
    case ValueReference(name)                                                          => name.value.show
  }
}
