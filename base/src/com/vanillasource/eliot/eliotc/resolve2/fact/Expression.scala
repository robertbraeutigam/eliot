package com.vanillasource.eliot.eliotc.resolve2.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

sealed trait Expression

object Expression {
  case class FunctionApplication(target: Sourced[ExpressionStack], argument: Sourced[ExpressionStack])
      extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[BigInt])    extends Expression
  case class StringLiteral(stringLiteral: Sourced[String])      extends Expression
  case class ParameterReference(parameterName: Sourced[String]) extends Expression
  case class ValueReference(valueName: Sourced[ValueFQN])       extends Expression
  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: Sourced[ExpressionStack],
      body: Sourced[ExpressionStack]
  ) extends Expression

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                                          => value.toString()
    case StringLiteral(Sourced(_, _, value))                                           => value
    case FunctionApplication(Sourced(_, _, targetValue), Sourced(_, _, argumentValue)) =>
      s"${targetValue.show}(${argumentValue.show})"
    case FunctionLiteral(param, _, body)                                               => param.value + " -> " + body.value.show
    case ParameterReference(name)                                                      => name.value
    case ValueReference(name)                                                          => name.value.show
  }
}
