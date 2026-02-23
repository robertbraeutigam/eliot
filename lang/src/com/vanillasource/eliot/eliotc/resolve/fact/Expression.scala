package com.vanillasource.eliot.eliotc.resolve.fact

import cats.syntax.all.*
import cats.Show
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

sealed trait Expression

object Expression {
  case class FunctionApplication(
      target: Sourced[TypeStack[Expression]],
      argument: Sourced[TypeStack[Expression]]
  ) extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[BigInt])    extends Expression
  case class StringLiteral(stringLiteral: Sourced[String])      extends Expression
  case class ParameterReference(parameterName: Sourced[String]) extends Expression
  case class ValueReference(valueName: Sourced[ValueFQN])       extends Expression
  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: Option[Sourced[TypeStack[Expression]]],
      body: Sourced[TypeStack[Expression]]
  ) extends Expression

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                                          => value.toString()
    case StringLiteral(Sourced(_, _, value))                                           => s"\"$value\""
    case FunctionApplication(Sourced(_, _, targetValue), Sourced(_, _, argumentValue)) =>
      s"${targetValue.show}(${argumentValue.show})"
    case FunctionLiteral(param, paramType, body)                                       =>
      s"(${paramType.map(_.value.show).getOrElse("<n/a>")} :: ${param.value}) -> ${body.value.show}"
    case ParameterReference(name)                                                      => name.value
    case ValueReference(name)                                                          => name.value.show
  }
}
