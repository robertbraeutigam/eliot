package com.vanillasource.eliot.eliotc.typesystem.fact

import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, TypeReference}
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.typesystem.fact.TypedExpression.Expression

case class TypedExpression(expressionType: TypeReference, expression: Expression)

object TypedExpression {
  sealed trait Expression

  case class FunctionApplication(target: Sourced[TypedExpression], argument: Sourced[TypedExpression])
      extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[BigInt])                                extends Expression
  case class StringLiteral(stringLiteral: Sourced[String])                                  extends Expression
  case class ParameterReference(parameterName: Sourced[String])                             extends Expression
  case class ValueReference(valueName: Sourced[FunctionFQN])                                extends Expression
  case class FunctionLiteral(parameter: ArgumentDefinition, body: Sourced[TypedExpression]) extends Expression
}
