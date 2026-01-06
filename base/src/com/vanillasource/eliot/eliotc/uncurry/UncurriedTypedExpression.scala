package com.vanillasource.eliot.eliotc.uncurry

import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, TypeReference}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/**
 * An uncurried typed expression, where function applications can have multiple arguments
 * and function literals can have multiple parameters.
 *
 * This reverses the currying transformation done by the resolver.
 */
case class UncurriedTypedExpression(expressionType: TypeReference, expression: UncurriedTypedExpression.Expression)

object UncurriedTypedExpression {
  sealed trait Expression

  /**
   * A function application with multiple arguments (uncurried).
   *
   * Example: f(a, b, c) instead of ((f a) b) c
   */
  case class FunctionApplication(
      target: Sourced[UncurriedTypedExpression],
      arguments: Seq[Sourced[UncurriedTypedExpression]]
  ) extends Expression

  /**
   * A function literal with multiple parameters (uncurried).
   *
   * Example: (a, b, c) -> body instead of a -> b -> c -> body
   */
  case class FunctionLiteral(
      parameters: Seq[ArgumentDefinition],
      body: Sourced[UncurriedTypedExpression]
  ) extends Expression

  case class IntegerLiteral(integerLiteral: Sourced[BigInt]) extends Expression

  case class StringLiteral(stringLiteral: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  case class ValueReference(valueName: Sourced[FunctionFQN]) extends Expression
}
