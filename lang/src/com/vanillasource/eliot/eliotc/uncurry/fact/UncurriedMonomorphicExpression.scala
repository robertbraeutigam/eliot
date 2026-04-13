package com.vanillasource.eliot.eliotc.uncurry.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Expression tree after uncurrying, with concrete (monomorphic) types. */
case class UncurriedMonomorphicExpression(
    expressionType: GroundValue,
    expression: UncurriedMonomorphicExpression.Expression
)

object UncurriedMonomorphicExpression {
  sealed trait Expression

  /** Multi-argument function application. Flattened from nested curried applications based on target arity. */
  case class FunctionApplication(
      target: Sourced[UncurriedMonomorphicExpression],
      arguments: Seq[Sourced[UncurriedMonomorphicExpression]]
  ) extends Expression

  /** Multi-parameter function literal. Flattened from nested lambdas. */
  case class FunctionLiteral(
      parameters: Seq[MonomorphicParameterDefinition],
      body: Sourced[UncurriedMonomorphicExpression]
  ) extends Expression

  case class IntegerLiteral(value: Sourced[BigInt]) extends Expression

  case class StringLiteral(value: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  /** Reference to a monomorphic value instance. */
  case class MonomorphicValueReference(
      valueName: Sourced[ValueFQN],
      typeArguments: Seq[GroundValue]
  ) extends Expression

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                       => value.toString()
    case StringLiteral(Sourced(_, _, value))                        => s"\"$value\""
    case FunctionApplication(Sourced(_, _, targetValue), arguments) =>
      targetValue.expression.show + arguments.map(_.value.expression.show).mkString("(", ", ", ")")
    case FunctionLiteral(parameters, body)                          =>
      parameters.map(_.name.value).mkString("(", ", ", ")") + " -> " + body.value.expression.show
    case MonomorphicValueReference(valueName, _)                    => valueName.value.show
    case ParameterReference(parameterName)                          => parameterName.value
  }
}
