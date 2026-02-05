package com.vanillasource.eliot.eliotc.uncurry.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Expression tree after uncurrying.
  */
case class UncurriedExpression(
    expressionType: ExpressionValue,
    expression: UncurriedExpression.Expression
)

object UncurriedExpression {
  sealed trait Expression

  /** Multi-argument function application. Flattened from nested curried applications based on target arity.
    */
  case class FunctionApplication(
      target: Sourced[UncurriedExpression],
      arguments: Seq[Sourced[UncurriedExpression]]
  ) extends Expression

  /** Multi-parameter function literal. Flattened from nested lambdas.
    */
  case class FunctionLiteral(
      parameters: Seq[ParameterDefinition],
      body: Sourced[UncurriedExpression]
  ) extends Expression

  case class IntegerLiteral(value: Sourced[BigInt]) extends Expression

  case class StringLiteral(value: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  /** Reference to a value.
    */
  case class ValueReference(valueName: Sourced[ValueFQN]) extends Expression

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                       => value.toString()
    case StringLiteral(Sourced(_, _, value))                        => s"\"$value\""
    case FunctionApplication(Sourced(_, _, targetValue), arguments) =>
      targetValue.expression.show + arguments.map(_.value.expression.show).mkString("(", ", ", ")")
    case FunctionLiteral(parameters, body)                          =>
      parameters.map(_.name.value).mkString("(", ", ", ")") + " -> " + body.value.expression.show
    case ValueReference(valueName)                                  => valueName.value.show
    case ParameterReference(parameterName)                          => parameterName.value
  }

}
