package com.vanillasource.eliot.eliotc.monomorphize.fact

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A runtime expression annotated with concrete (ground) types. All types are fully evaluated Values with no free type
  * variables.
  */
case class MonomorphicExpression(
    expressionType: Value,
    expression: MonomorphicExpression.Expression
)

object MonomorphicExpression {
  sealed trait Expression

  case class FunctionApplication(
      target: Sourced[MonomorphicExpression],
      argument: Sourced[MonomorphicExpression]
  ) extends Expression

  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: Value,
      body: Sourced[MonomorphicExpression]
  ) extends Expression

  case class IntegerLiteral(value: Sourced[BigInt]) extends Expression

  case class StringLiteral(value: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  /** Reference to a specialized (monomorphic) function instance.
    */
  case class MonomorphicValueReference(
      valueName: Sourced[ValueFQN],
      typeArguments: Seq[Value]
  ) extends Expression
}
