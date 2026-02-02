package com.vanillasource.eliot.eliotc.uncurry2.fact

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Expression tree after uncurrying, using concrete Value types.
  */
case class Uncurried2Expression(
    expressionType: Value,
    expression: Uncurried2Expression.Expression
)

object Uncurried2Expression {
  sealed trait Expression

  /** Multi-argument function application. Flattened from nested curried applications based on target arity.
    */
  case class FunctionApplication(
      target: Sourced[Uncurried2Expression],
      arguments: Seq[Sourced[Uncurried2Expression]]
  ) extends Expression

  /** Multi-parameter function literal. Flattened from nested lambdas up to optimal arity.
    */
  case class FunctionLiteral(
      parameters: Seq[Parameter2Definition],
      body: Sourced[Uncurried2Expression]
  ) extends Expression

  case class IntegerLiteral(value: Sourced[BigInt]) extends Expression

  case class StringLiteral(value: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  /** Reference to an uncurried monomorphic value.
    */
  case class ValueReference(
      valueName: Sourced[ValueFQN],
      typeArguments: Seq[Value]
  ) extends Expression
}
