package com.vanillasource.eliot.eliotc.monomorphize3.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A runtime expression annotated with concrete ground types. All types are fully evaluated GroundValues with no free
  * type variables or unsolved metas.
  */
case class Monomorphic3Expression(
    expressionType: GroundValue,
    expression: Monomorphic3Expression.Expression
)

object Monomorphic3Expression {
  sealed trait Expression

  case class FunctionApplication(
      target: Sourced[Monomorphic3Expression],
      argument: Sourced[Monomorphic3Expression]
  ) extends Expression

  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: GroundValue,
      body: Sourced[Monomorphic3Expression]
  ) extends Expression

  case class IntegerLiteral(value: Sourced[BigInt]) extends Expression

  case class StringLiteral(value: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  case class MonomorphicValueReference(
      valueName: Sourced[ValueFQN],
      typeArguments: Seq[GroundValue]
  ) extends Expression
}
