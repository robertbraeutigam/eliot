package com.vanillasource.eliot.eliotc.core.fact

import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.source.content.Sourced

trait Expression

object Expression {

  // Reference to a named value
  case class NamedValueReference(valueName: Sourced[String], qualifier: Option[Sourced[String]] = None)
      extends Expression
  // Apply an argument to an expression (assumed to be a function)
  case class FunctionApplication(target: Sourced[ExpressionStack], argument: Sourced[ExpressionStack])
      extends Expression
  // Function literal, i.e. a lambda expression, i.e. an ad-hoc function, i.e. an unnamed function
  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: ExpressionStack,
      body: Sourced[ExpressionStack]
  ) extends Expression
  // Integer literal
  case class IntegerLiteral(integerLiteral: Sourced[String]) extends Expression
  // String literal
  case class StringLiteral(stringLiteral: Sourced[String])   extends Expression
}
