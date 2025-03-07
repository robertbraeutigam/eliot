package com.vanillasource.eliot.eliotc.resolve.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.resolve.fact.ArgumentDefinition
import com.vanillasource.eliot.eliotc.source.Sourced

sealed trait Expression

object Expression {
  case class FunctionApplication(functionName: Sourced[FunctionFQN], arguments: Seq[Expression]) extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[BigInt])                                     extends Expression
  case class ParameterReference(parameterName: Sourced[String])                                  extends Expression
  case class FunctionLiteral(parameter: ArgumentDefinition, body: Sourced[Expression])           extends Expression

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                   => value.toString()
    case FunctionApplication(Sourced(_, _, value), ns @ _ :: _) =>
      s"${value.show}(${ns.map(_.show).mkString(", ")})"
    case FunctionApplication(Sourced(_, _, value), _)           => value.show
    case FunctionLiteral(parameter, body)                       => parameter.name.value + " -> " + body.value.show
    case ParameterReference(name)                               => name.show
  }
}
