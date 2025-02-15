package com.vanillasource.eliot.eliotc.resolve

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.FunctionFQN
import com.vanillasource.eliot.eliotc.source.Sourced

sealed trait Expression

object Expression {
  case class FunctionApplication(functionName: Sourced[FunctionFQN], arguments: Seq[Expression]) extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[BigInt])                                     extends Expression
  case class ParameterReference(parameterName: Sourced[String])                                  extends Expression

  given Show[Expression] = new Show[Expression] {
    override def show(e: Expression): String = e match
      case IntegerLiteral(Sourced(_, _, value))                   => value.toString()
      case FunctionApplication(Sourced(_, _, value), ns @ x :: _) =>
        s"${value.show}(${ns.map(show).mkString(", ")})"
      case FunctionApplication(Sourced(_, _, value), _)           => value.show
      case ParameterReference(name)                               => name.show
  }
}
