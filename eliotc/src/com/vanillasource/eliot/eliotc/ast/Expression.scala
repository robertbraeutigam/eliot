package com.vanillasource.eliot.eliotc.ast

import cats.Show
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

sealed trait Expression

object Expression {
  case class FunctionApplication(functionName: Sourced[Token], arguments: Seq[Expression]) extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[Token])                                extends Expression

  given Show[Expression] = new Show[Expression] {
    override def show(e: Expression): String = e match
      case IntegerLiteral(Sourced(_, _, value))                   => value.toString
      case FunctionApplication(Sourced(_, _, value), ns @ x :: _) =>
        s"${value.toString}(${ns.map(show).mkString(", ")})"
      case FunctionApplication(Sourced(_, _, value), _)           => value.toString
  }
}
