package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.Primitives.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.*

sealed trait Expression

object Expression {
  case class FunctionApplication(functionName: Sourced[Token], arguments: Seq[Expression]) extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[Token])                                extends Expression

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                   => value.toString
    case FunctionApplication(Sourced(_, _, value), ns @ x :: _) =>
      s"${value.toString}(${ns.map(_.show).mkString(", ")})"
    case FunctionApplication(Sourced(_, _, value), _)           => value.toString
  }

  given ASTComponent[Expression] = new ASTComponent[Expression] {
    override def parser: Parser[Sourced[Token], Expression] =
      functionApplication or integerLiteral

    private val functionApplication: Parser[Sourced[Token], Expression] = for {
      name <- acceptIf(isIdentifier, "function name")
      args <- argumentListOf(parser)
    } yield FunctionApplication(name, args)

    private val integerLiteral: Parser[Sourced[Token], Expression] = for {
      lit <- acceptIf(isIntegerLiteral, "integer literal")
    } yield IntegerLiteral(lit)
  }
}
