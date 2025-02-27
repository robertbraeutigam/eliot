package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.Primitives.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.*

sealed trait Expression

object Expression {
  case class FunctionApplication(functionName: Sourced[Token], arguments: Seq[Expression]) extends Expression
  case class FunctionLiteral(parameters: Seq[ArgumentDefinition], body: Expression)        extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[Token])                                extends Expression

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                   => value.toString
    case FunctionApplication(Sourced(_, _, value), ns @ _ :: _) =>
      s"${value.toString}(${ns.map(_.show).mkString(", ")})"
    case FunctionApplication(Sourced(_, _, value), _)           => value.toString
    case FunctionLiteral(parameters, body)                      => parameters.map(_.show).mkString("(", ", ", ")") + " -> " + body.show
  }

  given ASTComponent[Expression] = new ASTComponent[Expression] {
    override def parser: Parser[Sourced[Token], Expression] =
      functionLiteral or functionApplication or integerLiteral

    private val functionApplication: Parser[Sourced[Token], Expression] = for {
      name <- acceptIf(isIdentifier, "function name")
      args <- optionalArgumentListOf(parser)
    } yield FunctionApplication(name, args)

    private val functionLiteral: Parser[Sourced[Token], Expression] = for {
      parameters <-
        optionalBracketedCommaSeparatedItems("(", component[ArgumentDefinition], ")") or
          component[ArgumentDefinition].map(Seq(_))
      _          <- symbol("->")
      body       <- parser
    } yield FunctionLiteral(parameters, body)

    private val integerLiteral: Parser[Sourced[Token], Expression] = for {
      lit <- acceptIf(isIntegerLiteral, "integer literal")
    } yield IntegerLiteral(lit)
  }
}
