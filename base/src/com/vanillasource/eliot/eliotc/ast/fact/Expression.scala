package com.vanillasource.eliot.eliotc.ast.fact

import cats.Show
import cats.syntax.all.*
import ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Expression
import Primitives.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.*

sealed trait Expression

object Expression {
  case class FunctionApplication(functionName: Sourced[String], arguments: Seq[Sourced[Expression]]) extends Expression
  case class QualifiedFunctionApplication(
      moduleName: Sourced[String],
      functionName: Sourced[String],
      arguments: Seq[Sourced[Expression]]
  ) extends Expression
  case class FunctionLiteral(parameters: Seq[ArgumentDefinition], body: Sourced[Expression])         extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[String])                                         extends Expression
  case class StringLiteral(stringLiteral: Sourced[String])                                           extends Expression

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                                                => value
    case StringLiteral(Sourced(_, _, value))                                                 => value
    case FunctionApplication(Sourced(_, _, value), ns @ _ :: _)                              =>
      s"$value(${ns.map(_.value.show).mkString(", ")})"
    case FunctionApplication(Sourced(_, _, value), _)                                        => value
    case QualifiedFunctionApplication(Sourced(_, _, module), Sourced(_, _, fn), ns @ _ :: _) =>
      s"$module::$fn(${ns.map(_.value.show).mkString(", ")})"
    case QualifiedFunctionApplication(Sourced(_, _, module), Sourced(_, _, fn), _)           => s"$module::$fn"
    case FunctionLiteral(parameters, body)                                                   => parameters.map(_.show).mkString("(", ", ", ")") + " -> " + body.show
  }

  given ASTComponent[Expression] = new ASTComponent[Expression] {
    override def parser: Parser[Sourced[Token], Expression] =
      functionLiteral.atomic() or qualifiedFunctionApplication
        .atomic() or functionApplication or integerLiteral or stringLiteral

    private val qualifiedFunctionApplication: Parser[Sourced[Token], Expression] = for {
      moduleParts <- acceptIf(isIdentifier, "module name").atLeastOnceSeparatedBy(symbol("."))
      _           <- symbol("::")
      fnName      <- acceptIf(isIdentifier, "function name")
      args        <- optionalArgumentListOf(sourced(parser))
    } yield {
      val moduleString = moduleParts.map(_.value.content).mkString(".")
      val outline      = Sourced.outline(moduleParts)
      QualifiedFunctionApplication(outline.as(moduleString), fnName.map(_.content), args)
    }

    private val functionApplication: Parser[Sourced[Token], Expression] = for {
      name <- acceptIf(isIdentifier, "function name")
      args <- optionalArgumentListOf(sourced(parser))
    } yield FunctionApplication(name.map(_.content), args)

    private val functionLiteral: Parser[Sourced[Token], Expression] = for {
      parameters <-
        bracketedCommaSeparatedItems("(", component[ArgumentDefinition], ")") or
          component[ArgumentDefinition].map(Seq(_))
      _          <- symbol("->")
      body       <- sourced(parser)
    } yield FunctionLiteral(parameters, body)

    private val integerLiteral: Parser[Sourced[Token], Expression] = for {
      lit <- acceptIf(isIntegerLiteral, "integer literal")
    } yield IntegerLiteral(lit.map(_.content))

    private val stringLiteral: Parser[Sourced[Token], Expression] = for {
      lit <- acceptIf(isStringLiteral, "string literal")
    } yield StringLiteral(lit.map(_.content))

  }
}
