package com.vanillasource.eliot.eliotc.ast.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.ast.parser.Parser.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

sealed trait Expression

object Expression {
  case class FunctionApplication(
      moduleName: Option[Sourced[String]],
      functionName: Sourced[String],
      arguments: Seq[Sourced[Expression]]
  ) extends Expression
  case class FunctionLiteral(parameters: Seq[LambdaParameterDefinition], body: Sourced[Expression]) extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[String])                                        extends Expression
  case class StringLiteral(stringLiteral: Sourced[String])                                          extends Expression

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                                             => value
    case StringLiteral(Sourced(_, _, value))                                              => value
    case FunctionApplication(Some(Sourced(_, _, module)), Sourced(_, _, fn), ns @ _ :: _) =>
      s"$module::$fn(${ns.map(_.value.show).mkString(", ")})"
    case FunctionApplication(Some(Sourced(_, _, module)), Sourced(_, _, fn), _)           => s"$module::$fn"
    case FunctionApplication(None, Sourced(_, _, value), ns @ _ :: _)                     =>
      s"$value(${ns.map(_.value.show).mkString(", ")})"
    case FunctionApplication(None, Sourced(_, _, value), _)                               => value
    case FunctionLiteral(parameters, body)                                                => parameters.map(_.show).mkString("(", ", ", ")") + " -> " + body.show
  }

  given ASTComponent[Expression] = new ASTComponent[Expression] {
    override def parser: Parser[Sourced[Token], Expression] =
      functionLiteral.atomic() or functionApplication or integerLiteral or stringLiteral

    private val moduleParser: Parser[Sourced[Token], Sourced[String]] =
      for {
        moduleParts <- acceptIf(isIdentifier, "module name").atLeastOnceSeparatedBy(symbol("."))
      } yield {
        val moduleString = moduleParts.map(_.value.content).mkString(".")
        val outline      = Sourced.outline(moduleParts)

        outline.as(moduleString)
      }

    private val functionApplication: Parser[Sourced[Token], Expression] = for {
      module <- (moduleParser <* symbol("::")).atomic().optional()
      fnName <- acceptIf(isIdentifier, "function name")
      args   <- optionalArgumentListOf(sourced(parser))
    } yield FunctionApplication(module, fnName.map(_.content), args)

    private val functionLiteral: Parser[Sourced[Token], Expression] = for {
      parameters <-
        bracketedCommaSeparatedItems("(", component[LambdaParameterDefinition], ")") or
          component[LambdaParameterDefinition].map(Seq(_))
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
