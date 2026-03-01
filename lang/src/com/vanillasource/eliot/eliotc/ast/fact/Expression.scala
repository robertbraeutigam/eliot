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
      genericArguments: Seq[Sourced[Expression]],
      arguments: Seq[Sourced[Expression]]
  ) extends Expression
  case class FunctionLiteral(parameters: Seq[LambdaParameterDefinition], body: Sourced[Expression]) extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[String])                                        extends Expression
  case class StringLiteral(stringLiteral: Sourced[String])                                          extends Expression
  case class FlatExpression(parts: Seq[Sourced[Expression]])                                        extends Expression
  case class MatchExpression(scrutinee: Sourced[Expression], cases: Seq[MatchCase])                 extends Expression

  case class MatchCase(pattern: Sourced[Pattern], body: Sourced[Expression])

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                                                 => value
    case StringLiteral(Sourced(_, _, value))                                                  => value
    case FunctionApplication(Some(Sourced(_, _, module)), Sourced(_, _, fn), ga, ns @ _ :: _) =>
      val gaStr = if (ga.isEmpty) "" else ga.map(_.value.show).mkString("[", ", ", "]")
      s"$module::$fn$gaStr(${ns.map(_.value.show).mkString(", ")})"
    case FunctionApplication(Some(Sourced(_, _, module)), Sourced(_, _, fn), ga, _)           =>
      val gaStr = if (ga.isEmpty) "" else ga.map(_.value.show).mkString("[", ", ", "]")
      s"$module::$fn$gaStr"
    case FunctionApplication(None, Sourced(_, _, value), ga, ns @ _ :: _)                     =>
      val gaStr = if (ga.isEmpty) "" else ga.map(_.value.show).mkString("[", ", ", "]")
      s"$value$gaStr(${ns.map(_.value.show).mkString(", ")})"
    case FunctionApplication(None, Sourced(_, _, value), ga, _)                               =>
      val gaStr = if (ga.isEmpty) "" else ga.map(_.value.show).mkString("[", ", ", "]")
      s"$value$gaStr"
    case FunctionLiteral(parameters, body)                                                    => parameters.map(_.show).mkString("(", ", ", ")") + " -> " + body.show
    case FlatExpression(parts)                                                                => parts.map(_.value.show).mkString(" ")
    case MatchExpression(scrutinee, cases)                                                    =>
      s"${scrutinee.value.show} match { ${cases.map(c => s"case ${c.pattern.value.show} -> ${c.body.value.show}").mkString(" ")} }"
  }

  // Shared sub-parsers, all using fullParser for inner expression positions

  private lazy val moduleParser: Parser[Sourced[Token], Sourced[String]] =
    for {
      moduleParts <- acceptIf(isIdentifier, "module name").atLeastOnceSeparatedBy(symbol("."))
    } yield {
      val moduleString = moduleParts.map(_.value.content).mkString(".")
      val outline      = Sourced.outline(moduleParts)
      outline.as(moduleString)
    }

  private lazy val integerLiteralParser: Parser[Sourced[Token], Expression] = for {
    lit <- acceptIf(isIntegerLiteral, "integer literal")
  } yield IntegerLiteral(lit.map(_.content))

  private lazy val stringLiteralParser: Parser[Sourced[Token], Expression] = for {
    lit <- acceptIf(isStringLiteral, "string literal")
  } yield StringLiteral(lit.map(_.content))

  private lazy val namedRefOrCallParser: Parser[Sourced[Token], Expression] = for {
    module   <- (moduleParser <* symbol("::")).atomic().optional()
    name     <- acceptIf(isIdentifierOrSymbol, "name")
    typeArgs <- optionalBracketedCommaSeparatedItems("[", sourced(fullParser), "]")
    args     <- bracketedCommaSeparatedItems("(", sourced(fullParser), ")").optional()
  } yield FunctionApplication(module, name.map(_.content), typeArgs, args.getOrElse(Seq.empty))

  private lazy val parenthesizedExprParser: Parser[Sourced[Token], Expression] =
    for {
      _ <- symbol("(")
      result <- fullParser
      _ <- symbol(")")
    } yield result

  private lazy val functionLiteralParser: Parser[Sourced[Token], Expression] = for {
    parameters <-
      bracketedCommaSeparatedItems("(", component[LambdaParameterDefinition], ")") or
        component[LambdaParameterDefinition].map(Seq(_))
    _          <- symbol("->")
    body       <- sourced(fullParser)
  } yield FunctionLiteral(parameters, body)

  private lazy val matchCaseParser: Parser[Sourced[Token], MatchCase] = for {
    _       <- keyword("case")
    pattern <- sourced(component[Pattern])
    _       <- symbol("->")
    body    <- sourced(fullParser)
  } yield MatchCase(pattern, body)

  private lazy val matchExpressionParser: Parser[Sourced[Token], Seq[MatchCase]] =
    matchCaseParser.atLeastOnce().between(keyword("match") *> symbol("{"), symbol("}"))

  /** Atoms for type positions: named references, parenthesized expressions, literals.
    * Excludes unparenthesized lambdas and match expressions to avoid ambiguity in type annotations.
    */
  private lazy val typeAtom: Parser[Sourced[Token], Expression] =
    parenthesizedExprParser.atomic() or
      namedRefOrCallParser or
      integerLiteralParser or
      stringLiteralParser

  /** Full atoms including lambdas. */
  private lazy val fullAtom: Parser[Sourced[Token], Expression] =
    functionLiteralParser.atomic() or typeAtom

  /** Full expression parser including lambdas and match expressions. */
  private lazy val fullParser: Parser[Sourced[Token], Expression] =
    for {
      parts      <- sourced(fullAtom).atLeastOnce()
      matchBlock <- matchExpressionParser.optional()
    } yield matchBlock match {
      case Some(cases) =>
        val scrutinee = if (parts.size == 1) parts.head else Sourced.outline(parts).as(FlatExpression(parts))
        MatchExpression(scrutinee, cases)
      case None        => FlatExpression(parts)
    }

  /** Restricted expression parser for type positions. Parses a single type atom: a named reference
    * (with optional generic and value arguments), a parenthesized expression, or a literal.
    * Complex type expressions (lambdas, operators, match) must be parenthesized.
    */
  lazy val typeParser: Parser[Sourced[Token], Expression] = typeAtom

  given ASTComponent[Expression] = new ASTComponent[Expression] {
    override def parser: Parser[Sourced[Token], Expression] = fullParser
  }
}
