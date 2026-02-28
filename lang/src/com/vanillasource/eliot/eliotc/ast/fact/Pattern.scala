package com.vanillasource.eliot.eliotc.ast.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.ast.parser.Parser.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

sealed trait Pattern

object Pattern {
  case class ConstructorPattern(
      moduleName: Option[Sourced[String]],
      constructorName: Sourced[String],
      subPatterns: Seq[Sourced[Pattern]],
      isTypePattern: Boolean = false
  ) extends Pattern

  case class VariablePattern(name: Sourced[String]) extends Pattern

  case class WildcardPattern(source: Sourced[String]) extends Pattern

  given Show[Pattern] = {
    case ConstructorPattern(Some(mod), name, pats, isType) =>
      val (open, close) = if (isType) ("[", "]") else ("(", ")")
      s"${mod.value}::${name.value}$open${pats.map(_.value.show).mkString(", ")}$close"
    case ConstructorPattern(None, name, pats, _) if pats.isEmpty => name.value
    case ConstructorPattern(None, name, pats, isType) =>
      val (open, close) = if (isType) ("[", "]") else ("(", ")")
      s"${name.value}$open${pats.map(_.value.show).mkString(", ")}$close"
    case VariablePattern(name)  => name.value
    case WildcardPattern(_)     => "_"
  }

  given ASTComponent[Pattern] = new ASTComponent[Pattern] {
    override def parser: Parser[Sourced[Token], Pattern] =
      wildcardPattern or constructorPattern.atomic() or variablePattern

    private val wildcardPattern: Parser[Sourced[Token], Pattern] =
      symbol("_").map(s => WildcardPattern(s.map(_.content)))

    private val moduleParser: Parser[Sourced[Token], Sourced[String]] =
      for {
        moduleParts <- acceptIf(isIdentifier, "module name").atLeastOnceSeparatedBy(symbol("."))
      } yield {
        val moduleString = moduleParts.map(_.value.content).mkString(".")
        val outline      = Sourced.outline(moduleParts)
        outline.as(moduleString)
      }

    private val constructorPattern: Parser[Sourced[Token], Pattern] = for {
      module         <- (moduleParser <* symbol("::")).atomic().optional()
      name           <- acceptIfAll(isIdentifier, isUpperCase)("constructor name")
      subPatternsOpt <- (
                          (symbol("[") *> symbol("]")).atomic().as((Seq.empty[Sourced[Pattern]], true)) or
                            bracketedCommaSeparatedItems("[", sourced(parser), "]").map((_, true)) or
                            bracketedCommaSeparatedItems("(", sourced(parser), ")").map((_, false))
                        ).optional()
    } yield {
      val (subPatterns, isType) = subPatternsOpt.getOrElse((Seq.empty, false))
      ConstructorPattern(module, name.map(_.content), subPatterns, isType)
    }

    private val variablePattern: Parser[Sourced[Token], Pattern] =
      acceptIfAll(isIdentifier, isLowerCase)("variable name").map(s => VariablePattern(s.map(_.content)))
  }
}
