package com.vanillasource.eliot.eliotc.core.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

sealed trait Pattern

object Pattern {
  case class ConstructorPattern(
      moduleName: Option[Sourced[String]],
      constructorName: Sourced[QualifiedName],
      subPatterns: Seq[Sourced[Pattern]]
  ) extends Pattern

  case class VariablePattern(name: Sourced[String]) extends Pattern

  case class WildcardPattern(source: Sourced[String]) extends Pattern

  given Show[Pattern] = {
    case ConstructorPattern(Some(mod), name, pats) if pats.isEmpty =>
      s"${mod.value}::${name.value.show}"
    case ConstructorPattern(Some(mod), name, pats) =>
      s"${mod.value}::${name.value.show}(${pats.map(_.value.show).mkString(", ")})"
    case ConstructorPattern(None, name, pats) if pats.isEmpty => name.value.show
    case ConstructorPattern(None, name, pats) =>
      s"${name.value.show}(${pats.map(_.value.show).mkString(", ")})"
    case VariablePattern(name) => name.value
    case WildcardPattern(_)    => "_"
  }
}
