package com.vanillasource.eliot.eliotc.resolve.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

sealed trait Pattern

object Pattern {
  case class ConstructorPattern(
      constructor: Sourced[ValueFQN],
      subPatterns: Seq[Sourced[Pattern]]
  ) extends Pattern

  case class VariablePattern(name: Sourced[String]) extends Pattern

  case class WildcardPattern(source: Sourced[String]) extends Pattern

  given Show[Pattern] = {
    case ConstructorPattern(ctor, pats) if pats.isEmpty => ctor.value.show
    case ConstructorPattern(ctor, pats) =>
      s"${ctor.value.show}(${pats.map(_.value.show).mkString(", ")})"
    case VariablePattern(name) => name.value
    case WildcardPattern(_)    => "_"
  }
}
