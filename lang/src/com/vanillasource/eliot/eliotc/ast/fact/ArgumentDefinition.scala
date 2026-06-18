package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import Primitives.{isIdentifier, sourced, symbol}
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import Parser.acceptIf

/** A value-argument binder, e.g. `x: Int` in `def f(x: Int)`.
  *
  * @param inferable
  *   Carries the `auto` marker for binders that originate as generic parameters but are represented as value arguments
  *   of a (type-)constructor function — e.g. the `[auto MIN, auto MAX]` of `type Int[..]` become this function's args.
  *   Plain value arguments are never `auto` (the marker is only written on generic-parameter binders). See
  *   [[GenericParameter.inferable]] and `docs/implicit-generics-plan.md`.
  */
case class ArgumentDefinition(name: Sourced[String], typeExpression: Sourced[Expression], inferable: Boolean = false)

object ArgumentDefinition {
  val signatureEquality: Eq[ArgumentDefinition] = (x: ArgumentDefinition, y: ArgumentDefinition) =>
    x.name.value === y.name.value && x.typeExpression.value.show === y.typeExpression.value.show

  given Show[ArgumentDefinition] = _.name.show

  given ASTComponent[ArgumentDefinition] = new ASTComponent[ArgumentDefinition] {
    override def parser: Parser[Sourced[Token], ArgumentDefinition] = for {
      name           <- acceptIf(isIdentifier, "argument name")
      _              <- symbol(":")
      typeExpression <- sourced(Expression.typeParser)
    } yield ArgumentDefinition(name.map(_.content), typeExpression)
  }
}
