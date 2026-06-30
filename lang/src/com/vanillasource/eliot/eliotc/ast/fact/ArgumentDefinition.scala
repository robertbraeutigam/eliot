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
  *   [[GenericParameter.inferable]].
  */
case class ArgumentDefinition(name: Sourced[String], typeExpression: Sourced[Expression], inferable: Boolean = false)

object ArgumentDefinition {
  val signatureEquality: Eq[ArgumentDefinition] = (x: ArgumentDefinition, y: ArgumentDefinition) =>
    x.name.value === y.name.value && x.typeExpression.value.show === y.typeExpression.value.show

  given Show[ArgumentDefinition] = _.name.show

  given ASTComponent[ArgumentDefinition] = new ASTComponent[ArgumentDefinition] {
    // The argument type uses `typeRunParser` (the shared type-position parser), so an infix type operator reads bare
    // here — `f: A => B`. The run stops at the `,` separator and the closing `)` of the argument list (both reserved,
    // non-type-atom tokens), so a single-atom type is still returned verbatim and the list structure is unaffected.
    override def parser: Parser[Sourced[Token], ArgumentDefinition] = for {
      name           <- acceptIf(isIdentifier, "argument name")
      _              <- symbol(":")
      typeExpression <- sourced(Expression.typeRunParser)
    } yield ArgumentDefinition(name.map(_.content), typeExpression)
  }
}
