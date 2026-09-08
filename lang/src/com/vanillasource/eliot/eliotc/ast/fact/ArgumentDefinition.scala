package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import cats.Eq
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
    x.name.value === y.name.value && x.typeExpression.value.render === y.typeExpression.value.render

  extension (self: ArgumentDefinition) def render: String = self.name.show

  given ASTComponent[ArgumentDefinition] = new ASTComponent[ArgumentDefinition] {
    // The argument type uses `typeRunParser` (the shared type-position parser), so an infix type operator reads bare
    // here — `f: A => B`. The run stops at the `,` separator and the closing `)` of the argument list (both reserved,
    // non-type-atom tokens), so a single-atom type is still returned verbatim and the list structure is unaffected.
    // A trailing `with name` binds a named implementation to the slot (effects v6, landed dark — see
    // [[Expression.WithBinding]]). Only a parameter's or a field's type admits it, which is why it is read here and not
    // in `typeRunParser`: a def's own return type stops at the `with` keyword and fails to parse.
    override def parser: Parser[Sourced[Token], ArgumentDefinition] = for {
      name           <- acceptIf(isIdentifier, "argument name")
      _              <- symbol(":")
      typeRun        <- sourced(Expression.typeRunParser)
      typeExpression <- Expression.typeWithBindings(typeRun)
    } yield ArgumentDefinition(name.map(_.content), typeExpression)
  }
}
