package com.vanillasource.eliot.eliotc.resolve2.fact

import cats.syntax.all.*
import cats.Show
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression.*

/** An expression stack's "bottom" describes a runtime value (like a constant, a function, or even a type), and each
  * layer above describes the underlying layer's "type". A single constant value like: "zero: Int = 0" will have 2
  * layers, with the first having a "0" as literal, and the layer over it being a reference to the "Int" value (which
  * can be considered a "type"). Each layer can refer to any value declared on the same level syntactically "before" the
  * usage site, or to any value on layers above.
  */
case class ExpressionStack(expressions: Seq[Expression])

object ExpressionStack {
  def of(expression: Expression) = ExpressionStack(Seq(expression))

  def empty = ExpressionStack(Seq.empty)
  
  given Show[ExpressionStack] = (stack: ExpressionStack) => stack.expressions.map(_.show).mkString(" => ")
}
