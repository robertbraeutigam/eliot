package com.vanillasource.eliot.eliotc.core.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import com.vanillasource.eliot.eliotc.core.fact.Expression.*

/** An expression stack's "bottom" describes a runtime value (like a constant, a function, or even a type), and each
  * layer above describes the underlying layer's "type". A single constant value like: "zero: Int = 0" will have 2
  * layers, with the first having a "0" as literal, and the layer over it being a reference to the "Int" value (which
  * can be considered a "type"). Each layer can refer to any value declared on the same level syntactically "before" the
  * usage site, or to any value on layers above.
  */
case class ExpressionStack[E](expressions: Seq[E])

object ExpressionStack {
  def of[E](expression: E) = ExpressionStack(Seq(expression))

  def empty[E] = ExpressionStack(Seq.empty[E])

  given expressionStackEquality[E: Eq]: Eq[ExpressionStack[E]] with {
    def eqv(x: ExpressionStack[E], y: ExpressionStack[E]): Boolean =
      x.expressions.length == y.expressions.length &&
        (x.expressions zip y.expressions).forall(Eq[E].eqv)
  }

  given Show[ExpressionStack[Expression]] = (stack: ExpressionStack[Expression]) =>
    stack.expressions.map(_.show).mkString(" => ")

  def prettyPrint[E: TreeDisplay](stack: ExpressionStack[E]): String = TreeDisplay.prettyPrint(stack)
}
