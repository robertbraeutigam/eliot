package com.vanillasource.eliot.eliotc.core.fact

import cats.syntax.all.*
import cats.{Eq, Show}

/** An expression stack's "bottom" describes a runtime value (like a constant, a function, or even a type), and each
  * layer above describes the underlying layer's "type". A single constant value like: "zero: Int = 0" will have 2
  * layers, with the first having a "0" as literal, and the layer over it being a reference to the "Int" value (which
  * can be considered a "type"). Each layer can refer to any value declared on the same level syntactically "before" the
  * usage site, or to any value on layers above.
  * @param expressions
  *   The actual expression, from lower index being more "runtime" and higher indices toward more "type".
  * @param hasRuntime
  *   Whether stack contains runtime expression. If true, that means stack starts with a "runtime" expression, if false,
  *   it is abstract and starts with signature.
  */
case class ExpressionStack[E](expressions: Seq[E], hasRuntime: Boolean) {
  def signature: Option[E] = if (hasRuntime) expressions.get(1) else expressions.get(0)

  def runtime: Option[E] = if (hasRuntime) expressions.get(0) else None

  def levelsWithLevel: Seq[(E, Int)] = expressions.zipWithIndex.map { (expr, idx) =>
    if (hasRuntime)
      (expr, idx)
    else
      (expr, idx + 1)
  }

  def maxLevel: Int = if (hasRuntime) expressions.length else expressions.length + 1
}

object ExpressionStack {
  def ofRuntime[E](expression: E) = ExpressionStack(Seq(expression), true)

  def empty[E] = ExpressionStack(Seq.empty[E], true)

  given [E: Show]: Show[ExpressionStack[E]] = stack =>
    if (stack.expressions.size <= 1) stack.expressions.map(_.show).mkString
    else s"(${stack.expressions.map(_.show).mkString(" :: ")})"
}
