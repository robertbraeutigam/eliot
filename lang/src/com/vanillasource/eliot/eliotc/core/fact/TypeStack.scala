package com.vanillasource.eliot.eliotc.core.fact

import cats.syntax.all.*
import cats.Show
import cats.data.NonEmptySeq

/** A type stack contains type levels for a value. The bottom level (index 0) is the "signature" - the type of the
  * runtime value. Each subsequent level describes the type of the level below. The implicit top level is always Type.
  *
  * For example, for "zero: Int = 0":
  *   - levels(0) = ValueReference(Int) (the signature)
  *   - Implicit: Type is the type of Int
  *
  * @param levels
  *   The type levels, from signature (index 0) to higher meta-levels.
  */
case class TypeStack[E](levels: NonEmptySeq[E]) {
  def signature: E = levels.head
}

object TypeStack {
  def of[E](expression: E): TypeStack[E] = TypeStack(NonEmptySeq.of(expression))

  def annotated[E](signature: E, kind: E): TypeStack[E] = TypeStack(NonEmptySeq.of(signature, kind))

  given [E: Show]: Show[TypeStack[E]] = stack =>
    if (stack.levels.size <= 1) stack.levels.map(_.show).toSeq.mkString
    else s"(${stack.levels.reverse.map(_.show).toSeq.mkString(" :: ")})"
}
