package com.vanillasource.eliot.eliotc.core.fact

import cats.{Eq, Show}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact
import com.vanillasource.eliot.eliotc.core.fact.Expression.structuralEqualityOption
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The core AST unifies data and functions into "names values". I.e. everything is a value, even types and functions.
  * Values can be "ad-hoc" (i.e. lambda expressions, i.e function literals), or named. Named values are those values
  * that have a dedicated name, so that they can be referred to in other expression. Note, that the expression stack is
  * split here because a named value can be "abstract" in which case it doesn't actually declare a value expression.
  * @param name
  *   The name of the value (i.e. of the function or type)
  * @param value
  *   The runtime value expression. This is None if the named value is abstract.
  */
case class NamedValue(
    name: Sourced[String],
    value: ExpressionStack[Expression]
)

object NamedValue {
  val signatureEquality: Eq[NamedValue] = (x: NamedValue, y: NamedValue) =>
    structuralEqualityOption.eqv(x.value.signature, y.value.signature)

  given Show[NamedValue] = (namedValue: NamedValue) =>
    s"Value name: ${namedValue.name.value}, value: ${namedValue.value.show}"

}
