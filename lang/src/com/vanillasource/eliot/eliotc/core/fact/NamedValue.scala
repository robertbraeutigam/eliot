package com.vanillasource.eliot.eliotc.core.fact

import cats.{Eq, Show}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact
import com.vanillasource.eliot.eliotc.core.fact.Expression.structuralEquality
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The core AST unifies data and functions into "names values". I.e. everything is a value, even types and functions.
  * Values can be "ad-hoc" (i.e. lambda expressions, i.e function literals), or named. Named values are those values
  * that have a dedicated name, so that they can be referred to in other expression.
  * @param name
  *   The name of the value (i.e. of the function or type)
  * @param runtime
  *   The runtime value expression. This is None if the named value is abstract.
  * @param typeStack
  *   The type levels for this value. The signature is at index 0.
  */
case class NamedValue(
    qualifiedName: Sourced[QualifiedName],
    runtime: Option[Expression],
    typeStack: TypeStack[Expression]
)

object NamedValue {
  val signatureEquality: Eq[NamedValue] = (x: NamedValue, y: NamedValue) =>
    structuralEquality.eqv(x.typeStack.signature, y.typeStack.signature)

  given Show[NamedValue] = (namedValue: NamedValue) =>
    s"${namedValue.qualifiedName.value}: ${namedValue.typeStack.show}"

}
