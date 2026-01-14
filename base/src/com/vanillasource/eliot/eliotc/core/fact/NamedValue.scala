package com.vanillasource.eliot.eliotc.core.fact

import cats.Eq
import com.vanillasource.eliot.eliotc.core.fact.Expression.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The core AST unifies data and functions into "names values". I.e. everything is a value, even types and functions.
  * Values can be "ad-hoc" (i.e. lambda expressions, i.e function literals), or named. Named values are those values
  * that have a dedicated name, so that they can be referred to in other expression. Note, that the expression stack is
  * split here because a named value can be "abstract" in which case it doesn't actually declare a value expression.
  * @param name
  *   The name of the value (i.e. of the function or type)
  * @param typeStack
  *   The expression stack representing the layers above the runtime value.
  * @param value
  *   The runtime value expression. This is None if the named value is abstract.
  */
case class NamedValue(name: Sourced[String], typeStack: ExpressionStack, value: Option[Sourced[Expression]])

object NamedValue {
  val signatureEquality: Eq[NamedValue] = (x: NamedValue, y: NamedValue) =>
    expressionStackEquality.eqv(x.typeStack, y.typeStack)

  private val expressionStackEquality: Eq[ExpressionStack] = (x: ExpressionStack, y: ExpressionStack) =>
    x.expressions.length == y.expressions.length &&
      (x.expressions zip y.expressions).forall(expressionEquality.eqv)

  private val expressionEquality: Eq[Expression] = (x: Expression, y: Expression) =>
    (x, y) match {
      case (NamedValueReference(n1, q1), NamedValueReference(n2, q2)) =>
        n1.value == n2.value && q1.map(_.value) == q2.map(_.value)
      case (FunctionApplication(t1, a1), FunctionApplication(t2, a2)) =>
        expressionStackEquality.eqv(t1.value, t2.value) && expressionStackEquality.eqv(a1.value, a2.value)
      case (FunctionLiteral(p1, pt1, b1), FunctionLiteral(p2, pt2, b2))  =>
        p1.value == p2.value && expressionStackEquality.eqv(pt1, pt2) && expressionStackEquality.eqv(b1.value, b2.value)
      case (IntegerLiteral(i1), IntegerLiteral(i2))                     => i1.value == i2.value
      case (StringLiteral(s1), StringLiteral(s2))                       => s1.value == s2.value
      case _                                                            => false
    }
}
