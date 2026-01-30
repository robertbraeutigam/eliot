package com.vanillasource.eliot.eliotc.typesystem2.types

import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.typesystem2.fact.TypedExpression

/** An expression value paired with its typed expression representation. */
case class TypeWithTyped(exprValue: ExpressionValue, typed: TypedExpression)

object TypeWithTyped {

  /** Result of building type expressions from an expression stack.
    *
    * @param exprValue
    *   The signature type (type of the value)
    * @param typedLevels
    *   All typed levels from signature (index 0) to higher levels (index 1, 2, ...)
    */
  case class Stack(exprValue: ExpressionValue, typedLevels: Seq[TypedExpression])
}
