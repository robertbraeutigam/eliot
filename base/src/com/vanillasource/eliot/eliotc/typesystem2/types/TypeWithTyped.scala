package com.vanillasource.eliot.eliotc.typesystem2.types

import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem2.fact.TypedExpression

/** An expression value paired with its typed expression representation. */
case class TypeWithTyped(exprValue: ExpressionValue, typed: TypedExpression)

object TypeWithTyped {

  /** An expression value paired with a typed expression stack. */
  case class Stack(exprValue: ExpressionValue, typed: Sourced[ExpressionStack[TypedExpression]])
}
