package com.vanillasource.eliot.eliotc.typesystem2.types

import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem2.fact.{NormalizedExpression, TypedExpression}

/** A normalized expression paired with its typed expression representation. */
case class TypeWithTyped(normalized: NormalizedExpression, typed: TypedExpression)

object TypeWithTyped {

  /** A normalized expression paired with a typed expression stack. */
  case class Stack(normalized: NormalizedExpression, typed: Sourced[ExpressionStack[TypedExpression]])
}
