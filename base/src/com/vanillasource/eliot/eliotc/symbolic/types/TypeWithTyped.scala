package com.vanillasource.eliot.eliotc.symbolic.types

import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression

/** An expression value paired with its typed expression representation. */
case class TypeWithTyped(exprValue: ExpressionValue, typed: TypedExpression)
