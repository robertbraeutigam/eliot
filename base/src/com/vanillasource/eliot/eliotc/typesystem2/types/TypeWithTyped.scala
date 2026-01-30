package com.vanillasource.eliot.eliotc.typesystem2.types

import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.typesystem2.fact.TypedExpression

/** An expression value paired with its typed expression representation. */
case class TypeWithTyped(exprValue: ExpressionValue, typed: TypedExpression)
