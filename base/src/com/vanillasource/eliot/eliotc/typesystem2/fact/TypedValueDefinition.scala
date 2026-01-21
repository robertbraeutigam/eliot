package com.vanillasource.eliot.eliotc.typesystem2.fact

import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class TypedValueDefinition(
    name: Sourced[String],
    typeExpression: Sourced[ExpressionStack[TypedExpression]],
    body: Option[Sourced[TypedExpression]]
)
