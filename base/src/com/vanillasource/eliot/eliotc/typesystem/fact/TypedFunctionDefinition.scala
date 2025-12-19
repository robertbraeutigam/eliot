package com.vanillasource.eliot.eliotc.typesystem.fact

import com.vanillasource.eliot.eliotc.resolve.fact.GenericParameter
import com.vanillasource.eliot.eliotc.source.pos.Sourced

case class TypedFunctionDefinition(
    name: Sourced[String],
    genericParameters: Seq[GenericParameter],
    body: Sourced[TypedExpression]
)
