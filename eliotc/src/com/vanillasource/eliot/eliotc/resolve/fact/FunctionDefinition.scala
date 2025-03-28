package com.vanillasource.eliot.eliotc.resolve.fact

import com.vanillasource.eliot.eliotc.source.Sourced

case class FunctionDefinition(
    name: Sourced[String],
    genericParameters: Seq[GenericParameter],
    valueType: TypeReference,
    body: Option[Sourced[Expression]]
)
