package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.source.Sourced

case class FunctionDefinition(
    name: Sourced[String],
    genericParameters: Seq[Sourced[String]],
    arguments: Seq[ArgumentDefinition],
    returnType: TypeReference,
    body: Option[Expression]
)
