package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.source.Sourced

case class FunctionDefinition(
    name: Sourced[String],
    arguments: Seq[ArgumentDefinition],
    typeDefinition: TypeDefinition,
    body: FunctionBody
)
