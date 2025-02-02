package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.module.TypeFQN
import com.vanillasource.eliot.eliotc.source.Sourced

case class FunctionDefinition(
    name: Sourced[String],
    genericParameters: Seq[Sourced[String]],
    arguments: Seq[ArgumentDefinition],
    returnType: Sourced[TypeFQN],
    body: Option[Expression]
)
