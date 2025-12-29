package com.vanillasource.eliot.eliotc.resolve.fact

import com.vanillasource.eliot.eliotc.pos.Sourced

case class DataDefinition(
    name: Sourced[String],
    genericParameters: Seq[GenericParameter],
    fields: Option[Seq[ArgumentDefinition]]
)
