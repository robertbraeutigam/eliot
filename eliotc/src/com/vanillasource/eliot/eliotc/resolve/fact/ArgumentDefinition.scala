package com.vanillasource.eliot.eliotc.resolve.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.pos.Sourced

case class ArgumentDefinition(name: Sourced[String], typeReference: TypeReference)
