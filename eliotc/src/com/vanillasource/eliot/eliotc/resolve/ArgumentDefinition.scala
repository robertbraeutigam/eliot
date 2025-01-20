package com.vanillasource.eliot.eliotc.resolve

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.TypeFQN
import com.vanillasource.eliot.eliotc.source.Sourced

case class ArgumentDefinition(name: Sourced[String], typeReference: Sourced[TypeFQN])
