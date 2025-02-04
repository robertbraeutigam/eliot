package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.module.TypeFQN
import com.vanillasource.eliot.eliotc.source.Sourced

sealed trait TypeReference

object TypeReference {
  case class DirectTypeReference(dataType: Sourced[TypeFQN]) extends TypeReference
  case class GenericTypeReference(name: Sourced[String])     extends TypeReference
}
