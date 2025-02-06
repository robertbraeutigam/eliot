package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.module.TypeFQN
import com.vanillasource.eliot.eliotc.source.Sourced
import cats.syntax.all._

sealed trait TypeReference

object TypeReference {
  case class DirectTypeReference(dataType: Sourced[TypeFQN]) extends TypeReference
  case class GenericTypeReference(name: Sourced[String])     extends TypeReference

  extension (typeReference: TypeReference) {
    def sourcedAt(source: Sourced[_]): TypeReference = typeReference match
      case DirectTypeReference(dataType) => DirectTypeReference(source.as(dataType.value))
      case GenericTypeReference(name)    => GenericTypeReference(source.as(name.value))
  }
}
