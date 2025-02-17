package com.vanillasource.eliot.eliotc.resolve

import cats.{Order, Show}
import com.vanillasource.eliot.eliotc.module.TypeFQN
import com.vanillasource.eliot.eliotc.source.Sourced
import cats.syntax.all.*

sealed trait TypeReference

object TypeReference {
  case class DirectTypeReference(dataType: Sourced[TypeFQN]) extends TypeReference
  case class GenericTypeReference(name: Sourced[String])     extends TypeReference

  given Show[TypeReference] = {
    case DirectTypeReference(dataType) => dataType.value.show
    case GenericTypeReference(name)    => name.value
  }

  given Order[TypeReference] = Order.by(_.source)

  extension (typeReference: TypeReference) {
    def sourcedAt(source: Sourced[_]): TypeReference = typeReference match
      case DirectTypeReference(dataType) => DirectTypeReference(source.as(dataType.value))
      case GenericTypeReference(name)    => GenericTypeReference(source.as(name.value))

    def sourcedAt(source: TypeReference): TypeReference = typeReference match
      case DirectTypeReference(dataType) => DirectTypeReference(source.source.as(dataType.value))
      case GenericTypeReference(name)    => GenericTypeReference(source.source.as(name.value))

    def shiftGenericToNamespace(namespace: String): TypeReference = typeReference match
      case DirectTypeReference(dataType) => typeReference
      case GenericTypeReference(name)    => GenericTypeReference(name.map(_ + namespace))

    def name: String = typeReference match
      case DirectTypeReference(dataType) => dataType.value.show
      case GenericTypeReference(name)    => name.value

    private def source = typeReference match
      case DirectTypeReference(dataType) => dataType
      case GenericTypeReference(name)    => name
  }
}
