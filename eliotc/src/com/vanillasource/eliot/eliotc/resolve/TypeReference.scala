package com.vanillasource.eliot.eliotc.resolve

import cats.Show
import com.vanillasource.eliot.eliotc.module.TypeFQN
import com.vanillasource.eliot.eliotc.source.Sourced
import cats.syntax.all.*

sealed trait TypeReference

object TypeReference {
  case class DirectTypeReference(dataType: Sourced[TypeFQN])   extends TypeReference
  case class ForAllGenericTypeReference(name: Sourced[String]) extends TypeReference
  case class ExistsGenericTypeReference(name: Sourced[String]) extends TypeReference

  given Show[TypeReference] = {
    case DirectTypeReference(dataType)    => dataType.value.show
    case ForAllGenericTypeReference(name) => name.value
    case ExistsGenericTypeReference(name) => name.value
  }

  extension (typeReference: TypeReference) {
    def sourcedAt(source: Sourced[_]): TypeReference = typeReference match
      case DirectTypeReference(dataType)    => DirectTypeReference(source.as(dataType.value))
      case ForAllGenericTypeReference(name) => ForAllGenericTypeReference(source.as(name.value))
      case ExistsGenericTypeReference(name) => ExistsGenericTypeReference(source.as(name.value))

    def sourcedAt(source: TypeReference): TypeReference = typeReference match
      case DirectTypeReference(dataType)    => DirectTypeReference(source.source.as(dataType.value))
      case ForAllGenericTypeReference(name) => ForAllGenericTypeReference(source.source.as(name.value))
      case ExistsGenericTypeReference(name) => ExistsGenericTypeReference(source.source.as(name.value))

    def instantiateType(): TypeReference = typeReference match
      case DirectTypeReference(_)           => typeReference
      case ForAllGenericTypeReference(name) => ExistsGenericTypeReference(name)
      case ExistsGenericTypeReference(_)    => typeReference

    private def source = typeReference match
      case DirectTypeReference(dataType)    => dataType
      case ForAllGenericTypeReference(name) => name
      case ExistsGenericTypeReference(name) => name

  }
}
