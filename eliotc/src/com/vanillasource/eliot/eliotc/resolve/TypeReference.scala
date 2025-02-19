package com.vanillasource.eliot.eliotc.resolve

import cats.{Order, Show}
import com.vanillasource.eliot.eliotc.module.TypeFQN
import com.vanillasource.eliot.eliotc.source.Sourced
import cats.syntax.all.*

sealed trait TypeReference

object TypeReference {
  case class DirectTypeReference(dataType: Sourced[TypeFQN], genericParameters: Seq[TypeReference])
      extends TypeReference
  case class GenericTypeReference(name: Sourced[String], genericParameters: Seq[TypeReference]) extends TypeReference

  given Show[TypeReference] = {
    case DirectTypeReference(dataType, genericParameters) =>
      dataType.value.show + (if (genericParameters.isEmpty) ""
                             else genericParameters.map(_.show).mkString("[", ", ", "]"))
    case GenericTypeReference(name, genericParameters)    =>
      name.value + (if (genericParameters.isEmpty) ""
                    else genericParameters.map(_.show).mkString("[", ", ", "]"))
  }

  extension (typeReference: TypeReference) {
    def sourcedAt(source: Sourced[_]): TypeReference = typeReference match
      case DirectTypeReference(dataType, genericParameters) =>
        DirectTypeReference(source.as(dataType.value), genericParameters)
      case GenericTypeReference(name, genericParameters)    =>
        GenericTypeReference(source.as(name.value), genericParameters)

    def sourcedAt(source: TypeReference): TypeReference = typeReference match
      case DirectTypeReference(dataType, genericParameters) =>
        DirectTypeReference(source.source.as(dataType.value), genericParameters)
      case GenericTypeReference(name, genericParameters)    =>
        GenericTypeReference(source.source.as(name.value), genericParameters)

    def shiftGenericToNamespace(namespace: String): TypeReference = typeReference match
      case DirectTypeReference(_, _)                     => typeReference
      case GenericTypeReference(name, genericParameters) =>
        GenericTypeReference(name.map(_ + namespace), genericParameters.map(_.shiftGenericToNamespace(namespace)))

    def name: String = typeReference match
      case DirectTypeReference(dataType, _) => dataType.value.show
      case GenericTypeReference(name, _)    => name.value

    private def source = typeReference match
      case DirectTypeReference(dataType, _) => dataType
      case GenericTypeReference(name, _)    => name
  }
}
