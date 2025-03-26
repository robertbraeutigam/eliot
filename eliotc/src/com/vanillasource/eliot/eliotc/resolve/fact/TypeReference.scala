package com.vanillasource.eliot.eliotc.resolve.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN
import com.vanillasource.eliot.eliotc.source.Sourced

sealed trait TypeReference

object TypeReference {
  case class DirectTypeReference(dataType: Sourced[TypeFQN], genericParameters: Seq[TypeReference])
      extends TypeReference
  case class GenericTypeReference(name: Sourced[String], genericParameters: Seq[TypeReference]) extends TypeReference

  // FIXME: using this for identifier is wrong, do something about this
  given Show[TypeReference] = {
    case DirectTypeReference(dataType, genericParameters) =>
      dataType.value.show + (if (genericParameters.isEmpty) ""
                             else genericParameters.map(_.show).mkString("[", ",", "]"))
    case GenericTypeReference(name, genericParameters)    =>
      name.value + (if (genericParameters.isEmpty) ""
                    else genericParameters.map(_.show).mkString("[", ",", "]"))
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

    def identifier: String = typeReference.show

    def genericParameters: Seq[TypeReference] = typeReference match
      case DirectTypeReference(_, genericParameters)  => genericParameters
      case GenericTypeReference(_, genericParameters) => genericParameters

    private def source = typeReference match
      case DirectTypeReference(dataType, _) => dataType
      case GenericTypeReference(name, _)    => name
  }
}
