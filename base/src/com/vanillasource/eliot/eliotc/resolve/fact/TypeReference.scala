package com.vanillasource.eliot.eliotc.resolve.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN
import com.vanillasource.eliot.eliotc.source.pos.Sourced

sealed trait TypeReference

object TypeReference {
  case class DirectTypeReference(dataType: Sourced[TypeFQN], genericParameters: Seq[TypeReference])
      extends TypeReference
  case class GenericTypeReference(name: Sourced[String], genericParameters: Seq[TypeReference]) extends TypeReference

  given fullyQualified: Show[TypeReference] = {
    case DirectTypeReference(dataType, genericParameters) =>
      given Show[TypeFQN] = TypeFQN.fullyQualified
      dataType.value.show + showParameters(genericParameters)(using fullyQualified)
    case GenericTypeReference(name, genericParameters)    =>
      name.value + showParameters(genericParameters)(using fullyQualified)
  }

  given unqualified: Show[TypeReference] = {
    case DirectTypeReference(dataType, genericParameters) =>
      given Show[TypeFQN] = TypeFQN.unqualified
      dataType.value.show + showParameters(genericParameters)(using unqualified)
    case GenericTypeReference(name, genericParameters)    =>
      name.value + showParameters(genericParameters)(using unqualified)
  }

  private def showParameters(parameters: Seq[TypeReference])(using Show[TypeReference]): String =
    if (parameters.isEmpty) {
      ""
    } else {
      parameters.map(_.show).mkString("[", ",", "]")
    }

  extension (typeReference: TypeReference) {
    def sourcedAt(source: Sourced[?]): TypeReference = typeReference match
      case DirectTypeReference(dataType, genericParameters) =>
        DirectTypeReference(source.as(dataType.value), genericParameters)
      case GenericTypeReference(name, genericParameters)    =>
        GenericTypeReference(source.as(name.value), genericParameters)

    def sourcedAt(source: TypeReference): TypeReference = typeReference match
      case DirectTypeReference(dataType, genericParameters) =>
        DirectTypeReference(source.source.as(dataType.value), genericParameters)
      case GenericTypeReference(name, genericParameters)    =>
        GenericTypeReference(source.source.as(name.value), genericParameters)

    // FIXME: using show for identifiaction purposes is bad!
    def identifier: String = fullyQualified.show(typeReference)

    def genericParameters: Seq[TypeReference] = typeReference match
      case DirectTypeReference(_, genericParameters)  => genericParameters
      case GenericTypeReference(_, genericParameters) => genericParameters

    def source = typeReference match
      case DirectTypeReference(dataType, _) => dataType
      case GenericTypeReference(name, _)    => name
  }
}
