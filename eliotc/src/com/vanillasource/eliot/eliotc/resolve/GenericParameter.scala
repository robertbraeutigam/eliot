package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.source.Sourced
import cats.syntax.all.*

sealed trait GenericParameter {
  val name: Sourced[String]
  val genericParameters: Seq[TypeReference]
}

object GenericParameter {
  case class UniversalGenericParameter(name: Sourced[String], genericParameters: Seq[TypeReference])
      extends GenericParameter
  case class ExistentialGenericParameter(name: Sourced[String], genericParameters: Seq[TypeReference])
      extends GenericParameter

  extension (genericParameter: GenericParameter) {
    def shiftToNamespace(namespace: String): GenericParameter = genericParameter match
      case ExistentialGenericParameter(name, genericParameters) =>
        ExistentialGenericParameter(
          name.map(_ + namespace),
          genericParameters.map(_.shiftGenericToNamespace(namespace))
        )
      case UniversalGenericParameter(name, genericParameters)   =>
        UniversalGenericParameter(name.map(_ + namespace), genericParameters.map(_.shiftGenericToNamespace(namespace)))

    def instantiate(): GenericParameter = genericParameter match
      case UniversalGenericParameter(name, genericParameters) => ExistentialGenericParameter(name, genericParameters)
      case ExistentialGenericParameter(_, _)                  => genericParameter
  }
}
