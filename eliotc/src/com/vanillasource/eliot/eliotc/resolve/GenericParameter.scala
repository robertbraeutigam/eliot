package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.source.Sourced
import cats.syntax.all.*

sealed trait GenericParameter {
  val name: Sourced[String]
}

object GenericParameter {
  case class UniversalGenericParameter(name: Sourced[String])   extends GenericParameter
  case class ExistentialGenericParameter(name: Sourced[String]) extends GenericParameter

  extension (genericParameter: GenericParameter) {
    def shiftToNamespace(namespace: String): GenericParameter = genericParameter match
      case ExistentialGenericParameter(name) => ExistentialGenericParameter(name.map(_ + namespace))
      case UniversalGenericParameter(name)   => UniversalGenericParameter(name.map(_ + namespace))

    def instantiate(): GenericParameter = genericParameter match
      case UniversalGenericParameter(name)   => ExistentialGenericParameter(name)
      case ExistentialGenericParameter(name) => genericParameter
  }
}
