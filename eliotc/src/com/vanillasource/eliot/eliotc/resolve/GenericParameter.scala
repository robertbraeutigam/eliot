package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.source.Sourced

trait GenericParameter {
  val name: Sourced[String]
}

object GenericParameter {
  case class UniversalGenericParameter(name: Sourced[String])   extends GenericParameter
  case class ExistentialGenericParameter(name: Sourced[String]) extends GenericParameter
}
