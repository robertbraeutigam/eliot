package com.vanillasource.eliot.eliotc.resolve.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.pos.Sourced

sealed trait GenericParameter {
  val name: Sourced[String]
  val genericParameters: Seq[TypeReference]
}

object GenericParameter {
  case class UniversalGenericParameter(name: Sourced[String], genericParameters: Seq[TypeReference])
      extends GenericParameter
  case class ExistentialGenericParameter(name: Sourced[String], genericParameters: Seq[TypeReference])
      extends GenericParameter
}
