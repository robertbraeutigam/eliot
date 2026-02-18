package com.vanillasource.eliot.eliotc.core.fact

import cats.kernel.Eq
import com.vanillasource.eliot.eliotc.source.content.Sourced

sealed trait Qualifier

object Qualifier {
  case object Default extends Qualifier
  case object Type extends Qualifier
  case class Ability(name: String) extends Qualifier
  case class AbilityImplementation(name: Sourced[String], parameters: Seq[Expression]) extends Qualifier

  given Eq[Qualifier] = Eq.fromUniversalEquals
}
