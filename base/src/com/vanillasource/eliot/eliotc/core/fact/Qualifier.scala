package com.vanillasource.eliot.eliotc.core.fact

import cats.kernel.Eq

sealed trait Qualifier

object Qualifier {
  case object Default extends Qualifier
  case object Type extends Qualifier
  case class Ability(name: String) extends Qualifier
  case class AbilityImplementation(name: String, parameters: Seq[Expression]) extends Qualifier

  given Eq[Qualifier] = Eq.fromUniversalEquals
}
