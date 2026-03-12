package com.vanillasource.eliot.eliotc.symbolic.fact

import cats.kernel.Eq
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicType

sealed trait Qualifier

object Qualifier {
  case object Default extends Qualifier
  case object Type extends Qualifier
  case class Ability(name: String) extends Qualifier
  case class AbilityImplementation(name: AbilityFQN, parameters: Seq[SymbolicType]) extends Qualifier

  given Eq[Qualifier] = Eq.fromUniversalEquals
}
