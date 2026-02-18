package com.vanillasource.eliot.eliotc.symbolic.fact

import cats.kernel.Eq
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN

sealed trait Qualifier

object Qualifier {
  case object Default extends Qualifier
  case object Type extends Qualifier
  case class Ability(name: String) extends Qualifier
  case class AbilityImplementation(name: AbilityFQN, parameters: Seq[ExpressionValue]) extends Qualifier

  given Eq[Qualifier] = Eq.fromUniversalEquals
}
