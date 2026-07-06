package com.vanillasource.eliot.eliotc.resolve.fact

import cats.Show
import cats.kernel.Eq

sealed trait Qualifier

object Qualifier {
  case object Default                                            extends Qualifier
  case object Type                                               extends Qualifier
  case class Ability(name: String)                              extends Qualifier
  case class AbilityImplementation(name: AbilityFQN, index: Int) extends Qualifier

  /** Renders a qualifier for user-facing messages. An [[AbilityImplementation]] shows just its ability name and index
    * (e.g. `PatternMatch#0`) rather than the full [[AbilityFQN]] structure.
    */
  given Show[Qualifier] with {
    override def show(qualifier: Qualifier): String = qualifier match {
      case Default                            => "Default"
      case Type                               => "Type"
      case Ability(name)                      => name
      case AbilityImplementation(name, index) => s"${name.abilityName}#$index"
    }
  }

  given Eq[Qualifier] = Eq.fromUniversalEquals
}
