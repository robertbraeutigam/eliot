package com.vanillasource.eliot.eliotc.resolve.fact

import cats.Show
import cats.kernel.Eq

sealed trait Qualifier

object Qualifier {
  case object Default                                            extends Qualifier
  case object Type                                               extends Qualifier
  case object Meta                                               extends Qualifier
  case class Ability(name: String)                                    extends Qualifier
  case class AbilityImplementation(name: AbilityFQN, pattern: String) extends Qualifier

  /** Renders a qualifier for user-facing messages. An [[AbilityImplementation]] shows its ability name and pattern key
    * (e.g. `PatternMatch#Person`) rather than the full [[AbilityFQN]] structure.
    */
  given Show[Qualifier] with {
    override def show(qualifier: Qualifier): String = qualifier match {
      case Default                                => "Default"
      case Type                                   => "Type"
      case Meta                                   => "Meta"
      case Ability(name)                          => name
      case AbilityImplementation(name, pattern)   => s"${name.abilityName}#$pattern"
    }
  }

  given Eq[Qualifier] = Eq.fromUniversalEquals
}
