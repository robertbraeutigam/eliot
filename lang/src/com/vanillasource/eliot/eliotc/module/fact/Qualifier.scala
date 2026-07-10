package com.vanillasource.eliot.eliotc.module.fact

import cats.Show
import cats.kernel.Eq

/** A qualifier is a sort-of additional namespace names can live in. It is used as technical namespaces as opposed to
  * user defined ones.
  */
sealed trait Qualifier

object Qualifier {

  /** A name in the "default" namespace, like all "normal" user-defined values.
    */
  case object Default extends Qualifier

  /** Denotes a name that points to a Type constructor.
    */
  case object Type extends Qualifier

  /** Functions belonging to a given ability.
    */
  case class Ability(name: String) extends Qualifier

  /** Function belongs to the given ability implementation.
    *
    * `pattern` is a canonical, position-independent key of *what* the implementation implements — the surface string of
    * its type-argument pattern plus its `where` guard, e.g. `Int[L1, H1], Int[L2, H2]` or
    * `Int[Smin, Smax], Int[Tmin, Tmax] where lessThanOrEqual…`.
    * It is the implementation's identity: two `implement` blocks are the same instance iff they share `(ability,
    * pattern)`, independent of source file, position, or order. This is what lets an implementation be split across
    * layers (abstract signatures in the base, bodies in a platform layer) and merged like any other name, and it keeps
    * two genuinely distinct instances of one ability in a module (e.g. `Eq[Type]` vs `Eq[String]`, or a guarded
    * instance whose `where` clause is part of its identity) apart. The pattern is *also* recoverable from the synthetic
    * marker function's signature;
    * this key is that same information, lifted into the name so the merge and dispatch can compare it directly.
    */
  case class AbilityImplementation(name: String, pattern: String) extends Qualifier

  /** Renders a qualifier for user-facing messages. An [[AbilityImplementation]] shows its ability name and pattern key
    * (e.g. `PatternMatch#Person` or `Arithmetic#Int[L1, H1], Int[L2, H2]`).
    */
  given Show[Qualifier] with {
    override def show(qualifier: Qualifier): String = qualifier match {
      case Default                              => "Default"
      case Type                                 => "Type"
      case Ability(name)                        => name
      case AbilityImplementation(name, pattern) => s"$name#$pattern"
    }
  }

  given Eq[Qualifier] = Eq.fromUniversalEquals
}
