package com.vanillasource.eliot.eliotc.ast.fact

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
    */
  case class AbilityImplementation(name: String, genericParameters: Seq[GenericParameter]) extends Qualifier

  given Eq[Qualifier] = Eq.fromUniversalEquals
}
