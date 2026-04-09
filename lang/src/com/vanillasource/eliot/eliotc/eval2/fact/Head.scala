package com.vanillasource.eliot.eliotc.eval2.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN

/** The head of a neutral term. Determines what kind of stuck computation this is. */
sealed trait Head

object Head {

  /** A bound parameter that escaped its binder (free in the current scope). */
  case class Param(name: String) extends Head

  /** A unification metavariable. */
  case class Meta(id: MetaId) extends Head

  /** A top-level value reference whose body is blocked from reducing. */
  case class Ref(vfqn: ValueFQN) extends Head

  given Show[Head] = {
    case Param(name) => name
    case Meta(id)    => s"?${id.value}"
    case Ref(vfqn)   => vfqn.show
  }
}
