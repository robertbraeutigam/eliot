package com.vanillasource.eliot.eliotc.monomorphize3.check

import com.vanillasource.eliot.eliotc.monomorphize3.domain.*
import com.vanillasource.eliot.eliotc.monomorphize3.unify.Unifier

/** Mutable state for the bidirectional type checker.
  *
  * @param env
  *   The current de Bruijn level environment
  * @param nameLevels
  *   Map from parameter name to de Bruijn level
  * @param unifier
  *   The unifier instance (carries its own meta store, depth, errors)
  */
case class CheckState(
    env: Env,
    nameLevels: Map[String, Int],
    unifier: Unifier
) {

  /** Bind a parameter with the given name and type, extending both env and nameLevels. */
  def bind(name: String, value: SemValue): CheckState = {
    val level = env.level
    CheckState(
      env.bind(name, value),
      nameLevels + (name -> level),
      unifier
    )
  }
}

object CheckState {
  def initial: CheckState = CheckState(
    Env.empty,
    Map.empty,
    Unifier.create(MetaStore.empty, 0)
  )
}
