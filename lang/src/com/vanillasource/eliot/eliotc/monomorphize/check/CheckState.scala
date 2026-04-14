package com.vanillasource.eliot.eliotc.monomorphize.check

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.MetaId
import com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier

/** Immutable state for the bidirectional type checker.
  *
  * @param env
  *   The current de Bruijn level environment
  * @param nameLevels
  *   Map from parameter name to de Bruijn level
  * @param unifier
  *   The unifier (carries meta store, depth, postponed, errors)
  * @param bindingCache
  *   Cache of fetched NativeBinding SemValues, keyed by ValueFQN
  * @param associatedTypeMetas
  *   Metavariables allocated for references to abstract associated ability types (e.g. `type MagicType` inside `ability
  *   Foo[T]`). Each unique vfqn gets one meta per check session; post-drain the meta is solved by unifying against the
  *   concrete impl's associated-type value.
  */
case class CheckState(
    env: Env,
    nameLevels: Map[String, Int],
    unifier: Unifier,
    bindingCache: Map[ValueFQN, Option[SemValue]],
    associatedTypeMetas: Map[ValueFQN, MetaId]
) {

  /** Bind a parameter with the given name and type, extending both env and nameLevels. */
  def bind(name: String, value: SemValue): CheckState = {
    val level = env.level
    copy(
      env = env.bind(name, value),
      nameLevels = nameLevels + (name -> level)
    )
  }

  def withUnifier(u: Unifier): CheckState = copy(unifier = u)

  def cacheBinding(vfqn: ValueFQN, value: Option[SemValue]): CheckState =
    copy(bindingCache = bindingCache + (vfqn -> value))

  def recordAssociatedTypeMeta(fqn: ValueFQN, id: MetaId): CheckState =
    copy(associatedTypeMetas = associatedTypeMetas + (fqn -> id))
}

object CheckState {
  def initial: CheckState = CheckState(
    Env.empty,
    Map.empty,
    Unifier.create(MetaStore.empty, 0),
    Map.empty,
    Map.empty
  )
}
