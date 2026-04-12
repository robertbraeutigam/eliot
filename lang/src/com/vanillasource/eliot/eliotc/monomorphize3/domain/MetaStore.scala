package com.vanillasource.eliot.eliotc.monomorphize3.domain

import scala.collection.immutable.IntMap

/** Mutable-style metavariable store for unification. Backed by IntMap for efficient allocation and lookup.
  *
  * @param entries
  *   Map from meta id to its solution (None = unsolved, Some = solved)
  * @param nextId
  *   Next fresh meta id to allocate
  */
case class MetaStore(entries: IntMap[Option[SemValue]], nextId: Int) {

  /** Allocate a fresh unsolved metavariable. Returns the new id and updated store. */
  def fresh: (SemValue.MetaId, MetaStore) = {
    val id = SemValue.MetaId(nextId)
    (id, MetaStore(entries.updated(nextId, None), nextId + 1))
  }

  /** Solve a metavariable. */
  def solve(id: SemValue.MetaId, value: SemValue): MetaStore =
    MetaStore(entries.updated(id.value, Some(value)), nextId)

  /** Look up a metavariable's current solution. */
  def lookup(id: SemValue.MetaId): Option[SemValue] =
    entries.getOrElse(id.value, None)
}

object MetaStore {
  val empty: MetaStore = MetaStore(IntMap.empty, 0)
}
