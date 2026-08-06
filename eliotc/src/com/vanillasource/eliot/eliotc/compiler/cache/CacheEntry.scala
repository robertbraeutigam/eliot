package com.vanillasource.eliot.eliotc.compiler.cache

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** A persisted cache entry: the keys a fact directly depended on, plus its value *when storable*.
  *
  * The entry plays two separate roles with separate needs:
  *
  *   - **Change-detection.** `directDeps` is validated recursively to decide whether the fact changed since last run.
  *     This never needs the value — for a non-serializable fact the parent drills *through* this entry's `directDeps`
  *     down to the leaves. An empty `directDeps` marks a *leaf* / starting point (e.g. a `FileStat`, a computed
  *     synthetic source, or a fact registered outside any generation), which is always recomputed and compared.
  *   - **Reconstruction.** [[value]] supplies the fact to a dependent that must recompute. It is present for
  *     persistable facts, and absent when the value cannot be stored (a `SemValue`-bearing fact) or was never
  *     materialised this run (validated structurally and carried forward).
  *
  * The value is held as a [[CachedValue]] rather than an `Option[CompilerFact]` so that a stored one can stay on disk
  * until something consumes it, and so that the equality cutoff can be a 16-byte comparison instead of a
  * materialise-and-compare. [[hasValue]] and [[matches]] are what change-detection asks, and neither forces a read.
  */
case class CacheEntry(
    stored: CachedValue,
    directDeps: Set[CompilerFactKey[?]]
) {

  /** The fact, read on demand. Prefer [[hasValue]] / [[matches]] where the value itself is not needed. */
  def value: Option[CompilerFact] = stored.materialise

  /** Whether a value is held, without reading it. */
  def hasValue: Boolean = stored.isPresent

  /** Whether `fact`, freshly recomputed, is the value this entry holds — the equality cutoff. */
  def matches(fact: CompilerFact): Boolean = stored.matches(fact)
}

object CacheEntry {

  /** Build an entry around a value already in memory. */
  def apply(value: Option[CompilerFact], directDeps: Set[CompilerFactKey[?]]): CacheEntry =
    CacheEntry(CachedValue.of(value), directDeps)
}
