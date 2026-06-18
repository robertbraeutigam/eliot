package com.vanillasource.eliot.eliotc.compiler.cache

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** A persisted cache entry: the keys a fact directly depended on, plus its value *when storable*.
  *
  * The entry plays two separate roles with separate serializability needs:
  *
  *   - **Change-detection.** `directDeps` is validated recursively to decide whether the fact changed since last run.
  *     This never needs the value — for a non-serializable fact the parent drills *through* this entry's `directDeps`
  *     down to the leaves. An empty `directDeps` marks a *generated leaf* / starting point (e.g. a `FileStat`), which is
  *     always recomputed.
  *   - **Reconstruction.** `value` supplies the fact to a dependent that must recompute. It is `Some` for serializable
  *     facts (also used for the equality cutoff), and `None` when the value cannot be stored (a `SemValue`-bearing fact)
  *     or was never materialised this run (validated structurally and carried forward).
  *
  * `injected` distinguishes a fact *registered directly* (via `registerFact`, e.g. the dynamic `main` source a backend
  * injects) from one a processor *generates*. No processor can reproduce an injected fact, so it is accepted from the
  * cache on sight rather than regenerated.
  */
case class CacheEntry(
    value: Option[CompilerFact],
    directDeps: Set[CompilerFactKey[?]],
    injected: Boolean = false
)
