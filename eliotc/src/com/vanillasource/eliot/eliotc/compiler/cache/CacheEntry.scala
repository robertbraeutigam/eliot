package com.vanillasource.eliot.eliotc.compiler.cache

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** A persisted fact together with the keys it directly depended on when it was produced.
  *
  * `directDeps` is the metadata the incremental engine needs: validating a cached fact means re-resolving each of these
  * keys and checking that it still has the value it had last run (see
  * [[com.vanillasource.eliot.eliotc.compiler.IncrementalFactGenerator]]). An empty `directDeps` marks a generated leaf /
  * "starting point" (a fact a processor produces with no dependencies, e.g. a `stat` of a source file) which is always
  * regenerated.
  *
  * `injected` distinguishes a fact that was *registered directly* (via `registerFact`, e.g. the dynamic main source a
  * backend injects) from one a processor *generates*. No processor can reproduce an injected fact, so it must be
  * accepted from the cache rather than regenerated; its cached value is authoritative for the build target.
  */
case class CacheEntry(
    fact: CompilerFact,
    directDeps: Set[CompilerFactKey[?]],
    injected: Boolean = false
)
