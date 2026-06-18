package com.vanillasource.eliot.eliotc.compiler.cache

import com.vanillasource.eliot.eliotc.processor.CompilerFactKey

/** The full persisted incremental-compilation cache: a version stamp (bumped whenever a persisted fact's shape changes,
  * so a stale cache is discarded rather than misread) and one [[CacheEntry]] per successfully generated fact.
  */
case class FactCacheData(
    version: Int,
    entries: Map[CompilerFactKey[?], CacheEntry]
)
