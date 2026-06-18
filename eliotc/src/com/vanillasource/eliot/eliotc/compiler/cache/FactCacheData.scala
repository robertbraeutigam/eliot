package com.vanillasource.eliot.eliotc.compiler.cache

import com.vanillasource.eliot.eliotc.processor.CompilerFactKey

/** The cache data the generator produces and consumes: a version stamp (bumped whenever a persisted fact's shape
  * changes, so a stale cache is discarded rather than misread) and one [[CacheEntry]] per fact reachable from the build
  * target. The compiler/configuration fingerprints that also guard reuse live in the on-disk header, not here (see
  * [[FactCache]]).
  */
case class FactCacheData(
    version: Int,
    entries: Map[CompilerFactKey[?], CacheEntry]
)
