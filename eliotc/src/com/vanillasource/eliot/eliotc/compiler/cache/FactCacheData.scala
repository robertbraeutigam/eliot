package com.vanillasource.eliot.eliotc.compiler.cache

import com.vanillasource.eliot.eliotc.processor.CompilerFactKey

/** The cache data the generator produces and consumes: one [[CacheEntry]] per fact reachable from the build target.
  *
  * There is deliberately **no version stamp** on it. One existed to discard a cache whose persisted fact shapes had
  * changed, which cannot happen without the compiler itself changing — and `CacheFingerprint.compiler` digests the
  * running compiler's whole classpath, so any such change already discards the cache. The fingerprints that guard
  * reuse live in the on-disk header, not here (see [[ContentAddressedCacheBackend]]).
  */
case class FactCacheData(
    entries: Map[CompilerFactKey[?], CacheEntry]
)
