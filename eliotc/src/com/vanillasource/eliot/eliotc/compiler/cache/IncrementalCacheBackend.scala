package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.IO

import java.nio.file.Path

/** The persistence boundary the session drives: seed the in-memory cache from disk ([[load]]) and flush it back
  * ([[save]]). Two implementations exist so the on-disk strategy can be A/B-measured against the current one:
  *
  *   - [[JavaSerializationCacheBackend]] — the default: one Java-serialized object graph per config
  *     (`FactCache`). Compact (shared structure), but load and save both scale with the *whole* accumulated graph.
  *   - [[MvStoreCacheBackend]] — a spike (H2 MVStore): one persistent per-entry key/value store that writes only
  *     the entries that actually changed since the last run. Selected only when `ELIOT_CACHE_BACKEND=mvstore`.
  *
  * See `docs/incremental-compilation.md` §11 for the load/save cost this spike targets.
  */
trait IncrementalCacheBackend {
  def load(): IO[Option[FactCacheData]]
  def save(data: FactCacheData): IO[Unit]
}

object IncrementalCacheBackend {

  /** Pick the backend for this session. Defaults to the Java-serialization graph; the MVStore spike is opt-in via
    * `ELIOT_CACHE_BACKEND=mvstore`, so every build and test keeps the current behaviour unless explicitly measuring.
    */
  def create(targetPath: Path, compilerFingerprint: String, configFingerprint: String): IO[IncrementalCacheBackend] =
    sys.env.get("ELIOT_CACHE_BACKEND") match {
      case Some("mvstore") => MvStoreCacheBackend.open(targetPath, compilerFingerprint, configFingerprint)
      case _               => IO.pure(JavaSerializationCacheBackend(targetPath, compilerFingerprint, configFingerprint))
    }
}
