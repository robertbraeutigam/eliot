package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.IO
import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactKeyCodecs

import java.nio.file.Path

/** The persistence boundary the session drives: seed the in-memory cache from disk ([[load]]) and flush it back
  * ([[save]]). Two implementations exist so the on-disk strategy can be A/B-measured against the current one:
  *
  *   - [[JavaSerializationCacheBackend]] — the default: one Java-serialized object graph per config
  *     (`FactCache`). Compact (shared structure), but load and save both scale with the *whole* accumulated graph.
  *   - [[ContentAddressedCacheBackend]] — the replacement §13 is building towards: explicit codecs over an
  *     append-only content-addressed object store. Selected by `ELIOT_CACHE_BACKEND=store` while it is measured
  *     against the default.
  *
  * See `docs/incremental-compilation.md` §11 for the load/save cost this spike targets.
  */
trait IncrementalCacheBackend {
  def load(): IO[Option[FactCacheData]]
  def save(data: FactCacheData): IO[Unit]
}

object IncrementalCacheBackend {

  /** Pick the backend for this session. Defaults to the Java-serialization graph, so every build and test keeps the
    * current behaviour unless explicitly measuring the replacement.
    */
  def create(
      targetPath: Path,
      compilerFingerprint: String,
      configFingerprint: String,
      keyCodecs: FactKeyCodecs.Registry
  ): IO[IncrementalCacheBackend] =
    sys.env.get("ELIOT_CACHE_BACKEND") match {
      case Some("store") =>
        ContentAddressedCacheBackend.create(targetPath, compilerFingerprint, configFingerprint, keyCodecs)
      case _               => IO.pure(JavaSerializationCacheBackend(targetPath, compilerFingerprint, configFingerprint))
    }
}
