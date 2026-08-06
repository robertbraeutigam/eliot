package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.IO

import java.nio.file.Path

/** The default backend: delegates straight to [[FactCache]], which persists the whole cache as a single
  * Java-serialized object graph per configuration. Stateless — every load reads and every save writes the entire
  * accumulated graph.
  */
final case class JavaSerializationCacheBackend(
    targetPath: Path,
    compilerFingerprint: String,
    configFingerprint: String
) extends IncrementalCacheBackend {

  override def load(): IO[Option[FactCacheData]] =
    FactCache.load(targetPath, compilerFingerprint, configFingerprint)

  override def save(data: FactCacheData): IO[Unit] =
    FactCache.save(targetPath, compilerFingerprint, configFingerprint, data)
}
