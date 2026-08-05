package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging

import java.io.{FileInputStream, FileOutputStream, InputStream, ObjectInputStream, ObjectOutputStream, OutputStream}
import java.nio.file.{Files, Path}

/** Persistence for the incremental-compilation cache, using Java serialization.
  *
  * Three eliot-specific realities are handled here:
  *
  *   1. `java.nio.file.Path` is not `Serializable`; the streams below transparently swap any `Path` for a serializable
  *      stand-in on write and back on read, so facts may carry `Path`s freely.
  *   2. Some fact *values* are fundamentally non-serializable (a `SemValue` carrying Scala closures). Rather than drop
  *      such a fact wholesale, `save` keeps its **edges** (`directDeps`) and stores `value = None`, so change-detection
  *      can still drill *through* it next run. Only the value is dropped, never the dependency structure. (An entry whose
  *      *key* or `directDeps` cannot serialize is dropped entirely, since its edge is unusable; this is not expected to
  *      happen — keys are first-order.)
  *   3. A different compiler or configuration must never be served a stale cache. Because the two fingerprints have
  *      opposite lifecycles, they are placed differently:
  *        - The **configuration** fingerprint (prod vs test, selected `main`, backend) changes rarely and the variants
  *          should *coexist*, so it goes in the **file name** — `.eliot-cache-<config>`. Two configs against the same
  *          target directory keep separate files instead of overwriting each other on every switch.
  *        - The **compiler** fingerprint changes on every recompile of the compiler and an old-compiler cache is dead
  *          weight, so it stays in the **header**: a compiler change fails the header check and `save` overwrites the
  *          same-named file in place, rather than minting a fresh file per compiler build.
  *      The full configuration fingerprint is *also* kept in the header as an exact-match backstop (the name is a
  *      truncated, sanitized index, not the source of truth); `load` returns the cache only if header and version match.
  *
  * Both directions are fail-safe: `load` returns `None` on any problem (missing file, deserialization error, version or
  * fingerprint mismatch), and `save` warns rather than failing the build if writing is impossible.
  *
  * Bump [[CACHE_VERSION]] whenever a persisted fact's shape changes, so an out-of-date cache is discarded.
  */
object FactCache extends Logging {
  val CACHE_VERSION: Int         = 34
  private val CACHE_FILE: String = ".eliot-cache"

  /** The complete on-disk image: the cache data plus the header that must match the current run to reuse it. */
  private case class OnDiskCache(compilerFingerprint: String, configFingerprint: String, data: FactCacheData)

  /** The cache file for a given configuration. The configuration fingerprint is folded into the name so distinct
    * configurations (prod, test, a different `main`) coexist under one target directory instead of clobbering each
    * other. The fingerprint is expected to be a hex digest; it is sanitized and truncated only to bound the name — the
    * header carries the full value for the exact check, so a (hex-impossible) name collision degrades to a cold build,
    * never to serving the wrong cache.
    */
  def cacheFile(targetDir: Path, configFingerprint: String): Path =
    targetDir.resolve(s"$CACHE_FILE-${configFingerprint.filter(_.isLetterOrDigit).take(16)}")

  def load(targetDir: Path, compilerFingerprint: String, configFingerprint: String): IO[Option[FactCacheData]] =
    IO.blocking {
      val file = cacheFile(targetDir, configFingerprint)
      if (Files.exists(file)) {
        val in = new FactInputStream(new FileInputStream(file.toFile))
        try {
          val onDisk = in.readObject().asInstanceOf[OnDiskCache]
          Option.when(
            onDisk.data.version == CACHE_VERSION &&
              onDisk.compilerFingerprint == compilerFingerprint &&
              onDisk.configFingerprint == configFingerprint
          )(onDisk.data)
        } finally in.close()
      } else None
    }.handleErrorWith(t => warn[IO]("Could not read incremental cache; doing a full compilation.", t).as(None))

  def save(
      targetDir: Path,
      compilerFingerprint: String,
      configFingerprint: String,
      data: FactCacheData
  ): IO[Unit] =
    IO.blocking {
      Files.createDirectories(targetDir)
      val prepared = data.entries.flatMap { case (key, entry) =>
        if (canSerialize(key) && canSerialize(entry.directDeps))
          Some(key -> entry.copy(value = entry.value.filter(canSerialize))) // keep edges; drop a non-serializable value
        else None                                                            // unusable edge ⇒ drop the whole entry
      }
      val onDisk = OnDiskCache(compilerFingerprint, configFingerprint, FactCacheData(data.version, prepared))
      val out    = new FactOutputStream(new FileOutputStream(cacheFile(targetDir, configFingerprint).toFile))
      try out.writeObject(onDisk)
      finally out.close()
      (data.entries.size, prepared.size, prepared.count(_._2.value.isEmpty))
    }.flatMap { case (total, kept, valueless) =>
      debug[IO](s"Incremental cache: $kept/$total entries persisted ($valueless edges-only).")
    }.handleErrorWith(t => warn[IO]("Could not write incremental cache; the next build will be a full one.", t))

  /** True if the object can be Java-serialized (with the `Path` stand-in applied); used to drop non-serializable fact
    * values (closures inside a `SemValue`) while keeping the rest.
    */
  private def canSerialize(value: Any): Boolean =
    try {
      val probe = new FactOutputStream(NullOutputStream)
      try probe.writeObject(value)
      finally probe.close()
      true
    } catch { case _: Exception => false }

  /** Serializable stand-in for `java.nio.file.Path`, whose implementations are not `Serializable`. */
  private case class SerializedPath(value: String)

  /** Replaces every `Path` in the object graph with a [[SerializedPath]] as it is written. */
  private class FactOutputStream(out: OutputStream) extends ObjectOutputStream(out) {
    enableReplaceObject(true)
    override protected def replaceObject(obj: AnyRef): AnyRef = obj match {
      case path: Path => SerializedPath(path.toString)
      case other      => other
    }
  }

  /** Restores every [[SerializedPath]] back to a `Path` as it is read. */
  private class FactInputStream(in: InputStream) extends ObjectInputStream(in) {
    enableResolveObject(true)
    override protected def resolveObject(obj: AnyRef): AnyRef = obj match {
      case SerializedPath(value) => Path.of(value)
      case other                 => other
    }
  }

  /** Discards everything; used to probe serializability without allocating buffers. */
  private object NullOutputStream extends OutputStream {
    override def write(b: Int): Unit                             = ()
    override def write(b: Array[Byte], off: Int, len: Int): Unit = ()
  }
}
