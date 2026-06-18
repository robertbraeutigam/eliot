package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging

import java.io.{FileInputStream, FileOutputStream, InputStream, ObjectInputStream, ObjectOutputStream, OutputStream}
import java.nio.file.{Files, Path}

/** Persistence for the incremental-compilation cache, using Java serialization of [[FactCacheData]].
  *
  * Two eliot-specific realities are handled here:
  *
  *   1. `java.nio.file.Path` is not `Serializable`; the streams below transparently swap any `Path` for a serializable
  *      stand-in on write and back on read, so facts may carry `Path`s freely.
  *   2. A few facts carry a `SemValue` (the NbE semantic domain), which contains Scala closures and is therefore
  *      fundamentally non-serializable — by any library, not just Java serialization. Rather than fail the whole cache,
  *      `save` writes only the entries that successfully serialize and silently drops the rest. A dropped fact simply has
  *      no prior entry next run and is regenerated, which is always safe under the backward-pull validation: correctness
  *      is preserved, only some incrementality is lost (facts that transitively depend on a non-serializable fact
  *      regenerate). See `docs/incremental-compilation.md`.
  *
  * Both directions are otherwise fail-safe: `load` returns `None` on any problem (missing file, deserialization error,
  * version mismatch), and `save` warns rather than failing the build if writing is impossible.
  *
  * Bump [[CACHE_VERSION]] whenever a persisted fact's shape changes, so an out-of-date cache is discarded.
  */
object FactCache extends Logging {
  val CACHE_VERSION: Int         = 1
  private val CACHE_FILE: String = ".eliot-cache"

  def cacheFile(targetDir: Path): Path = targetDir.resolve(CACHE_FILE)

  def load(targetDir: Path): IO[Option[FactCacheData]] =
    IO.blocking {
      val file = cacheFile(targetDir)
      if (Files.exists(file)) {
        val in = new FactInputStream(new FileInputStream(file.toFile))
        try {
          val data = in.readObject().asInstanceOf[FactCacheData]
          Option.when(data.version == CACHE_VERSION)(data)
        } finally in.close()
      } else None
    }.handleErrorWith(t => warn[IO]("Could not read incremental cache; doing a full compilation.", t).as(None))

  def save(targetDir: Path, data: FactCacheData): IO[Unit] =
    IO.blocking {
      Files.createDirectories(targetDir)
      val serializable = data.entries.filter(canSerialize)
      val out          = new FactOutputStream(new FileOutputStream(cacheFile(targetDir).toFile))
      try out.writeObject(FactCacheData(data.version, serializable))
      finally out.close()
      data.entries.size - serializable.size
    }.flatMap(dropped => debug[IO](s"Incremental cache: $dropped non-serializable fact(s) not persisted.").whenA(dropped > 0))
      .handleErrorWith(t => warn[IO]("Could not write incremental cache; the next build will be a full one.", t))

  /** True if the (key, entry) pair can be Java-serialized; used to drop facts carrying closures (`SemValue`). */
  private def canSerialize(entry: (Any, Any)): Boolean =
    try {
      val probe = new FactOutputStream(NullOutputStream)
      try probe.writeObject(entry)
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
