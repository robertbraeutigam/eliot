package com.vanillasource.eliot.eliotc.compiler.cache

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  InputStream,
  ObjectInputStream,
  ObjectOutputStream,
  OutputStream
}
import java.nio.file.Path

/** Per-object Java serialization for cache facts, factored out so a per-entry store (see
  * [[MvStoreCacheBackend]]) can encode/decode individual keys and entries independently of the whole-graph
  * [[FactCache]] stream.
  *
  * It handles the same eliot-specific reality as [[FactCache]]: `java.nio.file.Path` is not `Serializable`, so a
  * serializable stand-in is swapped in on write and back on read, letting facts carry `Path`s freely.
  *
  * Note that per-object framing deliberately gives up the cross-entry structure sharing (back-references, single
  * class-descriptor table) that the whole-graph stream provides — that trade-off is measured in
  * `docs/incremental-compilation.md` §5 and §12 (~17× on disk), and is why this path is a spike selected only by an
  * env flag. The resolution is §13: explicit per-type codecs over a content-addressed store, which removes Java
  * serialization from the cache rather than framing it differently.
  */
object FactSerialization {

  /** Serialize a single value to an independent byte frame. Throws if the value is not serializable. */
  def toBytes(value: Any): Array[Byte] = {
    val bytes = new ByteArrayOutputStream()
    val out   = new FactObjectOutputStream(bytes)
    try out.writeObject(value)
    finally out.close()
    bytes.toByteArray
  }

  /** Restore a value from a frame produced by [[toBytes]]. */
  def fromBytes(bytes: Array[Byte]): AnyRef = {
    val in = new FactObjectInputStream(new ByteArrayInputStream(bytes))
    try in.readObject()
    finally in.close()
  }

  /** True if the value can be Java-serialized (with the `Path` stand-in applied); used to drop a non-serializable
    * fact value (a `SemValue` closure) while keeping the rest, exactly as the whole-graph path does.
    */
  def canSerialize(value: Any): Boolean =
    try {
      val probe = new FactObjectOutputStream(NullOutputStream)
      try probe.writeObject(value)
      finally probe.close()
      true
    } catch { case _: Exception => false }

  /** Serializable stand-in for `java.nio.file.Path`, whose implementations are not `Serializable`. */
  private case class SerializedPath(value: String)

  private class FactObjectOutputStream(out: OutputStream) extends ObjectOutputStream(out) {
    enableReplaceObject(true)
    override protected def replaceObject(obj: AnyRef): AnyRef = obj match {
      case path: Path => SerializedPath(path.toString)
      case other      => other
    }
  }

  private class FactObjectInputStream(in: InputStream) extends ObjectInputStream(in) {
    enableResolveObject(true)
    override protected def resolveObject(obj: AnyRef): AnyRef = obj match {
      case SerializedPath(value) => Path.of(value)
      case other                 => other
    }
  }

  private object NullOutputStream extends OutputStream {
    override def write(b: Int): Unit                             = ()
    override def write(b: Array[Byte], off: Int, len: Int): Unit = ()
  }
}
