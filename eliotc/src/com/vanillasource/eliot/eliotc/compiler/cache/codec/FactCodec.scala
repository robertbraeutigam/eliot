package com.vanillasource.eliot.eliotc.compiler.cache.codec

import java.io.{DataInput, DataOutput}
import java.math.BigInteger
import java.net.URI
import java.nio.file.{Path, Paths}
import scala.collection.mutable
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

/** An explicit binary encoding for one type, written and read field by field.
  *
  * This exists to replace Java serialization in the incremental cache (`docs/incremental-compilation.md` §13). Two
  * properties are the point, and both are lost the moment encoding is done reflectively or by `ObjectOutputStream`:
  *
  *   - **Nothing is written that the reader does not need.** A codec emits its own fields; the type is known from the
  *     position in the encoding, so no class descriptor — no class name, no `serialVersionUID`, no field names or
  *     types — appears in the data. Per-stream class descriptors are one of the two terms in the 7.5× / 17× per-entry
  *     blowups measured in §5 and §12.
  *   - **A type with no codec is a compile error, never a silent encoding.** There is no `FactCodec` for a function or
  *     an array, so a value that cannot satisfy the cache's equality-stability invariant (§2,
  *     `read(write(v)) == recompute(v)`) cannot be encoded by accident. That is exactly the defect class of §3.1
  *     (`SemValue.VNative`'s lambda serializes fine and never compares equal) and §4 (`ClassFile`'s array compares by
  *     reference): a reflective walker reproduces both silently, while a missing given cannot.
  *
  * Instances for product and sum types come from [[FactCodec.derived]] via the compiler's `Mirror`, so a case class
  * costs one line and states nothing about its own shape. Every instance is named rather than auto-derived: a type
  * that participates in a reference cycle (`Expression`, `SemValue`, `GroundValue` are all deeply recursive) would
  * otherwise expand its own derivation forever, and the named instance is also what makes coverage a list one can
  * read.
  *
  * Integers are varint-encoded, so the small values that dominate real data (collection sizes, sum-type tags, source
  * positions) cost one byte instead of four.
  *
  * **Structure sharing is a property of the [[Output]], not of the codecs.** A derived codec writes every product and
  * sum through [[Output.shared]]; whether that deduplicates is the stream's decision. With sharing off, each value is
  * written out in full — independent frames, the §5 layout. With sharing on, a value **equal** to one already written
  * costs a back-reference instead of its bytes, which is the byte-level model of §13's content-addressed store
  * (dedup by value, not by identity, so structurally identical subtrees from unrelated facts collapse). Measuring the
  * two against each other is what tells you whether explicit codecs alone pay, or only pay with sharing.
  */
trait FactCodec[A] {

  /** Write `value` to `out`. Must write exactly what [[read]] consumes, and nothing that identifies the type. */
  def write(out: FactCodec.Output, value: A): Unit

  /** Read back a value written by [[write]] from the same position in the stream. */
  def read(in: FactCodec.Input): A
}

object FactCodec {

  def apply[A](using codec: FactCodec[A]): FactCodec[A] = codec

  /** A sink for encoded values, optionally deduplicating equal ones.
    *
    * @param sharing
    *   when true, a value equal to one already written is replaced by a back-reference to it. Ids are assigned in
    *   write order, and only *after* the value's body is written, so the reader assigns the same ids as it goes.
    */
  final class Output(val raw: DataOutput, sharing: Boolean) {
    private val ids = mutable.HashMap.empty[Any, Int]

    def shared(value: Any)(body: => Unit): Unit =
      if (!sharing) body
      else
        ids.get(value) match {
          case Some(id) => raw.writeByte(0); writeVarInt(raw, id)
          case None     => raw.writeByte(1); body; ids.put(value, ids.size)
        }
  }

  /** The reading counterpart of [[Output]]; `sharing` must match the writer's. */
  final class Input(val raw: DataInput, sharing: Boolean) {
    private val values = mutable.ArrayBuffer.empty[Any]

    def shared[A](body: => A): A =
      if (!sharing) body
      else if (raw.readByte() == 1) {
        val value = body
        values += value
        value
      } else values(readVarInt(raw)).asInstanceOf[A]
  }

  /** Encode a single value to an independent byte array, with no cross-value sharing. Used by the conformance
    * harness for the per-frame measurement; the real store writes many values into one sharing stream.
    */
  def toBytes[A](value: A)(using codec: FactCodec[A]): Array[Byte] = {
    val bytes = new java.io.ByteArrayOutputStream()
    val out   = new java.io.DataOutputStream(bytes)
    codec.write(new Output(out, sharing = false), value)
    out.flush()
    bytes.toByteArray
  }

  def fromBytes[A](bytes: Array[Byte])(using codec: FactCodec[A]): A =
    codec.read(new Input(new java.io.DataInputStream(new java.io.ByteArrayInputStream(bytes)), sharing = false))

  /** Encode many values into one stream that deduplicates equal sub-values — the byte-level model of the
    * content-addressed store, and the number §13's size premise actually rests on.
    */
  def toSharedBytes[A](values: Seq[A])(using codec: FactCodec[A]): Array[Byte] = {
    val bytes  = new java.io.ByteArrayOutputStream()
    val stream = new java.io.DataOutputStream(bytes)
    val out    = new Output(stream, sharing = true)
    values.foreach(codec.write(out, _))
    stream.flush()
    bytes.toByteArray
  }

  def fromSharedBytes[A](bytes: Array[Byte], count: Int)(using codec: FactCodec[A]): Seq[A] = {
    val in = new Input(new java.io.DataInputStream(new java.io.ByteArrayInputStream(bytes)), sharing = true)
    Seq.fill(count)(codec.read(in))
  }

  // --- leaves -------------------------------------------------------------------------------------------------

  given FactCodec[Int] = instance((out, value) => writeVarInt(out.raw, value), in => readVarInt(in.raw))

  given FactCodec[Long] = instance((out, value) => writeVarLong(out.raw, value), in => readVarLong(in.raw))

  given FactCodec[Boolean] = instance((out, value) => out.raw.writeBoolean(value), _.raw.readBoolean())

  given FactCodec[Char] = instance((out, value) => writeVarInt(out.raw, value.toInt), in => readVarInt(in.raw).toChar)

  given FactCodec[Double] = instance((out, value) => out.raw.writeDouble(value), _.raw.readDouble())

  given FactCodec[Byte] = instance((out, value) => out.raw.writeByte(value.toInt), _.raw.readByte())

  given FactCodec[Unit] = instance((_, _) => (), _ => ())

  /** Shared, like every product: a compiler's facts repeat the same handful of names, module paths and file URIs
    * thousands of times, and a repeated string that costs its bytes again is the single largest term in an unshared
    * encoding (measured: sharing the *leaves* as well as the products is most of the win, not a refinement of it).
    */
  given FactCodec[String] = shared(
    instance(
      (out, value) => {
        val bytes = value.getBytes("UTF-8")
        writeVarInt(out.raw, bytes.length)
        out.raw.write(bytes)
      },
      in => {
        val bytes = new Array[Byte](readVarInt(in.raw))
        in.raw.readFully(bytes)
        new String(bytes, "UTF-8")
      }
    )
  )

  given FactCodec[BigInteger] = instance(
    (out, value) => {
      val bytes = value.toByteArray
      writeVarInt(out.raw, bytes.length)
      out.raw.write(bytes)
    },
    in => {
      val bytes = new Array[Byte](readVarInt(in.raw))
      in.raw.readFully(bytes)
      new BigInteger(bytes)
    }
  )

  given FactCodec[BigInt] = instance(
    (out, value) => FactCodec[BigInteger].write(out, value.bigInteger),
    in => BigInt(FactCodec[BigInteger].read(in))
  )

  /** A `URI` is encoded as its string form. Note that [[com.vanillasource.eliot.eliotc.source.content.Sourced]]
    * compares URIs by scheme-specific-part, so a round-tripped URI is equal under that comparison too.
    */
  given FactCodec[URI] = shared(
    instance((out, value) => FactCodec[String].write(out, value.toString), in => URI.create(FactCodec[String].read(in)))
  )

  given FactCodec[java.time.Instant] = instance(
    (out, value) => { writeVarLong(out.raw, value.getEpochSecond); writeVarInt(out.raw, value.getNano) },
    in => java.time.Instant.ofEpochSecond(readVarLong(in.raw), readVarInt(in.raw).toLong)
  )

  given FactCodec[java.io.File] = instance(
    (out, value) => FactCodec[String].write(out, value.getPath),
    in => new java.io.File(FactCodec[String].read(in))
  )

  /** A `Path` is encoded as its string form, mirroring the serializable stand-in `FactCache` needs for the same
    * reason: `java.nio.file.Path` is not `Serializable` and carries a filesystem reference.
    */
  given FactCodec[Path] = shared(
    instance((out, value) => FactCodec[String].write(out, value.toString), in => Paths.get(FactCodec[String].read(in)))
  )

  // --- containers ---------------------------------------------------------------------------------------------

  given [A](using codec: FactCodec[A]): FactCodec[Option[A]] = instance(
    (out, value) =>
      value match {
        case Some(a) => out.raw.writeBoolean(true); codec.write(out, a)
        case None    => out.raw.writeBoolean(false)
      },
    in => Option.when(in.raw.readBoolean())(codec.read(in))
  )

  given [A](using codec: FactCodec[A]): FactCodec[Seq[A]] = collection(_.toSeq)

  given [A](using codec: FactCodec[A]): FactCodec[List[A]] = collection(_.toList)

  given [A](using codec: FactCodec[A]): FactCodec[Vector[A]] = collection(_.toVector)

  given [A](using codec: FactCodec[A]): FactCodec[Set[A]] = collection(_.toSet)

  given [K, V](using keyCodec: FactCodec[K], valueCodec: FactCodec[V]): FactCodec[Map[K, V]] = instance(
    (out, value) => {
      writeVarInt(out.raw, value.size)
      value.foreach { case (k, v) => keyCodec.write(out, k); valueCodec.write(out, v) }
    },
    in => Seq.fill(readVarInt(in.raw))(keyCodec.read(in) -> valueCodec.read(in)).toMap
  )

  given [A, B](using leftCodec: FactCodec[A], rightCodec: FactCodec[B]): FactCodec[(A, B)] = instance(
    (out, value) => { leftCodec.write(out, value._1); rightCodec.write(out, value._2) },
    in => (leftCodec.read(in), rightCodec.read(in))
  )

  private def collection[A, C <: Iterable[A]](build: Seq[A] => C)(using codec: FactCodec[A]): FactCodec[C] = instance(
    (out, value) => { writeVarInt(out.raw, value.size); value.foreach(codec.write(out, _)) },
    in => build(Seq.fill(readVarInt(in.raw))(codec.read(in)))
  )

  // --- derivation ---------------------------------------------------------------------------------------------

  /** Derive a codec for a product (write each field in declaration order) or a sum (write a varint tag from the
    * mirror's ordinal, then the selected case). Both go through [[Output.shared]], so the stream decides whether a
    * repeated value costs its bytes or a back-reference.
    *
    * The element codecs are held in a `lazy val` so a recursive type — a sum whose cases contain the sum again — does
    * not force its own instance while that instance is still being initialised. The instance itself must still be
    * *named* (`given x: FactCodec[T] = FactCodec.derived`); an auto-derived given would re-expand `derived` at every
    * recursive occurrence and never terminate at compile time.
    */
  inline def derived[A](using mirror: Mirror.Of[A]): FactCodec[A] = {
    lazy val elementCodecs = summonCodecs[mirror.MirroredElemTypes]

    inline mirror match {
      case sum: Mirror.SumOf[A]         =>
        instance(
          (out, value) =>
            out.shared(value) {
              val tag = sum.ordinal(value)
              writeVarInt(out.raw, tag)
              elementCodecs(tag).asInstanceOf[FactCodec[A]].write(out, value)
            },
          in => in.shared(elementCodecs(readVarInt(in.raw)).asInstanceOf[FactCodec[A]].read(in))
        )
      case product: Mirror.ProductOf[A] =>
        instance(
          (out, value) =>
            out.shared(value) {
              value
                .asInstanceOf[Product]
                .productIterator
                .zip(elementCodecs.iterator)
                .foreach { case (field, codec) => codec.asInstanceOf[FactCodec[Any]].write(out, field) }
            },
          in => in.shared(product.fromProduct(Tuple.fromArray(elementCodecs.map(_.read(in)).toArray)))
        )
    }
  }

  private inline def summonCodecs[T <: Tuple]: List[FactCodec[?]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[FactCodec[t]] :: summonCodecs[ts]
    }

  /** A codec for a sum case that is a plain `object` rather than a `case object`, and so has no `Mirror`. It writes
    * nothing: the sum's tag already identifies it.
    */
  def singleton[A](value: A): FactCodec[A] = instance((_, _) => (), _ => value)

  /** Route a codec through the stream's sharing table, so an equal value already written costs a back-reference. */
  private def shared[A](codec: FactCodec[A]): FactCodec[A] =
    instance((out, value) => out.shared(value)(codec.write(out, value)), in => in.shared(codec.read(in)))

  def instance[A](writer: (Output, A) => Unit, reader: Input => A): FactCodec[A] =
    new FactCodec[A] {
      override def write(out: Output, value: A): Unit = writer(out, value)
      override def read(in: Input): A                 = reader(in)
    }

  // --- varints ------------------------------------------------------------------------------------------------

  /** Zig-zag encoded so small negative numbers stay short, then written seven bits at a time, low group first, with
    * the high bit marking continuation.
    */
  private def writeVarInt(out: DataOutput, value: Int): Unit = writeVarLong(out, value.toLong)

  private def readVarInt(in: DataInput): Int = readVarLong(in).toInt

  private def writeVarLong(out: DataOutput, value: Long): Unit = {
    var remaining = (value << 1) ^ (value >> 63) // zig-zag
    while ((remaining & ~0x7fL) != 0) {
      out.writeByte(((remaining & 0x7f) | 0x80).toInt)
      remaining >>>= 7
    }
    out.writeByte(remaining.toInt)
  }

  private def readVarLong(in: DataInput): Long = {
    var shift  = 0
    var result = 0L
    var byte   = in.readByte().toInt
    while ((byte & 0x80) != 0) {
      result |= (byte & 0x7fL) << shift
      shift += 7
      byte = in.readByte().toInt
    }
    result |= byte.toLong << shift
    (result >>> 1) ^ -(result & 1) // un-zig-zag
  }
}
