package com.vanillasource.eliot.eliotc.compiler.cache.codec

import java.io.{ByteArrayOutputStream, DataOutput, DataOutputStream, OutputStream}
import scala.collection.mutable

/** A [[FactCodec.Output]] that carves the encoding into separately addressed objects instead of one stream — the write
  * half of the content-addressed store (`docs/incremental-compilation.md` §13, §17).
  *
  * Every value written through [[FactCodec.Output.shared]] becomes its own object: its body is encoded into a nested
  * buffer with its children already reduced to references, and the parent records only the reference. An object whose
  * content is already present is not stored again, so structurally identical subtrees from unrelated facts collapse
  * whether or not they are the same instance — and, unlike an in-stream back-reference, whether or not they were
  * written by the same run.
  *
  * **Two identities, deliberately, and the distinction is the whole design (§17):**
  *
  *   - an object's [[ObjectId]] is a Merkle hash — its own bytes, with each child contributing *its id*, never a
  *     position. It therefore means the same thing in any store, which is what lets a recomputed value be compared to
  *     a stored one by 16 bytes without reading the stored side, and what lets dedup span runs.
  *   - an object's on-disk **reference is its byte offset** in the append-only body region, written as a varint. §13
  *     called for the id itself; measured, that costs 5.72× what the shared stream does, because 79% of a fact graph's
  *     objects are referred to exactly once and a 16-byte reference dwarfs the ~15-byte body it points at. An offset
  *     is stable for the same reason a sequence number is — the region only ever grows — and it is *self-locating*, so
  *     no table has to be written or read to resolve one.
  *
  * **The store is untyped, deliberately.** Codecs write nothing that identifies a type, so distinct types routinely
  * encode to identical bytes — a `URI` and its own string form, `Sourced[String]` and `Sourced[Token]`. Such values
  * share one stored object, and that is sound: the reader always supplies the codec, so the same bytes decode
  * correctly for each. What must *not* be shared is a reader's cache of decoded values, which is why
  * [[ContentAddressedInput]] keys its memo by codec as well as offset.
  *
  * Hashing is memoized by value, so encoding costs one digest per *distinct* object rather than one per occurrence.
  *
  * @param existing
  *   where the objects already in the store live, so an encoding reuses them instead of appending them again
  * @param firstOffset
  *   the offset the body region continues at, i.e. its current length
  */
final class ContentAddressedOutput(existing: Map[ObjectId, Int], firstOffset: Int) extends FactCodec.Output {
  private val added   = new ByteArrayOutputStream()
  private val offsets = mutable.HashMap.from(existing)
  private val ids     = mutable.HashMap.empty[Any, ObjectId]
  private var current = ContentAddressedOutput.Frame()

  override def raw: DataOutput = current.tee

  override def shared(value: Any)(body: => Unit): Unit = {
    val id = ids.getOrElse(value, remember(value, store(value, body)))

    FactCodec.writeVarInt(current.bodyOut, offsets(id))
    current.hash.write(id.bytes)
  }

  /** Encode one value as an object of its own and answer its id — the store's entry point, as opposed to
    * [[FactCodec.Output.shared]], which is what codecs call on their way down.
    */
  def write[A](value: A)(using codec: FactCodec[A]): ObjectId = store(value, codec.write(this, value))

  /** Everything this encoding adds to the body region, to be appended at `firstOffset`. Objects found already present
    * contribute nothing, which is what makes a save write only what is new.
    */
  def appendedBytes: Array[Byte] = added.toByteArray

  /** Where every object now lives, the pre-existing ones included — the dedup index the next run starts from. */
  def objectOffsets: Map[ObjectId, Int] = offsets.toMap

  /** Not `getOrElseUpdate`: computing the id writes the value's children, which inserts into this very map, and a
    * `mutable.HashMap` may not be modified while it is deciding where to put an entry.
    */
  private def remember(value: Any, id: ObjectId): ObjectId = {
    ids.update(value, id)
    id
  }

  private def store(value: Any, body: => Unit): ObjectId = {
    val frame    = ContentAddressedOutput.Frame()
    val previous = current

    current = frame
    try body
    finally current = previous

    val id = ObjectId.of(frame.hash.toByteArray)

    if (!offsets.contains(id)) {
      offsets.update(id, firstOffset + added.size())
      added.write(frame.body.toByteArray)
    }

    id
  }
}

object ContentAddressedOutput {

  /** Start an encoding against an empty store. */
  def empty: ContentAddressedOutput = new ContentAddressedOutput(Map.empty, 0)

  /** One object's two sinks, written in lockstep: `body` receives the bytes that are stored, `hash` the bytes the
    * object's identity is taken over. They differ in exactly one place — a child contributes its *offset* to the body
    * and its *id* to the hash — which is what keeps an id independent of where anything happens to sit.
    */
  private final class Frame {
    val body: ByteArrayOutputStream = new ByteArrayOutputStream()
    val hash: ByteArrayOutputStream = new ByteArrayOutputStream()
    val bodyOut: DataOutput         = new DataOutputStream(body)
    val tee: DataOutput             = new DataOutputStream(new Frame.Tee(body, hash))
  }

  private object Frame {
    def apply(): Frame = new Frame

    /** Writes every byte to both sinks. */
    private final class Tee(first: OutputStream, second: OutputStream) extends OutputStream {
      override def write(byte: Int): Unit = {
        first.write(byte)
        second.write(byte)
      }

      override def write(bytes: Array[Byte], offset: Int, length: Int): Unit = {
        first.write(bytes, offset, length)
        second.write(bytes, offset, length)
      }
    }
  }
}
