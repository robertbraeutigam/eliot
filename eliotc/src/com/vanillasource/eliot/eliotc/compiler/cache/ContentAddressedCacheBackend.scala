package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.cache.codec.{
  ContentAddressedInput,
  ContentAddressedOutput,
  FactCodec,
  FactKeyCodecs,
  ObjectId,
  ObjectIdOutput
}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.{DataInputStream, DataOutputStream}
import java.nio.file.{Files, Path, StandardOpenOption}

/** The incremental cache over a content-addressed object store (`docs/incremental-compilation.md` §13, §17).
  *
  * Two files, with opposite lifecycles, which is why they are two:
  *
  *   - **the body region** (`.eliot-objects-<config>`) is **append-only**. Objects are referred to by byte offset, so
  *     an offset written last run still resolves this run precisely because nothing before it ever moves. A save
  *     appends what is new and rewrites nothing.
  *   - **the index** (`.eliot-index-<config>`) is rewritten every save. It is the only thing a load must read in full:
  *     per entry, where its key is, where its value is, and where each dependency's key is.
  *
  * **A value is never written twice.** This is not an optimisation but what makes the append model work at all: on a
  * warm build every entry survives, and re-encoding them would append the entire graph again on every run. A value
  * still in the store is placed from its [[ObjectId]] without being read, and one held in memory is *hashed before it
  * is encoded* — so a world leaf that recomputed to an equal-but-fresh object resolves to the bytes already on disk.
  * A warm build therefore appends nothing at all.
  *
  * Duplicate storage is still *possible* here, unlike in §13's original framing, because the ids carried between runs
  * are the entries' roots rather than every object: a genuinely new value re-appends any sub-object it shares with
  * something already stored. That is the deliberate trade for not persisting a whole `ObjectId → offset` index, which
  * measured *larger than the object bodies themselves*. Compaction reclaims it, and compaction is needed regardless —
  * the bodies are untyped bytes, so a mark-and-sweep cannot walk them and GC is necessarily a decode-and-re-encode of
  * the live entries.
  *
  * Both directions are fail-safe, as with [[FactCache]]: `load` answers `None` on any problem (missing file, format or
  * fingerprint mismatch, a body region that does not match the index it was written with), and `save` warns rather
  * than failing the build.
  */
final class ContentAddressedCacheBackend private (
    objectsFile: Path,
    indexFile: Path,
    compilerFingerprint: String,
    configFingerprint: String,
    keyCodecs: FactKeyCodecs.Registry,
    placement: Ref[IO, ContentAddressedCacheBackend.Placement]
) extends IncrementalCacheBackend
    with Logging {

  override def load(): IO[Option[FactCacheData]] =
    IO.blocking(readFiles())
      .flatMap(_.traverse { case (data, next) => placement.set(next).as(data) })
      .handleErrorWith(t => warn[IO]("Could not read the incremental cache; doing a full compilation.", t).as(None))

  override def save(data: FactCacheData): IO[Unit] =
    placement.get
      .flatMap(current => IO.blocking(writeFiles(data, current)))
      .flatMap { case (next, written, dropped) =>
        placement.set(next) >> reportSave(data, next, written, dropped)
      }
      .handleErrorWith(t => warn[IO]("Could not write the incremental cache; the next build will be a full one.", t))

  private def readFiles(): Option[(FactCacheData, ContentAddressedCacheBackend.Placement)] =
    Option
      .when(Files.exists(indexFile) && Files.exists(objectsFile))(())
      .flatMap { _ =>
        val bodies = Files.readAllBytes(objectsFile)
        val in     = new DataInputStream(Files.newInputStream(indexFile))

        try readIndex(in, bodies)
        finally in.close()
      }

  private def readIndex(
      in: DataInputStream,
      bodies: Array[Byte]
  ): Option[(FactCacheData, ContentAddressedCacheBackend.Placement)] = {
    val header    = ContentAddressedCacheBackend.Header(in.readUTF(), in.readInt(), in.readInt(), in.readUTF(), in.readUTF(), in.readInt())
    val acceptable =
      header.magic == ContentAddressedCacheBackend.MAGIC &&
        header.formatVersion == ContentAddressedCacheBackend.FORMAT_VERSION &&
        header.cacheVersion == FactCache.CACHE_VERSION &&
        header.compilerFingerprint == compilerFingerprint &&
        header.configFingerprint == configFingerprint &&
        header.bodyLength == bodies.length

    Option.when(acceptable) {
      val names   = Seq.fill(FactCodec.readVarInt(in))(in.readUTF())
      val input   = new ContentAddressedInput(bodies)
      val entries = Seq.fill(FactCodec.readVarInt(in))(readEntry(in, names, input))

      (
        FactCacheData(header.cacheVersion, entries.map(read => read.key -> read.entry).toMap),
        ContentAddressedCacheBackend.Placement(
          bodies.length,
          entries.map(read => read.key -> read.keyOffset).toMap,
          entries.flatMap(read => read.located.map { case (stored, offset) => stored.id -> offset }).toMap
        )
      )
    }
  }

  private def readEntry(in: DataInputStream, names: Seq[String], input: ContentAddressedInput): Read = {
    val (key, keyOffset) = readKey(in, names, input)
    val located          = Option.when(in.readBoolean())(readStoredValue(in, input, key))
    val deps             = Seq.fill(FactCodec.readVarInt(in))(readKey(in, names, input)._1).toSet

    Read(key, keyOffset, located, CacheEntry(located.map(_._1).getOrElse(CachedValue.Absent), deps))
  }

  /** A value is *located*, not read: the offset and id come out of the index, and the bytes behind them are touched
    * only if something consumes the fact. Comparing a recomputed value against it needs the id alone.
    */
  private def readStoredValue(
      in: DataInputStream,
      input: ContentAddressedInput,
      key: CompilerFactKey[?]
  ): (CachedValue.Stored, Int) = {
    val offset = FactCodec.readVarInt(in)
    val id     = ObjectId(in.readLong(), in.readLong())

    (new CachedValue.Stored(id, () => materialise(input, key, offset), identify(key, _)), offset)
  }

  /** Hash a recomputed fact exactly as the store hashed the one it holds. */
  private def identify(key: CompilerFactKey[?], fact: CompilerFact): ObjectId =
    new ObjectIdOutput().identify(fact)(using key.valueCodec.get.asInstanceOf[FactCodec[CompilerFact]])

  /** Read a key's type name index and offset, then decode it with the codec that name registers. */
  private def readKey(in: DataInputStream, names: Seq[String], input: ContentAddressedInput): (CompilerFactKey[?], Int) = {
    val name   = names(FactCodec.readVarInt(in))
    val offset = FactCodec.readVarInt(in)

    (input.read(offset)(using keyCodecs(name)), offset)
  }

  private def materialise(input: ContentAddressedInput, key: CompilerFactKey[?], offset: Int): CompilerFact =
    input.read(offset)(using key.valueCodec.get.asInstanceOf[FactCodec[CompilerFact]])

  /** One entry as it comes back, with where its parts live so the next save can reuse them untouched. */
  private case class Read(
      key: CompilerFactKey[?],
      keyOffset: Int,
      located: Option[(CachedValue.Stored, Int)],
      entry: CacheEntry
  )

  private def writeFiles(
      data: FactCacheData,
      current: ContentAddressedCacheBackend.Placement
  ): (ContentAddressedCacheBackend.Placement, Int, Int) = {
    val out      = new ContentAddressedOutput(current.values, current.bodyLength)
    val located  = Located(out, current)
    val encoded  = data.entries.toSeq.flatMap { case (key, entry) => located.entry(key, entry) }
    val appended = out.appendedBytes

    Files.createDirectories(objectsFile.getParent)
    // Appending is only sound on top of the region the index was read from; with no prior placement (a cold start, or
    // a rejected load) whatever is on disk is unreachable and must be replaced rather than extended.
    if (current.bodyLength === 0) Files.write(objectsFile, appended)
    else Files.write(objectsFile, appended, StandardOpenOption.APPEND)
    writeIndex(data.version, current.bodyLength + appended.length, encoded)

    (
      ContentAddressedCacheBackend.Placement(
        current.bodyLength + appended.length,
        located.keys,
        encoded.flatMap(_.value).map(placed => placed.id -> placed.offset).toMap
      ),
      appended.length,
      data.entries.size - encoded.size
    )
  }

  private def writeIndex(version: Int, bodyLength: Int, entries: Seq[ContentAddressedCacheBackend.Encoded]): Unit = {
    val names = entries.flatMap(entry => entry.keyName +: entry.deps.map(_._1)).distinct
    val index = names.zipWithIndex.toMap
    val out   = new DataOutputStream(Files.newOutputStream(indexFile))

    try {
      out.writeUTF(ContentAddressedCacheBackend.MAGIC)
      out.writeInt(ContentAddressedCacheBackend.FORMAT_VERSION)
      out.writeInt(version)
      out.writeUTF(compilerFingerprint)
      out.writeUTF(configFingerprint)
      out.writeInt(bodyLength)
      FactCodec.writeVarInt(out, names.size)
      names.foreach(out.writeUTF)
      FactCodec.writeVarInt(out, entries.size)
      entries.foreach { entry =>
        FactCodec.writeVarInt(out, index(entry.keyName))
        FactCodec.writeVarInt(out, entry.keyOffset)
        out.writeBoolean(entry.value.isDefined)
        entry.value.foreach { placed =>
          FactCodec.writeVarInt(out, placed.offset)
          out.writeLong(placed.id.high)
          out.writeLong(placed.id.low)
        }
        FactCodec.writeVarInt(out, entry.deps.size)
        entry.deps.foreach { case (name, offset) =>
          FactCodec.writeVarInt(out, index(name))
          FactCodec.writeVarInt(out, offset)
        }
      }
    } finally out.close()
  }

  private def reportSave(
      data: FactCacheData,
      next: ContentAddressedCacheBackend.Placement,
      written: Int,
      dropped: Int
  ): IO[Unit] =
    debug[IO](
      s"Incremental cache: ${data.entries.size - dropped}/${data.entries.size} entries persisted, " +
        s"$written bytes appended (${next.bodyLength} total)."
    ) >> warn[IO](s"$dropped cache entries had a fact key type with no registered codec.").whenA(dropped > 0)

  /** Places each key and value in the store, reusing where it already lives and encoding only what is new.
    *
    * An entry is placed **whole or not at all**. A key type with no registered codec cannot be read back, and dropping
    * merely the *dependency* that names it would leave an entry claiming fewer inputs than it has — under-invalidation,
    * the one failure direction that produces a wrong build rather than a slow one.
    */
  private final class Located(out: ContentAddressedOutput, prior: ContentAddressedCacheBackend.Placement) {
    private var keyOffsets: Map[CompilerFactKey[?], Int] = prior.keys

    def keys: Map[CompilerFactKey[?], Int] = keyOffsets

    def entry(key: CompilerFactKey[?], entry: CacheEntry): Option[ContentAddressedCacheBackend.Encoded] =
      for {
        keyOffset  <- keyLocation(key)
        depOffsets <- entry.directDeps.toSeq.traverse(dep => keyLocation(dep).map(FactKeyCodecs.nameOf(dep) -> _))
      } yield ContentAddressedCacheBackend.Encoded(
        FactKeyCodecs.nameOf(key),
        keyOffset,
        Option.when(key.valueCodec.isDefined)(entry.stored).flatMap(valueLocation(key, _)),
        depOffsets
      )

    private def keyLocation(key: CompilerFactKey[?]): Option[Int] =
      keyOffsets.get(key).orElse {
        keyCodecs.get(FactKeyCodecs.nameOf(key)).map { codec =>
          val offset = out.write(key)(using codec.asInstanceOf[FactCodec[CompilerFactKey[?]]]).offset

          keyOffsets = keyOffsets.updated(key, offset)
          offset
        }
      }

    /** Where a value goes, **without reading it unless it has to, and without writing it unless it is new**.
      *
      * A value still in the store is placed from its id alone — materialising it merely to write it back is precisely
      * the cost the lazy read exists to avoid.
      *
      * One held in memory is **identified before it is placed**, and that order is the point. Encoding descends into
      * a value's children and appends each as it goes, so by the time the writer can tell that the value as a whole is
      * one it already holds, it has already appended the whole subtree underneath it. Hashing first — which costs no
      * bytes and no buffers — asks the only question that matters: is this exactly what is on disk already? On a warm
      * build the answer is yes for every world leaf, which is otherwise the one thing that grows the store every run.
      */
    private def valueLocation(key: CompilerFactKey[?], value: CachedValue): Option[ContentAddressedOutput.Placed] =
      value match {
        case CachedValue.Absent         => None
        case stored: CachedValue.Stored =>
          prior.values.get(stored.id).map(ContentAddressedOutput.Placed(stored.id, _))
        case CachedValue.InMemory(fact) =>
          val codec = key.valueCodec.get.asInstanceOf[FactCodec[CompilerFact]]

          Some(held(fact, codec).getOrElse(out.write(fact)(using codec)))
      }

    /** The value's existing place, if the store already holds exactly it. Skipped when there is nothing to match
      * against, so a cold build does not hash everything twice.
      */
    private def held(fact: CompilerFact, codec: FactCodec[CompilerFact]): Option[ContentAddressedOutput.Placed] =
      Option
        .when(prior.values.nonEmpty)(new ObjectIdOutput().identify(fact)(using codec))
        .flatMap(id => prior.values.get(id).map(ContentAddressedOutput.Placed(id, _)))
  }
}

object ContentAddressedCacheBackend {
  private val MAGIC: String          = "ELIOT-CAS"
  private val FORMAT_VERSION: Int    = 2
  private val OBJECTS_FILE: String   = ".eliot-objects"
  private val INDEX_FILE: String     = ".eliot-index"

  def create(
      targetPath: Path,
      compilerFingerprint: String,
      configFingerprint: String,
      keyCodecs: FactKeyCodecs.Registry
  ): IO[ContentAddressedCacheBackend] =
    Ref
      .of[IO, Placement](Placement(0, Map.empty, Map.empty))
      .map(
        new ContentAddressedCacheBackend(
          fileFor(targetPath, OBJECTS_FILE, configFingerprint),
          fileFor(targetPath, INDEX_FILE, configFingerprint),
          compilerFingerprint,
          configFingerprint,
          keyCodecs,
          _
        )
      )

  /** One pair of files per configuration, named as [[FactCache.cacheFile]] is and for the same reason: distinct
    * configurations coexist under one target directory instead of clobbering each other, with the header carrying the
    * full fingerprint as the exact check.
    */
  private def fileFor(targetDir: Path, prefix: String, configFingerprint: String): Path =
    targetDir.resolve(s"$prefix-${configFingerprint.filter(_.isLetterOrDigit).take(16)}")

  private case class Header(
      magic: String,
      formatVersion: Int,
      cacheVersion: Int,
      compilerFingerprint: String,
      configFingerprint: String,
      bodyLength: Int
  )

  /** One entry as it goes into the index. */
  private case class Encoded(
      keyName: String,
      keyOffset: Int,
      value: Option[ContentAddressedOutput.Placed],
      deps: Seq[(String, Int)]
  )

  /** Where everything the store already holds lives, carried between a load and the saves that follow it.
    *
    * `values` is keyed by **content**, not by which key or which object: that is what lets a leaf recomputed to an
    * equal-but-fresh value resolve to the bytes already on disk instead of appending them again.
    */
  private case class Placement(
      bodyLength: Int,
      keys: Map[CompilerFactKey[?], Int],
      values: Map[ObjectId, Int]
  )
}
