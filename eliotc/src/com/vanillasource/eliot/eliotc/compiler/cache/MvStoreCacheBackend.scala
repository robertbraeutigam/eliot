package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerFactKey
import org.h2.mvstore.{MVMap, MVStore}

import java.nio.file.{Files, Path}
import java.util.Base64
import scala.jdk.CollectionConverters.*

/** Spike: an incremental cache backend over H2's MVStore (a pure-JVM, single-file, log-structured key/value store).
  *
  * The store *is* the persistent memory of what has already been written: it stays open for the session and is seeded
  * on disk with the previous run's entries. Each entry is stored under its own key — `base64(serialize(factKey)) ->
  * serialize(entry)` — so persistence is per-entry rather than one whole-graph blob.
  *
  * The point of the spike (`docs/incremental-compilation.md` §11, and the "remember what is already stored" design
  * discussion) is that **`save` writes only the delta**. Every entry is compared against a held [[snapshot]] of what
  * the store already contains, by ordinary value equality:
  *
  *   - an entry equal to its stored form is neither re-serialized nor rewritten (the common case on a warm build,
  *     including the re-computed world-leaf facts whose values are unchanged);
  *   - only new, changed, or non-value-comparable entries are serialized and `put`;
  *   - entries gone from the run are `remove`d.
  *
  * `commit` then flushes only the pages those puts/removes dirtied. So an unchanged tree writes essentially nothing,
  * and a one-fact change writes essentially one fact.
  *
  * Two honest limitations of the spike, both to be read off the measurement rather than assumed away:
  *
  *   - **Load is still eager.** `load` deserializes every entry to rebuild [[FactCacheData]], so the load cost is not
  *     expected to drop here — cutting it needs lazy value materialisation (the graph/value split), a separate step.
  *   - **Per-entry framing gives up cross-entry structure sharing**, so on-disk size may grow relative to the shared
  *     graph (the §5 trade-off). The spike is exactly how we measure whether that growth is acceptable.
  *
  * The [[snapshot]] holds entries in their *prepared* (stored) shape, so a fact whose value cannot be serialized
  * (dropped to `None` on disk) compares unequal to its live value and is rewritten every run — the same small
  * unstable set the whole-graph path also cannot cache; here it merely costs a per-run rewrite, not a cascade.
  *
  * The store is not explicitly closed (there is no session-teardown hook, and `persist` may run per-compile in the
  * server loop); `commit` keeps the file crash-consistent, and the process exit releases it. A proper close hook is a
  * follow-up if this backend graduates from a spike.
  */
final class MvStoreCacheBackend private (
    store: MVStore,
    compilerFingerprint: String,
    configFingerprint: String,
    snapshot: Ref[IO, Map[CompilerFactKey[?], CacheEntry]]
) extends IncrementalCacheBackend
    with Logging {

  private val meta: MVMap[String, String]         = store.openMap("meta")
  private val entries: MVMap[String, Array[Byte]] = store.openMap("entries")

  override def load(): IO[Option[FactCacheData]] =
    IO.blocking(readFreshEntries())
      .flatMap {
        case Some(map) => snapshot.set(map).as(Some(FactCacheData(FactCache.CACHE_VERSION, map)))
        case None      => resetHeader() >> snapshot.set(Map.empty).as(None)
      }
      .handleErrorWith(t => warn[IO]("Could not read MVStore cache; doing a full compilation.", t).as(None))

  /** Read the whole store back into a map, but only if the header matches this compiler+config+version; otherwise the
    * stored entries are stale and the caller resets the header for a cold build.
    */
  private def readFreshEntries(): Option[Map[CompilerFactKey[?], CacheEntry]] = {
    val headerOk =
      Option(meta.get("version")).contains(FactCache.CACHE_VERSION.toString) &&
        Option(meta.get("compilerFingerprint")).contains(compilerFingerprint) &&
        Option(meta.get("configFingerprint")).contains(configFingerprint)

    Option.when(headerOk) {
      entries
        .entrySet()
        .asScala
        .iterator
        .map { e =>
          val key   = FactSerialization.fromBytes(Base64.getDecoder.decode(e.getKey)).asInstanceOf[CompilerFactKey[?]]
          val entry = FactSerialization.fromBytes(e.getValue).asInstanceOf[CacheEntry]
          key -> entry
        }
        .toMap
    }
  }

  private def resetHeader(): IO[Unit] = IO.blocking {
    entries.clear()
    writeHeader()
    store.commit()
    ()
  }

  private def writeHeader(): Unit = {
    meta.put("version", FactCache.CACHE_VERSION.toString)
    meta.put("compilerFingerprint", compilerFingerprint)
    meta.put("configFingerprint", configFingerprint)
    ()
  }

  override def save(data: FactCacheData): IO[Unit] =
    snapshot.get
      .flatMap { snap =>
        IO.blocking(writeDelta(snap, data)).flatMap { case (nextSnapshot, total, written, removed) =>
          snapshot.set(nextSnapshot) >>
            debug[IO](s"MVStore cache: $written written, $removed removed, ${total - written} unchanged of $total.")
        }
      }
      .handleErrorWith(t => warn[IO]("Could not write MVStore cache; the next build will be a full one.", t))

  /** Apply only the difference between `snap` (what the store already holds) and `data` (this run's cache), and return
    * the new snapshot plus counts for logging. Unchanged entries are skipped without being serialized.
    */
  private def writeDelta(
      snap: Map[CompilerFactKey[?], CacheEntry],
      data: FactCacheData
  ): (Map[CompilerFactKey[?], CacheEntry], Int, Int, Int) = {
    val removedKeys = snap.keySet.diff(data.entries.keySet)
    removedKeys.foreach(key => keyId(key).foreach(entries.remove))

    var written             = 0
    val dirtyPrepared       = Map.newBuilder[CompilerFactKey[?], CacheEntry]
    data.entries.foreach { case (key, rawEntry) =>
      if (!snap.get(key).contains(rawEntry))                        // value-equal to stored ⇒ no serialize, no write
        prepare(key, rawEntry) match {
          case Some(prepared) =>
            keyId(key).foreach { id =>
              entries.put(id, FactSerialization.toBytes(prepared))
              dirtyPrepared += key -> prepared
              written += 1
            }
          case None =>
            keyId(key).foreach(entries.remove)                       // unusable edge ⇒ drop, as the whole-graph path does
        }
    }

    writeHeader()
    val _ = store.commit()
    ((snap -- removedKeys) ++ dirtyPrepared.result(), data.entries.size, written, removedKeys.size)
  }

  /** Prepare an entry for storage exactly as [[FactCache.save]] does: an entry whose key or dependency edges cannot
    * serialize is unusable and dropped (`None`); a non-serializable *value* is dropped to `None`, keeping the edges.
    */
  private def prepare(key: CompilerFactKey[?], entry: CacheEntry): Option[CacheEntry] =
    Option.when(FactSerialization.canSerialize(key) && FactSerialization.canSerialize(entry.directDeps))(
      entry.copy(value = entry.value.filter(FactSerialization.canSerialize))
    )

  /** The stable, content-addressed store key for a fact key: base64 of its serialization. Deterministic serialization
    * of equal (first-order) keys is what makes a changed entry overwrite its own row rather than orphan it.
    */
  private def keyId(key: CompilerFactKey[?]): Option[String] =
    try Some(Base64.getEncoder.encodeToString(FactSerialization.toBytes(key)))
    catch { case _: Exception => None }
}

object MvStoreCacheBackend {

  def open(targetPath: Path, compilerFingerprint: String, configFingerprint: String): IO[MvStoreCacheBackend] =
    for {
      _        <- IO.blocking(Files.createDirectories(targetPath))
      store    <- IO.blocking(new MVStore.Builder().fileName(cacheFile(targetPath, configFingerprint).toString).open())
      snapshot <- Ref.of[IO, Map[CompilerFactKey[?], CacheEntry]](Map.empty)
    } yield new MvStoreCacheBackend(store, compilerFingerprint, configFingerprint, snapshot)

  /** The MVStore file for a configuration. Keyed by the config fingerprint in the name, mirroring
    * [[FactCache.cacheFile]], so distinct configurations coexist under one target directory.
    */
  private def cacheFile(targetDir: Path, configFingerprint: String): Path =
    targetDir.resolve(s".eliot-cache-mv-${configFingerprint.filter(_.isLetterOrDigit).take(16)}")
}
