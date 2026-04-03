# Incremental Compilation Design

## 1. Problem Statement

Currently, every invocation of the ELIOT compiler recomputes all facts from scratch: reading source files, tokenizing, parsing, type checking, monomorphizing, and generating bytecode. For a growing codebase, this becomes increasingly slow even when only a single file changes — or nothing changes at all.

### Goals

1. **Fast no-change path**: If no source files have changed since the last compilation, the compiler should do essentially nothing. This must be fast — just filesystem `stat` calls, no file reads, no deserialization, no computation.
2. **Minimal recomputation on change**: When source files change, only facts that are actually affected should be recomputed. The system should detect when a recomputed intermediate fact produces the same result as before, and stop propagation at that point.
3. **Completely generic**: The mechanism must be confined to the `eliotc` module. No processor implementation should need to know about or deal with incremental compilation. The mechanism relies solely on the property that **all processors are stateless pure functions**.
4. **Configurable persistence**: Not all facts need to be serialized to disk. "Intermediate" facts that are cheap to recompute (tokenization, parsing) can be excluded from persistence. A generic, plugin-configurable mechanism controls which fact types are persisted.

### Key Property Exploited

All `CompilerProcessor` implementations are stateless and deterministic: given the same input facts, they produce the same output facts. The only external inputs are source files (read by `SourceContentReader`). Therefore, if we can determine that all inputs to a fact are unchanged, the fact itself is unchanged.

## 2. Core Idea: I/O as Facts with Initial and FileStat

The central design insight is to model I/O operations as facts within the existing fact system:

### Initial Fact

A fact `Initial(compilerStart: Instant)` is produced at the start of every compilation run. Its value is the current timestamp, so it **naturally changes with every invocation**. Any fact that depends on `Initial` will always be regenerated.

### Reusing the Existing FileStat Fact

The codebase already has `FileStat(file: File, lastModified: Option[Instant])` and `FileStatProcessor` in the `source.stat` package. `FileStatProcessor` performs the `stat` syscall and returns the file's modification time. This is exactly what the plan needs for change detection.

Rather than introducing a new `UpdateTime` fact, we modify `FileStatProcessor` to depend on `Initial`:

```scala
class FileStatProcessor extends SingleFactProcessor[FileStat.Key] with Logging {
  override protected def generateSingleFact(key: FileStat.Key): CompilerIO[FileStat] =
    for {
      _ <- getFactOrAbort(Initial.Key)  // ← new: depend on Initial → always re-run
      result <- IO(key.file.lastModified()).attempt
                  .map(_.toOption)
                  .map(lastModified => FileStat(key.file, lastModified.filter(_ > 0L).map(Instant.ofEpochMilli)))
                  .to[CompilerIO]
    } yield result
}
```

The key property: **if the file hasn't changed, `FileStat` produces the same value as last time**. This makes it the natural boundary between the external world and the deterministic fact system.

### How the Existing Dependency Chain Works

The current codebase already has the following dependency chain for file-based sources:

```
SourceContent(uri) → FileContent(file) → [reads file directly]
                   → ResourceContent(uri) → [reads resource directly]
```

`FileStatProcessor` exists but is not currently in the dependency chain for `FileContent`. With incremental compilation, `FileContentReader` is modified to depend on `FileStat`:

```scala
class FileContentReader extends SingleFactProcessor[FileContent.Key] with Logging {
  override protected def generateSingleFact(key: FileContent.Key): CompilerIO[FileContent] =
    for {
      _ <- getFactOrAbort(FileStat.Key(key.file))  // ← new: establishes dependency
      // ... existing file reading logic ...
    } yield result
}
```

This creates the dependency chain that makes everything work:

```
Initial (always changes)
  ← FileStat(Foo.els) (always runs, produces file mtime)
    ← FileContent(Foo.els) (only runs if mtime changed)
      ← SourceContent(Foo.els) (only runs if content changed)
        ← SourceTokens(Foo.els) (only runs if content changed)
          ← ... entire downstream pipeline
```

If the file's mtime is unchanged, `FileStat` produces the same hash as cached → `FileContent` doesn't need to re-run → nothing downstream runs.

**Note:** `SourceContentReader` handles both `file://` URIs (via `FileContent`) and resource URIs (via `ResourceContent`). Resource content is read from the classpath and is effectively immutable at compile time, so it does not need `Initial`-based invalidation.

## 3. Recursive Dependency Validation

### How Validation Works

The incremental engine has no knowledge of which facts are "special." It only knows three things about each cached fact: what its direct dependencies were, what their hashes were, and what its own content hash was.

```scala
case class CacheEntry(
    directDependencies: Set[CompilerFactKey[?]],
    directDependencyHashes: Map[CompilerFactKey[?], Long],
    contentHash: Long,
    serializedFact: Option[Array[Byte]]
)
```

To validate a cached fact, the engine recursively validates its direct dependencies first (each of which validates its own dependencies, and so on). Once all dependencies are validated or recomputed, their current hashes are compared against the stored `directDependencyHashes`. If all match, the fact is still valid.

```scala
def validate(
    key: CompilerFactKey[?],
    cache: FactCacheData,
    currentHashes: mutable.Map[CompilerFactKey[?], Long]
): Boolean = {
  if (currentHashes.contains(key)) return true  // already validated this run

  cache.entries.get(key) match {
    case None => false  // not in cache, must generate
    case Some(entry) =>
      // First, recursively validate all dependencies
      entry.directDependencies.foreach(dep => validate(dep, cache, currentHashes))

      // Then check if dependency hashes still match
      val depsUnchanged = entry.directDependencyHashes.forall {
        case (depKey, expectedHash) =>
          currentHashes.get(depKey).contains(expectedHash)
      }
      if (depsUnchanged) {
        currentHashes(key) = entry.contentHash
        true
      } else {
        false  // must recompute
      }
  }
}
```

### Why This Is Not Expensive

The recursion bottoms out naturally at facts with no dependencies in the cache — either because they were freshly registered at startup (like `Initial`), or because they have no cached entry. Each fact is visited at most once thanks to the `currentHashes` memoization map. The total work is proportional to the number of dependency edges, traversed once.

In the common case (nothing changed), `Initial` is already in memory with a fresh value → `FileStat` facts are recomputed (cheap `stat` calls) → their hashes match → validation propagates up instantly, each fact confirmed in O(number of direct dependencies).

### Example

For `MonomorphicValue(Foo.bar)` which transitively depends on `Foo.els` and `String.els`:

```
validate(MonomorphicValue(Foo.bar))
  → validate(OperatorResolvedValue(Foo.bar))
    → validate(MatchDesugaredValue(Foo.bar))
      → validate(ResolvedValue(Foo.bar))
        → validate(UnifiedModuleValue(Foo.bar))
          → validate(ModuleValue(Foo.els, Foo.bar))
            → validate(SourceContent(Foo.els))
              → validate(FileContent(Foo.els))
                → validate(FileStat(Foo.els))
                  → validate(Initial)
                     Initial is in memory (fresh) → hash differs from cached → return false
                  → FileStat must recompute → stat file → same mtime → same hash ✓
                → FileContent deps unchanged → valid ✓
              → SourceContent deps unchanged → valid ✓
            → ModuleValue deps unchanged → valid ✓
          → UnifiedModuleValue deps unchanged → valid ✓
        → ResolvedValue deps unchanged → valid ✓
      → MatchDesugaredValue deps unchanged → valid ✓
    → OperatorResolvedValue deps unchanged → valid ✓
  → also validates String.els chain (similar)
  → MonomorphicValue deps unchanged → valid ✓
```

Each node is visited once. Second time `Initial` is reached (via `String.els` chain), it's already in `currentHashes` — instant return.

## 4. Hash-Based Propagation Cutoff on Recomputation

When a dependency hash changes (e.g., a file was modified), facts depending on it need recomputation. But not necessarily ALL of them — hash-based cutoff minimizes work.

### The Mechanism

Each cache entry stores `directDependencyHashes` — the content hashes of its immediate dependencies at the time it was computed. When a fact's validation fails (some dependency hash differs):

1. **Recompute the fact** using its processor
2. **Hash the result** and compare with the cached `contentHash`:
   - If the hash matches → content is unchanged despite input change → **cutoff**: dependents are still valid
   - If the hash differs → fact truly changed → dependents must be revalidated

This is integrated directly into the recursive validation from Section 3. When validation finds a dependency mismatch, it recomputes the fact, and if the new content hash matches the cached one, it records the same hash — making the fact appear unchanged to its dependents.

### Example: Signature Unchanged

File `Foo.els` changes, but `Foo.bar`'s type signature stays the same (only body changes):

```
1.  FileStat(Foo.els) → CHANGED (new mtime)
2.  FileContent(Foo.els) → dirty → recompute (re-read file) → hash CHANGED
3.  SourceContent(Foo.els) → dirty → recompute → hash CHANGED
4.  SourceTokens(Foo.els) → dirty → recompute → hash CHANGED
5.  SourceAST(Foo.els) → dirty → recompute → hash CHANGED
6.  CoreAST(Foo.els) → dirty → recompute → hash CHANGED
7.  ModuleValue(Foo.els, Foo.bar) → dirty → recompute → hash CHANGED (body differs)
8.  UnifiedModuleValue(Foo.bar) → dirty → recompute → hash CHANGED
9.  ResolvedValue(Foo.bar) → dirty → recompute → hash CHANGED
10. MatchDesugaredValue(Foo.bar) → dirty → recompute → hash CHANGED
11. OperatorResolvedValue(Foo.bar) → dirty → recompute → hash CHANGED
12. MonomorphicValue(Foo.bar) → dirty → recompute → hash CHANGED (body differs)
13. MonomorphicValue(Other.baz) calls Foo.bar:
    - Check directDependencyHashes:
      MonomorphicValue(Foo.bar) hash CHANGED
    - Must recompute → result is SAME (only uses Foo.bar's signature)
    → contentHash MATCHES cached → CUTOFF: propagation stops
14. Everything downstream of Other.baz → untouched ✓
```

### Integration with Recursive Validation

Propagation cutoff is part of the validation logic from Section 3. When a fact fails the dependency hash check, instead of immediately declaring it invalid, the engine recomputes it and checks the content hash:

```scala
def validateOrRecompute(
    key: CompilerFactKey[?],
    cache: FactCacheData,
    currentHashes: mutable.Map[CompilerFactKey[?], Long]
): Unit = {
  if (currentHashes.contains(key)) return  // already handled

  cache.entries.get(key) match {
    case None =>
      // Not in cache — generate fresh, record hash
      val fact = generate(key)
      currentHashes(key) = computeHash(fact)

    case Some(entry) =>
      // Recursively validate dependencies first
      entry.directDependencies.foreach(dep =>
        validateOrRecompute(dep, cache, currentHashes)
      )

      val depsUnchanged = entry.directDependencyHashes.forall {
        case (depKey, expectedHash) =>
          currentHashes.get(depKey).contains(expectedHash)
      }

      if (depsUnchanged) {
        // All inputs unchanged → fact is clean
        currentHashes(key) = entry.contentHash
      } else {
        // Some input changed → recompute
        val newFact = generate(key)
        val newHash = computeHash(newFact)
        currentHashes(key) = newHash
        // If newHash == entry.contentHash, dependents will see
        // unchanged hashes → cutoff propagates naturally
      }
  }
}
```

## 5. Architecture Overview

```
┌──────────────────────────────────────────────────────────────┐
│                      Compiler.scala                          │
│  (load cache → create generator → run → save cache)         │
│  Uses InitialProcessor instead of NullProcessor              │
└──────────────────────┬───────────────────────────────────────┘
                       │
                       ▼
┌──────────────────────────────────────────────────────────────┐
│            IncrementalFactGenerator                           │
│  (implements CompilationProcess, replaces FactGenerator       │
│   with cache lookups and dependency tracking)                 │
│                                                              │
│  On getFact(key):                                            │
│   1. In-memory cache hit? → return                           │
│   2. Persistent cache entry exists?                          │
│      → recursively validate dependencies                     │
│      → all dep hashes match? → return from cache             │
│      → some differ? → recompute → check content hash cutoff  │
│   3. Not in cache → generate with DependencyTrackingProcess  │
│                                                              │
│  ┌───────────────────┐  ┌────────────────────────┐           │
│  │ DependencyTracking │  │   CacheValidator       │           │
│  │ Process            │  │   (recursive dep check  │           │
│  │ (records getFact   │  │    + propagation cutoff)│           │
│  │  calls per fact)   │  │                        │           │
│  └───────────────────┘  └────────────────────────┘           │
│                                                              │
│  ┌───────────────────────────────────────────────┐           │
│  │              FactCache                         │           │
│  │  (persistent storage on disk)                  │           │
│  │  - direct dependency hashes                    │           │
│  │  - content hashes                              │           │
│  │  - serialized facts (persistent only)          │           │
│  └───────────────────────────────────────────────┘           │
└──────────────────────────────────────────────────────────────┘
```

### New Files (all in `eliotc` module)

| File | Package | Purpose |
|------|---------|---------|
| `Initial.scala` | `compiler.cache` | `Initial` fact, key, and processor |
| `CacheEntry.scala` | `compiler.cache` | Cache entry data structure |
| `FactCache.scala` | `compiler.cache` | Cache persistence (load/save) |
| `CacheValidator.scala` | `compiler.cache` | Recursive validation + propagation cutoff |
| `DependencyTrackingProcess.scala` | `compiler.cache` | Records fact dependencies |
| `IncrementalFactGenerator.scala` | `compiler` | Cache-aware `CompilationProcess` |

### Modified Files

| File | Change |
|------|--------|
| `Compiler.scala` (eliotc) | Register `Initial` processor, load/save cache, use `IncrementalFactGenerator` |
| `CompilerPlugin.scala` (eliotc) | Add `ephemeralFactTypes()` method (default: empty set) |
| `FileStatProcessor.scala` (lang) | Add `getFactOrAbort(Initial.Key)` before stat call |
| `FileContentReader.scala` (lang) | Add `getFactOrAbort(FileStat.Key(key.file))` before reading file |
| `LangPlugin.scala` (lang) | Override `ephemeralFactTypes()` to declare cheap fact types |

## 6. New Facts and Modified Processors

### Initial (new fact in `eliotc` module)

```scala
package com.vanillasource.eliot.eliotc.compiler.cache

import java.time.Instant
import com.vanillasource.eliot.eliotc.processor.*

case class Initial(startTime: Instant) extends CompilerFact {
  override def key(): CompilerFactKey[Initial] = Initial.Key
}

object Initial {
  case object Key extends CompilerFactKey[Initial]
}
```

```scala
class InitialProcessor(startTime: Instant)
    extends SingleKeyTypeProcessor[Initial.Key.type] {
  override protected def generateFact(key: Initial.Key.type): CompilerIO[Unit] =
    registerFactIfClear(Initial(startTime))
}
```

The `startTime` is captured once at compilation start and injected into the processor. This ensures `Initial` produces a deterministic value within a single run but a different value across runs.

### FileStat (existing fact — modified processor)

`FileStat` already exists in `lang/source/stat/` with the right structure:

```scala
case class FileStat(file: File, lastModified: Option[Instant]) extends CompilerFact {
  override def key(): CompilerFactKey[FileStat] = FileStat.Key(file)
}

object FileStat {
  case class Key(file: File) extends CompilerFactKey[FileStat]
}
```

`FileStatProcessor` is modified to depend on `Initial`:

```scala
class FileStatProcessor extends SingleFactProcessor[FileStat.Key] with Logging {
  override protected def generateSingleFact(key: FileStat.Key): CompilerIO[FileStat] =
    for {
      _ <- getFactOrAbort(Initial.Key)  // ← new: depend on Initial → always re-run
      result <- IO(key.file.lastModified()).attempt
                  .map(_.toOption)
                  .map(lastModified => FileStat(key.file, lastModified.filter(_ > 0L).map(Instant.ofEpochMilli)))
                  .to[CompilerIO]
    } yield result
}
```

### FileContent (existing fact — modified processor)

`FileContentReader` is modified to explicitly depend on `FileStat`, creating the invalidation chain:

```scala
class FileContentReader extends SingleFactProcessor[FileContent.Key] with Logging {
  override protected def generateSingleFact(key: FileContent.Key): CompilerIO[FileContent] =
    for {
      _ <- getFactOrAbort(FileStat.Key(key.file))  // ← new: establishes dependency on file mtime
      // ... existing file reading logic unchanged ...
    } yield result
}
```

### Why This Works

- `Initial` changes every run → `FileStat` always re-runs (it depends on `Initial`)
- `FileStat` is a cheap `stat` call → negligible cost even for many files
- If file is unchanged → `FileStat` produces same `lastModified` → same hash → downstream is cached
- If file changed → `FileStat` produces different `lastModified` → different hash → downstream recomputes

The beauty is that this integrates naturally with the existing fact system. No special external fingerprinting mechanism needed — it's all facts and dependencies, reusing the infrastructure already in place.

## 7. Dependency Tracking

During fact generation, we record which facts each generated fact depends on. This follows the existing `TrackedCompilationProcess` pattern from the visualization system (`eliotc/.../visualization/TrackedCompilationProcess.scala`), which already wraps `CompilationProcess` to record `getFact` and `registerFact` calls for the fact graph visualization.

### DependencyTrackingProcess

```scala
package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.{IO, Ref}
import com.vanillasource.eliot.eliotc.processor.*

final class DependencyTrackingProcess(
    underlying: CompilationProcess,
    dependencies: Ref[IO, Set[CompilerFactKey[?]]]
) extends CompilationProcess {

  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K
  ): IO[Option[V]] =
    dependencies.update(_ + key) >> underlying.getFact(key)

  override def registerFact(value: CompilerFact): IO[Unit] =
    underlying.registerFact(value)
}
```

Each fact generation runs in its own fiber (via `.start` in `FactGenerator`), and gets its own `DependencyTrackingProcess` instance — no concurrency issues.

### What Gets Stored

After compilation, each fact's recorded dependencies and content hash are saved to the cache. No transitive analysis is needed at save time — the recursive validation at load time handles transitivity naturally.

## 8. Cache Storage Format

### CacheEntry

```scala
case class CacheEntry(
    directDependencies: Set[CompilerFactKey[?]],
    directDependencyHashes: Map[CompilerFactKey[?], Long],
    contentHash: Long,
    serializedFact: Option[Array[Byte]]  // None for ephemeral facts
)
```

| Field | Purpose |
|-------|---------|
| `directDependencies` | Set of immediate dependency keys. For recursive validation traversal. |
| `directDependencyHashes` | Hashes of immediate dependencies. For propagation cutoff during recomputation. |
| `contentHash` | Hash of this fact's serialized form. For cutoff: detect unchanged content. |
| `serializedFact` | Serialized fact data. `None` for ephemeral facts. |

### FactCacheData

```scala
case class FactCacheData(
    version: Int,
    entries: Map[CompilerFactKey[?], CacheEntry]
)
```

No separate source fingerprints needed — file freshness is modeled as `FileStat` facts within the same system.

### Persistence

```scala
object FactCache {
  private val CACHE_VERSION = 1
  private val CACHE_FILE    = ".eliot-cache"

  def load(targetDir: Path): IO[Option[FactCacheData]] =
    IO.blocking {
      val cacheFile = targetDir.resolve(CACHE_FILE).toFile
      if (cacheFile.exists()) {
        val ois  = new ObjectInputStream(new FileInputStream(cacheFile))
        val data = ois.readObject().asInstanceOf[FactCacheData]
        ois.close()
        if (data.version == CACHE_VERSION) Some(data) else None
      } else None
    }.handleError(_ => None)

  def save(targetDir: Path, data: FactCacheData): IO[Unit] =
    IO.blocking {
      Files.createDirectories(targetDir)
      val oos = new ObjectOutputStream(
        new FileOutputStream(targetDir.resolve(CACHE_FILE).toFile)
      )
      oos.writeObject(data)
      oos.close()
    }
}
```

Cache location: `target/.eliot-cache` (alongside other build artifacts).

### Serialization Strategy

All ELIOT compiler facts are Scala 3 case classes, which automatically extend `Product with Serializable`. Java serialization works out of the box with zero changes to existing fact types.

Content hashing uses `MurmurHash3.bytesHash` on the serialized bytes — fast and sufficient for change detection (not security-critical).

Cache version mismatches (e.g., after compiler changes) cause the entire cache to be discarded and rebuilt from scratch. This is safe — worst case is a single full recompilation.

## 9. The IncrementalFactGenerator

This is the central component. It implements `CompilationProcess` and sits between the plugins and the fact generation machinery.

### getFact Flow

```
getFact(key)
  │
  ├─ In-memory cache (this run)? ────────Yes────► return cached
  │
  ├─ Persistent cache entry exists?
  │    │
  │    ├─ Recursively validate all direct dependencies
  │    │   (each dependency validates its own dependencies, etc.)
  │    │
  │    ├─ All dependency hashes match cached? ──Yes──► deserialize & return
  │    │                                               (or regenerate if ephemeral)
  │    │
  │    └─ Some dependency hash differs:
  │         Recompute fact → hash result
  │         │
  │         ├─ Content hash matches cached → CUTOFF
  │         │   (dependents see unchanged hash)
  │         │
  │         └─ Content hash differs → propagate
  │
  └─ Not in cache → generate fresh (with dependency tracking)
```

### Sketch

```scala
final class IncrementalFactGenerator(
    generator: CompilerProcessor,
    cache: Option[FactCacheData],
    ephemeralTypes: Set[Class[?]],
    errors: Ref[IO, Chain[CompilerError]],
    facts: Ref[IO, Map[CompilerFactKey[?], Deferred[IO, Option[CompilerFact]]]],
    directDependencies: Ref[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]],
    currentHashes: Ref[IO, Map[CompilerFactKey[?], Long]]
) extends CompilationProcess {

  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K
  ): IO[Option[V]] =
    for {
      existing <- checkInMemory(key)
      result   <- existing match {
                    case Some(deferred) => deferred.get.map(_.map(_.asInstanceOf[V]))
                    case None           => getFactWithCache(key)
                  }
    } yield result

  private def getFactWithCache[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K
  ): IO[Option[V]] =
    cache.flatMap(_.entries.get(key)) match {
      case Some(entry) => validateAndReturn(key, entry)
      case None        => generateFresh(key)
    }

  private def validateAndReturn[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K,
      entry: CacheEntry
  ): IO[Option[V]] =
    for {
      // Recursively validate all direct dependencies first
      _        <- entry.directDependencies.toList.traverse_(dep => getFact(dep))
      hashes   <- currentHashes.get
      depsMatch = entry.directDependencyHashes.forall { case (dk, dh) =>
                    hashes.get(dk).contains(dh)
                  }
      result   <- if (depsMatch) loadFromCache(key, entry)
                  else recomputeWithCutoff(key, entry)
    } yield result

  /** Recompute fact, but check if content hash is unchanged (cutoff). */
  private def recomputeWithCutoff[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K,
      entry: CacheEntry
  ): IO[Option[V]] =
    for {
      result  <- generateFresh(key)
      hashes  <- currentHashes.get
      // If the new content hash matches cached, dependents see no change
      // (cutoff happens naturally — their dependency hashes will still match)
    } yield result

  private def loadFromCache[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K,
      entry: CacheEntry
  ): IO[Option[V]] =
    entry.serializedFact match {
      case Some(bytes) =>
        val fact = deserialize(bytes).asInstanceOf[V]
        currentHashes.update(_.updated(key, entry.contentHash)) >>
          registerInMemory(key, Some(fact)).as(Some(fact))
      case None =>
        // Ephemeral fact — not serialized, must regenerate
        generateFresh(key)
    }

  /** Generate with dependency tracking, record hash. */
  private def generateFresh[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K
  ): IO[Option[V]] =
    for {
      modifyResult <- modifyAtomicallyFor(key)
      _            <- (for {
                        depTracker      <- Ref.of[IO, Set[CompilerFactKey[?]]](Set.empty)
                        trackingProcess  = new DependencyTrackingProcess(this, depTracker)
                        _               <- generator.generate(key)
                                             .run(trackingProcess)
                                             .runS(Chain.empty)
                                             .fold(identity, identity)
                                             .flatMap(es => errors.update(_ ++ es))
                        deps            <- depTracker.get
                        _               <- directDependencies.update(_.updated(key, deps))
                      } yield ())
                        .recoverWith(_ => modifyResult._1.complete(None).void)
                        .start
                        .whenA(modifyResult._2)
      result       <- modifyResult._1.get
      // Record content hash for propagation cutoff
      _            <- result.traverse_ { fact =>
                        val hash = computeContentHash(fact)
                        currentHashes.update(_.updated(key, hash))
                      }
    } yield result.map(_.asInstanceOf[V])
}
```

The engine has no concept of "sentinel" or "special" facts. `Initial` happens to always be freshly registered at startup (so it's always in-memory with a new value), and `FileStat` depends on `Initial` (so it always recomputes). But the engine doesn't know or care — it just sees facts, dependencies, and hashes.

**Relationship to existing `FactGenerator`**: `IncrementalFactGenerator` replaces `FactGenerator` entirely. It reuses the same `Deferred`-based in-memory caching pattern (see `FactGenerator.modifyAtomicallyFor`) but adds persistent cache lookups, dependency tracking, and hash-based validation on top.

## 10. Persistent vs Ephemeral Facts

### Rationale

Not all facts are worth serializing to disk. Below is the complete list of current fact types:

| Fact Type | Category | Rationale |
|-----------|----------|-----------|
| `Initial` | **Never cached** | Always changes (compilation start time) |
| `FileStat` | Ephemeral | Always re-runs (stat call), cheap |
| `FileContent` | Ephemeral | Re-read from disk if needed |
| `ResourceContent` | Ephemeral | Re-read from classpath if needed |
| `SourceContent` | Ephemeral | Delegates to FileContent/ResourceContent |
| `PathScan` | Ephemeral | Cheap filesystem scan |
| `SourceTokens` | Ephemeral | Tokenization is fast |
| `SourceAST` | Ephemeral | Parsing is fast |
| `CoreAST` | Ephemeral | Pure transformation, fast |
| `ModuleNames` | Ephemeral | Simple extraction from CoreAST |
| `ModuleValue` | Ephemeral | Simple extraction from CoreAST |
| `UnifiedModuleNames` | **Persistent** | Cross-file merge checkpoint |
| `UnifiedModuleValue` | **Persistent** | Cross-file merge checkpoint |
| `NamedEvaluable` | **Persistent** | Type evaluation of values and data types |
| `ResolvedValue` | **Persistent** | Name resolution is cross-module |
| `MatchDesugaredValue` | **Persistent** | Match exhaustiveness checking |
| `OperatorResolvedValue` | **Persistent** | Operator precedence resolution |
| `AbilityImplementation` | **Persistent** | Cross-module ability lookup |
| `AbilityImplementationCheck` | **Persistent** | Implementation validation |
| `MonomorphicValue` | **Persistent** | Monomorphization is expensive |
| `UsedNames` | **Persistent** | Reachability analysis |
| `UncurriedMonomorphicValue` | **Persistent** | Checkpoint before codegen |
| `GeneratedModule` | **Persistent** | Bytecode generation is expensive |
| `GenerateExecutableJar` | **Persistent** | Final JAR output |

### How Ephemeral Facts Work

For ephemeral facts, the cache stores the `directDependencies`, `directDependencyHashes`, and `contentHash` — but NOT the `serializedFact` (it's `None`).

- **Nothing changed**: Dependency hashes match all the way down → the persistent facts downstream are loaded from cache. Ephemeral facts are never needed (they're intermediaries).
- **Something changed**: Ephemeral facts in the dirty subtree are regenerated. This is fast (they're cheap computations). Their new hashes participate in propagation cutoff.

### Plugin Registration

Plugins declare ephemeral fact types via a new method on `CompilerPlugin`:

```scala
// Addition to CompilerPlugin trait:
def ephemeralFactTypes(): Set[Class[? <: CompilerFactKey[?]]] = Set.empty
```

`LangPlugin` overrides (this is the plugin that registers all language processors):

```scala
override def ephemeralFactTypes(): Set[Class[? <: CompilerFactKey[?]]] = Set(
  classOf[FileStat.Key],
  classOf[FileContent.Key],
  classOf[ResourceContent.Key],
  classOf[SourceContent.Key],
  classOf[PathScan.Key],
  classOf[SourceTokens.Key],
  classOf[SourceAST.Key],
  classOf[CoreAST.Key],
  classOf[ModuleNames.Key],
  classOf[ModuleValue.Key]
)
```

## 11. Integration with Compiler.scala

### Current Compilation Flow

The current `Compiler.runWithConfiguration` does the following:
1. Selects target plugin via `isSelectedBy()`
2. Collects activated plugins via `collectActivatedPlugins()`
3. Configures plugins via `configure()` (StateT over Configuration)
4. Initializes processors via `initialize()` (StateT over CompilerProcessor, starting with `NullProcessor()`)
5. Creates `FactVisualizationTracker` and wraps processors with `TrackedCompilerProcessor`
6. Creates `FactGenerator` (in-memory only, no persistence)
7. Runs target plugin
8. Prints errors and generates visualization

### Modified Compilation Flow

```scala
private def runWithConfiguration(configuration: Configuration, plugins: Seq[CompilerPlugin]): IO[Unit] =
  plugins.find(_.isSelectedBy(configuration)) match {
    case None => User.compilerGlobalError("No target plugin selected.")
    case Some(targetPlugin) =>
      for {
        _                <- debug[IO](s"Selected target plugin: ${targetPlugin.getClass.getSimpleName}")
        activatedPlugins  = collectActivatedPlugins(targetPlugin, configuration, plugins)
        newConfiguration <- activatedPlugins.traverse_(_.configure()).runS(configuration)

        // Collect ephemeral fact types from all plugins
        ephemeralTypes    = activatedPlugins.flatMap(_.ephemeralFactTypes()).toSet

        // Collect processors, prepending Initial processor (replaces NullProcessor)
        startTime        <- IO.realTimeInstant
        processor        <- activatedPlugins.traverse_(_.initialize(newConfiguration)).runS(
                              InitialProcessor(startTime)  // replaces NullProcessor()
                            )

        // Wrap with visualization tracking (as before)
        tracker          <- FactVisualizationTracker.create()
        wrappedProcessors = processor.wrapWith(wrapProcessor(_, tracker))

        // Load persistent cache
        targetPath        = newConfiguration.get(targetPathKey).get
        cacheData        <- FactCache.load(targetPath)

        // Create incremental fact generator (replaces FactGenerator.create)
        generator        <- IncrementalFactGenerator.create(
                              wrappedProcessors, cacheData, ephemeralTypes
                            )

        // Run compilation
        _                <- targetPlugin.run(newConfiguration, generator)

        // Save updated cache
        newCacheData     <- generator.buildCacheData()
        _                <- FactCache.save(targetPath, newCacheData)

        // Print errors, generate visualization (as before)
        errors           <- generator.currentErrors()
        _                <- errors.traverse_(_.print())
        _                <- tracker.generateVisualization(
                              newConfiguration.get(visualizeFactsKey)
                                .getOrElse(targetPath.resolve("fact-visualization.html"))
                            )
      } yield ()
  }
```

**Key changes from current code:**
- `NullProcessor()` replaced by `InitialProcessor(startTime)` as the initial processor
- `FactGenerator.create(wrappedProcessors)` replaced by `IncrementalFactGenerator.create(...)`
- Cache load/save added around compilation
- Ephemeral types collected from plugins

### Fast Path

The fast path emerges naturally from the fact system:

1. Plugin calls `getFact(GenerateExecutableJar.Key(main))`
2. `IncrementalFactGenerator` finds it in cache, recursively validates dependencies
3. Validation walks down to `FileStat` facts → each re-runs (depends on `Initial` which changed) → stat files
4. All mtimes match → all hashes match → validation propagates back up → return cached fact
5. **Zero file reads, zero deserialization of intermediate facts, zero computation**

The only work done is: load cache file + N stat calls + recursive hash comparisons + deserialize the one requested fact.

## 12. Building the Cache After Compilation

After compilation completes, the `IncrementalFactGenerator` builds a new `FactCacheData`:

```scala
def buildCacheData(): IO[FactCacheData] =
  for {
    allFacts  <- collectAllFacts()
    deps      <- directDependencies.get
    hashes    <- currentHashes.get
    entries    = allFacts.map { case (key, fact) =>
                   val serialized   = serialize(fact)
                   val contentHash  = MurmurHash3.bytesHash(serialized).toLong
                   val isPersistent = !ephemeralTypes.contains(key.getClass)
                   val factDeps     = deps.getOrElse(key, Set.empty)
                   val depHashes    = factDeps.flatMap(d => hashes.get(d).map(d -> _)).toMap
                   key -> CacheEntry(
                     directDependencies = factDeps,
                     directDependencyHashes = depHashes,
                     contentHash = contentHash,
                     serializedFact = if (isPersistent) Some(serialized) else None
                   )
                 }
  } yield FactCacheData(
    version = FactCache.CACHE_VERSION,
    entries = entries.toMap
  )
```

## 13. Edge Cases

### New Source Files

Not in cache → `FileStat(file)` has no cached entry → generated fresh → `FileContent(file)` generated fresh → `SourceContent(uri)` generated fresh → all downstream fresh. Cache updated to include the new file on save.

### Deleted Source Files

`FileStat(file)` runs → file doesn't exist → `lastModified` returns `None` → hash differs from cached → downstream invalidated. `FileContentReader` reports a compiler error for missing files and aborts that fact chain. Facts that depended on the deleted file fail to generate (as today).

### Synthetic Sources (JvmProgramGenerator)

`JvmProgramGenerator` calls `addSource(file, content)` to register a synthetic `SourceContent` directly. This fact doesn't depend on `FileStat` — it depends on the processor's inputs (`UsedNames`, etc.). It's tracked like any other derived fact through the generic dependency mechanism.

### Cache Corruption

`FactCache.load` catches all exceptions → corrupted cache = no cache → full recompilation.

### Side-Effecting Processors (JAR Writing)

`JvmProgramGenerator` writes JAR files during fact generation. If the fact is cached and valid, the JAR from the previous run is still valid — no regeneration, no writing. If the JAR is manually deleted but the cache thinks it's valid, the user must clean the cache (`rm -rf target/`).

## 14. Required Changes Summary

### New Files (eliotc module)

All under `eliotc/src/com/vanillasource/eliot/eliotc/`:

1. **`compiler/cache/Initial.scala`** — `Initial` fact + `InitialProcessor`
2. **`compiler/cache/CacheEntry.scala`** — `CacheEntry` and `FactCacheData`
3. **`compiler/cache/FactCache.scala`** — Cache persistence (load/save)
4. **`compiler/cache/CacheValidator.scala`** — Validation + propagation cutoff
5. **`compiler/cache/DependencyTrackingProcess.scala`** — Dependency recording
6. **`compiler/IncrementalFactGenerator.scala`** — Cache-aware `CompilationProcess`

### Modified Files

| File | Change |
|------|--------|
| `eliotc/.../compiler/Compiler.scala` | Register `InitialProcessor`, load/save cache, use `IncrementalFactGenerator` instead of `FactGenerator` |
| `eliotc/.../plugin/CompilerPlugin.scala` | Add `ephemeralFactTypes()` method (default: empty set) |
| `lang/.../source/stat/FileStatProcessor.scala` | Add `getFactOrAbort(Initial.Key)` before stat call |
| `lang/.../source/file/FileContentReader.scala` | Add `getFactOrAbort(FileStat.Key(key.file))` before reading file |
| `lang/.../plugin/LangPlugin.scala` | Override `ephemeralFactTypes()` to declare cheap fact types |

### No Changes Required

- No other processor implementations need modification
- No fact types need modification (already `Serializable` via case class)
- No changes to `CompilerIO` monad
- No changes to `CompilerFactKey` or `CompilerFact` traits
- No changes to `SourceContentReader` (it already depends on `FileContent` which will depend on `FileStat`)
- JVM module unchanged (except `GenerateExecutableJar` facts are cached like any other)

## 15. Performance Characteristics

| Scenario | Work Done |
|----------|-----------|
| Nothing changed | Load cache + N `stat` calls (via `FileStat`) + deserialize requested facts. Microseconds to milliseconds. |
| One file changed, no signature changes | Re-stat all files + re-read changed file + recompute its pipeline + hash comparisons at each level. Cutoff stops at first unchanged signature. |
| One file changed, signature changed | Same as above but propagation continues further. Cutoff stops when downstream fact content matches. |
| Everything changed | Full recompilation + cache save overhead. |
| First compilation (no cache) | Full recompilation + dependency tracking + cache save. Slightly slower due to tracking overhead. |

## 16. Future Enhancements

1. **Output file existence checks**: Verify that side-effect outputs (JAR files) still exist before declaring cache valid
2. **Parallel `FileStat` computation**: Run `FileStat` facts concurrently for faster startup
3. **Partial cache loading**: Deserialize only requested facts, not the entire cache file
4. **Fine-grained fact hashing**: Hash "signature" and "body" parts of facts separately for better propagation cutoff
5. **CompilerIO purity enforcement**: Restrict the `CompilerIO` monad to prevent processors from performing arbitrary IO, formally guaranteeing the pure function property that the cache relies on
6. **File watching mode**: Use filesystem watchers for instant change detection in a long-running compiler daemon
