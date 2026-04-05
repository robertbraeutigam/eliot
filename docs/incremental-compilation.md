# Incremental Compilation Design

## 1. Problem Statement

Currently, every invocation of the ELIOT compiler recomputes all facts from scratch: reading source files, tokenizing, parsing, type checking, monomorphizing, and generating bytecode. For a growing codebase, this becomes increasingly slow even when only a single file changes — or nothing changes at all.

### Goals

1. **Fast no-change path**: If no source files have changed since the last compilation, the compiler should do essentially nothing. This must be fast — just filesystem `stat` calls, no file reads, no computation.
2. **Minimal recomputation on change**: When source files change, only facts that are actually affected should be recomputed. The system should detect when a recomputed intermediate fact produces the same result as before, and stop propagation at that point.
3. **Completely generic**: The mechanism must be confined to the `eliotc` module. No processor implementation should need to know about or deal with incremental compilation. The mechanism relies solely on the property that **all processors are stateless pure functions**.
4. **All facts cached**: Every computed fact is stored in the persistent cache. Each fact's value is needed for equality-based cutoff comparison during incremental validation. There is no ephemeral/persistent distinction.

### Key Property Exploited

All `CompilerProcessor` implementations are stateless and deterministic: given the same input facts, they produce the same output facts. The only external inputs are source files (read by `SourceContentReader`). Therefore, if we can determine that all inputs to a fact are unchanged, the fact itself is unchanged.

## 2. Core Idea: Leaf Facts as the External Boundary

The central design insight is that **leaf facts** — facts with no dependencies — form the boundary between the external world and the deterministic fact system. The incremental engine always recomputes leaf facts (since there are no dependencies to check), then uses equality comparison to determine if anything changed.

### FileStat as a Leaf Fact

The codebase already has `FileStat(file: File, lastModified: Option[Instant])` and `FileStatProcessor` in the `source.stat` package. `FileStatProcessor` performs the `stat` syscall and returns the file's modification time.

`FileStat` has **no fact dependencies** — it only performs a syscall. This makes it a natural leaf: the incremental engine always recomputes it, and if the result equals the cached value, nothing downstream needs to run.

```scala
class FileStatProcessor extends SingleFactProcessor[FileStat.Key] with Logging {
  override protected def generateSingleFact(key: FileStat.Key): CompilerIO[FileStat] =
    IO(key.file.lastModified()).attempt
      .map(_.toOption)
      .map(lastModified => FileStat(key.file, lastModified.filter(_ > 0L).map(Instant.ofEpochMilli)))
      .to[CompilerIO]
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
FileStat(Foo.els) (leaf — always recomputed, produces file mtime)
  ← FileContent(Foo.els) (only runs if mtime changed)
    ← SourceContent(Foo.els) (only runs if content changed)
      ← SourceTokens(Foo.els) (only runs if content changed)
        ← ... entire downstream pipeline
```

If the file's mtime is unchanged, `FileStat` produces the same value as cached → `FileContent` doesn't need to re-run → nothing downstream runs.

**Note:** `SourceContentReader` handles both `file://` URIs (via `FileContent`) and resource URIs (via `ResourceContent`). Resource content is read from the classpath and is effectively immutable at compile time, so it has no leaf dependencies and doesn't need revalidation.

## 3. Leaf-Based Forward Validation

### Cache Entry Structure

Each cached fact stores three things: its **leaf dependencies** (the transitive sources of external input), its **forward keys** (what directly depends on it), and the **fact value** itself.

```scala
case class CacheEntry(
    leafDependencies: Set[CompilerFactKey[?]],
    forwardKeys: Set[CompilerFactKey[?]],
    fact: CompilerFact
)
```

| Field | Purpose |
|-------|---------|
| `leafDependencies` | Transitive leaf facts — the boundary between external world and deterministic computation. A leaf is any fact with no dependencies (e.g., `FileStat` keys, `OutputFileStat` keys). Leaves are always recomputed because there are no dependencies to validate. |
| `forwardKeys` | Facts that directly depend on THIS fact (reverse/forward edges). If A depends on B, then B's `forwardKeys` contains A. Used for forward propagation walk. |
| `fact` | The actual cached `CompilerFact` value. Compared by structural equality (`==`) for cutoff detection. |

### How Validation Works

To validate a cached fact, the engine checks its leaf dependencies first. If all leaves are unchanged, the fact is valid. If a leaf changed, the engine walks forward from the changed leaf, recomputing facts along the way and checking for equality-based cutoff.

**Step 1**: Request fact F. Look up its `CacheEntry`. Get its `leafDependencies`.

**Step 2**: Recompute each leaf fact (e.g., `FileStat` runs a `stat` call). Compare the recomputed value with the stored `entry.fact` using Scala case class `==`.

**Step 3**: If all leaves are equal to their cached values → **F is valid**. Return the cached fact. No further work needed.

**Step 4**: If a leaf is not equal → walk forward from that leaf using `forwardKeys`. At each step, recompute the fact by running its processor. Compare the result with the cached `entry.fact` by equality.

**Step 5**: If equal at any point → **cutoff**. Stop propagation on this path. The fact is unchanged despite its inputs changing.

**Step 6**: If not equal → update the cached value and continue forward until either cutoff occurs or the originally requested fact is reached and recomputed.

### Why This Is Efficient

In the no-change case, only leaf facts are recomputed (cheap `stat` calls). All equality checks pass at the leaf level, so no forward walking happens at all. The work is O(number of leaves) — typically one per source file — rather than O(number of facts in the dependency subtree).

When a leaf does change, the forward walk only touches facts in the "dirty cone" emanating from that leaf. Each fact is visited at most once (memoized by a visited set). Cutoff stops propagation as soon as a recomputed fact equals its cached value.

### Example: Nothing Changed

For `MonomorphicValue(Foo.bar)` which transitively depends on `Foo.els` and `String.els`:

```
Request: MonomorphicValue(Foo.bar)
  leafDependencies = {FileStat(Foo.els), FileStat(String.els)}

Step 1: Recompute FileStat(Foo.els) → stat call → same mtime → equals cached → CUTOFF
Step 2: Recompute FileStat(String.els) → stat call → same mtime → equals cached → CUTOFF
All leaves unchanged → MonomorphicValue(Foo.bar) is valid → return cached fact
```

Only 2 stat calls. No intermediate facts visited. No forward walking.

### Example: File Changed

```
Request: MonomorphicValue(Foo.bar)
  leafDependencies = {FileStat(Foo.els), FileStat(String.els)}

Step 1: Recompute FileStat(Foo.els) → stat call → different mtime → NOT equal
  Forward-walk from FileStat(Foo.els):
    forwardKeys = {FileContent(Foo.els)}
    Recompute FileContent(Foo.els) → re-read file → NOT equal → continue
      forwardKeys = {SourceContent(Foo.els)}
      Recompute SourceContent(Foo.els) → NOT equal → continue
        → ... forward through pipeline ...
          → Recompute ResolvedValue(Foo.bar) → EQUAL → CUTOFF
            (body changed but this fact didn't — propagation stops)

Step 2: Recompute FileStat(String.els) → stat call → same mtime → CUTOFF

MonomorphicValue(Foo.bar) was never reached by forward walk
  → all paths cut off → still valid → return cached fact
```

## 4. Equality-Based Propagation Cutoff

When a leaf changes, facts depending on it need recomputation. But not necessarily ALL of them — equality-based cutoff minimizes work.

### The Mechanism

During the forward walk from a changed leaf, each recomputed fact is compared with its cached value using structural equality (`==` on Scala case classes). Since all processors are pure functions, if a recomputed fact equals the cached value, all of its dependents are guaranteed to produce the same results too — so propagation stops.

This is the key insight: **a change in a source file may not change every derived fact**. For example, changing a function body doesn't change its type signature. Facts that depend only on the signature will be equal to their cached values, cutting off propagation.

### Example: Signature Unchanged

File `Foo.els` changes, but `Foo.bar`'s type signature stays the same (only body changes):

```
1.  FileStat(Foo.els) → recompute → NOT equal (new mtime)
2.  FileContent(Foo.els) → recompute (re-read file) → NOT equal
3.  SourceContent(Foo.els) → recompute → NOT equal
4.  SourceTokens(Foo.els) → recompute → NOT equal
5.  SourceAST(Foo.els) → recompute → NOT equal
6.  CoreAST(Foo.els) → recompute → NOT equal
7.  ModuleValue(Foo.els, Foo.bar) → recompute → NOT equal (body differs)
8.  UnifiedModuleValue(Foo.bar) → recompute → NOT equal
9.  ResolvedValue(Foo.bar) → recompute → NOT equal
10. MatchDesugaredValue(Foo.bar) → recompute → NOT equal
11. OperatorResolvedValue(Foo.bar) → recompute → NOT equal
12. MonomorphicValue(Foo.bar) → recompute → NOT equal (body differs)
13. MonomorphicValue(Other.baz) calls Foo.bar:
    Recompute → result EQUALS cached (only uses Foo.bar's signature)
    → CUTOFF: propagation stops
14. Everything downstream of Other.baz → untouched ✓
```

### Why Equality Instead of Hashing

Using structural equality on the actual fact values (rather than comparing hashes of serialized bytes) has several advantages:

- **Simpler**: No hash computation, no hash storage, no hash collisions to worry about.
- **Exact**: Equality is precise — no false positives from hash collisions.
- **Natural**: Scala case classes provide structural equality out of the box.
- **Unified storage**: Every cache entry stores the fact itself, serving both as the cached result and the comparison value. No separate `serializedFact: Option[Array[Byte]]` with `None` for some facts.

## 5. Architecture Overview

```
┌──────────────────────────────────────────────────────────────┐
│                      Compiler.scala                          │
│  (load cache → create generator → run → save cache)         │
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
│      → check leaf dependencies (recompute leaves)            │
│      → all leaves equal? → return cached fact                │
│      → some leaf changed? → forward-walk with cutoff         │
│   3. Not in cache → generate with DependencyTrackingProcess  │
│                                                              │
│  ┌───────────────────┐                                       │
│  │ DependencyTracking │                                       │
│  │ Process            │                                       │
│  │ (records getFact   │                                       │
│  │  calls per fact)   │                                       │
│  └───────────────────┘                                       │
│                                                              │
│  ┌───────────────────────────────────────────────┐           │
│  │              FactCache                         │           │
│  │  (persistent storage on disk)                  │           │
│  │  - leaf dependencies per fact                  │           │
│  │  - forward keys per fact                       │           │
│  │  - cached fact values                          │           │
│  └───────────────────────────────────────────────┘           │
└──────────────────────────────────────────────────────────────┘
```

### New Files (all in `eliotc` module)

| File | Package | Purpose |
|------|---------|---------|
| `OutputFileStat.scala` | `compiler.cache` | `OutputFileStat` fact, key, and processor |
| `CacheEntry.scala` | `compiler.cache` | Cache entry data structure |
| `FactCache.scala` | `compiler.cache` | Cache persistence (load/save) |
| `DependencyTrackingProcess.scala` | `compiler.cache` | Records fact dependencies |
| `IncrementalFactGenerator.scala` | `compiler` | Cache-aware `CompilationProcess` |

### Modified Files

| File | Change |
|------|--------|
| `Compiler.scala` (eliotc) | Load/save cache, use `IncrementalFactGenerator` instead of `FactGenerator` |
| `FileContentReader.scala` (lang) | Add `getFactOrAbort(FileStat.Key(key.file))` before reading file |
| `JvmProgramGenerator.scala` (jvm) | Add `getFactOrAbort(OutputFileStat.Key(jarFile))` dependency before writing JAR |
| `JvmPlugin.scala` (jvm) | Register `OutputFileStatProcessor` |

## 6. New Facts and Modified Processors

### OutputFileStat (new fact in `eliotc` module)

Output files (like JAR files) are side effects of fact generation. The cache has no way to know if they were deleted externally. `OutputFileStat` bridges this gap by checking whether an output file is still present and unmodified since the last compilation.

```scala
package com.vanillasource.eliot.eliotc.compiler.cache

import java.io.File
import com.vanillasource.eliot.eliotc.processor.*

case class OutputFileStat(file: File, upToDate: Boolean) extends CompilerFact {
  override def key(): CompilerFactKey[OutputFileStat] = OutputFileStat.Key(file)
}

object OutputFileStat {
  case class Key(file: File) extends CompilerFactKey[OutputFileStat]
}
```

```scala
class OutputFileStatProcessor(cacheTimestamp: Option[Instant])
    extends SingleFactProcessor[OutputFileStat.Key] with Logging {
  override protected def generateSingleFact(key: OutputFileStat.Key): CompilerIO[OutputFileStat] = {
    // No fact dependencies — this is a leaf fact, always recomputed
    val result = cacheTimestamp match {
      case None =>
        // Cold start (no cache). Everything will be computed fresh anyway.
        // Return true so the value is stable for future runs.
        OutputFileStat(key.file, upToDate = true)
      case Some(cacheTime) =>
        val fileExists         = key.file.exists()
        val fileOlderThanCache = key.file.lastModified() <= cacheTime.toEpochMilli
        OutputFileStat(key.file, upToDate = fileExists && fileOlderThanCache)
    }
    IO.pure(result).to[CompilerIO]
  }
}
```

`OutputFileStat` is a **leaf fact** — it has no fact dependencies. The `cacheTimestamp` (modification time of the cache file, or `None` on cold start) is passed as a constructor parameter to the processor, not obtained via `getFact`.

**How it works across runs:**

- **Run 1 (cold start, no cache):** `cacheTimestamp` is `None` → returns `upToDate = true`. This value is stored in the cache. Since there's no prior cache, every fact is computed fresh anyway — the value only matters as a baseline for future runs.
- **Run 2 (nothing changed, JAR exists):** `cacheTimestamp` is `Some(t)`, JAR exists with mtime ≤ t → returns `upToDate = true`. Same value as Run 1 → no invalidation → zero extra work.
- **Run N (JAR deleted):** `cacheTimestamp` is `Some(t)`, JAR missing → returns `upToDate = false`. Value differs → invalidates the downstream `GenerateExecutableJar` → JAR gets regenerated. All upstream facts (`GeneratedModule`, etc.) are still cached and unchanged.
- **Run N+1 (JAR restored):** Returns `upToDate = true` again → same value as before deletion → stable.

**Dependency chain for JAR generation:**

```
OutputFileStat(target/Hello.jar) (leaf — always recomputed, checks file presence)
  ← GenerateExecutableJar (depends on OutputFileStat + GeneratedModules)
    ← GeneratedModule(s) (cached if sources unchanged)
```

**Side-effecting processors** that write files (like `JvmProgramGenerator`) add a dependency on `OutputFileStat` for their output path. If the output is up-to-date, the fact is cached and the write is skipped entirely. If not, the fact is invalidated, the processor re-runs, and rewrites the file.

### FileStat (existing fact — no modification needed)

`FileStat` already exists in `lang/source/stat/` with the right structure and is already a leaf fact (no fact dependencies):

```scala
case class FileStat(file: File, lastModified: Option[Instant]) extends CompilerFact {
  override def key(): CompilerFactKey[FileStat] = FileStat.Key(file)
}

object FileStat {
  case class Key(file: File) extends CompilerFactKey[FileStat]
}
```

`FileStatProcessor` has no fact dependencies — it just performs a `stat` syscall:

```scala
class FileStatProcessor extends SingleFactProcessor[FileStat.Key] with Logging {
  override protected def generateSingleFact(key: FileStat.Key): CompilerIO[FileStat] =
    IO(key.file.lastModified()).attempt
      .map(_.toOption)
      .map(lastModified => FileStat(key.file, lastModified.filter(_ > 0L).map(Instant.ofEpochMilli)))
      .to[CompilerIO]
}
```

Because `FileStat` has no dependencies, the incremental engine treats it as a leaf and always recomputes it. No modification to the existing processor is needed.

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

- `FileStat` is a leaf (no dependencies) → always recomputed by the incremental engine
- `FileStat` is a cheap `stat` call → negligible cost even for many files
- If file is unchanged → `FileStat` produces same value → equality check passes → downstream is cached
- If file changed → `FileStat` produces different value → equality check fails → forward walk begins

The beauty is that this integrates naturally with the existing fact system. No special sentinel facts or external fingerprinting needed — leaf facts are simply facts with no dependencies, and the engine always recomputes them.

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

After compilation, the recorded direct dependencies are used to compute two derived structures for each cache entry:

- **`leafDependencies`**: Computed by walking the direct dependency graph transitively until reaching facts with no dependencies (i.e., leaf facts like `FileStat` and `OutputFileStat`). These are the external-world boundary for this fact.
- **`forwardKeys`**: The inverse of direct dependencies. For each dependency edge A → B (A depends on B), B's `forwardKeys` includes A. This enables the forward walk from changed leaves.

## 8. Cache Storage Format

### CacheEntry

```scala
case class CacheEntry(
    leafDependencies: Set[CompilerFactKey[?]],
    forwardKeys: Set[CompilerFactKey[?]],
    fact: CompilerFact
)
```

| Field | Purpose |
|-------|---------|
| `leafDependencies` | Transitive leaf facts (boundary with external world). For quick leaf checking on validation. |
| `forwardKeys` | Facts that directly depend on this fact. For forward propagation walk when a leaf changes. |
| `fact` | The cached fact value. For equality comparison at every level and for returning to callers. |

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

  def timestamp(targetDir: Path): IO[Option[Instant]] =
    IO.blocking {
      val cacheFile = targetDir.resolve(CACHE_FILE).toFile
      if (cacheFile.exists()) Some(Instant.ofEpochMilli(cacheFile.lastModified()))
      else None
    }.handleError(_ => None)
}
```

Cache location: `target/.eliot-cache` (alongside other build artifacts).

### Serialization Strategy

All ELIOT compiler facts are Scala 3 case classes, which automatically extend `Product with Serializable`. Java serialization works out of the box with zero changes to existing fact types. Case classes also provide structural equality via `==`, which is used for cutoff detection.

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
  │    ├─ Recompute each leaf in leafDependencies
  │    │   Compare with cached leaf value (equality)
  │    │
  │    ├─ All leaves equal? ──Yes──► return cached fact
  │    │
  │    └─ Some leaf not equal:
  │         Forward-walk from changed leaf using forwardKeys
  │         At each step: recompute → compare with cached (equality)
  │         │
  │         ├─ Equal → CUTOFF (stop this path)
  │         │
  │         └─ Not equal → continue forward
  │              Eventually reaches requested fact or all paths cut off
  │
  └─ Not in cache → generate fresh (with dependency tracking)
```

### Sketch

```scala
final class IncrementalFactGenerator(
    generator: CompilerProcessor,
    cache: Option[FactCacheData],
    errors: Ref[IO, Chain[CompilerError]],
    facts: Ref[IO, Map[CompilerFactKey[?], Deferred[IO, Option[CompilerFact]]]],
    directDependencies: Ref[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]],
    validated: Ref[IO, Set[CompilerFactKey[?]]]
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
      // Recompute each leaf and check equality
      changedLeaves <- entry.leafDependencies.toList.flatTraverse { leafKey =>
                         recomputeLeaf(leafKey).map {
                           case true  => List.empty   // leaf unchanged
                           case false => List(leafKey) // leaf changed
                         }
                       }
      result <- if (changedLeaves.isEmpty) returnCached(key, entry)
                else forwardWalkAndReturn(key, changedLeaves)
    } yield result

  /** Recompute a leaf fact and compare with cached value.
    * Returns true if unchanged, false if changed. */
  private def recomputeLeaf(leafKey: CompilerFactKey[?]): IO[Boolean] =
    for {
      alreadyValid <- validated.get.map(_.contains(leafKey))
      result       <- if (alreadyValid) IO.pure(true)
                      else for {
                        newFact   <- generateFresh(leafKey)
                        cachedFact = cache.flatMap(_.entries.get(leafKey)).map(_.fact)
                        unchanged  = newFact == cachedFact
                        _         <- validated.update(_ + leafKey).whenA(unchanged)
                      } yield unchanged
    } yield result

  /** Walk forward from changed leaves, recomputing facts along the way.
    * At each step, check equality with cached value for cutoff. */
  private def forwardWalkAndReturn[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K,
      changedLeaves: List[CompilerFactKey[?]]
  ): IO[Option[V]] =
    for {
      // Walk forward from each changed leaf
      _ <- forwardWalkFrom(changedLeaves.toSet, targetKey = key)
      // After walk, key is either recomputed (in memory) or untouched (still valid)
      alreadyComputed <- checkInMemory(key)
      result <- alreadyComputed match {
                  case Some(deferred) => deferred.get.map(_.map(_.asInstanceOf[V]))
                  case None           =>
                    // Forward walk didn't reach this key — all paths cut off
                    cache.flatMap(_.entries.get(key)) match {
                      case Some(entry) => returnCached(key, entry)
                      case None        => generateFresh(key)
                    }
                }
    } yield result

  /** Walk forward from a set of changed keys, recomputing and checking cutoff. */
  private def forwardWalkFrom(
      changed: Set[CompilerFactKey[?]],
      targetKey: CompilerFactKey[?]
  ): IO[Unit] = {
    // Process changed keys, collecting newly changed forward keys
    changed.toList.traverse_ { changedKey =>
      cache.flatMap(_.entries.get(changedKey)) match {
        case None => IO.unit  // no forward links
        case Some(entry) =>
          entry.forwardKeys.toList.traverse_ { fwdKey =>
            for {
              alreadyDone <- validated.get.map(_.contains(fwdKey))
              _           <- (for {
                               newFact    <- generateFresh(fwdKey)
                               cachedFact  = cache.flatMap(_.entries.get(fwdKey)).map(_.fact)
                               unchanged   = newFact == cachedFact
                               _          <- if (unchanged) validated.update(_ + fwdKey)
                                             else forwardWalkFrom(Set(fwdKey), targetKey)
                             } yield ()).unlessA(alreadyDone)
            } yield ()
          }
      }
    }
  }

  private def returnCached[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K,
      entry: CacheEntry
  ): IO[Option[V]] = {
    val fact = entry.fact.asInstanceOf[V]
    validated.update(_ + key) >>
      registerInMemory(key, Some(fact)).as(Some(fact))
  }

  /** Generate with dependency tracking. */
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
    } yield result.map(_.asInstanceOf[V])
}
```

**Relationship to existing `FactGenerator`**: `IncrementalFactGenerator` replaces `FactGenerator` entirely. It reuses the same `Deferred`-based in-memory caching pattern (see `FactGenerator.modifyAtomicallyFor`) but adds persistent cache lookups, leaf-based validation, and equality-based forward walk on top.

## 10. All Facts Cached

All facts are stored in the cache. Unlike a hash-based design where cheap intermediate facts could be excluded, equality-based cutoff requires the actual fact value at every level in the dependency graph. There is no ephemeral/persistent distinction, no plugin method to declare fact categories, and no `Option` wrapper around the stored fact.

This simplifies the design: every `CacheEntry` has the same structure, and the `IncrementalFactGenerator` handles all facts uniformly.

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

        // Load persistent cache
        targetPath        = newConfiguration.get(targetPathKey).get
        cacheData        <- FactCache.load(targetPath)
        cacheTimestamp   <- FactCache.timestamp(targetPath)

        // Collect processors (OutputFileStatProcessor needs cacheTimestamp)
        processor        <- activatedPlugins.traverse_(_.initialize(newConfiguration, cacheTimestamp))
                              .runS(NullProcessor())

        // Wrap with visualization tracking (as before)
        tracker          <- FactVisualizationTracker.create()
        wrappedProcessors = processor.wrapWith(wrapProcessor(_, tracker))

        // Create incremental fact generator (replaces FactGenerator.create)
        generator        <- IncrementalFactGenerator.create(wrappedProcessors, cacheData)

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
- `FactGenerator.create(wrappedProcessors)` replaced by `IncrementalFactGenerator.create(wrappedProcessors, cacheData)`
- Cache load/save added around compilation
- `cacheTimestamp` passed to plugin initialization (needed by `OutputFileStatProcessor`)

### Fast Path

The fast path emerges naturally from the fact system:

1. Plugin calls `getFact(GenerateExecutableJar.Key(main))`
2. `IncrementalFactGenerator` finds it in cache, checks its `leafDependencies`
3. Recomputes each leaf (facts with no dependencies): `FileStat` facts (stat source files) and `OutputFileStat(target/Hello.jar)` (check JAR presence)
4. All leaves equal to cached values → return cached fact immediately
5. **Zero file reads, zero intermediate fact computation, zero JAR writes**

The only work done is: load cache file + N source stat calls + 1 output stat call + equality comparisons at the leaf level.

## 12. Building the Cache After Compilation

After compilation completes, the `IncrementalFactGenerator` builds a new `FactCacheData`:

```scala
def buildCacheData(): IO[FactCacheData] =
  for {
    allFacts <- collectAllFacts()  // Map[CompilerFactKey[?], CompilerFact]
    deps     <- directDependencies.get
    entries   = {
      // Compute leaf sets by walking direct dependencies transitively
      val leafSets = allFacts.keys.map { key =>
        key -> computeLeafSet(key, deps)
      }.toMap

      // Compute forward keys (inverse of direct dependencies)
      val forwardMap = mutable.Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]()
        .withDefaultValue(Set.empty)
      deps.foreach { case (key, depKeys) =>
        depKeys.foreach(dep => forwardMap(dep) = forwardMap(dep) + key)
      }

      allFacts.map { case (key, fact) =>
        key -> CacheEntry(
          leafDependencies = leafSets.getOrElse(key, Set.empty),
          forwardKeys = forwardMap.getOrElse(key, Set.empty),
          fact = fact
        )
      }
    }
  } yield FactCacheData(
    version = FactCache.CACHE_VERSION,
    entries = entries.toMap
  )

/** Walk the dependency graph to find leaf facts.
  * A leaf is a fact with no dependencies. */
private def computeLeafSet(
    key: CompilerFactKey[?],
    deps: Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]
): Set[CompilerFactKey[?]] = {
  val factDeps = deps.getOrElse(key, Set.empty)
  if (factDeps.isEmpty) {
    // This fact IS a leaf (no dependencies)
    Set(key)
  } else {
    // Recurse: this fact's leaves are the union of its dependencies' leaves
    factDeps.flatMap(dep => computeLeafSet(dep, deps))
  }
}
```

## 13. Edge Cases

### New Source Files

Not in cache → `FileStat(file)` has no cached entry → generated fresh → `FileContent(file)` generated fresh → `SourceContent(uri)` generated fresh → all downstream fresh. Cache updated to include the new file on save.

### Deleted Source Files

`FileStat(file)` runs → file doesn't exist → `lastModified` returns `None` → value differs from cached → forward walk begins → downstream invalidated. `FileContentReader` reports a compiler error for missing files and aborts that fact chain. Facts that depended on the deleted file fail to generate (as today).

### Synthetic Sources (JvmProgramGenerator)

`JvmProgramGenerator` calls `addSource(file, content)` to register a synthetic `SourceContent` directly. This fact doesn't depend on `FileStat` — it depends on the processor's inputs (`UsedNames`, etc.). It's tracked like any other derived fact through the generic dependency mechanism.

### Cache Corruption

`FactCache.load` catches all exceptions → corrupted cache = no cache → full recompilation.

### Side-Effecting Processors (JAR Writing)

`JvmProgramGenerator` writes JAR files during fact generation and depends on `OutputFileStat` for its output path. This handles all cases:

- **JAR exists, sources unchanged:** `OutputFileStat` returns `upToDate = true` (equals cached) → `GenerateExecutableJar` is valid → no rewrite.
- **JAR deleted, sources unchanged:** `OutputFileStat` returns `upToDate = false` (differs from cached `true`) → `GenerateExecutableJar` invalidated → processor re-runs, rewrites JAR. All upstream `GeneratedModule` facts are still cached.
- **JAR exists, sources changed:** Upstream invalidation propagates through `GeneratedModule` → `GenerateExecutableJar` re-runs regardless of `OutputFileStat`.

## 14. Required Changes Summary

### New Files (eliotc module)

All under `eliotc/src/com/vanillasource/eliot/eliotc/`:

1. **`compiler/cache/OutputFileStat.scala`** — `OutputFileStat` fact + `OutputFileStatProcessor`
2. **`compiler/cache/CacheEntry.scala`** — `CacheEntry` and `FactCacheData`
3. **`compiler/cache/FactCache.scala`** — Cache persistence (load/save)
4. **`compiler/cache/DependencyTrackingProcess.scala`** — Dependency recording
5. **`compiler/IncrementalFactGenerator.scala`** — Cache-aware `CompilationProcess`

### Modified Files

| File | Change |
|------|--------|
| `eliotc/.../compiler/Compiler.scala` | Load/save cache, use `IncrementalFactGenerator` instead of `FactGenerator` |
| `lang/.../source/file/FileContentReader.scala` | Add `getFactOrAbort(FileStat.Key(key.file))` before reading file |
| `jvm/.../jargen/JvmProgramGenerator.scala` | Add `getFactOrAbort(OutputFileStat.Key(jarFile))` dependency before writing JAR |
| `jvm/.../plugin/JvmPlugin.scala` | Register `OutputFileStatProcessor` |

### No Changes Required

- No other processor implementations need modification
- No fact types need modification (already `Serializable` via case class)
- No changes to `CompilerIO` monad
- No changes to `CompilerFactKey` or `CompilerFact` traits
- No changes to `CompilerPlugin` trait
- No changes to `FileStatProcessor` (already a leaf with no dependencies)
- No changes to `LangPlugin` or any plugin's configuration
- No changes to `SourceContentReader` (it already depends on `FileContent` which will depend on `FileStat`)

## 15. Performance Characteristics

| Scenario | Work Done |
|----------|-----------|
| Nothing changed | Load cache + N `stat` calls (via `FileStat`) + equality checks at leaf level. No forward walking. Microseconds to milliseconds. |
| One file changed, no signature changes | Re-stat all files + forward-walk from changed leaf + recompute facts along the path + equality cutoff stops at first unchanged fact. |
| One file changed, signature changed | Same as above but forward walk continues further. Cutoff stops when a downstream fact equals its cached value. |
| Everything changed | Full recompilation + cache save overhead. |
| First compilation (no cache) | Full recompilation + dependency tracking + cache save. Slightly slower due to tracking overhead. |

## 16. Future Enhancements

1. **Parallel `FileStat` computation**: Run `FileStat` facts concurrently for faster startup
2. **Partial cache loading**: Deserialize only requested facts, not the entire cache file
3. **Fine-grained equality**: Implement custom equality on specific fact types to enable earlier cutoff (e.g., compare only the type signature portion of a value fact, ignoring body changes)
4. **CompilerIO purity enforcement**: Restrict the `CompilerIO` monad to prevent processors from performing arbitrary IO, formally guaranteeing the pure function property that the cache relies on
5. **File watching mode**: Use filesystem watchers for instant change detection in a long-running compiler daemon
