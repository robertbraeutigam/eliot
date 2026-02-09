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

## 2. Core Idea: I/O as Facts with Initial and UpdateTime

The central design insight is to model I/O operations as facts within the existing fact system, using two new built-in facts:

### Initial Fact

A fact `Initial(compilerStart: Instant)` is produced at the start of every compilation run. Its value is the current timestamp, so it **naturally changes with every invocation**. Any fact that depends on `Initial` will always be regenerated.

### UpdateTime Fact

A fact `UpdateTime(file, lastModified: Long)` depends on `Initial`. Because `Initial` always changes, `UpdateTime` is always regenerated — it stats the file and returns its modification time. This is a cheap operation (one `stat` syscall per file).

The key property: **if the file hasn't changed, `UpdateTime` produces the same value as last time**. This makes it an ideal sentinel for cache validation.

### How SourceContentReader Fits In

Currently, `SourceContentReader` reads a file from disk whenever `SourceContent.Key(file)` is requested. With incremental compilation, it is modified to first request `UpdateTime.Key(file)`:

```scala
class SourceContentReader extends SingleKeyTypeProcessor[SourceContent.Key] {
  override protected def generateFact(key: SourceContent.Key): CompilerIO[Unit] =
    for {
      _       <- getFactOrAbort(UpdateTime.Key(key.file))  // ← new: establishes dependency
      content <- readFileContent(key.file)
      _       <- content.traverse_(registerFactIfClear)
    } yield ()
}
```

This single line change creates the dependency chain that makes everything work:

```
Initial (always changes)
  ← UpdateTime(Foo.els) (always runs, produces file mtime)
    ← SourceContent(Foo.els) (only runs if mtime changed)
      ← SourceTokens(Foo.els) (only runs if content changed)
        ← ... entire downstream pipeline
```

If the file's mtime is unchanged, `UpdateTime` produces the same hash as cached → `SourceContent` doesn't need to re-run → nothing downstream runs.

## 3. Flat Sentinel Hashes: Non-Recursive Validation

### The Problem with Recursive Validation

A naive cache validator would recursively walk the dependency tree to check if a fact is valid: check each dependency, which checks its dependencies, and so on down to source files. This is expensive — for a deeply nested dependency chain, validation itself becomes a significant cost.

### Flat Sentinel Approach

Instead, each cached fact stores a **pre-computed flat map of all sentinel facts it transitively depends on**, along with their hashes at computation time:

```scala
case class CacheEntry(
    sentinelHashes: Map[CompilerFactKey[?], Long],
    directDependencyHashes: Map[CompilerFactKey[?], Long],
    contentHash: Long,
    serializedFact: Option[Array[Byte]]
)
```

The `sentinelHashes` field contains keys like `UpdateTime.Key(Foo.els) → 839271` — the hash of each `UpdateTime` fact that this cached fact transitively depends on. This is a **flattened transitive closure** computed once when the cache is saved.

### Validation is O(S) with No Recursion

To validate a cached fact:

1. Run all `UpdateTime` facts referenced in `sentinelHashes` (cheap stat calls)
2. Hash each result
3. Compare with stored hashes
4. If ALL match → fact is valid → return from cache

No dependency tree walking. No intermediate fact checking. Just sentinel comparisons.

```scala
def isValid(entry: CacheEntry, currentSentinelHashes: Map[CompilerFactKey[?], Long]): Boolean =
  entry.sentinelHashes.forall { case (key, expectedHash) =>
    currentSentinelHashes.get(key).contains(expectedHash)
  }
```

### Computing Sentinel Hashes

During compilation, sentinel hashes propagate upward through the dependency chain:

- **Sentinel facts** (depend directly on `Initial`): `sentinelHashes = {self.key → hash(self)}`
- **Derived facts**: `sentinelHashes = union of all dependencies' sentinelHashes`

This is computed incrementally during compilation via the dependency tracker.

Example for `TypeCheckedValue(Foo.bar)` which depends on files `Foo.els` and `String.els`:

```
TypeCheckedValue(Foo.bar).sentinelHashes = {
  UpdateTime.Key(Foo.els) → 7392841,
  UpdateTime.Key(String.els) → 1829374,
  UpdateTime.Key(Unit.els) → 9283741
}
```

Validation: run three `stat` calls, hash results, compare. Done.

## 4. Hash-Based Propagation Cutoff on Recomputation

When a sentinel hash changes (a file was modified), facts depending on it need recomputation. But not necessarily ALL of them — hash-based cutoff minimizes work.

### The Mechanism

Each cache entry also stores `directDependencyHashes` — the content hashes of its immediate dependencies at the time it was computed. When recomputing the dirty subtree:

1. **Identify dirty sentinels**: `UpdateTime` facts whose hash differs from cached
2. **Identify potentially dirty facts**: All facts whose `sentinelHashes` reference a dirty sentinel
3. **Process in dependency order** (from sentinels toward final outputs):
   - For each potentially dirty fact, check `directDependencyHashes`
   - If all direct dependencies' current hashes match → **cutoff**: fact is actually clean, skip
   - If any differ → recompute → hash result → compare with cached `contentHash`:
     - If matches → content unchanged despite input change → propagation stops here
     - If differs → fact truly changed → continue to dependents

### Example: Signature Unchanged

File `Foo.els` changes, but `Foo.bar`'s type signature stays the same (only body changes):

```
1. UpdateTime(Foo.els) → CHANGED (new mtime)
2. SourceContent(Foo.els) → dirty → recompute (re-read file) → hash CHANGED
3. SourceTokens(Foo.els) → dirty → recompute → hash CHANGED
4. SourceAST(Foo.els) → dirty → recompute → hash CHANGED
5. CoreAST(Foo.els) → dirty → recompute → hash CHANGED
6. ModuleValue(Foo.els, Foo.bar) → dirty → recompute → hash CHANGED (body differs)
7. ResolvedValue(Foo.bar) → dirty → recompute → hash CHANGED
8. TypeCheckedValue(Foo.bar) → dirty → recompute → hash CHANGED (body differs)
9. TypeCheckedValue(Other.baz) calls Foo.bar:
   - Check directDependencyHashes:
     TypeCheckedValue(Foo.bar) hash CHANGED
   - Must recompute → result is SAME (only uses Foo.bar's signature)
   → contentHash MATCHES cached → CUTOFF: propagation stops
10. Everything downstream of Other.baz → untouched ✓
```

### Dependency Order Processing

The dirty subtree is processed bottom-up (from sentinels toward outputs). This ensures that when checking a fact's `directDependencyHashes`, all dependencies have already been processed and their current hashes are known.

```scala
def recomputeDirtySubtree(
    dirtyFacts: Set[CompilerFactKey[?]],
    cache: FactCacheData,
    currentHashes: mutable.Map[CompilerFactKey[?], Long]
): Unit = {
  val sorted = topologicalSort(dirtyFacts, cache)
  for (key <- sorted) {
    val entry = cache.entries(key)
    val depsUnchanged = entry.directDependencyHashes.forall {
      case (depKey, expectedHash) =>
        currentHashes.getOrElse(depKey, expectedHash) == expectedHash
    }
    if (depsUnchanged) {
      // Cutoff: all inputs unchanged, fact is clean
      currentHashes(key) = entry.contentHash
    } else {
      // Must recompute
      val newFact = recompute(key)
      val newHash = computeHash(newFact)
      currentHashes(key) = newHash
      // If hash matches cached, dependents may still be clean
    }
  }
}
```

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
│  (implements CompilationProcess, wraps normal generation      │
│   with cache lookups and dependency tracking)                 │
│                                                              │
│  On getFact(key):                                            │
│   1. In-memory cache hit? → return                           │
│   2. Persistent cache hit + sentinel hashes valid? → return  │
│   3. Otherwise → generate with DependencyTrackingProcess     │
│                                                              │
│  ┌───────────────────┐  ┌────────────────────────┐           │
│  │ DependencyTracking │  │   CacheValidator       │           │
│  │ Process            │  │   (flat sentinel check  │           │
│  │ (records getFact   │  │    + propagation cutoff)│           │
│  │  calls per fact)   │  │                        │           │
│  └───────────────────┘  └────────────────────────┘           │
│                                                              │
│  ┌───────────────────────────────────────────────┐           │
│  │              FactCache                         │           │
│  │  (persistent storage on disk)                  │           │
│  │  - cache entries with sentinel hashes          │           │
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
| `UpdateTime.scala` | `compiler.cache` | `UpdateTime` fact, key, and processor |
| `CacheEntry.scala` | `compiler.cache` | Cache entry data structure |
| `FactCache.scala` | `compiler.cache` | Cache persistence (load/save) |
| `CacheValidator.scala` | `compiler.cache` | Flat validation + propagation cutoff |
| `DependencyTrackingProcess.scala` | `compiler.cache` | Records fact dependencies |
| `IncrementalFactGenerator.scala` | `compiler` | Cache-aware `CompilationProcess` |

### Modified Files

| File | Change |
|------|--------|
| `Compiler.scala` | Register `Initial`/`UpdateTime` processors, load/save cache |
| `CompilerPlugin.scala` | Add `ephemeralFactTypes()` method |
| `SourceContentReader.scala` | Add `getFactOrAbort(UpdateTime.Key(file))` call |
| `BasePlugin.scala` | Override `ephemeralFactTypes()` |

## 6. New Facts and Processors

### Initial

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

### UpdateTime

```scala
package com.vanillasource.eliot.eliotc.compiler.cache

import java.io.File
import com.vanillasource.eliot.eliotc.processor.*

case class UpdateTime(file: File, lastModified: Long) extends CompilerFact {
  override def key(): CompilerFactKey[UpdateTime] = UpdateTime.Key(file)
}

object UpdateTime {
  case class Key(file: File) extends CompilerFactKey[UpdateTime]
}
```

```scala
class UpdateTimeProcessor
    extends SingleKeyTypeProcessor[UpdateTime.Key] {
  override protected def generateFact(key: UpdateTime.Key): CompilerIO[Unit] =
    for {
      _ <- getFactOrAbort(Initial.Key)  // depend on Initial → always re-run
      mtime <- IO.blocking(key.file.lastModified()).to[CompilerIO]
      _ <- registerFactIfClear(UpdateTime(key.file, mtime))
    } yield ()
}
```

### Why This Works

- `Initial` changes every run → `UpdateTime` always re-runs (it depends on `Initial`)
- `UpdateTime` is a cheap `stat` call → negligible cost even for many files
- If file is unchanged → `UpdateTime` produces same `lastModified` → same hash → downstream is cached
- If file changed → `UpdateTime` produces different `lastModified` → different hash → downstream recomputes

The beauty is that this integrates naturally with the fact system. No special external fingerprinting mechanism needed — it's all facts and dependencies.

## 7. Dependency Tracking

During fact generation, we record which facts each generated fact depends on. This follows the existing `TrackedCompilationProcess` pattern from the visualization system.

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

### Sentinel Hash Propagation

After compilation, sentinel hashes are computed by propagating through the dependency graph:

```scala
def computeSentinelHashes(
    factKey: CompilerFactKey[?],
    directDeps: Map[CompilerFactKey[?], Set[CompilerFactKey[?]]],
    factHashes: Map[CompilerFactKey[?], Long],
    memo: mutable.Map[CompilerFactKey[?], Map[CompilerFactKey[?], Long]]
): Map[CompilerFactKey[?], Long] = {
  memo.getOrElseUpdate(factKey, {
    val deps = directDeps.getOrElse(factKey, Set.empty)
    if (deps.isEmpty) {
      // Leaf fact (no dependencies) — not a sentinel
      Map.empty
    } else if (deps.contains(Initial.Key)) {
      // This fact depends on Initial → it IS a sentinel
      Map(factKey -> factHashes(factKey))
    } else {
      // Union of all dependencies' sentinel hashes
      deps.flatMap(dep =>
        computeSentinelHashes(dep, directDeps, factHashes, memo)
      ).toMap
    }
  })
}
```

This runs once at cache save time. The result is stored per cache entry.

## 8. Cache Storage Format

### CacheEntry

```scala
case class CacheEntry(
    sentinelHashes: Map[CompilerFactKey[?], Long],
    directDependencyHashes: Map[CompilerFactKey[?], Long],
    contentHash: Long,
    serializedFact: Option[Array[Byte]]  // None for ephemeral facts
)
```

| Field | Purpose |
|-------|---------|
| `sentinelHashes` | Flat map of `UpdateTime` keys → hashes. For fast O(S) validation. |
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

No separate source fingerprints needed — file freshness is modeled as `UpdateTime` sentinel facts within the same system.

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
  │    ├─ Is key a sentinel (depends on Initial)?
  │    │    │
  │    │    Yes → always generate fresh
  │    │         (UpdateTime: stat the file)
  │    │
  │    ├─ All sentinelHashes match current? ──Yes──► deserialize & return
  │    │                                             (or regenerate if ephemeral)
  │    │
  │    └─ Some sentinel changed:
  │         Check directDependencyHashes
  │         against current dependency hashes
  │         │
  │         ├─ All match → CUTOFF: fact is clean
  │         │   despite sentinel change → return cached
  │         │
  │         └─ Some differ → generate fresh
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
      case Some(entry) if !isSentinel(entry) =>
        validateAndReturn(key, entry)
      case _ =>
        generateFresh(key)
    }

  private def validateAndReturn[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K,
      entry: CacheEntry
  ): IO[Option[V]] =
    for {
      // Ensure all referenced sentinels have been computed this run
      _              <- entry.sentinelHashes.keys.toList.traverse_(ensureSentinelComputed)
      sentinelHashes <- currentHashes.get
      sentinelsValid  = entry.sentinelHashes.forall { case (sk, sh) =>
                          sentinelHashes.get(sk).contains(sh)
                        }
      result         <- if (sentinelsValid) loadFromCache(key, entry)
                        else checkCutoffOrRegenerate(key, entry)
    } yield result

  /** Check if direct dependency hashes match — propagation cutoff. */
  private def checkCutoffOrRegenerate[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K,
      entry: CacheEntry
  ): IO[Option[V]] =
    for {
      hashes   <- currentHashes.get
      depsMatch = entry.directDependencyHashes.forall { case (dk, dh) =>
                    hashes.get(dk).contains(dh)
                  }
      result   <- if (depsMatch) {
                    // CUTOFF: direct inputs unchanged despite sentinel change
                    currentHashes.update(_.updated(key, entry.contentHash)) >>
                      loadFromCache(key, entry)
                  } else {
                    generateFresh(key)
                  }
    } yield result

  private def loadFromCache[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K,
      entry: CacheEntry
  ): IO[Option[V]] =
    entry.serializedFact match {
      case Some(bytes) =>
        val fact = deserialize(bytes).asInstanceOf[V]
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

### Sentinel Detection

A sentinel fact is one that depends directly on `Initial.Key`. This is determined from the cached dependency graph:

```scala
private def isSentinel(entry: CacheEntry): Boolean =
  entry.directDependencyHashes.contains(Initial.Key)
```

Sentinels are never served from cache — they're always regenerated (they're cheap, like `stat` calls).

## 10. Persistent vs Ephemeral Facts

### Rationale

Not all facts are worth serializing to disk:

| Fact Type | Category | Rationale |
|-----------|----------|-----------|
| `Initial` | **Never cached** | Always changes |
| `UpdateTime` | **Sentinel** | Always re-runs (stat call) |
| `SourceContent` | Ephemeral | Re-read from disk if needed |
| `PathScan` | Ephemeral | Cheap to recompute |
| `SourceTokens` | Ephemeral | Tokenization is fast |
| `SourceAST` | Ephemeral | Parsing is fast |
| `CoreAST` | Ephemeral | Pure transformation, fast |
| `ModuleNames` | Ephemeral | Simple extraction |
| `ModuleValue` | Ephemeral | Simple extraction |
| `UnifiedModuleNames` | **Persistent** | Cross-file checkpoint |
| `UnifiedModuleValue` | **Persistent** | Cross-file checkpoint |
| `ResolvedValue` | **Persistent** | Name resolution |
| `TypeCheckedValue` | **Persistent** | Type checking is expensive |
| `MonomorphicValue` | **Persistent** | Monomorphization is expensive |
| `UsedNames` | **Persistent** | Reachability analysis |
| `UncurriedValue` | **Persistent** | Checkpoint before codegen |
| `GeneratedModule` | **Persistent** | Bytecode generation is expensive |

### How Ephemeral Facts Work

For ephemeral facts, the cache stores the `sentinelHashes`, `directDependencyHashes`, and `contentHash` — but NOT the `serializedFact` (it's `None`).

- **Nothing changed**: Sentinel hashes match → the persistent facts downstream are loaded from cache. Ephemeral facts are never needed (they're intermediaries).
- **Something changed**: Ephemeral facts in the dirty subtree are regenerated. This is fast (they're cheap computations). Their new hashes participate in propagation cutoff.

### Plugin Registration

Plugins declare ephemeral fact types via a new method on `CompilerPlugin`:

```scala
// Addition to CompilerPlugin trait:
def ephemeralFactTypes(): Set[Class[? <: CompilerFactKey[?]]] = Set.empty
```

`BasePlugin` overrides:

```scala
override def ephemeralFactTypes(): Set[Class[? <: CompilerFactKey[?]]] = Set(
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

### Modified Compilation Flow

```scala
private def runWithConfiguration(
    configuration: Configuration,
    plugins: Seq[CompilerPlugin]
): IO[Unit] =
  plugins.find(_.isSelectedBy(configuration)) match {
    case None => User.compilerGlobalError("No target plugin selected.")
    case Some(targetPlugin) =>
      for {
        activatedPlugins  <- // ... as before ...
        newConfiguration  <- // ... as before ...

        // Collect ephemeral fact types from all plugins
        ephemeralTypes = activatedPlugins.flatMap(_.ephemeralFactTypes()).toSet

        // Collect processors, prepending Initial and UpdateTime processors
        startTime     <- IO.realTimeInstant
        processor     <- activatedPlugins.traverse_(_.initialize(newConfiguration)).runS(
                           SequentialCompilerProcessors(Seq(
                             InitialProcessor(startTime),
                             UpdateTimeProcessor()
                             // NullProcessor() replaced by these
                           ))
                         )

        // Wrap with visualization tracking (as before)
        tracker       <- FactVisualizationTracker.create()
        wrappedProcessors = processor.wrapWith(wrapProcessor(_, tracker))

        // Load persistent cache
        targetPath     = newConfiguration.get(targetPathKey).get
        cacheData     <- FactCache.load(targetPath)

        // Create incremental fact generator
        generator     <- IncrementalFactGenerator.create(
                           wrappedProcessors, cacheData, ephemeralTypes
                         )

        // Run compilation
        _             <- targetPlugin.run(newConfiguration, generator)

        // Save updated cache
        newCacheData  <- generator.buildCacheData()
        _             <- FactCache.save(targetPath, newCacheData)

        // Print errors, generate visualization (as before)
        errors        <- generator.currentErrors()
        _             <- errors.traverse_(_.print())
        _             <- tracker.generateVisualization(...)
      } yield ()
  }
```

### Fast Path

The fast path emerges naturally from the fact system:

1. Plugin calls `getFact(GenerateExecutableJar.Key(main))`
2. `IncrementalFactGenerator` finds it in cache, checks sentinel hashes
3. Sentinel hashes reference `UpdateTime` facts → stat all source files
4. All mtimes match → all sentinel hashes match → return cached fact
5. **Zero file reads, zero deserialization of intermediate facts, zero computation**

The only work done is: load cache file + N stat calls + deserialize the one requested fact.

## 12. Building the Cache After Compilation

After compilation completes, the `IncrementalFactGenerator` builds a new `FactCacheData`:

```scala
def buildCacheData(): IO[FactCacheData] =
  for {
    allFacts  <- collectAllFacts()
    deps      <- directDependencies.get
    hashes    <- currentHashes.get
    entries    = allFacts.flatMap { case (key, fact) =>
                   // Skip Initial — never cached
                   if (key == Initial.Key) None
                   else {
                     val serialized   = serialize(fact)
                     val contentHash  = MurmurHash3.bytesHash(serialized).toLong
                     val isPersistent = !ephemeralTypes.contains(key.getClass)
                     val factDeps     = deps.getOrElse(key, Set.empty)
                     val depHashes    = factDeps.flatMap(d => hashes.get(d).map(d -> _)).toMap
                     val sentinels    = computeSentinelHashes(key, deps, hashes)
                     Some(key -> CacheEntry(
                       sentinelHashes = sentinels,
                       directDependencyHashes = depHashes,
                       contentHash = contentHash,
                       serializedFact = if (isPersistent) Some(serialized) else None
                     ))
                   }
                 }
  } yield FactCacheData(
    version = FactCache.CACHE_VERSION,
    entries = entries.toMap
  )
```

## 13. Edge Cases

### New Source Files

Not in cache → `UpdateTime(file)` has no cached entry → generated fresh → `SourceContent(file)` generated fresh → all downstream fresh. Cache updated to include the new file on save.

### Deleted Source Files

`UpdateTime(file)` runs → file doesn't exist → `lastModified` returns 0 → hash differs from cached → downstream invalidated. `SourceContentReader` silently ignores missing files (no fact produced). Facts that depended on the deleted file fail to generate (as today).

### Synthetic Sources (JvmProgramGenerator)

`JvmProgramGenerator` calls `addSource(file, content)` to register a synthetic `SourceContent` directly. This fact doesn't depend on `UpdateTime` — it depends on the processor's inputs (`UsedNames`, etc.). It's tracked like any derived fact, with its sentinel hashes being the union of its dependencies' sentinels.

### Cache Corruption

`FactCache.load` catches all exceptions → corrupted cache = no cache → full recompilation.

### Side-Effecting Processors (JAR Writing)

`JvmProgramGenerator` writes JAR files during fact generation. If the fact is cached and valid, the JAR from the previous run is still valid — no regeneration, no writing. If the JAR is manually deleted but the cache thinks it's valid, the user must clean the cache (`rm -rf target/`).

## 14. Required Changes Summary

### New Files (eliotc module)

All under `eliotc/src/com/vanillasource/eliot/eliotc/`:

1. **`compiler/cache/Initial.scala`** — `Initial` fact + `InitialProcessor`
2. **`compiler/cache/UpdateTime.scala`** — `UpdateTime` fact + `UpdateTimeProcessor`
3. **`compiler/cache/CacheEntry.scala`** — `CacheEntry` and `FactCacheData`
4. **`compiler/cache/FactCache.scala`** — Cache persistence (load/save)
5. **`compiler/cache/CacheValidator.scala`** — Validation + propagation cutoff
6. **`compiler/cache/DependencyTrackingProcess.scala`** — Dependency recording
7. **`compiler/IncrementalFactGenerator.scala`** — Cache-aware `CompilationProcess`

### Modified Files

| File | Change |
|------|--------|
| `eliotc/.../compiler/Compiler.scala` | Register `Initial`/`UpdateTime` processors, load/save cache, use `IncrementalFactGenerator` |
| `eliotc/.../plugin/CompilerPlugin.scala` | Add `ephemeralFactTypes()` method (default: empty set) |
| `base/.../source/content/SourceContentReader.scala` | Add `getFactOrAbort(UpdateTime.Key(key.file))` before reading file |
| `base/.../plugin/BasePlugin.scala` | Override `ephemeralFactTypes()` to declare cheap fact types |

### No Changes Required

- No processor implementations (except `SourceContentReader`) need modification
- No fact types need modification (already `Serializable` via case class)
- No changes to `CompilerIO` monad
- No changes to `CompilerFactKey` or `CompilerFact` traits
- JVM module unchanged

## 15. Performance Characteristics

| Scenario | Work Done |
|----------|-----------|
| Nothing changed | Load cache + N `stat` calls + deserialize requested facts. Microseconds to milliseconds. |
| One file changed, no signature changes | Re-stat all files + re-read changed file + recompute its pipeline + hash comparisons at each level. Cutoff stops at first unchanged signature. |
| One file changed, signature changed | Same as above but propagation continues further. Cutoff stops when downstream fact content matches. |
| Everything changed | Full recompilation + cache save overhead. |
| First compilation (no cache) | Full recompilation + dependency tracking + cache save. Slightly slower due to tracking overhead. |

## 16. Future Enhancements

1. **Output file existence checks**: Verify that side-effect outputs (JAR files) still exist before declaring cache valid
2. **Parallel sentinel computation**: Run `UpdateTime` facts concurrently for faster startup
3. **Partial cache loading**: Deserialize only requested facts, not the entire cache file
4. **Fine-grained fact hashing**: Hash "signature" and "body" parts of facts separately for better propagation cutoff
5. **CompilerIO purity enforcement**: Restrict the `CompilerIO` monad to prevent processors from performing arbitrary IO, formally guaranteeing the pure function property that the cache relies on
6. **File watching mode**: Use filesystem watchers for instant change detection in a long-running compiler daemon
