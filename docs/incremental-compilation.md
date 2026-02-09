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

## 2. Architecture Overview

The incremental compilation system introduces four new components in the `eliotc` module, plus modifications to the existing `Compiler` and `FactGenerator`:

```
┌─────────────────────────────────────────────────────────────┐
│                     Compiler.scala                          │
│  (modified to load/save cache around compilation)           │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│              IncrementalFactGenerator                        │
│  (new: wraps FactGenerator with cache-awareness)            │
│                                                             │
│  ┌─────────────────┐  ┌──────────────────┐                  │
│  │ CacheValidator   │  │ DependencyTracker│                  │
│  │ (validates cache │  │ (records getFact │                  │
│  │  entries against │  │  calls per fact  │                  │
│  │  source changes) │  │  generation)     │                  │
│  └────────┬────────┘  └────────┬─────────┘                  │
│           │                    │                             │
│           ▼                    ▼                             │
│  ┌─────────────────────────────────────────┐                │
│  │             FactCache                    │                │
│  │  (persistent storage on disk)            │                │
│  │  - source fingerprints                   │                │
│  │  - dependency graph                      │                │
│  │  - content hashes                        │                │
│  │  - serialized facts (persistent only)    │                │
│  └─────────────────────────────────────────┘                │
└─────────────────────────────────────────────────────────────┘
```

### New Files (all in `eliotc` module)

| File | Package | Purpose |
|------|---------|---------|
| `SourceFingerprint.scala` | `compiler.cache` | Source file mtime + content hash |
| `FactCache.scala` | `compiler.cache` | Persistent cache storage (load/save) |
| `CacheValidator.scala` | `compiler.cache` | Validates cached facts against source changes |
| `DependencyTracker.scala` | `compiler.cache` | Tracks fact-to-fact dependencies during generation |
| `IncrementalFactGenerator.scala` | `compiler` | Cache-aware fact generator wrapping `FactGenerator` |

### Modified Files

| File | Change |
|------|--------|
| `Compiler.scala` | Load cache before compilation, save after |
| `CompilerPlugin.scala` | Add `ephemeralFactTypes()` method |
| `FactGenerator.scala` | Minor: expose pre-population of facts |

## 3. Source Fingerprinting

Source files are the roots of the entire fact dependency graph. Every fact ultimately derives from one or more `SourceContent` facts, which are read from disk by `SourceContentReader`.

### SourceFingerprint

```scala
package com.vanillasource.eliot.eliotc.compiler.cache

import java.io.File
import java.nio.file.Files

case class SourceFingerprint(
    file: File,
    lastModifiedTime: Long,
    contentHash: Long
)

object SourceFingerprint {
  def compute(file: File): Option[SourceFingerprint] =
    if (file.exists()) {
      val mtime = file.lastModified()
      val hash  = computeContentHash(file)
      Some(SourceFingerprint(file, mtime, hash))
    } else None

  /** Fast check: has the file changed since the fingerprint was taken? */
  def hasChanged(fingerprint: SourceFingerprint): Boolean =
    !fingerprint.file.exists() ||
      fingerprint.file.lastModified() != fingerprint.lastModifiedTime

  /** Accurate check: read file and compare content hash.
    * Only called when mtime differs (file was touched).
    */
  def hasContentChanged(fingerprint: SourceFingerprint): Boolean =
    !fingerprint.file.exists() ||
      computeContentHash(fingerprint.file) != fingerprint.contentHash

  private def computeContentHash(file: File): Long =
    scala.util.hashing.MurmurHash3.bytesHash(
      Files.readAllBytes(file.toPath)
    ).toLong
}
```

### Change Detection Protocol

1. **Fast path** (`stat` only, no file reads): Check `lastModifiedTime` for all fingerprinted files. If ALL match → nothing changed → skip compilation entirely.
2. **Mtime-differs path**: For files where mtime differs, read the file and compare `contentHash`. If hash matches → file was touched but content is identical → treat as unchanged.
3. **Content-changed path**: File genuinely changed → proceed with dependency-based invalidation.

This two-level check ensures the common case (nothing changed) is extremely fast — just one `stat` syscall per source file.

## 4. Dependency Tracking

During compilation, we need to record which facts each generated fact depends on. This is the foundation for invalidation: if any dependency of fact F has changed, F might need recomputation.

### DependencyTracker

The dependency tracker follows the existing `TrackedCompilationProcess` pattern from the visualization system. It wraps `CompilationProcess` to intercept `getFact` calls.

```scala
package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.{IO, Ref}
import com.vanillasource.eliot.eliotc.processor.{
  CompilationProcess, CompilerFact, CompilerFactKey
}

/** Wraps a CompilationProcess to record all getFact calls made during
  * a fact generation. Each generation context gets its own instance.
  */
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

### Integration with FactGenerator

The key integration point is in `FactGenerator.getFact`, where generation is launched:

```scala
// Current code (FactGenerator.scala line 19-27):
generator.generate(key).run(this).runS(Chain.empty)...

// With dependency tracking:
// 1. Create a fresh dependency set for this generation
// 2. Wrap `this` with DependencyTrackingProcess
// 3. Run the generator with the wrapped process
// 4. After completion, record the dependencies for key
```

Each fact generation runs in its own fiber (via `.start`), so each gets its own `DependencyTrackingProcess` instance. There are no concurrency issues — the `Ref` is fiber-local in practice (only one fiber writes to it).

### Dependency Graph Structure

After compilation, the dependency graph looks like:

```
SourceContent.Key(Foo.els)
  ← SourceTokens.Key(Foo.els)
    ← SourceAST.Key(Foo.els)
      ← CoreAST.Key(Foo.els)
        ← ModuleNames.Key(Foo.els)
        ← ModuleValue.Key(Foo.els, Foo.bar)
          ← UnifiedModuleValue.Key(Foo.bar)  [also depends on PathScan]
            ← ResolvedValue.Key(Foo.bar)
              ← TypeCheckedValue.Key(Foo.bar)
                ← MonomorphicValue.Key(Foo.bar, [Int])
```

Cross-file dependencies appear naturally. For example, if `Foo.bar` calls `Baz.qux`, then:
```
ResolvedValue.Key(Foo.bar) depends on UnifiedModuleValue.Key(Baz.qux)
```

This is captured automatically by the dependency tracker — when `ValueResolver` calls `getFact(UnifiedModuleValue.Key(Baz.qux))` while generating `ResolvedValue.Key(Foo.bar)`, the dependency is recorded.

## 5. Fact Caching

### Serialization Strategy

All ELIOT compiler facts are Scala 3 case classes. In Scala 3, case classes automatically extend `Product with Serializable`. This means **Java serialization works out of the box** with zero changes to existing fact types.

The serialization approach:

1. **Serialize facts** using `ObjectOutputStream` to `Array[Byte]`
2. **Deserialize facts** using `ObjectInputStream` from `Array[Byte]`
3. **Content hashing**: Compute `MurmurHash3.bytesHash` of the serialized bytes

Java serialization is not the fastest, but it only runs once (at cache save/load), not in the hot compilation loop. For a compiler cache, simplicity and zero-change compatibility outweigh raw speed.

### Cache Compatibility

Class changes between compiler versions would break Java deserialization. To handle this:

1. Store a **cache version number** derived from the compiler version
2. On load, if the version doesn't match → discard entire cache, compile from scratch
3. This is safe: the worst case is a full recompilation, not incorrect results

### Persistent vs Ephemeral Facts

Not all facts are worth persisting to disk. Some are cheap to recompute from their inputs:

| Fact Type | Category | Rationale |
|-----------|----------|-----------|
| `SourceContent` | **Source** | Never cached; always read from disk |
| `PathScan` | **Source** | Never cached; always scanned |
| `SourceTokens` | Ephemeral | Tokenization is fast |
| `SourceAST` | Ephemeral | Parsing is fast |
| `CoreAST` | Ephemeral | Pure transformation, fast |
| `ModuleNames` | Ephemeral | Simple extraction, fast |
| `ModuleValue` | Ephemeral | Simple extraction, fast |
| `UnifiedModuleNames` | Persistent | Cross-file, useful checkpoint |
| `UnifiedModuleValue` | Persistent | Cross-file, useful checkpoint |
| `ResolvedValue` | Persistent | Name resolution, moderate cost |
| `TypeCheckedValue` | **Persistent** | Type checking is expensive |
| `MonomorphicValue` | **Persistent** | Monomorphization is expensive |
| `UsedNames` | Persistent | Reachability analysis |
| `UncurriedValue` | Persistent | Checkpoint before codegen |
| `GeneratedModule` | **Persistent** | Bytecode generation is expensive |

For ephemeral facts, the cache stores **only the content hash and dependencies**, not the serialized data. This saves disk space while still allowing validation: if an ephemeral fact's inputs are unchanged, its hash is known to be the same, so its dependents can be validated without recomputing the ephemeral fact.

### Ephemeral Fact Registration

Plugins declare which of their fact types are ephemeral during initialization. This is done via a new method on `CompilerPlugin`:

```scala
// Addition to CompilerPlugin trait:
def ephemeralFactTypes(): Set[Class[? <: CompilerFactKey[?]]] = Set.empty
```

The `BasePlugin` would override this:

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

## 6. Cache Storage Format

### FactCache

The cache is stored as a single file using Java serialization:

```scala
package com.vanillasource.eliot.eliotc.compiler.cache

import java.io.*
import java.nio.file.{Files, Path}

case class CacheEntry(
    dependencies: Set[CompilerFactKey[?]],
    contentHash: Long,
    serializedFact: Option[Array[Byte]]  // None for ephemeral facts
)

case class FactCacheData(
    version: Int,
    sourceFingerprints: Map[File, SourceFingerprint],
    entries: Map[CompilerFactKey[?], CacheEntry]
)

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
    }.handleError(_ => None)  // Corrupted cache → treat as no cache

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

### Cache Location

The cache file is stored in the target directory (from `Compiler.targetPathKey`), e.g., `target/.eliot-cache`. This keeps it alongside other build artifacts and makes it easy to clean (delete target directory).

## 7. Cache Validation Algorithm

This is the core of the incremental compilation system. The validator determines which cached facts are still valid and which need recomputation.

### CacheValidator

```scala
package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.{IO, Ref}
import com.vanillasource.eliot.eliotc.processor.CompilerFactKey

enum ValidationState {
  case Valid        // Fact is confirmed unchanged
  case Invalid      // Fact needs recomputation
  case Unknown      // Not yet validated
}

final class CacheValidator(
    cache: FactCacheData,
    validationStates: Ref[IO, Map[CompilerFactKey[?], ValidationState]]
) {

  /** Validate whether a cached fact is still valid.
    * Returns true if the fact can be used from cache.
    */
  def isValid(key: CompilerFactKey[?]): IO[Boolean] =
    validationStates.get.map(_.get(key)).flatMap {
      case Some(ValidationState.Valid)   => IO.pure(true)
      case Some(ValidationState.Invalid) => IO.pure(false)
      case _                             => computeValidity(key)
    }

  private def computeValidity(key: CompilerFactKey[?]): IO[Boolean] =
    cache.entries.get(key) match {
      case None        =>
        // Not in cache at all
        markAndReturn(key, ValidationState.Invalid, false)
      case Some(entry) =>
        // Mark as Invalid first (handles cycles)
        validationStates.update(_.updated(key, ValidationState.Invalid)) >>
          entry.dependencies
            .toList
            .traverse(dep => isValid(dep))
            .map(_.forall(identity))
            .flatMap {
              case true  => markAndReturn(key, ValidationState.Valid, true)
              case false => markAndReturn(key, ValidationState.Invalid, false)
            }
    }

  private def markAndReturn(
      key: CompilerFactKey[?],
      state: ValidationState,
      result: Boolean
  ): IO[Boolean] =
    validationStates.update(_.updated(key, state)).as(result)
}
```

### Source Fact Validation

Source facts (`SourceContent.Key`) are validated differently — by checking the source file fingerprint:

```scala
private def computeValidity(key: CompilerFactKey[?]): IO[Boolean] =
  key match {
    case sourceKey: SourceContent.Key =>
      validateSourceFact(sourceKey)
    case _ =>
      validateDerivedFact(key)
  }

private def validateSourceFact(key: SourceContent.Key): IO[Boolean] = {
  val fingerprint = cache.sourceFingerprints.get(key.file)
  fingerprint match {
    case None     => markAndReturn(key, ValidationState.Invalid, false)
    case Some(fp) =>
      if (!SourceFingerprint.hasChanged(fp)) {
        // Mtime matches → definitely unchanged
        markAndReturn(key, ValidationState.Valid, true)
      } else if (!SourceFingerprint.hasContentChanged(fp)) {
        // Mtime differs but content hash matches → file was touched, not changed
        markAndReturn(key, ValidationState.Valid, true)
      } else {
        markAndReturn(key, ValidationState.Invalid, false)
      }
  }
}
```

**Important**: The validator needs to know about `SourceContent.Key` specifically. This is the ONE place where the mechanism touches a concrete fact type. However, this can be made generic by introducing a marker: facts whose keys contain a `File` and have no dependencies in the cache are "source facts." Or alternatively, source facts are those with an empty dependency set in the cache.

A cleaner generic approach: source facts are simply facts that have **no recorded dependencies** in the cache. The `SourceContentReader` doesn't call `getFact` on anything — it reads from the filesystem. So `SourceContent` facts will have an empty dependency set. For facts with empty dependencies, validation checks the source fingerprint associated with the fact key.

```scala
private def computeValidity(key: CompilerFactKey[?]): IO[Boolean] =
  cache.entries.get(key) match {
    case None        => markAndReturn(key, ValidationState.Invalid, false)
    case Some(entry) =>
      if (entry.dependencies.isEmpty) {
        // Source fact: validate via fingerprint
        validateViaFingerprint(key)
      } else {
        // Derived fact: validate via dependencies
        validateViaDependencies(key, entry)
      }
  }
```

### Full Validation Flow

```
Start: Plugin requests getFact(GenerateExecutableJar.Key(main))
  │
  ▼
Is GenerateExecutableJar.Key(main) in cache? ──No──► Generate normally
  │
  Yes
  │
  ▼
Validate GenerateExecutableJar.Key(main):
  │
  ├── Dependency: UsedNames.Key(main)
  │     ├── Dependency: MonomorphicValue.Key(main, [])
  │     │     ├── Dependency: TypeCheckedValue.Key(main)
  │     │     │     ├── Dependency: ResolvedValue.Key(main)
  │     │     │     │     ├── Dependency: UnifiedModuleValue.Key(main)
  │     │     │     │     │     ├── Dependency: ModuleValue.Key(main.els, main)
  │     │     │     │     │     │     ├── Dependency: CoreAST.Key(main.els)
  │     │     │     │     │     │     │     ├── ... ← SourceContent.Key(main.els)
  │     │     │     │     │     │     │              │
  │     │     │     │     │     │     │              ▼
  │     │     │     │     │     │     │         Check fingerprint
  │     │     │     │     │     │     │         for main.els
  ...   ...   ...   ...   ...   ...   ...
  │
  ▼
All dependencies valid? ──Yes──► Return cached fact (FAST PATH)
  │
  No (some source file changed)
  │
  ▼
Generate only the invalid subtree
```

## 8. The IncrementalFactGenerator

This is the main component that ties everything together. It wraps the normal `FactGenerator` with cache-awareness.

### Design

```scala
package com.vanillasource.eliot.eliotc.compiler

import cats.data.Chain
import cats.effect.{Deferred, IO, Ref}
import com.vanillasource.eliot.eliotc.compiler.cache.*
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.processor.*

final class IncrementalFactGenerator(
    generator: CompilerProcessor,
    cache: Option[FactCacheData],
    validator: CacheValidator,
    ephemeralTypes: Set[Class[?]],
    errors: Ref[IO, Chain[CompilerError]],
    facts: Ref[IO, Map[CompilerFactKey[?], Deferred[IO, Option[CompilerFact]]]],
    dependencies: Ref[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]],
    sourceFingerprints: Ref[IO, Map[java.io.File, SourceFingerprint]]
) extends CompilationProcess {

  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K
  ): IO[Option[V]] =
    for {
      // Check if already computed in this run
      existing <- facts.get.map(_.get(key))
      result   <- existing match {
                    case Some(deferred) => deferred.get.map(_.map(_.asInstanceOf[V]))
                    case None           => getFactWithCache(key)
                  }
    } yield result

  private def getFactWithCache[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K
  ): IO[Option[V]] =
    for {
      // Try to load from cache
      cachedValid <- validator.isValid(key)
      result      <- if (cachedValid) loadFromCache(key)
                     else generateFresh(key)
    } yield result

  /** Load a cached fact. For persistent facts, deserialize.
    * For ephemeral facts, must regenerate (but we know inputs are valid).
    */
  private def loadFromCache[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K
  ): IO[Option[V]] =
    cache.flatMap(_.entries.get(key)) match {
      case Some(entry) if entry.serializedFact.isDefined =>
        // Persistent fact: deserialize from cache
        val fact = deserialize(entry.serializedFact.get).asInstanceOf[V]
        registerInMemory(key, Some(fact)).as(Some(fact))
      case _ =>
        // Ephemeral or missing serialized data: regenerate
        // (inputs are cached/valid, so this is fast)
        generateFresh(key)
    }

  /** Generate a fact normally, with dependency tracking. */
  private def generateFresh[V <: CompilerFact, K <: CompilerFactKey[V]](
      key: K
  ): IO[Option[V]] =
    for {
      modifyResult <- modifyAtomicallyFor(key)
      _            <- (
                        for {
                          depTracker <- Ref.of[IO, Set[CompilerFactKey[?]]](Set.empty)
                          trackingProcess = new DependencyTrackingProcess(this, depTracker)
                          _ <- generator.generate(key)
                                 .run(trackingProcess)
                                 .runS(Chain.empty)
                                 .fold(identity, identity)
                                 .flatMap(es => errors.update(_ ++ es))
                          deps <- depTracker.get
                          _    <- dependencies.update(_.updated(key, deps))
                        } yield ()
                      ).recoverWith(_ => modifyResult._1.complete(None).void)
                        .start
                        .whenA(modifyResult._2)
      result       <- modifyResult._1.get
    } yield result.map(_.asInstanceOf[V])

  // ... (registerFact, modifyAtomicallyFor similar to FactGenerator)

  override def registerFact(fact: CompilerFact): IO[Unit] =
    for {
      _ <- recordSourceFingerprint(fact)
      _ <- modifyAtomicallyFor(fact.key())
             .flatMap(_._1.complete(Some(fact)).void)
    } yield ()

  /** For SourceContent facts, record the fingerprint. */
  private def recordSourceFingerprint(fact: CompilerFact): IO[Unit] =
    fact match {
      case sc: CompilerFact if isSourceContentFact(fact) =>
        extractFile(fact).flatMap { file =>
          SourceFingerprint.compute(file).traverse_ { fp =>
            sourceFingerprints.update(_.updated(file, fp))
          }
        }
      case _ => IO.unit
    }
}
```

### Hash-Based Propagation Cutoff

When a fact is recomputed (because a dependency changed), its content hash is compared with the cached hash. If they match, the fact's dependents are still valid despite the dependency change.

This is implemented in the validator: after recomputation, update the validation state based on hash comparison.

```scala
/** Called after a fact is regenerated. Checks if the new fact
  * matches the cached hash. If so, marks it as Valid (content
  * didn't change despite input changes).
  */
def recordRecomputed(
    key: CompilerFactKey[?],
    newFact: CompilerFact
): IO[Boolean] = {
  val newHash = computeContentHash(newFact)
  val oldHash = cache.entries.get(key).map(_.contentHash)

  if (oldHash.contains(newHash)) {
    // Content unchanged despite input change → mark valid
    // This stops propagation to dependents
    validationStates.update(_.updated(key, ValidationState.Valid))
      .as(true)
  } else {
    // Content actually changed
    validationStates.update(_.updated(key, ValidationState.Invalid))
      .as(false)
  }
}
```

This cutoff is critical for performance. For example, if `Foo.els` changes but the signature of `Foo.bar` remains the same:

1. `SourceContent.Key(Foo.els)` → Invalid (file changed)
2. `SourceTokens.Key(Foo.els)` → Invalid → recomputed → hash changed
3. `SourceAST.Key(Foo.els)` → Invalid → recomputed → hash changed
4. `CoreAST.Key(Foo.els)` → Invalid → recomputed → hash changed
5. `ModuleValue.Key(Foo.els, Foo.bar)` → Invalid → recomputed → hash changed (body differs)
6. `ResolvedValue.Key(Foo.bar)` → Invalid → recomputed → hash changed
7. `TypeCheckedValue.Key(Foo.bar)` → Invalid → recomputed → hash changed (body differs)
8. Functions calling `Foo.bar` → their `TypeCheckedValue` recomputed → **hash matches** (only used signature, which is unchanged) → **propagation stops**

## 9. Integration with Compiler.scala

### Modified Compilation Flow

```scala
// In Compiler.runWithConfiguration:

for {
  // ... plugin selection, configuration, processor initialization ...

  // Load incremental cache
  targetPath <- IO.pure(newConfiguration.get(targetPathKey).get)
  cacheData  <- FactCache.load(targetPath)

  // Fast path: check if all source files unchanged
  allUnchanged <- cacheData.traverse(checkAllSourcesUnchanged)

  _ <- if (allUnchanged.contains(true)) {
    // FAST PATH: Nothing changed. Skip compilation entirely.
    info[IO]("All sources unchanged, skipping compilation.")
  } else {
    for {
      // Create cache validator
      validator <- CacheValidator.create(cacheData)

      // Collect ephemeral fact types from all plugins
      ephemeralTypes = activatedPlugins
        .flatMap(_.ephemeralFactTypes())
        .toSet

      // Create incremental fact generator
      generator <- IncrementalFactGenerator.create(
        wrappedProcessors, cacheData, validator, ephemeralTypes
      )

      // Run compilation
      _ <- targetPlugin.run(newConfiguration, generator)

      // Save updated cache
      newCacheData <- generator.buildCacheData()
      _            <- FactCache.save(targetPath, newCacheData)

      // Print errors
      errors <- generator.currentErrors()
      _      <- errors.traverse_(_.print())
    } yield ()
  }
} yield ()
```

### Fast Path Detail

The "all sources unchanged" check is just:

```scala
def checkAllSourcesUnchanged(cache: FactCacheData): IO[Boolean] =
  IO.blocking {
    cache.sourceFingerprints.values.forall { fp =>
      fp.file.exists() && fp.file.lastModified() == fp.lastModifiedTime
    }
  }
```

This is a pure filesystem `stat` loop. No file reads, no deserialization, no computation. For a project with 100 source files, this takes microseconds.

## 10. Building the Cache After Compilation

After compilation completes, the `IncrementalFactGenerator` builds a `FactCacheData` from:

1. **Source fingerprints**: Collected during compilation from `SourceContent` facts
2. **Dependency graph**: Collected by `DependencyTracker` during each fact generation
3. **Content hashes**: Computed from each fact's serialized form
4. **Serialized facts**: For persistent (non-ephemeral) facts only

```scala
def buildCacheData(): IO[FactCacheData] =
  for {
    currentFacts <- collectAllFacts()
    deps         <- dependencies.get
    fingerprints <- sourceFingerprints.get
    entries       = currentFacts.map { case (key, fact) =>
                      val serialized  = serialize(fact)
                      val hash        = MurmurHash3.bytesHash(serialized).toLong
                      val isPersistent = !ephemeralTypes.contains(key.getClass)
                      key -> CacheEntry(
                        dependencies = deps.getOrElse(key, Set.empty),
                        contentHash = hash,
                        serializedFact = if (isPersistent) Some(serialized) else None
                      )
                    }
  } yield FactCacheData(
    version = FactCache.CACHE_VERSION,
    sourceFingerprints = fingerprints,
    entries = entries
  )
```

## 11. Handling Edge Cases

### New Source Files

A source file not in the cache → no fingerprint → `SourceContent.Key(file)` is marked invalid → generated normally. All downstream facts are also new (not in cache) → generated normally. Cache is updated to include the new file.

### Deleted Source Files

A source file in the cache but no longer exists → fingerprint check fails → `SourceContent.Key(file)` marked invalid. The `SourceContentReader` silently ignores missing files (returns no fact). All facts depending on it will fail to generate (as they do today).

On cache save, the deleted file's fingerprint is not included (since no `SourceContent` fact was registered). Old cache entries for facts from the deleted file are naturally evicted.

### New Dependencies (Code Change Creates New Import)

If file A is edited to add an import of B, the dependency graph changes: `ResolvedValue.Key(A.foo)` now depends on `UnifiedModuleValue.Key(B.bar)`. Since A changed, A's facts are recomputed. The new dependency is recorded during recomputation. No special handling needed.

### Cache Corruption

If the cache file is corrupted or can't be deserialized → treated as "no cache" → full recompilation. The `FactCache.load` method catches all exceptions.

### Side-Effecting Processors (JvmProgramGenerator)

`JvmProgramGenerator` writes JAR files as a side effect during fact generation. If its fact (`GenerateExecutableJar`) is cached and valid:

- The cached fact is returned → the JAR from the previous run is still valid
- No regeneration → no JAR writing → fast

If the JAR file is manually deleted but the fact is cached:
- The compiler thinks everything is fine (fact is valid)
- The JAR is missing → user must clean the cache

This is acceptable for an initial implementation. A future enhancement could add output file existence checks.

### Concurrent Compilation

The cache file is read at startup and written after compilation. If two compilations run concurrently on the same target directory, they might overwrite each other's cache. This is acceptable — the worst case is a cache miss on the next run.

## 12. Handling SourceContent Registration by Non-Readers

The `JvmProgramGenerator` calls `addSource(sourceDir.resolve("main.els").toFile, content)` to register a synthetic source file. This is a `SourceContent` fact registered directly, not by reading from disk.

For the cache:
- The synthetic `SourceContent` fact has no file on disk → no fingerprint
- It's generated by a processor, so it has dependencies (on the processor's inputs)
- It can be tracked like any other derived fact, with its own dependency set
- Validation checks dependencies, not the (non-existent) file

This requires the fingerprint-based validation to be triggered only for facts with empty dependency sets that correspond to actual files, not for synthetically registered source content.

## 13. Required Changes Summary

### New Files (eliotc module)

1. **`eliotc/src/.../compiler/cache/SourceFingerprint.scala`** — Source file fingerprinting
2. **`eliotc/src/.../compiler/cache/FactCache.scala`** — Cache persistence (load/save)
3. **`eliotc/src/.../compiler/cache/CacheValidator.scala`** — Validation logic
4. **`eliotc/src/.../compiler/cache/DependencyTrackingProcess.scala`** — Dependency recording
5. **`eliotc/src/.../compiler/IncrementalFactGenerator.scala`** — Cache-aware fact generator

### Modified Files

| File | Change |
|------|--------|
| `eliotc/.../compiler/Compiler.scala` | Load/save cache, fast-path check, use `IncrementalFactGenerator` |
| `eliotc/.../plugin/CompilerPlugin.scala` | Add `ephemeralFactTypes()` method (default: empty set) |
| `base/.../plugin/BasePlugin.scala` | Override `ephemeralFactTypes()` to exclude cheap fact types |

### No Changes Required

- No processor implementations need modification
- No fact types need modification (already `Serializable` via case class)
- No changes to `CompilerIO` monad
- No changes to `CompilerFactKey` or `CompilerFact` traits
- JVM module unchanged

## 14. Performance Characteristics

| Scenario | Cost |
|----------|------|
| Nothing changed | N × `stat` syscalls (N = number of source files). Microseconds. |
| One file changed, no signature changes | Recompute pipeline for that file + hash comparison at each stage. Propagation stops at first unchanged checkpoint. |
| One file changed, signature changed | Recompute pipeline for that file + all dependents. Propagation stops when downstream hashes match. |
| Everything changed | Full recompilation (same as current). Plus cache save overhead. |
| First compilation (no cache) | Full recompilation + cache save. Slightly slower than current due to dependency tracking and serialization overhead. |

## 15. Future Enhancements

1. **Output file existence checks**: Validate that side-effect outputs (JAR files) still exist
2. **Parallel cache validation**: Validate independent subtrees concurrently
3. **Partial cache loading**: Load only the facts that are actually requested, not the entire cache
4. **File watching**: Instead of polling mtimes, use filesystem watchers for instant change detection
5. **Fine-grained fact hashing**: Hash only the "signature" part of facts to improve propagation cutoff (e.g., hash TypeCheckedValue's signature separately from its body)
6. **CompilerIO purity enforcement**: Restrict the CompilerIO monad to prevent processors from performing arbitrary IO, formally guaranteeing the pure function property that the cache relies on
