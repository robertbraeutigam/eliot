# Incremental Compilation Design

## 1. Problem Statement

Currently, every invocation of the ELIOT compiler recomputes all facts from scratch: reading source files, tokenizing, parsing, type checking, monomorphizing, and generating bytecode. For a growing codebase, this becomes increasingly slow even when only a single file changes — or nothing changes at all.

### Goals

1. **Fast no-change path**: If no source files have changed since the last compilation, the compiler should do essentially no *heavy* work — no file reads, no tokenizing/parsing/type-checking, no bytecode generation. It still walks the cached dependency graph to prove nothing changed (see §15), so for now the no-change cost scales with the number of *facts*, not the number of source files. That is fine until a codebase reaches thousands of files; the O(files) fast path is left as a future enhancement (§16).
2. **Minimal recomputation on change**: When source files change, only facts that are actually affected should be recomputed. When a recomputed intermediate fact produces the same value as before, propagation stops there.
3. **Completely generic**: The mechanism is confined to the `eliotc` module. No processor implementation needs to know about incremental compilation. The mechanism relies solely on the property that **all processors are stateless pure functions** of their input facts.
4. **All successful facts cached**: Every fact that successfully generates is stored in the persistent cache, together with the set of facts it directly depended on. Failures are never cached (§9), so errors re-surface on every run until fixed.

### Key Property Exploited

All `CompilerProcessor` implementations are stateless and deterministic: given the same input facts, they produce the same output fact. The only external inputs are source files (read at the bottom of the chain). Therefore, **if every fact a given fact directly depended on still has the same value, that fact is unchanged** — without re-running its processor.

## 2. Core Idea: Backward, Demand-Driven Validation

This design follows the proven "verifying traces" strategy (as in Shake / *Build Systems à la Carte*), the same approach used by the `tally/pipeline` project that this compiler's processor infrastructure was derived from.

Each cached fact records the **set of facts it directly depended on** (the keys it called `getFact` on while it was generated) plus its **value**. Validation is a recursive *pull*:

> A cached fact is still valid **iff every one of its direct dependencies, when resolved again, has the same value it had last run.**

Resolving a dependency recurses the same way, bottoming out at **leaf facts** — facts with no dependencies. A leaf (e.g. `FileStat`, which just does a `stat` syscall) is always recomputed, because there is nothing to validate. Its freshly computed value is then compared, by equality, against the value its dependents recorded.

There is **no forward edge set and no forward walk**. The graph is only ever traversed backward, on demand, from the fact actually requested. This is dramatically simpler than a forward-propagation design and is naturally correct under concurrency (see §8): resolving a fact always pulls *current* values for its inputs, so there is no risk of recomputing against a stale, not-yet-propagated input.

### Leaf facts as the external boundary

```
FileStat(Foo.els)        ← leaf: no deps, always re-stat'd, value = mtime
  ← FileContent(Foo.els) ← depends on FileStat; re-read only if mtime changed
    ← SourceContent(...)
      ← SourceTokens(...)
        ← ... entire downstream pipeline
```

If `Foo.els`'s mtime is unchanged, `FileStat(Foo.els)` produces the same value → `FileContent(Foo.els)` is accepted from cache without re-reading the file → nothing downstream re-runs.

## 3. Cache Entry Structure

```scala
package com.vanillasource.eliot.eliotc.compiler.cache

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

/** A persisted fact together with the keys it directly depended on when produced. */
case class CacheEntry(
    fact: CompilerFact,
    directDeps: Set[CompilerFactKey[?]],
    injected: Boolean = false
)

case class FactCacheData(
    version: Int,
    entries: Map[CompilerFactKey[?], CacheEntry]
)
```

That is the entire metadata: the fact value, and its direct dependency keys. No transitive leaf sets, no forward/reverse edges — all of that is replaced by on-demand recursion.

| Field | Purpose |
|-------|---------|
| `fact` | The cached value. Compared by `==` for cutoff, and returned to callers when accepted. |
| `directDeps` | The keys this fact's processor read via `getFact`. Validated recursively to decide accept-vs-regenerate. An **empty** set marks a *generated* leaf / "starting point" that is always regenerated. |
| `injected` | `true` for a fact that was **registered directly** (via `registerFact`), not produced by a processor — e.g. the dynamic main source a backend injects (`addDynamicSource`). No processor can reproduce it, so it must be *accepted* from the cache, never regenerated (see §13). Distinguished from a generated leaf because an injected fact was never the target of a `regenerate` and so has no entry in the `directDeps` map when the cache is built. |

## 4. The Algorithm

`IncrementalFactGenerator` replaces `FactGenerator`. It keeps the existing concurrent core unchanged — the `Deferred`-per-key map (`modifyAtomicallyFor`) that guarantees each fact is computed at most once per run, the per-fiber generation, and the `IOLocal` ancestor chain used for recursion detection (`activeFactKeys`). It adds exactly two behaviors: **consult the prior snapshot** before generating, and **record direct dependencies** while generating.

### getFact flow

```
getFact(key)
  │
  ├─ modifyAtomicallyFor(key) → (deferred, isFirst)     // unchanged from FactGenerator
  │
  ├─ if isFirst: start fiber → resolve(key, deferred)
  │
  └─ deferred.get                                        // blocks until resolved (this run)

resolve(key, deferred)
  │
  ├─ prior entry is injected?            → ACCEPT (nothing can regenerate it)
  ├─ prior entry exists AND has non-empty deps?
  │     ├─ Yes → depsUnchanged(entry.directDeps)?
  │     │          ├─ true  → ACCEPT: complete deferred with entry.fact, carry deps forward
  │     │          └─ false → REGENERATE
  │     └─ No (no prior, or a generated leaf with empty deps) → REGENERATE
  │
depsUnchanged(deps) = for every dep: getFact(dep) resolves to the same value
                      it had last run (prior.fact eq/== current)
```

### Sketch

```scala
final class IncrementalFactGenerator(
    generator: CompilerProcessor,
    prior: Map[CompilerFactKey[?], CacheEntry],   // prior run's snapshot, read-only
    errors: Ref[IO, Chain[CompilerError]],
    facts: Ref[IO, Map[CompilerFactKey[?], Deferred[IO, Option[CompilerFact]]]],
    directDeps: Ref[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]],
    activeKeys: IOLocal[List[CompilerFactKey[?]]]
) extends CompilationProcess with Logging {

  override def activeFactKeys: IO[List[CompilerFactKey[?]]] = activeKeys.get

  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): IO[Option[V]] =
    for {
      modifyResult <- modifyAtomicallyFor(key)                       // (deferred, isFirst)
      _            <- resolve(key, modifyResult._1).start.whenA(modifyResult._2)
      result       <- modifyResult._1.get
    } yield result.map(_.asInstanceOf[V])

  /** First-time request: accept the prior value if still valid, otherwise regenerate. An injected fact (registered
    * directly, never produced by a processor) is always accepted — nothing can regenerate it. */
  private def resolve(key: CompilerFactKey[?], deferred: Deferred[IO, Option[CompilerFact]]): IO[Unit] =
    prior.get(key) match {
      case Some(entry) if entry.injected            => acceptPrior(key, entry, deferred)
      case Some(entry) if entry.directDeps.nonEmpty =>
        depsUnchanged(entry.directDeps).flatMap {
          case true  => acceptPrior(key, entry, deferred)
          case false => regenerate(key)
        }
      case _ => regenerate(key)   // no prior entry, or a generated leaf (empty deps) → always run
    }

  /** Every recorded dependency still resolves to the value it had last run. */
  private def depsUnchanged(deps: Set[CompilerFactKey[?]]): IO[Boolean] =
    deps.toList.forallM { dep =>
      getFact(dep).map {
        case Some(current) => prior.get(dep).exists(p => (p.fact eq current) || p.fact == current)
        case None          => false        // dep no longer producible ⇒ treat as changed
      }
    }

  /** Accept the cached fact. An injected fact is intentionally NOT recorded in `directDeps`, so it is re-classified as
    * injected when the cache is rebuilt; a generated fact carries its trace forward to re-persist. */
  private def acceptPrior(key, entry, deferred): IO[Unit] =
    directDeps.update(_.updated(key, entry.directDeps)).unlessA(entry.injected) >>
      deferred.complete(Some(entry.fact)).void

  /** Run the processor. `key` is marked present in `directDeps` up front (so a generated leaf is not mistaken for an
    * injected fact), and reads accumulate into that entry *as they happen* — before the fact's Deferred is completed by
    * `registerFact` — so the dependency set is complete the moment the fact becomes observable (no race with the
    * end-of-run cache build). The Deferred completion is reordered into `getFact`'s post-resolve safety net. */
  private def regenerate(key: CompilerFactKey[?]): IO[Unit] =
    directDeps.update(d => d.updated(key, d.getOrElse(key, Set.empty))) >>
      generator.generate(key).run(new DependencyTrackingProcess(this, key, directDeps)).runS(Chain.empty)
        .fold(identity, identity)
        .flatMap(es => errors.update(_ ++ es))

  override def registerFact(fact: CompilerFact): IO[Unit] =
    modifyAtomicallyFor(fact.key()).flatMap(_._1.complete(Some(fact)).void)

  // modifyAtomicallyFor, currentErrors, currentFacts: unchanged from FactGenerator
}
```

Notes:

- **`acceptPrior` carries the trace forward** for generated facts (re-persists with the same `directDeps`), but not for injected facts (so they stay classified injected).
- **`regenerate` records deps incrementally, before the fact is observable.** The wrapper writes each read into `directDeps(key)` as it happens; because a processor reads its inputs before registering its output, the set is complete by the time `registerFact` completes the Deferred. Recording is per-generation, so `getFact` calls made during *validation* (`depsUnchanged`) are not attributed to any fact — no shared scope to pollute.
- **`activeKeys` is updated for the whole of `resolve`** (in `getFact`, around `resolve`) — covering both validation and generation, since `key`'s Deferred is pending throughout, so any in-progress re-request of `key` is correctly visible to recursion detection.

## 5. Equality-Based Cutoff

When a dependency's value is recomputed and compared:

```scala
(prior.fact eq current) || prior.fact == current
```

- The `eq` check is a fast path: on a no-change run, an accepted dependency's `current` value *is* the same object as `prior.fact` (we completed its `Deferred` with the prior fact), so the comparison is O(1) and never descends into the value's structure.
- The `==` check is structural Scala case-class equality, used when a fact was actually regenerated (e.g. a leaf `FileStat`, or any fact downstream of a change). This is where the cutoff happens: **a change in a source file may not change every derived fact.** Changing a function body does not change its type signature, so a fact that depends only on the signature recomputes to a value equal to its cached one — and propagation stops there.

Equality on the actual values (rather than hashes) is simpler, exact (no collisions), and free (case classes provide `==`).

## 6. Dependency Tracking

Recording uses the same decorator pattern the codebase already has in `visualization/TrackedCompilationProcess.scala` (which wraps `CompilationProcess` to record `getFact`/`registerFact` for the fact-graph visualization).

```scala
package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.{IO, Ref}
import com.vanillasource.eliot.eliotc.processor.*

/** Wraps the compilation process so every getFact made while generating `key` is recorded into `directDeps(key)` — as
  * it happens, before the fact is observable. A fresh instance is used per generation, so recording is per-fiber. */
final class DependencyTrackingProcess(
    underlying: CompilationProcess,
    key: CompilerFactKey[?],
    directDeps: Ref[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]]
) extends CompilationProcess {

  override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](dep: K): IO[Option[V]] =
    directDeps.update(d => d.updated(key, d.getOrElse(key, Set.empty) + dep)) >> underlying.getFact(dep)

  override def registerFact(value: CompilerFact): IO[Unit] = underlying.registerFact(value)

  override def activeFactKeys: IO[List[CompilerFactKey[?]]] = underlying.activeFactKeys
}
```

## 7. Building the Cache After a Run

```scala
def buildCacheData(): IO[FactCacheData] =
  for {
    factMap <- currentFacts()        // Map[key, fact] — only facts that resolved to Some
    deps    <- directDeps.get
  } yield FactCacheData(
    version = FactCache.CACHE_VERSION,
    entries = factMap.map { case (key, fact) =>
      key -> CacheEntry(fact, deps.getOrElse(key, Set.empty), injected = !deps.contains(key))
    }
  )
```

Three properties fall out for free:

- **Generated vs injected is read off `directDeps`.** A fact a processor generated was the target of a `regenerate`, which marks it present in `directDeps` (empty for a leaf). A fact that was only `registerFact`'d — injected — never was, so `!deps.contains(key)` classifies it. (For this to be race-free the dependency set is recorded *before* the fact becomes observable; see §4.)
- **Only reachable facts are kept.** A fact in the prior snapshot that was never requested this run (e.g. a source file removed from the build) is simply absent from `factMap` and drops out of the new cache. The cache self-prunes.
- **Facts registered as a side-effect** (a multi-fact processor that registers facts other than the requested key) get no `directDeps` entry → empty set → treated as a leaf next run → always regenerated. This is conservative but **safe**: it can only cost incrementality, never correctness. (A future refinement could attribute the generation's deps to every fact it co-registered.)

## 8. Concurrency

The existing `FactGenerator` resolves facts concurrently: `getFact` forks a fiber per first-time key and blocks all requesters on a shared `Deferred`. This design preserves that exactly:

- **Memoization / single-computation** is the existing `Deferred` map — untouched.
- **Dependency recording** is per-generation: each `DependencyTrackingProcess` records only into its own `key`'s entry, and does so *before* the fact's `Deferred` completes (see §4), so concurrent generations never interleave their recordings and no consumer can observe a fact with an incomplete dependency set. This is the crucial adaptation over the `tally` reference, which uses a single shared scope stack that is only correct because `tally` runs sequentially.
- **Validation** (`depsUnchanged`) calls `getFact`, which is itself guarded by the `Deferred` map, so a dependency validated/regenerated concurrently by two paths is still computed once.
- **Shared mutable state** is only `errors`, `facts`, and `directDeps` — all `Ref`s updated atomically. The `prior` snapshot is immutable after load.

No forward walk means no shared "visited"/"validated" set to race on — another reason backward-pull is the safer fit for eliot's concurrent model.

## 9. Failures Are Never Cached → Errors Re-Surface

A processor that hits an error reports it (into the `errors` chain) and `abort`s, producing **no fact** — its `Deferred` resolves to `None`. `buildCacheData` only persists facts that resolved to `Some`, so:

- A failed fact has no cache entry → next run it has no prior → it is **regenerated**, re-running its processor and **re-emitting its error**.
- Everything in the failed fact's dependency cone either also failed (and likewise regenerates) or was accepted from cache around it — either way the error re-surfaces.

This keeps a broken build **visibly broken on every run**, instead of silently "succeeding" from cache. (See [[feedback_gaps_must_be_failsafe]].)

**Required invariant:** an error must coincide with a fact-generation *failure* (the abort-on-`compileError` convention the codebase already follows — see `.claude/rules/eliot-design.md`). If a processor ever emitted an error while still producing a fact, that error would be cached-over and not re-surface. A test should pin this down (a run that errors, then an unchanged rerun, must re-print the error).

## 10. Leaf Fact: `FileStat` (existing, one processor change)

`FileStat` already exists in `lang/source/stat/` and is already a leaf (no dependencies — just a `stat` syscall):

```scala
case class FileStat(file: File, lastModified: Option[Instant]) extends CompilerFact { ... }
```

The only change is to make `FileContentReader` depend on it, so a changed mtime invalidates the (expensive) file read:

```scala
class FileContentReader extends SingleFactProcessor[FileContent.Key] with Logging {
  override protected def generateSingleFact(key: FileContent.Key): CompilerIO[FileContent] =
    for {
      _      <- getFactOrAbort(FileStat.Key(key.file))   // ← new: establish the leaf dependency
      result <- /* existing read logic unchanged */
    } yield result
}
```

Why this is enough:

- `FileStat` is a leaf → always recomputed (cheap stat).
- File unchanged → same `FileStat` value → `FileContent` accepted from cache → **no read**.
- File changed → `FileStat` differs → `FileContent` regenerates (re-reads) → forward effects propagate by the normal pull/cutoff.

Without this edge, `FileContent` itself would be a leaf (it reads the file directly) and would re-read every file on every run.

## 11. Output Files: `OutputFileStat` (optional, for JAR regeneration)

Pure facts are skipped safely when unchanged. But a processor that writes a **file** (the JAR) has a side effect the cache can't see: if the JAR is deleted, a no-change run would accept everything from cache and never rewrite it. `OutputFileStat` closes that gap as another leaf, modeled exactly like `FileStat`:

```scala
package com.vanillasource.eliot.eliotc.compiler.cache

case class OutputFileStat(file: File, present: Boolean) extends CompilerFact {
  override def key(): CompilerFactKey[OutputFileStat] = OutputFileStat.Key(file)
}
object OutputFileStat { case class Key(file: File) extends CompilerFactKey[OutputFileStat] }

class OutputFileStatProcessor extends SingleFactProcessor[OutputFileStat.Key] with Logging {
  override protected def generateSingleFact(key: OutputFileStat.Key): CompilerIO[OutputFileStat] =
    IO(key.file.exists()).map(OutputFileStat(key.file, _)).to[CompilerIO]   // leaf: no deps
}
```

The JAR-writing processor takes a dependency on it:

```scala
_ <- getFactOrAbort(OutputFileStat.Key(jarFile))   // before writing the JAR
```

- **JAR present, sources unchanged** → `present = true`, equals cached → JAR fact accepted → no rewrite.
- **JAR deleted** → `present = false`, differs from cached `true` → JAR fact regenerates → rewrite. Upstream `GeneratedModule` facts stay cached.
- **JAR present, sources changed** → upstream invalidation reaches the JAR fact regardless.

A deliberately accepted wart (for simplicity): because the dependency is read *before* the write, the value recorded on the run that (re)creates the JAR reflects the pre-write state, so a deletion settles after **one extra "echo" rebuild**, then is stable. This is bounded and correct, and avoids threading a cache timestamp through plugin initialization. It can be tightened later if it ever matters.

Using a plain `present: Boolean` rather than a timestamp is what keeps `OutputFileStat` self-consistent: writing the JAR changes its mtime, so an mtime-valued output fact would self-invalidate every run, whereas presence is stable across the write.

## 12. Persistence

```scala
object FactCache {
  val CACHE_VERSION = 1
  private val CACHE_FILE = ".eliot-cache"

  def load(targetDir: Path): IO[Option[FactCacheData]] =
    IO.blocking {
      val f = targetDir.resolve(CACHE_FILE).toFile
      if (f.exists()) {
        val ois  = new ObjectInputStream(new FileInputStream(f))
        val data = ois.readObject().asInstanceOf[FactCacheData]
        ois.close()
        Option.when(data.version == CACHE_VERSION)(data)
      } else None
    }.handleError(_ => None)            // any load failure ⇒ no cache ⇒ full recompile

  def save(targetDir: Path, data: FactCacheData): IO[Unit] =
    IO.blocking {
      Files.createDirectories(targetDir)
      val oos = new ObjectOutputStream(new FileOutputStream(targetDir.resolve(CACHE_FILE).toFile))
      oos.writeObject(data); oos.close()
    }.handleErrorWith(t => warn[IO]("Could not write incremental cache; next build will be full.", t))
```

The implemented `FactCache` (see `eliotc/.../compiler/cache/FactCache.scala`) is the sketch above plus two eliot-specific realities discovered in practice:

- **Format: Java serialization.** Most eliot facts are Scala 3 case classes (`Product with Serializable`) with structural `==`, so this works with almost zero per-type code. Cache location: `target/.eliot-cache`.
- **`java.nio.file.Path` is not `Serializable`.** Its implementations (`sun.nio.fs.UnixPath`) don't implement `Serializable`, and several facts carry `Path`s. `FactCache` handles this generically with custom streams: `ObjectOutputStream.replaceObject` swaps every `Path` for a small serializable stand-in on write, and `ObjectInputStream.resolveObject` restores it on read. No per-fact code.
- **Some facts are *fundamentally* non-serializable, and that is fine.** A handful of facts (`NativeBinding`, `TransparentBinding`) carry a `SemValue` — the NbE semantic domain, whose `VLam`/`VPi`/`VNative`/`Lazy` variants hold **Scala closures**. No serializer (Java, circe, anything) can persist a closure. Rather than fail the whole cache, `save` **serializes each entry independently and drops the ones that can't** (probing each with a throwaway stream first, then writing the survivors in one shared-structure pass). A dropped fact simply has no prior entry next run and is **regenerated** — always safe under backward-pull (correctness is preserved; only some incrementality is lost: a fact that transitively depends on a non-serializable fact will regenerate because the dependency has no prior to compare against). In practice this means the front-end (read → tokenize → parse → resolve) and the ground codegen facts cache and accept across runs, while the `SemValue`-bearing monomorphize facts and their dependents regenerate.
- **Version + discard-on-failure.** A `CACHE_VERSION` bump (or any deserialization error after a compiler change) discards the whole cache → one full recompile. Bump `CACHE_VERSION` whenever a persisted fact's shape changes.
- **Both directions are fail-safe.** `load` returns `None` on any problem; `save` warns and the build still succeeds even if writing is impossible. Degrades to "no incrementality," never to a crash or wrong output.
- **Why not circe?** An explicit per-type codec registry (as in `tally`'s `ArtifactCodecs`) would be more debuggable on disk, but it would *not* solve the `SemValue` problem — closures can't be encoded to JSON either. The drop-non-serializable approach is needed regardless of the library, so Java serialization (least boilerplate) is kept.

### Injected facts

A backend can inject a fact directly with `registerFact` rather than have a processor produce it — the JVM backend's `addDynamicSource` registers a synthetic `SourceContent`/`PathScan` for the generated `main` entry point. No processor can (re)produce such a fact, so on a warm build the engine must **accept it from the cache, not regenerate it** (regenerating would invoke `PathScanner`, which fails to find the synthetic path, aborting the whole build). The `injected` flag on `CacheEntry` (§3) carries this: a fact that was the target of a `regenerate` is *generated* and recorded in `directDeps`; one that was only `registerFact`'d is *injected* and accepted on sight. The cached value is authoritative for a given build target; changing the target (e.g. a different `-m` main) is a configuration change handled by a clean build, not by source-mtime invalidation.

## 13. Integration with `Compiler.scala`

Only `runWithConfiguration` changes — load the cache, swap `FactGenerator` for `IncrementalFactGenerator`, save the cache after the run:

```scala
newConfiguration <- activatedPlugins.traverse_(_.configure()).runS(configuration)
processor        <- activatedPlugins.traverse_(_.initialize(newConfiguration)).runS(NullProcessor())
tracker          <- FactVisualizationTracker.create()
wrappedProcessors = processor.wrapWith(wrapProcessor(_, tracker))

targetPath        = newConfiguration.get(targetPathKey).get
cacheData        <- FactCache.load(targetPath)                                  // NEW
generator        <- IncrementalFactGenerator.create(wrappedProcessors, cacheData) // replaces FactGenerator.create

_                <- targetPlugin.run(newConfiguration, generator)
_                <- generator.buildCacheData().flatMap(FactCache.save(targetPath, _))  // NEW

errors           <- generator.currentErrors()
_                <- errors.traverse_(_.print())
_                <- tracker.generateVisualization(/* unchanged */)
```

No change to `CompilerPlugin.initialize` (the `present`-based `OutputFileStat` needs no cache timestamp), `CompilerIO`, `CompilerFact`/`CompilerFactKey`, or any plugin configuration. `IncrementalFactGenerator` implements `CompilationProcess` and exposes `currentErrors()`, `currentFacts()`, and `buildCacheData()`.

## 14. Worked Examples

### Nothing changed
```
Request MonomorphicValue(Foo.bar)   (no prior-deps validation needed at the leaves)
  resolve: prior entry exists, deps = {SourceTokens(Foo.els), ...}
  depsUnchanged walks down to leaves:
    FileStat(Foo.els)    → re-stat → same mtime → accepted everywhere up the chain (eq fast-path)
    FileStat(String.els) → re-stat → same mtime → accepted
  all deps unchanged ⇒ ACCEPT cached MonomorphicValue(Foo.bar). No reads, no type-checking.
```
Work done: the cached graph is walked (one map lookup + one `eq`/cheap `==` per edge) and each source file is `stat`'d once. No heavy processor runs.

### A function body changed, signature unchanged
```
FileStat(Foo.els)            → different mtime → regenerate
FileContent(Foo.els)         → re-read → differs → regenerate
... → ResolvedValue(Foo.bar) → differs (body) → regenerate
MonomorphicValue(Foo.bar)    → differs (body) → regenerate
MonomorphicValue(Other.baz)  → recompute → EQUALS cached (uses only Foo.bar's signature) → CUTOFF
  everything downstream of Other.baz → accepted from cache
```

### Source file deleted
`FileStat(file)` → `lastModified = None` → differs → `FileContent` regenerates → `FileContentReader` reports "Could not read file." and aborts → that fact (and its cone) fail → not cached → error re-surfaces next run too.

### New source file
No prior entry anywhere along its chain → everything regenerates fresh → added to the cache on save.

### JAR deleted (with `OutputFileStat`)
`OutputFileStat(jar)` → `present = false` → differs from cached `true` → JAR fact regenerates → JAR rewritten (upstream facts stay cached). Settles after one echo rebuild (§11).

## 15. Performance Characteristics

| Scenario | Work done |
|----------|-----------|
| Nothing changed | Load cache + walk the cached dependency graph of the requested fact: one map lookup and one `eq`/cheap `==` per edge, plus one `stat` per source file. No reads, no type-checking, no codegen. **Scales with fact count** (see §16). |
| One file changed, no signature change | Re-stat all files; re-read + reparse the changed file; pull forward until equality cutoff stops propagation. |
| One file changed, signature changed | As above, but the cutoff is reached later (downstream of the first fact that recomputes equal). |
| Everything changed | Full recompilation + cache write. |
| First compilation (no cache) | Full recompilation + dependency recording + cache write. Slightly slower than today due to recording overhead. |

The no-change cost is **O(V + E)** in the requested fact's transitive graph (every fact visited once thanks to `Deferred` memoization; every edge a cheap comparison) plus **O(files)** stat calls and one full cache deserialization. For codebases up to ~hundreds of files this is comfortably fast; it is deliberately *not* optimized to O(files) yet (§16).

**Caveat from the `SemValue` reality (§12):** facts carrying closures (`NativeBinding`/`TransparentBinding`) are not persisted, so they — and any fact that depends on them — regenerate every run. In practice the incremental savings are concentrated in the front-end (skip re-reading/-tokenizing/-parsing/-resolving unchanged files) and the no-change codegen accept; the monomorphize type-checking is largely re-run. Full monomorphize-phase caching would require those facts to be quoted to a ground, serializable form (or a fundamentally different persistence strategy for the NbE domain).

## 16. Future Enhancements

1. **O(files) no-change fast path** (the real scaling fix, when thousands of files arrive): split the cache into a tiny, separately-loadable *source-fingerprint index* (file → mtime) and the bulk fact payloads. On a no-change run, load only the index, `stat` the files, and short-circuit *without* deserializing the fact graph or walking it — making the no-change cost scale with file count, not fact count. The backward-pull algorithm below is unchanged; this is purely a fast-path gate in front of it.
2. **Partial cache loading**: deserialize only the facts actually touched, rather than the whole file.
3. **Fine-grained equality**: custom `==` on heavy fact types to cut off earlier (e.g. compare only the signature portion of a value fact, ignoring body changes).
4. **`CompilerIO` purity enforcement**: restrict the monad so processors cannot perform arbitrary IO, formally guaranteeing the purity the cache relies on.
5. **File-watching daemon**: filesystem watchers for instant change detection in a long-running compiler.

## 17. Required Changes Summary

### New files (`eliotc` module, under `compiler/cache/`)
1. `CacheEntry.scala` — `CacheEntry` (fact + `directDeps` + `injected`)
2. `FactCacheData.scala` — `FactCacheData` (version + entries)
3. `FactCache.scala` — load/save (Java serialization + `Path` stand-in + drop-non-serializable)
4. `DependencyTrackingProcess.scala` — per-generation, race-free dependency recording into `directDeps(key)`
5. `OutputFileStat.scala` + `OutputFileStatProcessor.scala` — `OutputFileStat` fact + leaf processor *(for JAR-deletion handling)*

### New / replaced
6. `compiler/IncrementalFactGenerator.scala` — replaces `FactGenerator` everywhere (keeps its `Deferred` core + `IOLocal` chain; `FactGenerator` is deleted). With `prior = None` it behaves identically, so the existing test base classes pass `None`.

### Modified
| File | Change |
|------|--------|
| `eliotc/.../compiler/Compiler.scala` | Load/save cache; use `IncrementalFactGenerator` |
| `eliotc/.../processor/CompilationProcess.scala` | Doc reference `FactGenerator` → `IncrementalFactGenerator` |
| `lang/.../source/file/FileContentReader.scala` | `getFactOrAbort(FileStat.Key(key.file))` before reading |
| `jvm/.../jargen/JvmProgramGenerator.scala` | `getFactOrAbort(OutputFileStat.Key(jarFile))` before writing the JAR |
| `jvm/.../plugin/JvmPlugin.scala` | Register `OutputFileStatProcessor` |
| `lang`/`jvm` test base classes | `FactGenerator.create(p)` → `IncrementalFactGenerator.create(p, None)` |

### No changes required
- No other processor implementations.
- No fact types changed. (Most are `Serializable` case classes; `SemValue`-carrying facts are simply not persisted — §12.)
- No changes to `CompilerIO`, `CompilerFact`/`CompilerFactKey`, `CompilationProcess`, `CompilerPlugin`, or plugin configuration.
- No `FileStatProcessor` change (already a leaf).
