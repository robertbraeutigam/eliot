# Incremental Compilation Design

## 1. Problem Statement

Every invocation of the ELIOT compiler recomputes all facts from scratch: reading source files,
tokenizing, parsing, type checking, monomorphizing, and generating bytecode. For a growing codebase
this is slow even when only a single file changes — or nothing changes at all.

### Goals

1. **Fast no-change path**: If no source files have changed since the last compilation, the compiler
   does essentially no *heavy* work — no file reads, no tokenizing/parsing/type-checking, no
   monomorphization, no bytecode generation. Crucially this must hold **even though many facts are not
   serializable** (see §3): a no-change run must not be forced to recompute the non-serializable layer.
2. **Minimal recomputation on change**: When source files change, only the affected facts recompute.
   When a recomputed *serializable* fact produces the same value as before, propagation cuts off there.
3. **Completely generic**: confined to the `eliotc` module. No processor needs to know about incremental
   compilation. The mechanism relies solely on **all processors being stateless, deterministic pure
   functions of their input facts**.
4. **Configuration- and compiler-version-safe**: a different compiler or a different configuration must
   *never* be served stale results from a previous one (§13).

### Key property exploited

All `CompilerProcessor` implementations are stateless and deterministic. The only external inputs are
source files (read at the bottom of the chain) plus a small set of compiler constants (§12). Therefore:

> **A fact is unchanged iff every external input in its transitive dependency cone is unchanged.**

This is decidable from (a) the recorded dependency graph and (b) the values of the *leaves* — and
**neither of those is the non-serializable part of a fact**. That is the whole basis of this design.

## 2. The two roles of a cached fact

A cached fact plays two completely separate roles, with completely separate serializability needs:

1. **Change-detection** — deciding "did this fact change since last run?". This only ever needs a
   yes/no answer, which is *always* representable even when the fact's value is not serializable.
2. **Reconstruction** — supplying a fact's value to a *dependent that must recompute*. This genuinely
   needs the value, but **only on the change path**, and only for the dependents actually recomputing.

The earlier design conflated these: it validated a fact by re-resolving it and comparing *values*, so a
non-serializable fact (dropped from the cache) had no prior to compare, forcing it — and everything
above it — to regenerate on *every* run. That made the entire monomorphize phase recompute each time.

This design separates the roles. Change-detection drills the recorded **dependency graph** down to the
**leaves**; it never materializes an intermediate value. Reconstruction happens lazily, only when a
genuinely-changed dependent reads a value.

## 3. What is and isn't serializable

Most facts are Scala 3 case classes (`Product with Serializable`) with structural `==`. Two realities:

- **`java.nio.file.Path` is not `Serializable`.** Handled transparently by swapping a `Path` for a
  stand-in on write and back on read (`FactCache`'s custom object streams). No per-fact code.
- **Some fact *values* are fundamentally non-serializable.** `NativeBinding` / `TransparentBinding` (and
  any monomorphize fact that carries one) hold a `SemValue` whose `VLam`/`VPi`/`VNative` variants contain
  **Scala closures** — the actual JVM intrinsics and compile-time evaluation logic. No serializer can
  persist a closure. **This is code, not data**; there is no "ground form" to convert it into.

The critical observation: **fact *keys* are first-order and always serializable**, even for the
non-serializable facts — `NativeBinding.Key(vfqn)`, `MonomorphicValue.Key(vfqn, Seq[GroundValue])`
(`GroundValue` is a quoted first-order tree, no closures). So the dependency graph (`Map[Key, Set[Key]]`)
is fully serializable even where some of the values at those keys are not.

Therefore: **store the dependency graph for every fact; store *values* only for facts that can be
serialized.** A non-serializable fact persists as an *edges-only* entry (`value = None`), which is all
change-detection needs to drill *through* it.

## 4. Cache Entry Structure

```scala
case class CacheEntry(
    value: Option[CompilerFact],          // None ⇒ value not stored: non-serializable, or never materialized
    directDeps: Set[CompilerFactKey[?]],  // the keys this fact's processor read via getFact
    injected: Boolean = false             // registered directly (no processor can reproduce it)
)
```

| Field | Purpose |
|-------|---------|
| `value` | The cached value, when storable. `Some` for serializable facts (used for the cutoff comparison and to serve dependents); `None` for non-serializable facts and for facts validated-but-not-materialized this run. |
| `directDeps` | Validated recursively to decide changed-vs-unchanged. An **empty** set marks a *generated leaf* (a starting point with no dependencies, e.g. a `FileStat`), which is always recomputed. |
| `injected` | `true` for a fact registered directly via `registerFact` (e.g. the dynamic `main` source a backend injects). No processor can reproduce it, so it is *accepted on sight* (§13). |

## 5. The Algorithm: per-dependency change-detection

The validity question a parent asks of each dependency is **"did this dependency change?"**, answered by
`depUnchanged(key)` (memoized once per run):

```
depUnchanged(key):
  prior(key) match
    none                       -> false                       # new / previously failed ⇒ changed
    injected                   -> true                         # accept on sight
    value present (leaf or
      serializable derived)    -> recompute key, compare to stored value   # CUTOFF happens here
    value absent, has deps     -> directDeps.forall(depUnchanged)          # STRUCTURAL drill, no materialize
    value absent, no deps      -> false                         # value-less leaf: cannot validate ⇒ changed (fail-safe)
```

- **Serializable deps** (including all leaves) are recomputed and compared by value — this is exactly the
  equality **cutoff**: a changed leaf whose derived value recomputes equal stops propagation. The `eq`
  fast path makes an accepted dependency's comparison O(1).
- **Value-less deps** (the non-serializable `SemValue` layer) are validated **structurally**: the parent
  recurses through their recorded edges to the leaves, *without ever materializing the `SemValue`*. If
  the structural check passes, the fact's prior edges-only entry is **carried forward** (so it can be
  drilled through again next run).

`getFact(key)` / `resolve` decide accept-vs-regenerate for the fact actually requested:

```
resolve(key):
  injected                          -> complete with the stored value      # accept on sight
  value present AND has deps        -> if directDeps.forall(depUnchanged) then accept stored value
                                       else regenerate                       # reconstruction hit or recompute
  otherwise                         -> regenerate                            # value-less, leaf, or no prior
```

A value-less fact is regenerated **only when its value is actually requested** — i.e. when a genuinely
changed dependent reads it. On a no-change run, no dependent regenerates, so no `SemValue` is ever built.

### No-change run, worked

```
request GeneratedJar
  resolve(GeneratedJar): value present, deps = {MonomorphicValue(main), OutputFileStat(jar), …}
    depUnchanged(MonomorphicValue(main)):     value-less ⇒ STRUCTURAL drill
      depUnchanged(NativeBinding(+)):         value-less ⇒ STRUCTURAL drill
        depUnchanged(UpToDate):               leaf, recompute (constant) == stored ⇒ true   (§12)
      depUnchanged(OperatorResolvedValue(…)): serializable ⇒ recompute? no — its deps drill to leaves,
                                              all unchanged ⇒ accepted from cache (eq) ⇒ compares equal
      … all unchanged
    depUnchanged(OutputFileStat(jar)):        leaf, present == true ⇒ true
  all deps unchanged ⇒ ACCEPT cached GeneratedJar.  No reads, no monomorphization, no codegen, no rewrite.
```

No `SemValue` is materialized; the monomorphize layer is never touched.

### A function body changed, signature unchanged

`FileStat` differs → `FileContent` re-reads → … → the *serializable* signature fact recomputes equal →
**cutoff**: its dependents see it unchanged and are accepted. Where the change does cross into the
value-less layer, those facts have no early cutoff (their structural token changes whenever any input
does), so their dependents recompute — but that is exactly the cone that genuinely changed.

## 6. Carry-forward and pruning

On a no-change run a value-less fact is validated structurally but **never materialized**, so it is not
in `currentFacts`. To keep it in the rebuilt cache (so it stays drillable), `depUnchanged` records its
prior entry into a `carriedForward` map whenever its structural check passes.

```scala
def buildCacheData(): IO[FactCacheData] =
  for {
    factMap <- currentFacts()           // materialized this run (regenerated or accepted)
    deps    <- directDependencies.get
    carried <- carriedForward.get       // value-less facts validated-clean but not materialized
  } yield {
    val fresh = factMap.map { (key, fact) =>
      key -> CacheEntry(Some(fact), deps.getOrElse(key, Set.empty), injected = !deps.contains(key))
    }
    FactCacheData(CACHE_VERSION, fresh ++ carried.filterKeys(k => !fresh.contains(k)))
  }
```

- **Pruning**: a fact neither materialized nor carried this run (e.g. a source removed from the build, or
  no longer reachable) is simply absent → drops out. The cache self-prunes.
- **Failures never cached**: a failed fact produces no value, is not in `currentFacts`, and is not
  carried (carry happens only on a *passing* structural check) → it has no entry → it regenerates and
  **re-emits its error** next run. A broken build stays visibly broken (cf. fail-safe principle).
- **Save-time stripping**: a freshly regenerated non-serializable fact is in `fresh` with `value = Some`,
  but `FactCache.save` strips any non-serializable value to `None` while keeping its edges (§11).

## 7. Dependency tracking

`DependencyTrackingProcess` wraps the process so every `getFact` made while generating `key` is recorded
into `directDeps(key)` **as it happens** — before the fact's `Deferred` completes — so the dependency set
is complete the moment the fact becomes observable (race-free under concurrency). A fresh wrapper per
generation makes recording per-fiber.

## 8. Concurrency

The concurrent core is unchanged: a `Deferred`-per-key map computes each fact at most once per run.
`depUnchanged` is memoized the same way (a `Deferred[Boolean]` per key), so a shared diamond is validated
once. Validation pulls *current* values for inputs, so there is no stale-input race. There is no forward
edge set and no forward walk — the graph is only ever traversed backward, on demand.

## 9. Leaf facts as the external boundary

A leaf is a fact with no dependencies; it is always recomputed (there is nothing to validate against) and
its freshly computed value is compared, by `==`, to the value its dependents recorded.

- `FileStat(file)` — a `stat` syscall; `FileContentReader` depends on it so a changed mtime invalidates
  the (expensive) read while an unchanged mtime serves the content from cache.
- `OutputFileStat(file)` — whether an output (e.g. the JAR) is present. A side-effecting writer depends on
  it, so deleting the output flips `present` to `false`, differs from the cached `true`, and forces a
  rewrite. Presence (not mtime) is used so writing the file does not self-invalidate the fact. Deletion
  settles after one extra "echo" rebuild, then is stable.
- `UpToDate` — the compiler-constant anchor (§12).

## 10. (removed — folded into §5/§9)

## 11. Persistence

- **Format: Java serialization**, at `target/.eliot-cache`. Most facts need zero per-type code.
- **`Path` stand-in**: custom `ObjectOutputStream.replaceObject` / `ObjectInputStream.resolveObject` swap
  every `Path` for a serializable stand-in and back.
- **Edges always; values when possible.** `save` writes every entry whose key and `directDeps` serialize.
  For each such entry it keeps the value only if the value serializes, otherwise stores `value = None`
  (edges only). A non-serializable fact thus remains *drillable* next run; it is no longer dropped
  wholesale. (Probing uses a throwaway stream per object.)
- **Header: `(compilerFingerprint, configFingerprint, CACHE_VERSION)`** (§13). `load` returns the cache
  only if all three match the current run; otherwise `None` ⇒ a full, cold build.
- **Fail-safe both ways**: `load` returns `None` on any problem (missing/corrupt/version/fingerprint
  mismatch); `save` warns rather than failing the build. Degrades to "no incrementality", never to a
  crash or wrong output. Bump `CACHE_VERSION` whenever a persisted shape changes.

## 12. `UpToDate`: the compiler-constant anchor

A handful of facts are *input-less compiler constants* — most notably the `NativeBinding`s emitted by
`SystemNativesProcessor` (arithmetic intrinsics, `Function`, `Type`, the `Bool` primitives). They read no
source, and their values are non-serializable closures. With no dependencies they would look exactly like
a *source leaf* (empty `directDeps`), which the engine always recomputes — and being value-less leaves
they would be classified "changed" every run, dragging their whole cone with them.

Rather than special-case "is this a pure constant?" in the engine, such a fact takes a dependency on a
single trivial leaf, `UpToDate`:

```scala
case class UpToDate() extends CompilerFact { override def key() = UpToDate.Key() }
object UpToDate { case class Key() extends CompilerFactKey[UpToDate] }
// UpToDateProcessor: a leaf that always produces the same constant value.
```

`UpToDate` is a serializable leaf whose recomputed value always `==` its stored value ⇒ always clean.
A constant fact that depends on it is therefore a *value-less non-leaf*, validated structurally to
`depUnchanged(UpToDate) = true` ⇒ never recomputed on the no-change path; reconstructed cheaply on demand
when a changed dependent reads it.

`SystemNativesProcessor` establishes this dependency with a plain `getFact(UpToDate.Key())` (the value is
immaterial — only the edge matters). It is intentionally *not* `getFactOrAbort`: if `UpToDateProcessor`
is absent (e.g. a minimal test bundle) the natives simply fall back to always-regenerate — a loss of
incrementality, never a failure. `UpToDateProcessor` is registered in `LangPlugin` for production.

This interlocks with §13: `UpToDate` means "constant *for this compiler+configuration*", which is sound
only because a compiler or configuration change invalidates the whole cache.

## 13. Compiler + configuration fingerprint

The "natives are constant" premise (and `UpToDate`) is sound **only if a compiler change reliably discards
the cache**. The cache header therefore carries two fingerprints, both checked on load:

- **`compilerFingerprint`** — derived *automatically* from a content/size/mtime digest of the compiler's
  own classpath (`CacheFingerprint.compiler`). It must **not** be a hand-bumped integer: an integer one
  forgets to bump (especially with uncommitted compiler edits) would serve stale results — a silent
  correctness gap. When Mill recompiles the compiler, the classpath digest moves and the cache is
  discarded.
- **`configFingerprint`** — a digest of the command-line arguments (`CacheFingerprint.config(args)`),
  which capture everything that steers fact computation: source roots, selected `main` (`-m`), target /
  backend, flags. A different configuration ⇒ a different fingerprint ⇒ a cold build. This generalizes
  "changing `-m` is a clean build" to *any* configuration dimension, mechanically and totally.

A mismatch on either (or on `CACHE_VERSION`) returns `None` from `load` ⇒ a full recompile. Keeping a
single self-invalidating cache file means only one configuration stays warm at a time; keying the cache
*path* by `hash(header)` to keep several warm simultaneously is a future option, not needed for
correctness.

## 14. Separated graph / lazy value loading (Stage B — future)

Stage A above makes the no-change run do **no compilation work**, but it still deserializes the whole
cache file (potentially MBs of serialized ASTs / bytecode) on load. Because validity needs only the graph
+ leaf values — never an intermediate value — storage can be split so a no-change run never touches the
bulk values:

1. **Signature gate** *(tiny, optional, loaded first)*: source mtimes + the two fingerprints + output
   presence. Match ⇒ exit, having loaded nothing else. This is the O(files) fast path.
2. **Graph index** *(small, always loaded otherwise)*: `Map[Key, (directDeps, injected)]` + leaf values.
   `depUnchanged` walks only this. No big values.
3. **Value store** *(large, lazy, random-access per key)*: one file per value keyed by `hash(key)` (easy
   GC; delete files for pruned keys), or a single blob + offset index. Deserialized **only** for the keys
   a reconstruction actually reads — on a no-change run, at most the requested top fact.

This is a pure storage optimization layered on the Stage-A algorithm, which is unchanged. It is **not yet
implemented**; Stage A is the correctness-complete deliverable.

## 15. Performance characteristics

| Scenario | Work done |
|----------|-----------|
| Nothing changed | Load cache; walk the requested fact's graph (one map lookup + one `eq`/cheap `==` per edge); one `stat` per source file. **No reads, no monomorphization, no codegen, no rewrite.** No `SemValue` materialized. |
| One file changed, no signature change | Re-stat all files; re-read + reparse the changed file; pull forward until the equality cutoff stops propagation. The value-less layer downstream of a real change recomputes (no early cutoff there). |
| One file changed, signature changed | As above, cutoff reached later. |
| Everything / new file | Full recompilation of the affected cone + cache write. |
| Different compiler or configuration | Cache discarded by the header (§13) ⇒ full cold build. |

The no-change cost is O(V+E) in the requested fact's transitive graph plus O(files) stats and one cache
deserialization. Stage B (§14) removes the deserialization-of-bulk-values cost and offers an O(files)
fast path.

## 16. Required changes summary

### New files (`eliotc`, under `compiler/cache/`)
1. `UpToDate.scala` + `UpToDateProcessor.scala` — the compiler-constant anchor (§12).
2. `CacheFingerprint.scala` — compiler (classpath) + config (args) fingerprints (§13).

### Modified
| File | Change |
|------|--------|
| `compiler/cache/CacheEntry.scala` | `fact: CompilerFact` → `value: Option[CompilerFact]` |
| `compiler/cache/FactCache.scala` | keep edges (strip only non-serializable values); fingerprint header in `load`/`save` |
| `compiler/IncrementalFactGenerator.scala` | per-dependency `depUnchanged` (serializable compare vs value-less structural drill); `carriedForward`; value-less `resolve` |
| `compiler/Compiler.scala` | compute the two fingerprints (thread `args` through), pass to `load`/`save` |
| `monomorphize/processor/SystemNativesProcessor.scala` | `getFact(UpToDate.Key())` anchor dependency |
| `plugin/LangPlugin.scala` | register `UpToDateProcessor()` |
| tests | `CacheEntry(Some(_), …)`; `FactCache` save/load fingerprint args |

### Unchanged
`FactCacheData` (still `version` + `entries`), `CompilerIO`, `CompilerFact`/`CompilerFactKey`,
`CompilationProcess`, `CompilerPlugin`, `DependencyTrackingProcess`, all other processors.
