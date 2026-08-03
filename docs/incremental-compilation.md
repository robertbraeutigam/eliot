# Incremental Compilation: The Load Floor and Cross-Run Reuse

Status: **THE REGENERATION TARGET IS MET (2026-08-03).** A warm build over an unchanged tree now regenerates only
world leaves — 249 `FileStat`, one `OutputFileStat`, one `UpToDate`, one synthetic-main `SourceContent` (~252
against the original 2129), with **no processor active but `FileStatProcessor`/`OutputFileStatProcessor`** and both
`MonomorphicTypeCheckProcessor`s at 0 calls. Two fixes got there — the `NativeBinding`/`UnifiedModuleNames`
totalization (removed the decline-poison, 2129 → 366) and the `UpToDate` anchor on every input-less contributor
(removed the edge-less-leaf bug and its typecheck cascade, 366 → 252). Both are landed and guarded by
`WarmBuildLeafOnlyTest`. §2 records the fixes as history.

**The problem has moved.** With processors no longer running on a warm build, the cost is no longer *recomputation*
— it is the **load floor** (§3): deserializing and re-validating a single monolithic `<target>/.eliot-cache`,
most of which is a **stable library front end that never changes and is never shared between compiler runs**. The
plan below attacks that on two fronts: a per-fact-type persistence decision (§4 Step A, shrinks the payload) and
**per-path fact collections** (§4 Step B, the centerpiece — makes a stable path's facts validate as a unit and
reuse across projects).

The engine works as designed. What follows is not a bug *in* the incremental algorithm but the next grain of reuse
the algorithm was never structured to exploit.

## 1. The model that already exists

A fact is computed by a processor from other facts. `DependencyTrackingProcess` records every `getFact` a
generation performs as a direct dependency of the key being generated, eagerly on each read, so a fact's
dependency set is complete by the time the fact becomes observable. At the end of a run
`IncrementalFactGenerator.buildCacheData` writes one `CacheEntry(value, directDeps)` per fact, and `FactCache`
persists the lot as a single Java-serialized object graph under `<target>/.eliot-cache`.

**Validation is backward and demand-driven.** Asked for a key, `resolve` accepts the cached value if the entry
has one *and* every recorded dependency is unchanged; otherwise it regenerates. "Unchanged" is decided per
dependency by `computeUnchanged`, which has exactly three branches:

| Prior entry                     | How it is validated                                        |
| ------------------------------- | ---------------------------------------------------------- |
| has a stored value              | recompute and compare — **the equality cutoff**             |
| no value, non-empty `directDeps`| **structural drill** through its edges, without materialising|
| no value, no edges              | cannot be validated ⇒ **changed**                           |

The equality cutoff is what stops propagation: a source file whose mtime moved but whose tokens recompute equal
invalidates nothing downstream. The structural drill is what lets a fact that cannot be stored still prove
itself unchanged, by reaching the leaves through its edges.

**Leaves are the boundary with the world.** An entry with no dependencies is always regenerated — that is how a
`FileStat` re-stats its file every run. Two facts exist purely to give that boundary the right shape:

- `UpToDate` — a constant with no inputs. A processor that is an *input-less compiler constant* would otherwise
  look exactly like a source leaf; depending on `UpToDate` turns it into a non-leaf whose drill terminates at a
  leaf that always compares equal. Every input-less native contributor now takes this edge (§2).
- `OutputFileStat` — the content digest, not the mtime, of a written artefact. An mtime would self-invalidate on
  every write; a digest is stable across a rewrite of the same bytes (which is what makes deterministic output a
  prerequisite, not a nicety) and still differs when the output is deleted, truncated or replaced. It is read
  *after* the write, so it records what the run actually left on disk. This dependency is the only thing tying an
  accepted-from-cache writer to the file's real state — presence alone, its earlier shape, let a corrupted jar
  survive a build that reported success.

**Fingerprints guard reuse.** `CacheFingerprint.compiler` digests every entry on the running compiler's
classpath (path, size, mtime; recursively for directories), so even an uncommitted recompile of the compiler
discards the cache — which is what makes "natives are constant within a compiler version" true. `config`
digests the command-line arguments. Both, plus `FactCache.CACHE_VERSION`, are matched on load.

Failures are deliberately never cached, so a fact that errored has no entry and re-emits its error every run
until fixed.

## 2. What was fixed (history)

The original diagnosis (written against `7588845b`) found a warm build recomputing 2129 facts — half its wall time
re-typechecking 251 monomorphic values that no source change touched. Instrumenting the three "changed" verdicts in
`computeUnchanged` isolated three permanent trigger classes, none of which could ever settle:

1. **Declines were not cached.** `BindingMergerProcessor` (and other total-looking readers) aborted when no
   contributor had a binding for a name — a correct answer, not a failure — and nothing was persisted, so every
   dependent read the absent key as *new/failed ⇒ changed* on the next run. **Fixed 2026-08-02** by the
   `NativeBinding` / `UnifiedModuleNames` / `ModuleAbilities` totalization (`docs/retire-optional-fact-reads.md`
   §6): those facts now always produce, with an explicit *absent* verdict that persists. **2129 → 366.**
2. **Input-less contributors were edge-less.** `StdlibNativesProcessor` answered from a static `Map` (or `None`)
   without reading any fact, so its `ContributedBinding`s carried no edges and were indistinguishable from a
   `FileStat` — regenerated unconditionally as world leaves (92 facts), and where the value was a non-serializable
   `SemValue` closure the edge-less entry read as *changed* every run and poisoned the ~4 `MonomorphicValue`s
   downstream (the 214 ms cascade). **Fixed 2026-08-03** by giving every input-less contributor the `UpToDate`
   edge, uniformly at the top of the generation so the hit, miss, and abort-guarded paths are all covered — the
   same two lines `SystemNativesProcessor` always carried. **366 → 252.**
3. **`MonomorphicValue` is equality-stable and can be *accepted*, not merely drilled.** Its own class-doc and
   `IncrementalFactGenerator`'s claimed the monomorphize layer cannot be persisted; that is wrong — it carries
   `GroundValue` and `MonomorphicExpression`, both ordinary data. (The class-docs still say otherwise; §5.)

252 is the leaf floor: 249 `FileStat` + `OutputFileStat` + `UpToDate` + the synthetic-main `SourceContent`.
`WarmBuildLeafOnlyTest` drives a real two-run `CompilationSession` over an untouched tree and asserts
`nonLeafRegenerations()` is empty, so a future contributor that forgets its `UpToDate` anchor is caught
automatically. A touch-one-source run moves the count a bounded amount (252 → 357 for a `demo(true)`→`demo(false)`
edit) and the output flips correctly, so real changes still propagate — the anchor only *widens* cache acceptance,
and this is the under-invalidation guard.

Measurements, for the record:

```
Before (7588845b): regenerated 2129;  4385 ms;  MonomorphicTypeCheck 1341 ms (51.6%), 251 calls
Totalization      : regenerated  366;   944 ms;  MonomorphicTypeCheck  214 ms (22.7%),   4 calls
UpToDate anchor   : regenerated  252;   724 ms;  MonomorphicTypeCheck    0 ms          ,   0 calls
```

## 3. The remaining cost: the load floor

Once the processors stop running, the warm-build cost is what is left when *nothing recomputes*:

- **Deserialization.** The whole ~17.9 MB `.eliot-cache` object graph is read and reconstructed on every process
  start, before the first fact is served. Java serialization's back-references make the graph compact but strictly
  sequential to decode.
- **Re-validation.** Every leaf is re-checked (the 249 file stats are unavoidable — they are the boundary with the
  world), and every accepted derived fact is validated against those leaves. For a small program on top of a large
  stable library, the overwhelming majority of that graph is the library's front end, which cannot have changed.
- **No cross-run sharing.** The cache lives under the build `<target>`. Two programs that both depend on the same
  library `--path` each keep their own monolithic cache; the library's front end is recomputed cold on each
  project's first build and re-deserialized in full on every warm build of each project. Nothing is shared, though
  the inputs are byte-identical.

The measured ~600 ms "compiler engine, cache and i/o" line is this floor. It does not shrink by fixing another
trigger class — there are none left to fix — only by **persisting less** (Step A) and by **validating and reusing
a stable path as a unit instead of fact-by-fact** (Step B).

### The equality-stability invariant (unchanged, and the reason Step A exists)

The cache does not need a fact to be *serializable*. It needs it to be **equality-stable**:

```
read(write(value)) == recompute(value)
```

Scala's function types extend `Serializable`, so a closure persists without complaint and comes back as a different
object; case-class equality over a function field, or over an array, is by reference. Such a value satisfies "did
this serialize?" and fails "will it still compare equal?". `FactCache` currently decides by trial serialization
(`canSerialize`), which can only ask the first question. Round-tripping every materialised fact over a full build
shows exactly three types that cannot be persisted safely — `ContributedBinding` and `NativeBinding` (both *mixed*
per instance: stable for a `None` contribution, unstable for a `SemValue`-closure `Leaf`) and `GeneratedModule`
(generated bytecode in a `ClassFile`, whose array compares by reference). The decision belongs to the **type**, not
the instance — a per-value decision would make the cache's behaviour depend on content.

## 4. The plan

### Step A — the persistence decision belongs to the fact type

Replace trial serialization with a codec the key supplies. The codec chooses a **persistable representation**, not
bytes, so the container keeps its single shared graph and its size (an earlier per-entry byte-framing attempt cost
**7.5× on disk: 17.9 MB → 134 MB**, because Java serialization's back-references and class descriptors are
per-stream and 16 800 independent frames re-write every `ValueFQN`, `ModuleName` and class descriptor):

```scala
trait FactCodec[V]:
  def persist(value: V): Either[Throwable, Option[Any]]   // None ⇒ deliberately not persisted
  def restore(stored: Any): Either[Throwable, V]
```

- The codec owns *whether* a value is persisted (the fix) and *what shape* it takes (the extensibility), but never
  touches a stream — so a codec cannot corrupt anything, and per-entry framing is unnecessary.
- Declining (`Right(None)`) and failing (`Left`) are distinct signals and must not be conflated. Encoding to *zero
  bytes* is neither: `UpToDate` is field-less, encodes to nothing, and must stay comparable — it is the anchor §2
  depends on.
- The choice is **abstract with no default**, so every key states it once. Both defaults are wrong: defaulting to
  persist reintroduces the closure-instability of §3; defaulting to decline would silently make a *leaf*
  unvalidatable — an edge-less declining `FileStat` disables the entire cache while everything still compiles.
- Default codec: identity (hand the fact to the shared graph, as today). `OpaqueCodec` for the three unstable
  types. Keep a serializability probe **only on the error path** — write the graph, and if it throws, retry with
  per-entry probing — so the steady-state cost disappears without losing the protection.

Step A is now motivated by the **load floor, not the regeneration count** (which is already at the leaf floor). Its
win is a smaller, cleaner payload and the removal of the steady-state `canSerialize` probe over every entry.

### Step B — per-path fact collections (cross-run reuse)

The centerpiece. Today the cache is one monolith per `<target>`, validated fact-by-fact and never shared. The
observation that makes a coarser grain sound: **the entire front end of a value is a function of that value's own
file plus the *signatures* it imports — never of `main`.** So it can be cached per source path and reused wherever
that path appears with the same inputs.

**Two tiers, split at the monomorphize seam.**

- **Pre-mono tier (per-path, shareable).** `PathScan`, `SourceContent`, token, ast, core, module (names/values),
  resolve, matchdesugar, operator, termination, row-elaboration, ability. Each such fact is a pure function of a
  file's content plus the *signatures* of the names it imports — and those imported signatures are themselves
  pre-mono facts. The tier is therefore self-contained given the contents of the paths it draws from.
- **Whole-program tier (per-target, as today).** `MonomorphicValue`, `RefinementTable`, `UsedValue`, uncurried,
  `GeneratedModule`, `OutputFileStat`. These are functions of the instantiation set reached from `main` — the
  use-site-verification cornerstone makes them irreducibly whole-program, so they **stay** in `<target>/.eliot-cache`
  and are not shared. The monomorphize seam *is* the tier boundary: it is exactly where "function of a file" ends
  and "function of the program" begins.

**Attribution — which path a fact belongs to.** Every pre-mono fact traces through its module name to a
`PathScan.Key(path, platform)`, whose result carries the concrete `file:` URIs that satisfied it; the serving
**mount root** (the `--path` layer) is recoverable from each URI's prefix. A fact belongs to the root(s) that serve
its module. Most facts belong to exactly one root; the base↔platform **layer merge** is the sole cross-root
coupling — a value declared abstractly in the base and concretely in a platform root has a `PathScan` (and thus
`directDeps`) spanning both roots, so its merged fact belongs to *both* (see the merge corner below).

**Validation hoists the equality cutoff to path granularity.** Aggregate the per-fact `directDeps` we already
record up to the root granularity: **root P depends on root Q** iff some fact attributed to P read some fact
attributed to Q. That yields a small **path dependency DAG** for free — no new bookkeeping, no declared inter-path
graph. Fingerprint each root's collection by:

```
fp(P) = digest( compilerFingerprint, platform, contents(P), { fp(Q) : P depends on Q } )
```

A root whose own contents and whose dependencies' fingerprints are unchanged has an unchanged `fp(P)`, and its
**entire collection is accepted with one compare** — no per-fact deserialize, no drill. Because layers point
downward (program → libraries → platform → base), editing the top program path changes only `fp(program)`; every
library and base collection keeps its fingerprint and its whole collection. That is precisely "stable paths don't
recompute," at collection rather than fact granularity. A fingerprint miss falls straight back to today's per-fact
validation — the coarse tier is an accelerator over the existing algorithm, never a replacement, so it can only
ever *accept less*, never accept something unsound.

**Cross-run reuse.** Because `fp(P)` is content-addressed and program-independent, a root's collection can be stored
in a **shared store** keyed by `fp(P)`, not under any one project's `<target>`. A second program depending on the
same library version finds the collection already present and reuses it instead of recomputing the front end cold —
the first build of a new project that shares libraries becomes nearly as cheap as a warm build. This is the reuse
the monolithic per-target cache structurally cannot give.

**Corners to get right (captured, not hand-waved):**

- **The merge straddles two roots.** A merged base↔platform value belongs to both roots' collections, so its
  fingerprint must fold in both roots' contents. Place it in the *more-derived* root's collection (the platform
  concrete side, which already depends on the base), so a base change invalidates via the dependency edge and the
  base collection stays a pure leaf of the DAG. Verify against the abstract↔concrete `signatureEquality` path.
- **Cross-root import cycles.** If two roots import from each other, the DAG has a cycle; collapse the strongly
  connected roots into one cache unit. Sound (a change to either invalidates the unit), just coarser — and rare,
  since layers are meant to point downward.
- **Where the store lives.** A global content-addressed store (e.g. `~/.eliot/cache/<fp>`) shares across projects
  but needs GC; a per-target index into a shared store is simpler to reason about but shares less. Library sources
  may be read-only, so the collection cannot live *inside* the path. Decide by measuring the cross-project win
  against the store-management cost.
- **Collection GC.** Content-addressed collections accumulate as library versions change. A simple LRU or
  build-count eviction over the store, run at process exit, keeps it bounded. Out of scope until Step B lands.
- **Platform scoping.** Collections are per `(root, platform)`: the compiler pool and runtime pool scan the same
  roots differently (the compiler pool adds each root's `eliot-compiler/` overlay), so a root has one collection
  per platform it is scanned in, fingerprinted independently.

### Smaller pending items

- **Cache declines (was Step 3).** Extend the entry to three outcomes: `Value` / `Opaque` (materialisable, value
  not persisted) / `Declined` (legitimately produced nothing). `regenerate` already distinguishes them — a decline
  is an explicit abort with zero errors and no registered fact; an error stays uncached so it re-surfaces.
  `resolve` completes a validated `Declined` entry with `None`; `computeUnchanged` drills its edges as for
  `Opaque`. Caching a decline reached *through* a missing upstream stays sound: the missing key is among the
  recorded edges, so if it starts producing, the decline invalidates. Bump `CACHE_VERSION`. *Lower priority now* —
  the totalization removed the decline-poison that made this urgent; it remains a correctness tidy-up and a small
  further load reduction.
- **Make the next regression visible (was Step 5).** `nonLeafRegenerations()` already exposes the metric to
  `WarmBuildLeafOnlyTest`. Add a permanent DEBUG-level trace of the three changed-verdicts — value differs / no
  prior entry / edge-less-and-unstorable — so "why did this rerun?" no longer needs hand-patching four call sites
  in `IncrementalFactGenerator`. Costs nothing with the logger off.
- **Two wrong class-docs.** `MonomorphicValue` and `IncrementalFactGenerator` both still claim the monomorphize
  layer cannot be persisted (§2 item 3 — it can). Correct them.
- **A dangling reference.** `FactCache`'s scaladoc points at "`docs/incremental-compilation.md` §13" for the
  fingerprints, which never existed. They are documented in §1 here; fix the reference.

## 5. Verification

- `./mill __.test`, plus cases in `IncrementalFactGeneratorTest` / `FactCacheTest` for decline-caching, opaque
  drilling, and a **law test** asserting `restore(persist(v))` equals a *freshly recomputed* value rather than a
  retained one. Round-trip equality alone would have passed the unstable `VNative` closures and missed the
  original bug.
- The standing metric assertion (`WarmBuildLeafOnlyTest`) stays green: a warm run over an unchanged tree
  regenerates only world leaves. For Step B, add its analogue — a two-project run sharing a library path reuses the
  library's collection with **zero** pre-mono regenerations attributed to that root.
- Fast example sweep plus **byte-identity** comparison against a cold build — the jar must be identical whether or
  not the cache (either tier) was used. Steps A and B both widen what is accepted from cache, so under-invalidation
  is the risk they carry; the byte-identity check is the guard. (Executable since jar entries got a fixed timestamp
  and order on 2026-07-31.)
- The other direction, which matters as much: touch one source file and confirm a bounded, *correct* subset
  recompiles — and, for Step B, that touching a file in one root invalidates that root's collection and its
  dependents' but **not** the roots below it.

## 6. Open items

- **The store design for Step B** — global content-addressed vs per-target index, and its GC policy. Decide by
  measuring the cross-project win, not by assumption.
- **`GeneratedModule` could be made comparable** by giving `ClassFile` a value-comparable representation instead of
  a bare array, which would let generated bytecode be cached rather than merely drilled through. Belongs to the
  whole-program tier, so it does not affect Step B; worth it only if the per-target tier's `GeneratedModule` drill
  shows up in the load floor after Steps A and B.
- **Which Step A representation** — the representation-based codec above (keeps 17.9 MB, gives up per-fact byte
  encodings) or a shared symbol/class-descriptor table that would make independent frames affordable. Measure what
  the table buys before committing to it; Step B may make it moot by shrinking the per-target payload to the
  whole-program tier alone.
