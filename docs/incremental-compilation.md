# Incremental Compilation: Why a Warm Build Still Computes

Status (2026-08-06): **regeneration is at the leaf floor; what is left is the cache format.** A warm build over an
unchanged tree re-runs only world leaves — §3's three regeneration defects are diagnosed and two are fixed (§8), and
`WarmBuildLeafOnlyTest` now pins the metric. The remaining cost is I/O: §11 measures **~60% of a warm build in cache
load + save**; §12 spikes a per-entry store that fixes save (978 → 28 ms) but not load, at 17× on disk; and **§13 is
the plan that resolves both** — explicit per-type codecs over a content-addressed object store, replacing Java
serialization outright.

§§1–2 are the model everything else refers to. §§3–10 are the diagnosis, its fixes and the retention changes, kept as
the record of what each defect looked like.

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
  leaf that always compares equal.
- `OutputFileStat` — the content digest, not the mtime, of a written artefact. An mtime would self-invalidate on
  every write; a digest is stable across a rewrite of the same bytes (which is what makes deterministic output a
  prerequisite, not a nicety) and still differs when the output is deleted, truncated or replaced. It is read
  *after* the write, so it records what the run actually left on disk. This dependency is the only thing tying an
  accepted-from-cache writer to the file's real state — presence alone, its earlier shape, let a corrupted jar
  survive a build that reported success.

**Fingerprints guard reuse.** `CacheFingerprint.compiler` digests every entry on the running compiler's
classpath (path, size, mtime; recursively for directories), so even an uncommitted recompile of the compiler
discards the cache — which is what makes "natives are constant within a compiler version" true. `config`
digests the effective `Configuration`: each `Configuration.Key` decides via `identityContribution` whether — and
as what — its value participates, so diagnostic-only flags (`--statistics`, `--visualize-facts`) and
unrepresentable injected values (a mount factory) opt out at their declaration rather than through a list of
exceptions (see `docs/cache-identity-configuration.md`). Both, plus `FactCache.CACHE_VERSION`, are matched on
load.

Failures are deliberately never cached, so a fact that errored has no entry and re-emits its error every run
until fixed.

## 2. The invariant the cache actually runs on

The cache does not need a fact to be *serializable*. It needs it to be **equality-stable**:

```
read(write(value)) == recompute(value)
```

Serializability is necessary and nowhere near sufficient, and the two are routinely different. Scala's function
types extend `Serializable`, so a closure persists without complaint and comes back as a different object;
case-class equality over a function field, or over an array, is by reference. Such a value satisfies "did this
serialize?" and fails "will it still compare equal?".

`FactCache` still decides by trial serialization (`canSerialize`), which can only ask the first question. Every
defect in §3 follows from that gap; §13 makes the invariant a codec law that can be enforced instead of an
accident of the serializer.

## 3. What a warm build actually did

Measured on the `eliot-build` tree (77 source files), warm build immediately after an identical one: **2 601 ms,
64% in processors, 2129 facts regenerated** — 979 `ContributedBinding`, 547 `FileStat`, 310 `NativeBinding`, 251
`MonomorphicValue`, 25 `RefinementTable`, 14 `CompilerMonomorphicValue`, one each of `UpToDate`, `SourceContent`,
`OutputFileStat`. The tokenizer did not run — no source file changed — yet half the wall time went on
re-typechecking 251 monomorphic values.

Instrumenting the three "changed" verdicts in `computeUnchanged` identified **three independent trigger classes**,
none of which could ever settle on any number of consecutive no-change builds:

- **3.1 Serializable, but never equal (11 facts).** `SemValue.VNative(paramType, fire: SemValue => SemValue)` is a
  case class holding a lambda. The lambda serializes (it captures only an FQN), so the value **is** persisted, and
  the recomputed lambda never equals the deserialized one — 11 facts report "value differs" every run, all
  `eliot.lang.String` natives. **Still live**, but no longer drives regeneration count (§8); §13 turns it into an
  explicit decline.
- **3.2 Unstorable *and* edge-less (6 facts).** Where a native's value genuinely fails to serialize the entry is
  stored edges-only — and the drill then has no edges to follow, because `StdlibNativesProcessor` reads no facts at
  all. Six `ContributedBinding`s landed in the "no value, no edges ⇒ changed" branch every run. **FIXED** by the
  `UpToDate` anchor (§6 Step 2, measured in §8).
- **3.3 Declines are not cached (55 facts, 23 reached).** `BindingMergerProcessor` aborts when no contributor has a
  binding for a name — a correct answer, not a failure. Nothing is persisted for such a demand, so next run
  `prior.get(key)` is `None`, which reads as *new / previously failed ⇒ changed* for every dependent that ever
  asked. **FIXED** by the `NativeBinding` / `UnifiedModuleNames` totalization
  (`docs/retire-optional-fact-reads.md` §6.1–6.2); §6 Step 3 would generalize it to any decline.

**The cascade, and why every trigger count above is a lower bound.** Any one of the three is enough on its own.
Once a `MonomorphicValue` is judged changed, regenerating it re-runs the checker for that value, which demands its
callees' values, and so on — 251 re-checks from a handful of roots. `forallM` short-circuits on the first changed
dependency, so more triggers may sit behind the ones that fire first.

## 4. Which fact types are equality-stable (measured, not inferred)

Round-tripping every materialised fact through Java serialization and comparing to itself, over a full build:

| Fact type            | Verdict                                      |
| -------------------- | -------------------------------------------- |
| `ContributedBinding` | 1695 stable, 36 unserializable, **11 unstable** |
| `NativeBinding`      | 65 stable, 248 unserializable, **12 unstable**  |
| `GeneratedModule`    | **26 unstable** (all of them)                   |
| *every other type*   | stable                                       |

- Exactly **three** fact types cannot be persisted: two carry `SemValue` closures, and `GeneratedModule` holds
  generated bytecode in a `ClassFile`, whose array compares by reference. `GeneratedModule` never surfaced as a
  trigger in §3 only because nothing downstream of it regenerated — and §13 makes it cacheable by normalizing the
  array in its codec.
- `ContributedBinding` and `NativeBinding` are *mixed* per instance — the same type is stable for a `None`
  contribution and unstable for a `Leaf`. A per-value decision would make the cache's behaviour depend on
  content; **the decision belongs to the type**.
- **`MonomorphicValue` is stable** — it carries `GroundValue` and `MonomorphicExpression`, both ordinary data.
  Those 719 facts can be *accepted* from cache, not merely drilled through.

These three types are exactly the explicit declines §13 has to carry.

## 5. Measured dead end: per-entry payload framing

The first Step 1 attempt gave each fact an independent byte frame, so a codec failing on one entry could not
mis-position a shared stream and corrupt every entry after it. It works, and it costs **7.5× on disk: 17.9 MB →
134 MB** — ≈ 8 KB per frame against ~1 KB in the shared graph. Java serialization's back-references *and* class
descriptors are **per-stream**: one shared stream writes each `ValueFQN`, `ModuleName`, `Sourced` and each class
descriptor once and refers back to it everywhere after; 16 800 independent frames re-write all of it every time.
§12 finding 3 reproduced the same result (~17×) from the write side.

> **The tension, stated once:** cross-entry structure sharing requires coupled sequential decoding; per-entry
> failure containment requires independent frames. **Plain Java serialization cannot provide both.** That is the
> case for owning the format outright (§13) — not for patching it with a shared descriptor table.

(A second symptom of that attempt — the warm build afterwards rebuilt everything, cache apparently unused —
was never diagnosed before the revert.)

## 6. The plan

### Step 1 — SUPERSEDED by §13

The original Step 1 was a representation-based `FactCodec` that chose *whether* and *in what shape* a value is
persisted while never touching a stream, so the container stayed Java's single shared graph and kept its size.
§11 and §12 then showed the container itself is the cost, so the container is what §13 replaces.

**Three rules survive into §13 unchanged**, and they are the durable part:

- The persistence decision belongs to the **type**, not to a value: §4's mixed types would otherwise make caching
  behaviour depend on content.
- **Declining** (deliberately not persisted) and **failing** (a codec error) are distinct signals and must not be
  conflated. Encoding to *zero bytes* is neither: `UpToDate` is field-less, encodes to nothing, and must stay
  comparable — it is the anchor the whole of §3.2 depends on.
- The choice is **abstract with no default**, so every key states it once. Both possible defaults are wrong:
  defaulting to persist reintroduces §3.1, and defaulting to decline would silently make a *leaf* unvalidatable —
  an edge-less declining `FileStat` disables the entire cache while everything still compiles and passes.

### Step 2 — every input-less contributor takes the `UpToDate` edge — LANDED 2026-08-03

`StdlibNativesProcessor`, `DataTypeNativesProcessor` and `MatchNativesProcessor` contribute without reading any
fact on at least one path, so their entries carried no edges and were indistinguishable from a `FileStat`. All
three got the anchor, applied uniformly at the top of the generation so early returns are covered. Measurement and
verification in §8.

The invariant to hold on to: **an empty dependency set means "world leaf, re-check me every run"**. A compiler
constant must anchor on *every* path. This cannot be automated in the engine — auto-anchoring an edge-less
generation would turn genuine world leaves into constants and never notice a changed source file again.

### Step 3 — cache declines

Extend the entry to three outcomes: `Value` / `Opaque` (materialisable, value not persisted) / `Declined`
(legitimately produced nothing). `regenerate` already has what it needs to tell them apart: a decline is an
explicit abort with **zero errors** and no registered fact; a generation that errored stays uncached so errors
keep re-surfacing. `resolve` then completes a validated `Declined` entry with `None` and runs nothing;
`computeUnchanged` drills its edges as for `Opaque`. Bump `CACHE_VERSION` (currently 34).

Caching a decline reached *through* a missing upstream stays sound: the missing key is among the recorded
edges, so if it starts producing, the decline invalidates.

§3.3's totalization removed the acute instance; this generalizes it, and is worth folding into §13's format work
rather than doing separately, since it changes the entry shape.

### Step 4 — iterate to a fixpoint

Re-measure and expect `ContributedBinding`, `NativeBinding` and `MonomorphicValue` regenerations to stay at zero.
Because of §3's cascade any survivor is likely a fourth instance of the same three classes; sweep fact values for
reference-equality fields (arrays, functions, `Ref`s) rather than waiting for them to surface.

### Step 5 — make the next regression visible

Answering "why did this rerun?" still requires hand-patching four call sites in `IncrementalFactGenerator`. Keep a
permanent DEBUG-level trace of the three changed-verdicts — value differs / no prior entry / edge-less and
unstorable — which costs nothing with the logger off. `nonLeafRegenerations()` gives the *count* today; this gives
the reason.

### Step 6 — verification

- `./mill __.test`, plus cases in `IncrementalFactGeneratorTest` / `FactCacheTest` for decline-caching, opaque
  drilling, and a **law test** asserting `restore(persist(v))` equals a *freshly recomputed* value rather than a
  retained one. Round-trip equality alone would have passed `VNative` and missed the bug.
- **LANDED**: the metric assertion itself — `WarmBuildLeafOnlyTest` compiles an unchanged tree twice in one session
  and requires `nonLeafRegenerations()` to be empty, so any contributor that forgets the anchor is caught
  automatically.
- Fast example sweep plus byte-identity comparison against a cold build — the jar must be identical whether or
  not the cache was used. (Executable only since 2026-07-31, when jar entries got a fixed timestamp and order.)
- The other direction, which matters as much: touch one source file and confirm a bounded, *correct* subset
  recompiles. Anything that widens what is accepted from cache carries under-invalidation as its risk.

### Target — REACHED (§8)

A warm build regenerates only world leaves, with no processor active but `FileStatProcessor` and
`OutputFileStatProcessor`.

## 7. Open items

- **The load floor.** With the processors silent, cache deserialization is the warm-build cost. Measured in §11,
  attacked by §13.
- **GC / retention.** §9 and §10 both leave entries accumulating with "a light LRU sweep if it ever matters" as the
  follow-up. §11 promoted it to a real lever (both load and save scale with the retained graph), and §13 makes it
  mandatory: a content-addressed store needs a mark-and-sweep from the live index roots.
- **`GeneratedModule` cacheability** — folded into §13, where a codec normalizing `ClassFile`'s array makes it fall
  out rather than being separate work.

## 8. Remeasurement after the totalization, and the Step 2 fix (2026-08-03)

The §3.3 totalization landed 2026-08-02. Re-measured on `examples.run jvm exe-jar -m DischargeDemo`, warm build
immediately after an identical one, instrumenting `regenerate` (why each fact re-ran) and `computeUnchanged` (which
dependency was judged changed):

```
Before (7588845b): regenerated 2129;  4385 ms;  MonomorphicTypeCheck 1341 ms (51.6%), 251 calls
After  (this tree): regenerated  366;   944 ms;  MonomorphicTypeCheck  214 ms (22.7%),   4 calls
```

The 366 broke down by *why* they re-ran — the taxonomy worth keeping, since a future regression will land in one of
these rows:

| count | reason              | fact type                | verdict                                            |
| ----- | ------------------- | ------------------------ | -------------------------------------------------- |
| 252   | value-leaf          | `FileStat` ×249, `OutputFileStat`, `UpToDate`, `SourceContent` | **correct** — world leaves    |
| **92**| **value-leaf**      | **`ContributedBinding`** | **BUG — §3.2**: edge-less, so read as a leaf       |
| **4** | **dep-changed**     | **`MonomorphicValue`**   | **BUG — the §3 cascade, 214 ms**                   |
| 18    | valueless-topdemand | `NativeBinding`, `ContributedBinding` | §3.1 residue, demanded top-level      |

"value-leaf" = the prior entry has a stored value but an **empty** `directDeps`, so `resolve` treats it as a world
leaf and regenerates it unconditionally. Instrumenting the changed-dependency verdict showed exactly **one**
edge-less *and* value-less `ContributedBinding` — neither a comparable value nor edges to drill — poisoning the 4
`MonomorphicValue`s that transitively depend on it and forcing the 214 ms re-typecheck.

**The fix (§6 Step 2).** Every input-less contributor got the same two lines `SystemNativesProcessor` already
carried, above the branch so hit, miss and abort-guarded paths are all covered:

```scala
getFactIfProduced(UpToDate.Key()) >> …the existing body…
```

Each such `ContributedBinding` becomes a non-leaf whose one edge is the always-equal `UpToDate` constant: `resolve`
validates it structurally and **accepts it from cache**, and the value-less one is drilled rather than
re-typechecked, which removes the cascade.

**Measured result**: **366 → 252 regenerations** — the leaf floor — with every native contributor,
`BindingMergerProcessor` and both `MonomorphicTypeCheckProcessor`s at **0 calls**. Wall time 944 → 724 ms. The
projected ~18 value-less §3.1 demands vanished too: they were demanded only *because* the 4 `MonomorphicValue`s
regenerated. **Step 1 is therefore not needed for regeneration count at all — only for the load floor.**

**Verification.** `./mill __.test` green; byte-identical cold-vs-warm jar; touch-one-source moved the warm count
252 → 357 (a bounded subset) with correct output, and back on restore. The `UpToDate` read stays
`getFactIfProduced` (tolerant): a minimal test bundle without `UpToDateProcessor` loses incrementality here but
never fails.

Reproduction: enable `<Logger name="com.vanillasource.eliot.eliotc.compiler.IncrementalFactGenerator"
level="debug"/>`, build twice, read the `Incremental run: regenerated N` line. (Since §9, `--statistics` no longer
perturbs the config fingerprint, so a warm build can be observed with it on.) The per-reason breakdown came from
temporary traces, reverted — §6 Step 5 would make it standing.

## 9. Per-configuration cache files (2026-08-05)

The cache was a single `<target>/.eliot-cache` whose header carried both fingerprints; a run whose header did not
match got a cold build and then **overwrote** the file. Two configurations sharing a target directory — prod and
test, two different `-m` mains — cleared each other on every switch, and neither was ever warm.

The fix keys the two fingerprints by their **opposite lifecycles**:

- the **config** fingerprint (rarely changes; variants should coexist) goes into the **file name**,
  `.eliot-cache-<config>`, so variants keep separate files under one target;
- the **compiler** fingerprint (changes on every recompile; an old-compiler cache is dead weight) stays in the
  **header**, so a compiler change fails the header check and `save` overwrites the *same-named* file in place —
  no new file per compiler build, no accumulation from compiler churn. The full config value is still in the
  header as an exact-match backstop, so the truncated, sanitized name is only an index: a name collision degrades
  to a cold build, never to serving the wrong cache.

This preserves the `UpToDate` soundness argument — each file is inherently scoped to one compiler+config, so an
anchored constant can never leak across a config boundary.

Paired change: `CacheFingerprint.config` **excludes the diagnostic-only flags** `--statistics` and
`--visualize-facts`. They observe every processor invocation but change no fact, so folding them in gave a
diagnostic run a fresh config — you could not even observe a warm build with `--statistics` on. `CACHE_VERSION`
32 → 33.

Known residual: a *permanently* retired config leaves its file behind. Bounded by real config diversity, not build
count; the sweep is §7.

## 10. One accumulating cache across examples (2026-08-05)

§9 gave each `-m` main its own file, so two examples never *replaced* each other — but never *shared* either: every
example cold-built the whole stdlib/prelude subgraph the first time. This step makes them share **one** file that
**accumulates**. Two independent mechanisms, both required:

1. **The selected `main` stops splitting the cache file.** `mainKey` moves from `namedKey` to
   `Configuration.demandScopedKey` (`docs/cache-identity-configuration.md` §5), so `CacheFingerprint.config` yields
   the *same* fingerprint for every main over the same roots/backend. Nothing else carries the main into the
   fingerprint, so this one reclassification fully collapses the dimension; a different backend or source set still
   gets its own file.
2. **`buildCacheData` accumulates instead of pruning.** It now **retains every prior entry the run did not touch**,
   merged under this run's fresh and carried entries, so another example's monomorphic values survive a build that
   never asked for them.

**Why this is sound — the demand-scoped invariant.** A `demandScopedKey` may exclude itself from the identity only
because the value it injects reaches the fact graph through exactly one **edge-less, always-regenerated leaf**: for
the selected main, the synthetic-entry `SourceContent(eliot-synthetic:main.els)` written by
`SyntheticMainSourceProcessor`. `resolve` never serves a leaf from cache, so every main-dependent fact is reached
from that leaf through recorded edges and is invalidated on demand the moment the main differs. A cache shared
across mains therefore *self-heals*. Facts whose key already encodes the main coexist under distinct keys, and
retained entries are never served stale — the next demand re-validates their dependencies down to the world leaves.

The invariant is the checkpoint for any future `demandScopedKey`: a key may be demand-scoped only if its whole
effect on facts is confined to in-key facts and self-healing edge-less leaves. A key whose value changes a *shared*
fact without an invalidating leaf in the path would miscompile under sharing; it must stay `namedKey`.

**The one subtlety in the accumulation.** Retention excludes every key the run *resolved* (`directDependencies`) or
*carried*, not merely those it materialised. A key regenerated to a **failure** this run is in `directDependencies`
but not in the fresh map, so it is dropped rather than kept at its stale prior success — otherwise a later run
whose inputs are stable at the failing value would accept the stale value and swallow the error.
`IncrementalFactGeneratorTest` pins both directions.

No `CACHE_VERSION` bump (the persisted shape is unchanged). Growth is §9's tradeoff one step larger, and the
resident-server (LSP) in-memory cache now accumulates across edits for the same reason.

## 11. The missing time is the cache, not the compile (2026-08-05)

With regeneration at the leaf floor, "where does a warm build's wall time actually go?" could no longer be answered
from `--statistics`: the report only spanned `compileOnce`, but the two dominant costs sit *outside* it. Cache
**load** happens in `CompilationSession.create`, before the window opens; cache **save** in `persist()`, after it
closes; the fingerprint digest and the `buildCacheData` merge were folded anonymously into the engine remainder.

`--statistics` now accounts for the **whole session lifecycle** (setup → persist), with four coarse cache phases as
explicit lines. An always-on `statistics/PhaseTimings` records them regardless of the flag — four wall-clock
measurements per run, negligible next to the phases themselves, unlike the per-invocation processor wrapping that
stays opt-in — and `ProcessorStatistics.report` subtracts them from the engine remainder so **every millisecond
still lands in exactly one row**.

Measured on the `DischargeDemo` warm build immediately after an identical one:

```
Compiler statistics: 2,985 ms total, 92 ms (3.1%) in processors
     75  FileStatProcessor          } the leaf floor — 249 stats + 1 output digest
      4  OutputFileStatProcessor    }
    156  (offering every key to every processor)     — dispatch
    110  (computing cache fingerprints)
    888  (loading the incremental cache)             — 29.7%
    111  (building the incremental cache graph)
    912  (persisting the incremental cache)          — 30.6%
    716  (compiler engine, plugin setup and i/o)
```

**~60% of a warm build is cache load + save (1,800 of 2,985 ms), and the processors do essentially nothing
(92 ms).** The load floor is no longer a suspected cost; it is the measured one, and it is the
deserialize/serialize of the single Java-serialized graph. Save is now as expensive as load because §10 made the
persisted graph *accumulate*, so both scale with the retained cache rather than with the current main.

Instrumentation only: no fact shape, cache format or `CACHE_VERSION` change.

## 12. Spike: a per-entry store (H2 MVStore), and what it measured (2026-08-06)

§11 pinned the warm-build cost to cache load + save. The obvious next idea — the store *remembers what it already
holds*, so an unchanged tree writes nothing and a one-fact change writes one fact — was prototyped rather than
argued about. The spike is **committed and env-gated**: it changes nothing unless explicitly measuring.

**What was built.** An `IncrementalCacheBackend` seam (`eliotc/src/…/compiler/cache/`) behind the session's
load/save. `JavaSerializationCacheBackend` is the default, delegating to the existing whole-graph `FactCache`.
`MvStoreCacheBackend` (selected by `ELIOT_CACHE_BACKEND=mvstore`) keeps one H2 MVStore open for the session and
stores each entry under its own content key, `base64(serialize(factKey)) → serialize(entry)`. On save it diffs
every entry against a **held snapshot** by value equality: an entry equal to its stored form is neither
re-serialized nor rewritten; only new/changed entries are `put` and gone ones `remove`d; `commit` flushes just the
pages those touched.

**Measured** (`examples.run jvm exe-jar -m DischargeDemo`, warm after an identical build, both backends on the same
machine; absolute figures run higher than §11's because it is a different box, and this tree's cache is 2.49 MB
against §5's 17.9 MB `eliot-build` tree):

```
phase            Java (default)     MVStore spike
cache load       1333 ms  30.7%     3113 ms  66.4%
cache save        978 ms  22.5%       28 ms   0.6%
build-cache       178 ms              72 ms
fingerprint       130 ms             146 ms
engine           1210 ms            1016 ms
processors        186 ms             108 ms
total            4342 ms            4687 ms
on-disk          2.49 MB            42 MB  (~17×)
```

**Three findings:**

1. **The delta-write hypothesis is confirmed.** Save collapses **978 → 28 ms**: a warm build writes essentially
   nothing. Every world-leaf fact — re-computed each run, so a fresh object — still compares value-equal to its
   stored form and is skipped, and the retained subgraph (§10) is untouched by construction.

2. **Per-entry storage alone makes *load worse*, not better (1333 → 3113 ms).** `load` still eagerly deserializes
   every entry, so per-entry framing only piles store overhead onto a bloated file. The load floor is not cut by
   *where* entries live; it is cut by not materializing the values a warm build's validation walk never needs —
   most facts are drilled through their edges or compared, not returned.

3. **Per-entry framing reproduces the §5 disk blowup (~17×).** Independent frames re-emit the shared `ValueFQN` /
   `ModuleName` / `Sourced` / class descriptors that one shared stream writes once.

**What it decided.** The engine was never the bottleneck — MVStore's diff/commit machinery delivered the delta
write on its own. The per-entry *framing* and the *eager load* are the costs, and both are properties of Java
serialization, not of the store. That is what §13 acts on; the earlier reading of these findings (build a shared
symbol/class-descriptor intern table beside the frames) is **superseded** — see §13's "what this makes obsolete".

**Handover notes.**

- **Turn it on:** `ELIOT_CACHE_BACKEND=mvstore` (unset / anything else ⇒ the Java graph). Cold run to populate,
  then a warm `--statistics` run; read the four cache-phase lines (§11). File: `<target>/.eliot-cache-mv-<config>`.
- **Dependency:** `com.h2database:h2-mvstore:2.2.224` on `eliotc` — pure-JVM, no native library.
- **Tests:** `eliotc.test` green; the default path is byte-for-byte unchanged (the flag is off). There is **no**
  test of the MVStore path itself — it is a spike.
- **Deliberate spike limitations:** load is eager (finding 2); the store is never explicitly closed (no session
  teardown hook — it relies on `commit` for crash-consistency and process exit to release the lock); the content
  key assumes deterministic serialization of equal first-order fact keys (a nondeterministic key orphans a row — a
  size leak, never a correctness fault); the small §3.1/§4 unstable set is rewritten every save.
- **Files:** `IncrementalCacheBackend`, `JavaSerializationCacheBackend`, `MvStoreCacheBackend`,
  `FactSerialization`, and the two-call-site wiring in `CompilationSession`.

## 13. The plan: explicit codecs over a content-addressed object store (2026-08-06)

§§11–12 leave two costs that look like two problems with two fixes — a shared intern table for save, a graph/value
split for load. They are one fix. **Own the format:** give every persisted type an explicit codec, and store
objects in a flat `id → bytes` map where every reference is an id.

### The shape

- **Explicit codecs, no Java serialization.** A codec writes its own fields and the type tag is a byte, so there
  are no class descriptors in the data at all — most of §5's 8 KB per frame.
- **The id is the hash of the object's own bytes, with children written as their ids.** An object is therefore
  stored at most once, ever — globally and *across runs*, where the shared graph dedups only within one file write.
- **Entries become an index**: `factKeyId → (valueId, depIds)`. That index is the only thing a load must read.

### Six properties that fall out rather than being engineered

1. **Duplicate storage is impossible by construction** — not "an intern table catches the common ones": writing an
   object whose id exists is a no-op. Monomorphization emits structurally identical subtrees everywhere, so the
   cache should land *below* today's 2.49 MB rather than merely back at it.
2. **No class descriptors**, so §5's dominant term disappears instead of being tabulated.
3. **Delta-write is free.** Nothing to diff: the spike's held snapshot, its value comparison, and even the
   refinement of driving save from the engine's regenerated set all become unnecessary.
4. **Lazy load falls out** — read the index, chase ids only for values actually consumed.
5. **The equality cutoff stops materializing anything.** This is what cuts load, and no Java-serialization layout
   offers it. §1's "recompute and compare" today deserializes the stored value to compare against the recomputed
   one; with content ids you hash the recomputed value and compare 16 bytes, never touching the stored side. Most
   warm-build facts are *validated*, not consumed, and today every one of them pays a full deserialize.
6. **`GeneratedModule` becomes cacheable** (§4, §7): its codec normalizes `ClassFile`'s array to a sequence, so the
   type stops being reference-compared. The same move makes §2's equality-stability a codec law that can be
   enforced instead of an accident of the serializer.

### What it costs, honestly

The reachable surface is ~300 case classes, 9 enums and 42 fact key types, nearly all in `lang`. Hand-writing 300
codecs is not the project — **Scala 3 `Mirror` derivation is**, covering products and sums of primitives, strings,
collections and each other mechanically. **The whole risk is what refuses to derive**, which is why that is the
first thing built. Expect the leaves — `Sourced`, `Path`/`URI`, `BigInteger`, `ClassFile`'s array — plus the three
§4 declines.

Three obligations the current format does not have:

- **GC.** Rewriting the whole graph implicitly collected garbage; a content store accumulates and needs a
  mark-and-sweep from the live index roots. §7's retention item becomes mandatory rather than a lever.
- **Hashing on write** — memoize by object identity within a run, so the cost is proportional to new objects rather
  than to the retained graph.
- **A collision is a correctness fault**, so the id must not be truncated below 128 bits.

Two things get *easier*. Versioning is a non-problem: `CacheFingerprint.compiler` already discards the cache on any
compiler change (§1), so codecs may change freely and there is no migration story to write. And the format stops
storing what it does not need, which is the whole reason Java serialization's output is an order of magnitude above
the information content.

### What this makes obsolete

- **The shared symbol / class-descriptor intern table** (§5's blockquote, §12 finding 3). It exists only to make
  Java serialization tolerable and dies on contact with owning the format. **Do not build it.**
- **Chunked / bucketed frames** — the cheaper variant of the same idea, obsolete for the same reason.
- **The representation-based `FactCodec` as a container design** (§6 Step 1). Its three decision rules survive; its
  container does not.

The MVStore spike keeps its value: it proved the write side and left the `IncrementalCacheBackend` seam this lands
behind. MVStore remains a fine engine for `objects: id → bytes` plus `entries: keyId → (valueId, depIds)`.

### Build order (de-risking first)

1. **Derivation prototype over one fact family** (the `resolve` phase facts) and enumerate what refuses to derive.
   This is the go/no-go: if the refusal set is the leaves plus the known declines, the rest is mechanical.
2. **Codec + id + store layout** behind the existing backend seam — index loaded eagerly, values written and read
   lazily. Fold in §6 Step 3 (cache declines) here, since it changes the entry shape anyway.
3. **Switch the equality cutoff to id comparison** — the load win, and the only step that changes what "unchanged"
   is *decided from*.
4. **GC sweep** from the live index roots.
5. **§6 Step 6 verification**, weighted toward under-invalidation: byte-identity cold-vs-warm, and
   touch-one-source-file for a bounded *correct* subset. Step 3 above changes the invalidation decision itself, so
   that direction matters more here than in any previous step.
