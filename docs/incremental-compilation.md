# Incremental Compilation: Why a Warm Build Still Computes

Status (2026-08-06): **the store works and is measured; it is not yet the default.** Regeneration has been at the
leaf floor since §8; the cost was I/O, with §11 measuring **~60% of a warm build in cache load + save**. §13's plan —
explicit per-type codecs over a content-addressed object store, replacing Java serialization outright — is built
through step 4, and behind `ELIOT_CACHE_BACKEND=store` a warm build is now **half** what it was, with cache I/O down
from ~64% of it to ~29%:

| Step | State |
| ---- | ----- |
| 1 — the encoding, coverage proven by the compiler | **DONE**, §14. 0.55× the Java graph with sharing, 4.89× without. |
| 2 — the fact model made encodable, decision on the key | **DONE**, §15. `CompilerFactKey.valueCodec`, abstract, no default. |
| 3 — the content-addressed store | **DONE**, §17. Wired behind `ELIOT_CACHE_BACKEND=store`. |
| 4 — lazy values, the cutoff by content id | **DONE**, §18. Warm build **50% faster** (load −79%, save −76%), **half the disk**, **zero growth**, jar byte-identical, under-invalidation checked. |
| 5–6 — compaction, flip the default | not started. |

The **default is still the Java-serialized whole graph**, so an ordinary build is unchanged until step 6 flips it.
§17 and §18 are the current state; §16 is the handover that preceded them and is superseded on two points it names.

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
  `eliot.lang.String` natives. **Still live** under the Java-serialized cache, but no longer drives regeneration count
  (§8); §15 turned it into an explicit decline on the fact key, so under the codecs it can no longer be written at all.
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
  trigger in §3 only because nothing downstream of it regenerated. **`GeneratedModule` is fixed since §14** —
  `ClassFile` compares its bytes — leaving two, which are exactly the two declines §15 states on the key.
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
- ~~**`GeneratedModule` cacheability**~~ — **CLOSED (§14)**: `ClassFile` now compares its bytes rather than its
  array reference, so generated bytecode is equality-stable and acceptable from cache.

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
2. **No class descriptors** at all, so that whole term disappears instead of being tabulated. (§5 read the
   per-entry blowup as "back-references *and* class descriptors"; §14 separates them and finds the sharing term is
   by far the larger — see the correction there.)
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

**The one hard sequencing constraint, from §14:** explicit codecs and content addressing **ship together**. Codecs
alone measured 4.89× *worse* than the Java graph, and only 0.55× once sub-values are shared — so a release that has
the first without the second puts a fivefold regression on disk. Everything below is one step 2 in that sense; the
numbering is the order to build in, not the order to land in.

1. **Derivation prototype, coverage proven by the compiler** — done and measured, §14. The refusal set is the leaves
   plus the known declines, and the numbers clear the go/no-go.
2. **Make the fact model encodable, and put the decision on the type** — done, §15.
3. **Codec + id + store layout** behind the existing backend seam — content-hash ids in place of the in-stream
   back-references, `entries: keyId → (valueId, depIds)` as the index, index read eagerly and values lazily. The
   `Output`/`Input` seam already localises sharing to one place, so this touches neither the codecs nor the
   derivation. Fold in §6 Step 3 (cache declines) here, since it changes the entry shape anyway.
4. **Switch the equality cutoff to id comparison** — the load win, and the only step that changes what "unchanged"
   is *decided from*.
5. **GC sweep** from the live index roots.
6. **§6 Step 6 verification**, weighted toward under-invalidation: byte-identity cold-vs-warm, and
   touch-one-source-file for a bounded *correct* subset. Step 4 above changes the invalidation decision itself, so
   that direction matters more here than in any previous step.

## 14. Step 1 measured: the codec prototype (2026-08-06)

§13's first step — build the encoding, let the compiler prove coverage, and measure — is done. Nothing about a real
build changes: `FactCodec` is a new type in `eliotc` that nothing calls, and the instances and harness live in
`jvm/test`. Two small main-source changes did land, both forced by findings below.

**What the compiler proved.** `FactCodecs` names one instance per type reachable from a fact — **299 derived, 2
hand-written, ~16 leaf instances** in `FactCodec` itself. Because a field whose type has no instance is a compile
error, coverage is not a claim: **39 of the 41 fact types encode, and the 2 that do not are exactly §4's prediction**
(`ContributedBinding` and `NativeBinding`, whose `BindingContribution.Leaf` carries a `SemValue` closure). Derivation
covers products, sums, enums and singletons with no per-type thought; deriving all 299 costs about **1 s of
`jvm.test` compile time** (2.1 s against 1.1 s for an ordinary file), which is not a cost worth weighing.

**The size result, and it inverts §5's reading.** Measured over the 1,659 facts of a small real build:

```
java serialization, one shared graph:      1,979,690 bytes   (baseline)
explicit codecs, independent frames:       9,673,313 bytes   4.89×
explicit codecs, shared sub-values:        1,090,704 bytes   0.55×
```

Two things follow, and the second is a **correction to §5 and to §13's claim 2**:

1. **Explicit codecs alone are a 4.89× regression.** Removing every class descriptor, tagging sums with one byte and
   varint-encoding all integers still loses badly to `ObjectOutputStream` — because independent frames give up
   structure sharing, and `ObjectOutputStream` back-references aggressively.
2. **Structure sharing, not class descriptors, is the dominant term.** §5 attributed the per-entry blowup to
   "back-references *and* class descriptors" and §13 called descriptors "§5's dominant term". With descriptors at
   *zero* the penalty is still 4.89×, so the descriptors were the smaller half. Sharing the leaves — `String`, `URI`,
   `Path` — mattered as much as sharing the products: a compiler's facts repeat the same names, module paths and file
   URIs thousands of times, and every `Sourced` carries a full file URI.

**What that means for §13: the design is confirmed, the step order is not optional.** Content addressing is not an
optimization layered on explicit codecs — it is the thing that makes them viable at all. Shipping codecs first and
adding ids later would put a 4.89× regression on disk in between. They land together or not at all. With sharing,
codecs come out **45% below** the Java graph *before* any cross-run dedup, which is the property §13 claim 1 rests
on and the first evidence that the store can be smaller than today's 2.49 MB rather than merely equal.

The sharing measured here is by **value equality within one stream**, which is the byte-level model of the
content-addressed store (a value equal to one already written costs a back-reference). The real store replaces the
back-reference with a content-hash id, which additionally dedups **across runs** — so this number is a floor, not a
ceiling.

**Two findings the plan did not predict**, both surfaced by the compiler refusing to derive:

- **`GroundValue.Direct(value: Any, …)` has an untyped payload slot.** No codec can be derived for `Any`, and
  `GroundValue` is reachable from most of the monomorphize layer, so this one field blocks a large part of the fact
  surface. At runtime the slot holds exactly three shapes (`BigInt`, `String`, `Boolean` — as `PostDrainQuoter`'s
  scaladoc already documents), so the prototype encodes it with a discriminating tag and fails loudly on anything
  else. **The real fix is `Direct(value: Literal)` over a small sealed sum**, which is a mechanical but
  cross-cutting change to the checker's pattern matches; it is step 2 work, not a blocker.
- **`core.fact.Expression` was an unsealed `trait`** whose cases all live in its own companion, so it had no `Mirror`.
  Sealed now — a one-word change the compiler verifies, with no behavioural effect.

**One open item closed.** §7 listed "`GeneratedModule` could be made comparable by giving `ClassFile` a
value-comparable representation". The law test failed on exactly those 8 facts, so `ClassFile` now compares its
bytes rather than its array reference (a `case class` with an explicit `equals`/`hashCode`). This is sound because
code generation is deterministic — the same property the byte-identity build guard already relies on — and it makes
generated bytecode **acceptable from cache** rather than merely drillable. All of `__.test` is green with it.

**What the harness checks, standing** (`FactCodecConformanceTest`, three assertions over a real build):

- every fact type a build materialises is either covered or explicitly declined;
- every covered fact round-trips to an **equal** value — the half of §2 that reaches runtime. The other half (a value
  that round-trips equal but *recomputes* different, i.e. `VNative`'s lambda) cannot reach the harness at all,
  because no `FactCodec` exists for a function. That is the fail-safe property the whole approach turns on: a
  reflective walker would encode both silently.
- the shared encoding is smaller than Java's shared graph.

**Where step 2 picks up.** Move the instances onto the types as `derives FactCodec` (deleting `FactCodecs`), replace
`Direct(value: Any)` with a sealed literal, then build the store: content-hash ids in place of the in-stream
back-references, `entries: keyId → (valueId, depIds)` as the index, values read lazily. The codec's `Output`/`Input`
already localise sharing to one place, so the swap from "back-reference within a stream" to "id into an object
table" touches neither the codecs nor the derivation.

## 15. Step 2: the persistence decision moves onto the fact key (2026-08-06)

§14's prototype proved the encoding; this makes it part of the compiler rather than of a test. Still nothing reads or
writes a cache differently — the store is step 3 — but after this the codecs are main-source and the decision is
stated where §6 Step 1 says it belongs.

**`GroundValue.Direct(value: Any)` is now `Direct(value: Literal)`** over a sealed enum of the four shapes that ever
occupied the slot: `IntegerValue`, `StringValue`, `BooleanValue`, `UnitValue`. The `Any` was not merely unencodable;
it meant no consumer could match exhaustively, and the defensive arms show it — `IntRepresentation.directBigInt` was
accepting `BigInt`, `java.math.BigInteger`, `Int` and `Long`, three of which could never occur. Construction sugar
keeps the call sites where the payload type is statically known unchanged; every *pattern* site is explicit, because
matching is where the closed set has to be visible. The compiler found all of them — an unreachable type test is an
error, not a warning — and `Direct` now derives its codec like everything else, so §14's hand-written stand-in is gone.

**`CompilerFactKey.valueCodec` is abstract with no default.** Every fact states its own persistence decision once:

```scala
case class Key(uri: URI) extends CompilerFactKey[SourceTokens] {
  override def valueCodec: Option[FactCodec[SourceTokens]] = Some(LangFactCodecs.sourceTokensCodec)
}
```

`None` is a **decline**, not a failure: the fact still takes part in change detection through its edges (§1's
structural drill), it just cannot be handed back from cache. Two facts decline, and the conformance test pins that it
is exactly those two — `ContributedBinding` and `NativeBinding`, whose `SemValue` closures are §3.1. A new fact type
that states nothing does not compile, which is the whole point: both possible defaults are wrong (§6 Step 1), and the
"defaulting to persist" one is the bug that started this document.

**The structural instances are grouped per layer, not written on each declaration** — `CoreFactCodecs` (7),
`LangFactCodecs` (199), `JvmFactCodecs` (4), each importing the ones below it. This is a deliberate departure from
"`derives FactCodec` on every type", for a reason worth recording: those 199 are *structural* types, not facts, and
have no persistence decision to make — the decision is a property of the fact, and that is exactly where it now lives.
Grouping them also keeps the compile-time coverage proof readable as one list. What made the split possible at all is
that `FactCodec.derived` now builds a sum's case codecs from their own `Mirror` instead of summoning an instance per
case (302 named instances → 210): an `enum` case cannot carry a `derives` clause, so a per-case requirement would have
made per-declaration derivation impossible for every sum in the fact model.

**The conformance test now checks the shipped decision**, not a test-side registry: it reads each fact's codec from
its own key. Its three assertions are the declines being exactly the two expected, the round-trip law over every
covered fact, and the shared encoding beating the Java graph (1,093,088 vs 1,979,690 bytes — 0.55×, unchanged by this
step). `__.test` is green.

**Step 3 can now be written against `key.valueCodec`.** The one thing it still needs and this step does not provide is
a **stable type tag**: encoding a heterogeneous fact stream is fine from the key, but *decoding* one has to pick a
codec before it has a key, so the store needs a tag→codec table that survives across runs. That is a store concern,
not a fact-model one, and it is the first thing step 3 has to settle.

## 16. Handover: what exists, and what step 3 has to decide first (2026-08-06)

### What is on disk, and what it is wired to

| File | What it is | Wired into a build? |
| ---- | ---------- | ------------------- |
| `eliotc/…/cache/codec/FactCodec.scala` | the encoding: trait, `Output`/`Input` (sharing lives here), leaves, containers, `derived` | no |
| `eliotc/…/cache/codec/CoreFactCodecs.scala` | 7 structural instances (positions, the cache's boundary facts) | no |
| `lang/…/codec/LangFactCodecs.scala` | 199 structural instances — the bulk of the fact model | no |
| `jvm/…/jvm/codec/JvmFactCodecs.scala` | 4 instances + the hand-written `ClassFile` codec | no |
| `CompilerFactKey.valueCodec` | the per-fact decision, abstract with no default | **yes** — every fact key implements it |
| `jvm/test/…/codec/FactCodecConformanceTest.scala` | the three standing checks (declines, round-trip law, size) | test only |
| `eliotc/…/cache/{IncrementalCacheBackend, JavaSerializationCacheBackend}.scala` | the load/save seam and today's default | **yes** |
| `eliotc/…/cache/{MvStoreCacheBackend, FactSerialization}.scala` | the §12 spike, `ELIOT_CACHE_BACKEND=mvstore` | env-gated only |

`valueCodec` is the only part that a build compiles against, and it computes nothing at run time. Everything else is
inert until a backend calls it.

### The first decision: a stable type tag

Encoding a heterogeneous fact stream works from the key — you have the fact, so you have its codec. **Decoding does
not**: the reader has to choose a codec *before* it has a key, from a tag in the bytes. So the store needs a
tag → codec table, and the tag has to mean the same thing on the next run.

Two properties it must have, and they pull against each other:

- **Stable across runs**, or the whole cache is unreadable after an unrelated change. A tag derived from ordering (a
  `Seq.indexOf`, as the conformance test's write-only codec uses) is not: adding one fact type shifts every tag after
  it. A tag derived from the fact class's *name* is stable, costs a shared string once given the intern table, and
  degrades safely — an unknown name is a cache miss, not a mis-decode.
- **Complete, or a hard error.** A fact type absent from the table must fail loudly at *save*, never write a value
  nothing can read back. `CompilerFactKey.valueCodec` is compile-time-complete for the *value*; the tag table is a
  runtime map, so it needs its own guard. The conformance test's decline assertion is the model to extend.

Note that `CacheFingerprint.compiler` already discards the whole cache on any compiler change (§1), so tags need to
survive only within one compiler build. That makes "hash of the class name" sufficient and removes any migration
story — worth remembering before designing something more elaborate.

### The order that matters, and the trap

§14 measured explicit codecs **without** sharing at 4.89× the Java graph and **with** sharing at 0.55×. So:

> **Do not land the codecs as a cache format before content addressing works.** The intermediate state is a fivefold
> disk regression, and it is a state that looks finished — the encoding is complete, the tests pass, the format is
> "modern". The sharing is not a follow-up optimization; it is the half that pays.

The sharing model is already localised: `FactCodec.Output.shared` / `Input.shared` is the *only* place a repeated
value is collapsed. Swapping the in-stream back-reference for a content-hash id into an object table touches that one
pair of methods and nothing else — not the codecs, not the derivation.

### What is left after step 3, unchanged from §13

Steps 4–6 of §13's build order: the equality cutoff becoming an id comparison (the load win, and the only change to
what "unchanged" is *decided from*), the GC sweep from the live index roots, and §6 Step 6's verification weighted
toward under-invalidation. §6 Step 3 (caching declines) should be folded into step 3, since it changes the entry
shape anyway.

### Two things to retire when step 3 lands

- **The MVStore spike** (`MvStoreCacheBackend`, `FactSerialization`, the `h2-mvstore` dependency). Its finding is
  banked in §12 and its layout is refuted by §14; MVStore itself may still be a fine engine for `id → bytes`, but the
  per-entry-Java-frame code has no future. Delete it rather than leave a second, wrong cache path behind an env var.
- **`FactCache.canSerialize`** and the trial-serialization probe (§2), once nothing reads the Java graph. It is the
  mechanism every §3 defect came from, and it has no place beside a compile-time-complete decision.

### Where the numbers came from, if they need re-running

`./mill jvm.test.testOnly com.vanillasource.eliot.eliotc.jvm.codec.FactCodecConformanceTest` prints the three sizes
and a per-fact-type breakdown for a small real build (1,659 facts). §11's four cache-phase lines come from
`--statistics` on a warm build; §12's handover notes say how to run the MVStore comparison while it still exists.

## 17. Step 3, first half: the object encoding, and why references are offsets (2026-08-06)

§13 specified the store as `id → bytes` with **children written as their ids**, and §16 made "a stable type tag" the
first thing to settle. Building it measured both away. What §13 got right is the shape — content-addressed objects,
dedup by construction, lazy chase — and that shape now exists and round-trips. What it got wrong is the reference.

**Measured, over the same 1,659 facts of §14, against the shared stream (1,090,464 bytes) rather than against Java,
because §14 already established the stream as the bar:**

```
explicit codecs, one shared stream:        1,090,464 bytes   (baseline)
content-addressed, 16-byte ids inline:     6,239,311 bytes   5.72×
content-addressed, varint object numbers:  1,173,712 bytes   1.08×
content-addressed, varint byte offsets:      980,192 bytes   0.90×
```

**§13's literal layout is a 5.7× regression, and the distribution says why.** Of 61,653 objects in a real build's fact
graph, **48,607 — 79% — are referred to exactly once**, holding 84% of the body bytes. Compiler facts decompose into
*tiny* objects: ~15 bytes each. A 16-byte reference does not annotate such a body, it doubles it, and four in five of
them are pointed at from exactly one place, so the reference can never be amortised over reuse. The multiply-referred
minority (12,370 objects, 158 KB) is where all the sharing value is, and it is small.

**The fix is to separate the two jobs an id was doing.** They pull in opposite directions and only one of them has to
be on disk:

- **Identity** — `ObjectId`, a 128-bit Merkle hash: the object's own bytes with each child contributing *its id*,
  never a position. Nothing run-specific enters it, so it means the same thing in any store. This is what dedup keys
  on and what lets a recomputed value be compared to a stored one by 16 bytes without reading the stored side (§13
  claim 5). It is computed during encoding and **never written into a body**.
- **Location** — the object's **byte offset** in the append-only body region, varint-encoded. Stable for exactly the
  reason a sequence number is (the region only grows), but *self-locating*: unlike numbers, offsets need no table to
  be written or read, which matters precisely because a per-object table entry is the cost that 79% of objects cannot
  carry. That is the whole gap between 1.08× and 0.90×.

All six of §13's properties survive this, because none of them was about the reference width. The store is 0.90× the
shared stream and 0.49× Java **before any cross-run dedup**, so §13 claim 1 — that owning the format lands *below*
today's 2.49 MB rather than merely level with it — now has a second measurement behind it.

### The type tag dissolves, and a subtler hazard replaces it

§16 required a stable tag → codec table because "decoding has to pick a codec before it has a key". With the store
laid out this way it does not: a fact's value is decoded with the codec its own key states, and every object below it
is reached structurally by a codec that already knows what it is reading. **No tag appears anywhere in the format.**

**What is left of the tag is keys, and only keys.** A fact's value is decoded by the codec its own key states; a
*key* has nothing above it to say what it is, and the validation walk must materialise dependency keys to ask the
engine to recompute what they identify. So keys are stored beside their class name, mapped back through
`FactKeyCodecs.Registry` — assembled per layer beside the structural instances, exactly as §16 specified: stable
across runs (a name, not an ordering), and complete-or-loud, since `valueCodec`'s compile-time completeness cannot
extend to a table that is consulted before a key exists. The conformance test pins that every key type a real build
materialises is registered and reads back equal. The two **declining** facts appear in the table like any other: a
decline withholds the value, never the edges, so its key is still stored and still walked.

What does need care is the opposite of what §16 expected. **The store is untyped on purpose**, and two different types
routinely encode to identical bytes — a `URI` and its own string form, `Sourced[String]` and `Sourced[Token]`. Sharing
their *storage* is sound: the bytes are the same and the reader always supplies the codec. Sharing a *reader's cache*
of decoded values is not, and that is a live bug, not a hypothetical — it is what the round-trip law caught on the
first build, across a dozen fact types. `ContentAddressedInput` therefore keys its memo on **offset and reader**, the
reader being an identity token each derived codec carries. Recording the shape, since the instinct is to reach for a
class name: a class name does not discriminate generic types, and `Sourced[A]` alone makes that routine.

Two mechanical traps, both worth naming because both look like working code:

- **`mutable.HashMap.getOrElseUpdate` may not be used here, on either side.** Computing an object's id writes its
  children, and decoding an object decodes its children — both re-enter the very map the update is being placed in.
  The symptom is not an exception; it is a silently wrong graph.
- **A `lazy val` cannot tie the self-reference knot** for a codec that names itself, because the builder forces it
  while initialising. A plain identity token is what the reader memo actually needs.

### What exists now, and what step 3 still owes

New in `eliotc/…/cache/codec/`: `ObjectId`, `ContentAddressedOutput` (write: frames, Merkle hashing, dedup, append),
`ContentAddressedInput` (read: follow an offset, memoize per reader). `FactCodec.Output`/`Input` are now traits with
`Plain` / `Sharing` implementations beside the content-addressed pair — the swap §16 predicted would touch one pair of
methods did touch exactly that pair, plus the `shared` signature on the read side. `FactCodecConformanceTest` gains
the store round-trip law and the size assertion. **Still nothing a build does has changed**: no backend uses this yet.

### The store as a backend, and what it measures (2026-08-06)

`ContentAddressedCacheBackend` is wired behind the existing seam and selected by **`ELIOT_CACHE_BACKEND=store`**, the
default still being the Java graph. Two files with opposite lifecycles, which is why they are two:

- **the body region** (`.eliot-objects-<config>`) is **append-only**. An offset written last run resolves this run
  precisely because nothing before it ever moves; a save appends what is new and rewrites nothing.
- **the index** (`.eliot-index-<config>`) is rewritten every save, and is the only thing a load must read in full.

**A carried-forward value is never re-encoded**, and this is not an optimisation — it is what makes the append model
work at all. On a warm build every entry survives, so re-encoding would append the whole graph every run. An entry's
value is written only where this run produced a *different object* for it, recognised by **reference**, since a value
carried out of the store is literally the object that was read. `ContentAddressedCacheBackendTest` pins it: saving
back what was loaded appends zero bytes.

An entry is placed **whole or not at all**. A key type with no registered codec cannot be read back, and dropping
merely the *dependency* naming it would leave an entry claiming fewer inputs than it has — under-invalidation, the one
failure direction that yields a wrong build rather than a slow one.

**Measured** (`examples.run jvm exe-jar -m DischargeDemo`, warm after an identical build, same machine, same tree as
§12):

```
phase              Java (default)      content store
cache load           537 ms  35.9%      276 ms  29.7%
cache save           395 ms  26.4%       97 ms  10.4%
build-cache           35 ms              33 ms
fingerprint           49 ms              46 ms
engine               375 ms             366 ms
processors            31 ms              29 ms
total              1,494 ms             930 ms        -38%
on-disk             2.49 MB            1.26 MB        0.51×
```

**Load −49%, save −75%, the whole warm build −38%, and the jar is byte-identical** across java-warm, store-cold and
store-warm. Note what has *not* been done yet: load still materialises every value eagerly, and the equality cutoff
still deserialises and compares. Both are §13 step 4, so the load column has considerably further to fall.

**Growth, measured rather than estimated**: a warm build appends **~17.9 KB**, 1.4% of the store — exactly the world
leaves, recomputed to equal-but-fresh objects that reference identity cannot recognise. So the store doubles in ~50
warm builds, which sets the compaction trigger comfortably. It is also **subsumed by step 4**: once the cutoff
compares `ObjectId`s, a recomputed leaf's id is computed anyway, and finding it equal to the stored one means its
offset is already known, so nothing is appended at all.

## 18. Step 4: the cutoff stops materializing, and the store stops growing (2026-08-06)

§13 step 4 was "switch the equality cutoff to id comparison". Building it found that the larger win was one layer
above: **most of the cutoff should not run at all.**

### The comparison that compared a value with itself

`computeUnchanged` validated a value-bearing derived fact by calling `getFactUntyped` on it — which resolves the key,
finds its dependencies unchanged, **accepts the cached value**, and hands it back, whereupon the entry compared that
value against itself. Every derived fact on a warm build was materialised to satisfy a tautology.

A fact is a pure function of its recorded dependencies — the assumption `acceptPrior` already rests on. So
dependencies that all hold mean **the fact holds**, with nothing recomputed and nothing read. The branch is now the
same drill the value-less facts always took, and the two differ only in the bookkeeping afterwards: a value-less
entry is carried forward to stay drillable, a value-bearing one is simply untouched and retained. Recompute-and-
compare is left with exactly the two cases that need it — a **world leaf**, whose only oracle is running it, and a
fact whose dependencies genuinely moved, where equality is what stops propagation.

Consequently a no-change run materialises **no fact value at all** beyond the leaves it must re-read. This is
backend-independent and helps the Java graph too — though not much, since that one deserialises everything at load
regardless.

### The cutoff itself, and the order that turned out to matter

`CacheEntry` holds a `CachedValue` rather than an `Option[CompilerFact]`: `hasValue` and `matches` are what
change-detection asks, and neither reads anything. A stored value is an `ObjectId` plus a thunk — `matches` hashes the
*recomputed* side with `ObjectIdOutput` and compares 16 bytes, never touching the stored side; the fact behind it is
decoded only where something consumes it (`acceptPrior`, and nowhere else).

The store's own growth turned out to need the same idea, applied in the opposite order, and this is the finding worth
keeping:

> **Identify before placing.** Seeding the writer with the ids it already holds is not enough. Encoding descends into
> a value's children and appends each as it goes, so by the time it can tell the value as a whole is a duplicate, the
> whole subtree underneath is already on disk. Hashing first costs no bytes and no buffers and asks the only question
> that matters.

With that, a warm build appends **zero bytes** — measured over three consecutive runs, the object region does not move
— where before it grew ~17.9 KB a run. The cold-build path skips the pre-check when there is nothing to match against,
so nothing is hashed twice.

### Measured

`examples.run jvm exe-jar -m DischargeDemo`, warm after an identical build, same machine and tree as §12/§17:

```
phase              Java (default)      content store
cache load           520 ms  35.9%      107 ms  14.7%      -79%
cache save           412 ms  28.5%      100 ms  13.8%      -76%
build-cache           18 ms              11 ms
fingerprint           46 ms              48 ms
engine               341 ms             349 ms
processors            36 ms              33 ms
total              1,447 ms             727 ms             -50%
on-disk             2.49 MB            1.29 MB             0.52×
warm-build growth        —              0 bytes
```

**A warm build is half what it was**, and cache I/O has gone from ~64% of it to ~29%. What is left in the load is
reading and decoding the index — every key, every dependency edge — which is now the floor rather than the values.

### Verification

- `__.test` green (871).
- **Byte-identity**: the jar is identical across java-warm, store-cold and store-warm.
- **Under-invalidation**, the direction that matters more (§6 Step 6): an mtime-only touch changes nothing; a real
  source edit produces a warm-build jar **byte-identical to a cold build of the same content**; reverting the edit
  restores the original jar exactly.

### Retired here

The **MVStore spike** (`MvStoreCacheBackend`, the `h2-mvstore` dependency, the `ELIOT_CACHE_BACKEND=mvstore` arm).
§16 scheduled it for removal once step 3 landed; carrying a refuted design through the entry-shape change was the
concrete reason to do it now. Its findings stay in §12. `FactSerialization` remains — the conformance test still
measures against Java as the baseline.

### What is left

- **§6 Step 3** — the entry shape carrying declines. The index already writes a per-entry flag to widen.
- **§13 step 5** — compaction: decode-and-re-encode of the live entries, not a mark-and-sweep, since the bodies are
  untyped bytes. Now much less urgent than it looked — a warm build no longer grows the store at all, so garbage
  accumulates only from genuinely changed facts.
- **§13 step 6** — flipping the default, and retiring `FactCache.canSerialize` with the Java graph.
- The load floor is now the **index**: ~101 KB read and decoded per build, one key per entry plus every edge. Making
  *that* lazy is the next thing worth measuring if the 107 ms matters.
