# Incremental Compilation: Why a Warm Build Still Computes

Status: **DIAGNOSED (2026-07-31); fix designed, none of it landed.** The three invalidation sources below are
measured and reproducible. Step 2 of the plan was written and validated, then reverted together with a Step 1
attempt that failed on cache size (§5). Everything here is written against `7588845b`.

The engine works as designed; what follows is not a bug *in* the incremental algorithm but a set of fact types
and outcomes the algorithm was never told the truth about.

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
  leaf that always compares equal. `SystemNativesProcessor` is the one processor that does this today.
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

## 2. The invariant the cache actually runs on

The cache does not need a fact to be *serializable*. It needs it to be **equality-stable**:

```
read(write(value)) == recompute(value)
```

Serializability is necessary and nowhere near sufficient, and the two are routinely different. Scala's function
types extend `Serializable`, so a closure persists without complaint and comes back as a different object;
case-class equality over a function field, or over an array, is by reference. Such a value satisfies "did this
serialize?" and fails "will it still compare equal?".

`FactCache` currently decides by trial serialization (`canSerialize`), which can only ask the first question.
Every defect in §3 follows from that gap.

## 3. What a warm build actually did

Measured on the `eliot-build` tree (`examples.run jvm exe-jar -m eliot.test.Runner …`, 77 source files),
immediately after an identical successful build:

```
Compiler statistics: 2,601 ms total, 1,670 ms (64.2%) in processors
   1341 ms  51.6%   251 calls   MonomorphicTypeCheckProcessor
     87 ms   3.3%   310 calls   BindingMergerProcessor
     63 ms   2.4%    14 calls   CompilerMonomorphicTypeCheckProcessor
     39 ms   1.5%   547 calls   FileStatProcessor
Incremental run: regenerated 2129 fact(s); 16453 materialised, 65 validated unchanged without recompute.
```

Regenerations by fact type: 979 `ContributedBinding`, 547 `FileStat`, 310 `NativeBinding`, 251
`MonomorphicValue`, 25 `RefinementTable`, 14 `CompilerMonomorphicValue`, and one each of `UpToDate`,
`SourceContent`, `OutputFileStat`. The tokenizer did not run — no source file changed — yet half the build's
wall time went on re-typechecking 251 monomorphic values.

Instrumenting the three "changed" verdicts in `computeUnchanged` identifies **three independent trigger
classes**. Each is permanent: none of them can ever settle, on any number of consecutive no-change builds.

### 3.1 Serializable, but never equal (11 facts)

`SemValue.VNative(paramType, fire: SemValue => SemValue)` is a case class holding a lambda. The lambda
serializes (it captures only an FQN), so the value **is** persisted, and the recomputed lambda never equals the
deserialized one. Eleven such facts report "value differs" on every run — all `eliot.lang.String` natives:
`repeat`, `substring`, `contains`, `indexOfInternal`, `length`, `endsWith`, `startsWith`, `isInteger`, `trim`,
plus the `NativeBinding`s for `contains` and `substring`.

### 3.2 Unstorable *and* edge-less (6 facts)

Where a native's value genuinely fails to serialize, the entry is stored edges-only — and the drill then has no
edges to follow, because `StdlibNativesProcessor` reads no facts at all. Six `ContributedBinding`s land in the
"no value, no edges ⇒ changed" branch every run: `Bool.fold`, `Bool.&&`, `Bool.!`, `Numeric.add`,
`Numeric.subtract`, `Compare.lessThanOrEqual`. This is precisely the case `SystemNativesProcessor` already
solves with the `UpToDate` anchor; the other contributors never got it.

### 3.3 Declines are not cached (55 facts, 23 reached)

`BindingMergerProcessor` aborts when no contributor has a binding for a name — a correct answer, not a failure.
At the end of a cold run 55 `NativeBinding` demands have concluded with no fact: `Option.Some`, `Option.None`,
`Either.Left`, `Pair`, `Unit.unit`, `List.empty`, `List.foldLeftInternal`, `Effect.pure/flatMap/map`,
`Suspend.suspend`, `Eq.equals`, `Combine.combine`, `Abort.abort`, `Throw.raise`, `jvm.IO`, `Interval`, the
`eliot.build` constructors. Nothing is persisted for them — nothing is *dropped* at save; they simply never
exist to be saved — so next run `prior.get(key)` is `None`, which reads as *new / previously failed ⇒ changed*
for every dependent that ever asked. 23 are reached during warm validation before short-circuiting.

### 3.4 The cascade, and why the trigger list is a lower bound

Any one of the three is enough on its own. Once a `MonomorphicValue` is judged changed, regenerating it re-runs
the checker for that value, which demands its callees' values, and so on — 251 re-checks from a handful of
roots. `forallM` short-circuits on the first changed dependency, so the counts above are a **lower bound**: more
triggers may sit behind the ones that fire first.

## 4. Which fact types are equality-stable (measured, not inferred)

Round-tripping every materialised fact through Java serialization and comparing to itself, over a full build:

| Fact type            | Verdict                                      |
| -------------------- | -------------------------------------------- |
| `ContributedBinding` | 1695 stable, 36 unserializable, **11 unstable** |
| `NativeBinding`      | 65 stable, 248 unserializable, **12 unstable**  |
| `GeneratedModule`    | **26 unstable** (all of them)                   |
| *every other type*   | stable                                       |

Three conclusions worth keeping:

- Exactly **three** fact types cannot be persisted: two carry `SemValue` closures, and `GeneratedModule` holds
  generated bytecode in a `ClassFile`, whose array compares by reference. `GeneratedModule` never surfaced as a
  trigger in §3 only because nothing downstream of it regenerated.
- `ContributedBinding` and `NativeBinding` are *mixed* per instance — the same type is stable for a `None`
  contribution and unstable for a `Leaf`. A per-value decision would make the cache's behaviour depend on
  content; the decision belongs to the **type**.
- **`MonomorphicValue` is stable.** Its own class documentation, and `IncrementalFactGenerator`'s, state that
  the monomorphize layer is `SemValue`-bearing and cannot be persisted. That is wrong: it carries `GroundValue`
  and `MonomorphicExpression`, both ordinary data. Those 719 facts can be *accepted* from cache, not merely
  drilled through — a significant part of the warm-build win. **The docs need correcting.**

## 5. Measured dead end: per-entry payload framing

The first Step 1 attempt gave each fact a `FactCodec` producing an independent byte `Payload`, so that a codec
failing on one entry could not mis-position a shared stream and corrupt every entry after it. It works, and it
costs **7.5× on disk: 17.9 MB → 134 MB**.

The cause is that Java serialization's back-references and class descriptors are **per-stream**. One shared
stream writes each `ValueFQN`, `ModuleName`, `Sourced` and each class descriptor once and refers back to it
everywhere after; 16 800 independent frames re-write all of it every time.

> **The tension, stated once:** cross-entry structure sharing requires coupled sequential decoding; per-entry
> failure containment requires independent frames. Plain Java serialization cannot provide both. Buying both
> means a shared symbol and class-descriptor table written once beside the frames — a real piece of work, which
> should be justified by load-time measurements rather than assumed.

(A second symptom of that attempt — the warm build afterwards rebuilt everything, cache apparently unused —
was never diagnosed before the revert.)

## 6. The plan

### Step 1 — the persistence decision belongs to the fact type

Replace trial serialization with a codec the key supplies. The revised shape has the codec choose a
**persistable representation**, not bytes, so the container keeps its single shared graph and its size:

```scala
trait FactCodec[V]:
  def persist(value: V): Either[Throwable, Option[Any]]   // None ⇒ deliberately not persisted
  def restore(stored: Any): Either[Throwable, V]
```

- The codec owns *whether* a value is persisted (the actual fix) and *what shape* it takes (the extensibility),
  but never touches a stream — so a codec cannot corrupt anything, and the framing of §5 is unnecessary.
- Declining (`Right(None)`) and failing (`Left`) are distinct signals and must not be conflated. Encoding to
  *zero bytes* is neither: `UpToDate` is field-less, encodes to nothing, and must stay comparable — it is the
  anchor the whole of §3.2 depends on.
- The choice is **abstract with no default**, so every key states it once. Both possible defaults are wrong:
  defaulting to persist reintroduces §3.1, and defaulting to decline would silently make a *leaf*
  unvalidatable — an edge-less declining `FileStat` disables the entire cache while everything still compiles
  and passes.
- Default codec: identity (hand the fact to the shared graph, as today). `OpaqueCodec` for the three types in
  §4. Keep a serializability probe **only on the error path** — write the graph, and if it throws, retry with
  per-entry probing — so the steady-state cost disappears without losing the protection.

### Step 2 — every input-less contributor takes the `UpToDate` edge (written, validated, reverted)

`StdlibNativesProcessor` reads nothing at all; `DataTypeNativesProcessor` and `MatchNativesProcessor` read facts
on their *hit* path but contribute `None` without reading for every name that is not theirs — which is most
names. All three need the anchor, applied uniformly at the top of the generation so early returns are covered.

The invariant to hold on to: **an empty dependency set means "world leaf, re-check me every run"**. A compiler
constant must anchor on *every* path. This cannot be automated in the engine — auto-anchoring an edge-less
generation would turn genuine world leaves into constants and never notice a changed source file again.

### Step 3 — cache declines

Extend the entry to three outcomes: `Value` / `Opaque` (materialisable, value not persisted) / `Declined`
(legitimately produced nothing). `regenerate` already has what it needs to tell them apart: a decline is an
explicit abort with **zero errors** and no registered fact; a generation that errored stays uncached so errors
keep re-surfacing. `resolve` then completes a validated `Declined` entry with `None` and runs nothing;
`computeUnchanged` drills its edges as for `Opaque`. Bump `CACHE_VERSION` (currently 31).

Caching a decline reached *through* a missing upstream stays sound: the missing key is among the recorded
edges, so if it starts producing, the decline invalidates.

### Step 4 — iterate to a fixpoint

Re-measure after 1–3 and expect `ContributedBinding`, `NativeBinding` and `MonomorphicValue` regenerations to
reach zero. Because of §3.4 any survivor is likely a fourth instance of the same three classes; sweep fact
values for reference-equality fields (arrays, functions, `Ref`s) rather than waiting for them to surface.

### Step 5 — make the next regression visible

Answering "why did this rerun?" currently requires hand-patching four call sites in `IncrementalFactGenerator`.
Keep a permanent DEBUG-level trace of the three changed-verdicts — value differs / no prior entry / edge-less
and unstorable — which costs nothing with the logger off.

### Step 6 — verification

- `./mill __.test`, plus cases in `IncrementalFactGeneratorTest` / `FactCacheTest` for decline-caching, opaque
  drilling, and a **law test** asserting `restore(persist(v))` equals a *freshly recomputed* value rather than a
  retained one. Round-trip equality alone would have passed `VNative` and missed the bug.
- An assertion on the metric itself: a warm run over an unchanged tree regenerates only leaf facts. This is the
  test that would have caught the current state.
- Fast example sweep plus byte-identity comparison against a cold build — the jar must be identical whether or
  not the cache was used. This step became executable only on 2026-07-31: until jar entries were given a fixed
  timestamp and a fixed order, two builds of the same sources produced different bytes, and the comparison had to
  be made over unzipped class files instead.
- The other direction, which matters as much: touch one source file and confirm a bounded, *correct* subset
  recompiles. Steps 1 and 3 both widen what is accepted from cache, so under-invalidation is the risk they
  carry.

### Target

A warm build regenerates only world leaves: 547 `FileStat`, one `OutputFileStat`, one `UpToDate` — around 549
facts against today's 2129, with no processor active but `FileStatProcessor`.

## 7. Open items

- **Which Step 1 to build**: the representation-based codec above (keeps 17.9 MB, gives up per-fact byte
  encodings), or the shared symbol table that would make independent frames affordable. Measure what the table
  buys before committing to it.
- **`GeneratedModule` could be made comparable** by giving `ClassFile` a value-comparable representation instead
  of a bare array, which would let generated bytecode be cached rather than merely drilled through. Not needed
  for the target above.
- **The load floor.** Once the processors stop running, the ~600 ms "compiler engine, cache and i/o" line and
  the 17.9 MB deserialization become the warm-build cost. Separate problem, worth measuring after Step 4.
- **A dangling reference**: `FactCache`'s scaladoc points at "`docs/incremental-compilation.md` §13" for the
  fingerprints, which never existed. They are documented in §1 here; the scaladoc reference should be corrected.
- **Two wrong class docs**: `MonomorphicValue` and `IncrementalFactGenerator` both claim the monomorphize layer
  cannot be persisted (§4).
