# Retiring the Optional Fact Read (`getFactIfProduced`)

Status: **PLAN (2026-08-02); step 1 landed.** Written against the current tree. §6.1 (the `NativeBinding`
totalization) is done: the fact now carries `semValue: Option[SemValue]`, `BindingMergerProcessor` publishes a
`None` payload instead of aborting, and the Bucket-1 readers (`MonomorphicTypeCheckProcessor`,
`CompilerMonomorphicTypeCheckProcessor`) read it with `getFactOrAbort`. The Bucket-3 cyclic-walk readers
(`BindingClosure`, `ReducedBindingClosure`) stay tolerant and only inspect the new `Option` payload. The rest is
still open.

## 1. The problem, restated

The processor-facing fact API is supposed to encode one rule: *a processor declares the facts it needs, and
absence is an error*. The 2026-07-04 refactor (`docs/architecture-review.md`, E1) made `getFact` private in
`CompilerIO` and exposed three intent-carrying reads:

- `getFactOrAbort` — absence aborts (the "I declare I need this" read);
- `getFactOrError(key)(error)` — absence raises a caller-owned message;
- `getFactIfProduced` — absence is tolerated, returns `Option`.

`getFactIfProduced` is a pure pass-through to the private primitive (`CompilerIO.scala`:
`def getFactIfProduced(key) = getFact(key)`). It was reserved for "the ~30 absence-is-expected-and-handled
sites." It has since grown to **~63 call sites across 33 production files**. In other words, the old `getFact`
resurfaced under a new name, and the escape hatch became a main road.

This document is the plan to close it back down — and, crucially, why doing so is the *same* work as fixing the
warm-build recompute problem in `docs/incremental-compilation.md`.

## 2. Why this is a caching problem, not just a hygiene problem

A `None` from `getFactIfProduced` is not free. Follow one through the engine.

When a processor generating fact `F` calls `getFactIfProduced(K)`:

1. `DependencyTrackingProcess.getFact` records `K` as a direct dependency of `F` **whether or not `K` was
   produced** (it records the edge, then notes `sawMissing` if the read came back empty).
2. If `K` was *not* produced — the producer declined via `abort`, or `K` names an absent module / native /
   ability impl — then `K` has **no cache entry**. Declines are not persisted.
3. `F` still produces its own value and is persisted **with a dependency edge pointing at a key that has no
   entry**.
4. On the next warm build, validating `F` runs `computeUnchanged(K)`, which hits
   `prior.get(K) == None ⇒ false` ("new / previously failed ⇒ changed", `IncrementalFactGenerator.scala`). So
   `F` is regenerated. **Every build. Forever.** The edge can never settle.

This is exactly the root cause catalogued in `docs/incremental-compilation.md` §3.3 ("Declines are not cached —
55 facts, 23 reached"), which cascades (§3.4) into 251 `MonomorphicValue` re-typechecks — **51.6% of warm-build
wall time**. The canonical poisoned key is `NativeBinding`, which `BindingMergerProcessor` declines
(`abort[SemValue]`) whenever nothing binds the name.

**The unifying insight.** *"A fact that may legitimately be absent"* and *"a fact the cache cannot represent"*
are the same thing: a key with no entry always reads as "changed". So **totalizing a fact — always producing it,
carrying present/absent inside an equality-stable value — removes the optional read *and* removes the cache-poison
edge in one move.** Retiring optional reads and fixing the warm-build recompute are the same reform seen from two
ends.

Totalizing is *possible* because every key is concrete. A producer invoked for `K` can always answer "here is
what I found for `K`, possibly nothing" instead of declining. The domain of answers is bounded by the key, and
totalizing produces no facts beyond those already demanded (generation is demand-driven).

## 3. The template already exists — it just stops one hop short

The `NativeBinding` pipeline is the pattern this plan generalizes, and it is *half done today*:

- **Inputs are total.** Each `ContributedBinding` supplier answers `ContributedBinding(vfqn, label, None)` rather
  than declining, for every name it is asked about. Its scaladoc states the invariant outright: "The fact is
  *total* — a supplier answers `None` rather than declining (aborting) — so the merger reads values with
  `getFactOrAbort` and a mis-wired supplier fails loudly." The one summarizing processor,
  `BindingMergerProcessor`, reads all suppliers with `getFactOrAbort`.
- **The output is not.** `BindingMergerProcessor.generateSingleFact` ends in `case None => abort[SemValue]` — when
  no supplier binds the name, it declines instead of publishing a `NativeBinding` with an absent payload. So all
  ~5 readers of the fact everyone actually consumes still use `getFactIfProduced`
  (`MonomorphicTypeCheckProcessor`, `CompilerMonomorphicTypeCheckProcessor` ×2, `BindingClosure`,
  `ReducedBindingClosure`).

The authoritative-summarizer architecture is exactly right; it was applied to the merger's inputs and left off
its output. Finishing that one hop is both the fix for the natives and the worked example for every other bucket
below.

## 4. The 63 sites, bucketed by *why* they tolerate absence

Only two buckets are "optional reads to eliminate." The third is legitimately tolerant and stays — but is
renamed so its intent cannot be confused with an existence query.

### Bucket 1 — Existence / routing / lookup probes → **totalize**

The bulk, and all the cache poison. These ask "does `X` exist in pool P?" / "is there a native / impl / binding
for this name?" — a total question over a concrete key.

- **Natives / bindings:** `BindingMergerProcessor` (output), `MonomorphicTypeCheckProcessor`,
  `CompilerMonomorphicTypeCheckProcessor`, `BindingClosure`, `ReducedBindingClosure`, `EscalatingReducer`.
- **Module membership / pool routing:** `CompilerNativesProcessor`, `DeclaringPool`,
  `RefinementChannelProcessor` (module-names probes), `SaturatedValueProcessor`, `DataTypeNativesProcessor`,
  `ModuleValueProcessor`, `ValueResolver`, `ModuleAbilityOverlapCheckProcessor`.
- **Ability lookups:** `AbilityResolver`, `AbilityImplementationProcessor`, `AbilityImplementationCheckProcessor`,
  `AbilityMatcher`, `ImplementationMarkerUtils`.

**Step 1a.** Make each declining producer total: instead of `abort`, emit a fact whose value carries the answer.
For `NativeBinding`, that is `NativeBinding(vfqn, Option[SemValue], platform)` (or a `Present/Absent` ADT). For
`UnifiedModuleNames`, a producer that emits an empty name-set fact for an absent module rather than declining.
Keep the payload **equality-stable** — an `absent` / empty case is trivially stable, which is what makes the cache
accept it.

**Step 1b.** Rewrite the readers from `getFactIfProduced(K).map(_.exists(...))` /
`getFactIfProduced(K).map(_.map(...))` to `getFactOrAbort(K)` inspecting the value. Preserve the deliberately-quiet
paths: e.g. `CompilerNativesProcessor.inCompilerPool` reads `UnifiedModuleNames` precisely to *avoid* the "Could
not find" error a direct value request would emit — a total empty-set fact keeps it quiet. Where a reader relied
on absence to emit "Could not find imported module", move that decision to inspecting `found == false` and calling
the error explicitly (this is the semantic-drift risk E1 flagged; it is handled per-reader, not by the primitive).

### Bucket 2 — Optional enrichment → **totalize where the key is bounded**

`ReconcileProcessor` (`RefinementTable`), `PostDrainQuoter` (`roleHint` via `UnifiedModuleValue`). Same treatment
as Bucket 1: a total fact with an empty / none case, read with `getFactOrAbort`.

### Bucket 3 — Skip-broken-callee and cyclic-walk reads → **keep tolerant, but rename**

These tolerate a genuine *upstream failure* (an error already reported) or a not-yet-resolved node in a
possibly-cyclic walk. They **cannot** be totalized: a value that failed to type-check has no `MonomorphicValue`,
and errors are deliberately never cached so they re-surface every run until fixed.

- **Skip-broken-callee (upstream errored):** `UsedNamesProcessor`, `CodegenProjection`, `ExpressionCodeGenerator`,
  `JvmClassGenerator`, `JvmProgramGenerator`, `Checker`, `CarrierKindChecker`, `CalculatedReturnResolver`,
  `Sourced` (synthetic-URI content).
- **Cyclic / not-yet-resolved walk:** `RowElaborationProcessor`, `RecursionChecker`, and the
  `NativeBinding`/`CompilerMonomorphicValue` self-recursion guards in `BindingClosure` / `ReducedBindingClosure`
  (already gated by `ancestors.contains(...)`).
- **Input-less compiler constant:** `SystemNativesProcessor`'s `UpToDate` touch-read — special, must stay
  tolerant (its own comment says so; it is the cache anchor of `docs/incremental-compilation.md` §3.2).

**Step 3.** Split the single tolerant primitive into narrowly-named variants so the residual tolerance is
self-documenting and cannot be reached for as a generic escape hatch:

- `getFactToleratingUpstreamFailure(K)` — "proceed past a callee that already reported an error";
- `getFactMaybeCyclic(K)` (or keep `getFactIfProduced` for exactly these) — a walk over a possibly-cyclic graph
  that consults `activeFactKeys` first.

Any *new* optional read then has to name its reason, making it a deliberate, reviewable choice.

## 5. The residual cache fix (complements Bucket 3)

Bucket 3 reads still create a poison edge whenever an upstream *declines* (as opposed to errors). Land **Step 3
of `docs/incremental-compilation.md`** — a three-outcome cache entry (`Value` / `Opaque` / `Declined`) where a
`Declined` entry stores the decline's edges and validates structurally. After this, a decline reached through a
bounded key stops invalidating its readers even where a tolerant read was kept. Bump `CACHE_VERSION`.

Totalizing (Buckets 1–2) is preferable wherever it applies because it turns the fact into an *accepted-from-cache*
value, not merely a drillable one; decline-caching is the safety net for the genuinely non-totalizable residual.

## 6. Sequencing

1. **Finish the natives** (`NativeBinding` total + its ~5 readers). Smallest, self-contained, and the worked
   example for the rest. Measure warm-build regeneration count before/after. **✓ Done.**
2. **Bucket 1 remainder** (module membership, ability lookups). Biggest cache win.
3. **Bucket 2.**
4. **Bucket 3 rename** (cheap; makes intent explicit and prevents backsliding).
5. **§5 decline-caching** for the residual.

## 7. Verification

- `./mill __.test`.
- New incremental assertion: a warm run over an unchanged tree regenerates **only world leaves**
  (`FileStat` / `OutputFileStat` / `UpToDate`) — the metric that proves the poison edges are gone
  (`docs/incremental-compilation.md` target: ~549 vs today's 2129).
- Fast example sweep + byte-identity comparison (cold vs warm jar) per the reference-verification harness recipes,
  to prove totalizing did not change output.
- Touch-one-file test: confirm a *bounded, correct* subset recompiles. Totalizing widens what is accepted from
  cache, so **under-invalidation is the risk** — a reader that used to regenerate on absence must now regenerate
  on the value flipping to `absent`, which the recorded edge to the (now total) key delivers.

## 8. What this does and does not aim for

- It is **not** "zero optional reads." It is "the only optional reads left are the ones where absence is a real
  domain answer" — broken callee, cyclic walk, the `UpToDate` anchor — each named for its reason.
- It **partly reverses a deliberate E1 decision.** E1 rejected total facts on cost grounds ("would have forced
  `Option` payloads through every `NativeBinding` / `UnifiedModuleValue` consumer"). The new justification is the
  caching data (51.6% of warm-build time). The cost is real and paid per reader; it is now worth paying.
- The engine-level `CompilationProcess.getFact` (public on the trait and its wrappers, `TODO.md`) is a separate,
  cosmetic item — that is the one seam all wrappers implement, not a processor-facing read. It can be renamed
  independently and is out of scope here.
