# Monomorphize architecture review: why issues keep surfacing, and where to cut

Status: **Review / direction.** This is a birds-eye evaluation of the `monomorphize` package (the NbE type
checker, `lang/src/com/vanillasource/eliot/eliotc/monomorphize/`) after a long run of changes driven by other
workstreams (reification, jvm-int, implicit generics, effects M0–M5). It diagnoses *why* the processor keeps
needing fixes and proposes a sequenced set of structural simplifications. No code is changed by this document;
it is the rationale and backlog the simplifications are validated against. It is congruent with — and partly
de-risks — `monomorphization-keying-plan.md` (whose B1 relevance analysis is the same explicit classification
recommended here).

## Verdict up front

The NbE **core is sound and still elegant** — do not touch it. `eval/Evaluator.scala` (196 lines), the uniform
`walkTypeStack` fold (`check/TypeStackLoop.scala:374`, ~10 lines), and the equality core of
`unify/Unifier.unifyForced` are clean and faithful to the cornerstone. The "no concept of generic parameters"
invariant holds: the type-stack fold is genuinely uniform.

The churn is **not** in the core. It is in a ring of features bolted *around* the equality core, none of which
is equality-shaped. The issues are not random; they have **one structural cause** (below). The `Checker` grew
from "simplified checker" (commit `ea02b324`) to **1071 lines**, and the churn concentrates exactly there:
`Checker.scala` (15 touches since May), `TypeStackLoop.scala` (12), `Unifier.scala` (6) — the three files that
host the bolt-ons.

## Root cause: an equality core hosting four non-equality features

The cornerstone gives one primitive: **type equality = definitional equality via NbE, computed locally, with a
metastore as the only mutable state.** But Eliot wants four things that are neither equality nor local. None can
be expressed as "force both sides and compare," so each is implemented as a **side-car that observes the
unifier's metastore and runs as a post-drain pass**:

| Feature | What it actually is | How it is grafted on | Size |
|---|---|---|---|
| Int widening / `Coerce` | a **directional subtyping** relation | `Checker.unifyOrCoerce` / `tryCoerce` / `coercionPayload` / `buildCoercedExpr` resolve an ability and splice nodes *outside* `unify` | ~180 |
| `pick(a,b)` join / `Combine` | a **least-upper-bound / lattice** | `combinable` / `candidates` in the unifier + `resolveCombines` / `resolveUpperBounds` / `pendingUpperBounds` in the checker | ~200 |
| Calculated returns (implicit generics) | **non-local inference** — a caller's type depends on a callee's *monomorphized body* | `readMonomorphicReturn` re-enters `getFact(MonomorphicValue.Key)` + an `activeFactKeys` recursion guard | ~200 |
| HKT carriers (effects) | a **kind system** | `recordCarrierMetas` / `verifyCarrierKinds` / `unsatisfiableApplication` reconstruct kinds *post-drain* from signatures | ~130 |
| Reification / erasure | an explicit **phase / staging** analysis | `PostDrainQuoter`'s gate + `eval/SemExpressionEvaluator` + `processor/BindingProcessor.reifyingWrap` re-derive which subterms are erased | ~250 |

Two consequences make the resulting bugs *non-obvious* — which is the lived experience of "fixed X while working
on Y":

### 1. The feature surface is a product, not a sum

The side-cars interact, and each interaction is a fresh special case. A combinable meta can also be a carrier; a
coercion's bounds can come from a combine join; a *calculated return whose argument bounds come from a combine
join* already needed its own diagnosis path (`reportUngroundCalculatedReturn`, `Checker.scala:826`). Every new
feature multiplies against all existing side-cars, so the count of interaction corners grows combinatorially.

### 2. Meta roles live in side-tables, not in the values *(resolved by D2)*

A `VMeta` is just an `Int`; *what role it plays* is recovered by membership in one of several `CheckState`
side-tables: `bindingCache`, `abstractTypeMetas`, `abilityResolutions`, `combineResolved`, `pendingUpperBounds`,
`typeStackValueParams`, `carrierKinds`, plus `combinable` / `candidates` inside the unifier. The post-check
section of `TypeStackLoop.processIO` is then an **implicit pipeline of ~8 ordered passes over this shared
blackboard**:

> **D2 collapsed the role-bearing tables.** `combinable` / `candidates` / `combineResolved` / `pendingUpperBounds`
> / `carrierKinds` / `abstractTypeMetas` are now one `Map[Int, MetaRole]` on the unifier (`bindingCache`,
> `abilityResolutions`, `typeStackValueParams` are not per-meta roles and stayed put). The finalizer is a total
> match on that role — see the D2 deliverable below.

```
drainAndResolveLoop → resolveUpperBounds → verifyCarrierKinds
→ failOnUndeterminedCalculatedReturn → defaultUnsolvedMetas → report → quote
```

and `defaultUnsolvedMetas` (`TypeStackLoop.scala:193`) sweeps **everything not in `abstractTypeMetas`** to
`VType`. That catch-all is a silent-miscompile generator: any meta a side-car forgets to resolve becomes `Type`
instead of erroring. The explicit fail-safes already stapled in front of it (`failOnUndeterminedCalculatedReturn`)
are the *symptom*, not the cure: they are hand-patched holes in a default that should not exist.

## The two fragilities closest to biting

Neither is a known live bug (the suite is green), but both are "works until a new feature reaches them," and
both deserve an adversarial test now.

### F1 — `VTopDef(fqn, None, spine)` was overloaded for three incompatible things *(stuck-native arm fixed by D3)*

The same physical shape represented:

1. **data / type constructors** — injective, rigid;
2. **abstract `def`s** (`some` / `none`, body-less platform signatures) — rigid, opaque, **not injective**;
3. **stuck native applications** — `add(?,?)`, `min(?,?)`, formerly emitted as `VTopDef(fqn, None, spine)` by
   `SystemNativesProcessor.stuck` so `Evaluator.renormalize` could re-fire them — and **definitely not injective**
   (`add(1,3) == add(2,2)`).

`Unifier.tryDecomposeApplied` injectivity-decomposes any *rigid applied* head: `?F[a] ~ add(x, y)` would have
solved `?F := add` and unified the arguments pointwise, treating `add` as an injective constructor. Likewise
`Quoter.quote` read any `VTopDef(_, None, spine)` back as a ground *type* `Structure(fqn, …)`, so a stuck native
surviving to quote-time became a nonsense ground type rather than an error.

> **D3 split out the stuck-native arm.** Stuck natives now carry a dedicated `SemValue.VStuckNative(fqn, spine)`
> head (produced by `SystemNativesProcessor.stuck` and `StdlibNativesProcessor`'s `inc`), so they are *no longer*
> `VTopDef`s. `tryDecomposeApplied` therefore never injectivity-decomposes one (a `VStuckNative` rhs simply falls to
> its `None`/postpone arm — no code change there, the distinct head does it), and `Quoter.quote` fails loudly
> (`Cannot quote stuck native application …`) instead of minting a nonsense `Structure`. Definitional equality of two
> stuck natives is the same FQN + pointwise-equal spine (new `unifyForced` arm). Cases 1 and 2 (constructor vs
> abstract def) remain a `VTopDef`; D3 did not need to split them (neither is non-injective in a way the unifier or
> quoter reaches in practice — only the native arm was the landmine).
>
> **Refinement learned from building it:** the *quoter* half is not "fail on the first stuck native you see." An
> **intermediate** function type — a partial application's codomain, or a curried head reference's type
> `(+) : Int[L] -> Int[R] -> Int[add(L,R), …]` — legitimately still carries an un-re-fired native in its codomain
> (it is never the value handed to the application-site `renormalize`, and the uncurry pass discards it). The old
> lenient quoter swept those under a nonsense `Structure`; a naïvely strict quoter rejects valid programs. The fix is
> a `deep` mode on `renormalize` that descends under `VPi` binders, used **only post-drain in `PostDrainQuoter`** —
> where every meta is already solved, so collapsing a binder's metas is safe. The shallow check-time `renormalize`
> (the application-result one) must stay shallow: descending there would collapse a still-combinable result meta
> mid-checking and break `Combine`. Covered by `unify/StuckNativeUnifyTest`.

### F2 — `defaultUnsolvedMetas` masks unresolved obligations as `Type` *(fixed by D2)*

Any meta not in the `abstractTypeMetas` protected set was solved to `VType` at the end of checking. A side-car
that failed to resolve its meta therefore produced a silently mistyped value, not a diagnostic. This was the
structural reason "gaps must be fail-safe" kept needing manual enforcement here. **D2 removed the catch-all:**
`defaultUnsolvedMetas` is now an exhaustive match over the sealed `MetaRole` (no `case _`), so the only protected
role is `AbstractAssoc` and a *new* role cannot silently fall through to `Type` — it forces a decision at the
match, and the `assertEveryMetaResolvedOrAbstract` postcondition backstops a slip.

## The explicit step to take (and the project is already heading there)

The cornerstone holds that compile-time/runtime is "incidental — just *when* you force." That is right for the
*core calculus and the evaluator*. But operationally the **compiler spends real effort reconstructing the
staging / role distinction it declined to represent**: `PostDrainQuoter.collectParamRefs` / `hasErased` /
`tryMaterialise`, `BindingProcessor.reifyingWrap` / `valuePositionRefs`, `CheckState.typeStackValueParams`,
`recordCarrierMetas`. Each is a separate re-derivation of "what role does this binder / meta play."

The monomorphization-keying plan's **B1 relevance analysis** already proposes to compute, statically and
pre-NbE, exactly such a classification per type parameter (phantom / representation / dispatch / reified). The
recommendation here is to recognise that this is **the missing explicit step for the checker too**: compute each
binder's role **once, on `SaturatedValue`, before checking** (the fact already carries `calculatedReturn` and
`inferableArity`), and *consume* it everywhere instead of re-deriving it:

- the quoter is **told** which binders are erased-but-value-position, instead of discovering it by ref-walking;
- carrier metas carry their kind **intrinsically**, instead of `verifyCarrierKinds` rebuilding it from
  signatures post-drain;
- `reifyingWrap` reads the reified set instead of inferring it.

One analysis, many consumers — and it de-risks the keying work, because the same classification is exercised by
type-checking, not only by codegen dedup.

## Deliverables (by leverage and risk)

Ordered so the cheap, high-value, low-risk items land first and de-risk the larger ones. Each is independently
valuable.

### D1 — Name the post-check resolution pipeline ✅ DONE (commit `7ed0c67b`)

Made `TypeStackLoop.processIO`'s tail an explicit, named structure terminating in an assertion "every meta is
resolved-or-explicitly-defaulted." No semantic change; suite green.

**Refinement learned from building it:** the tail is **tiered, not a flat ordered list** as predicted here. The
one hard ordering is a *phase boundary* — `resolveUpperBounds` may assume saturation has reached its fixed point
(every `Combine` meta is joined) — which no pairwise `runsAfter` can express. So the passes split into a
**saturation fixed-point loop** (drain-interleaved ability + `Combine` resolution) and a **linear finalization
tier**, then the finalizer (`defaultUnsolvedMetas`), then the assertion. `drain` is the equality core settling
*between* feature passes — deliberately not a pass. This is the concrete answer to the hook-architecture question:
the plug-in *mechanism* is real (named `PostDrainPass`es, a closed `PassContext` input that no pass reaches
past), but any registry must be **two-level (phase, then order)**, and the features stay coupled through the
shared metastore — D1 makes that coupling explicit rather than dissolving it. The postcondition is the seam **D2**
plugs into (`assertEveryMetaResolvedOrAbstract`; verified live by neutering the finalizer and confirming it
fires, so a forgotten role in D2 cannot silently default to `Type` — the F2 cure, prepared).

### D2 — Replace the meta side-sets with one `Map[MetaId, MetaRole]` ADT ✅ DONE (highest structural payoff)

Introduced `MetaRole` (`monomorphize/domain/MetaRole.scala`) and folded the six side-sets — `combinable` /
`candidates` (unifier) and `carrierKinds` / `abstractTypeMetas` / `pendingUpperBounds` / `combineResolved`
(`CheckState`) — into one `Map[Int, MetaRole]`. `defaultUnsolvedMetas` is now a **total, catch-all-free match on
roles**: an `AbstractAssoc` stays unsolved, a `Plain` or `Instantiation` defaults to `Type`. F2 is deleted: a
future role added to the sealed ADT forces an explicit decision at that match (non-exhaustiveness is reported)
instead of silently inheriting "default everything to `Type`." Suite green; covered by the new
`unify/UnifierRoleTest` (role lifecycle + the finalizer's protected set is *exactly* the `AbstractAssoc` metas).

**Three refinements learned from building it** (the predicted ADT was close but not exact):

- **The roles are not disjoint, so they are not a flat enum.** A *carrier is a combinable instantiation meta* —
  `peelLams` marks every peeled binder combinable, and `recordCarrierMetas` additionally tags the higher-kinded
  ones with a kind. So the review's separate `Combinable(candidates)` and `Carrier(kind)` collapse into one
  `MetaRole.Instantiation(combinable, candidates, combineResolved, upperBounds, carrierKind)` — the carrier kind is
  an *optional attribute* of an instantiation meta, and `combineResolved` / `pendingUpperBounds` are its fields
  too. The role ADT is `Plain | Instantiation(...) | AbstractAssoc(fqn)`.
- **The map lives on the `Unifier`, not `CheckState`.** The combinable/candidate aspects are read *and written
  inside* `unify`/`solveMeta`/`taintMetasIn`, so they must sit where the (immutable) unifier can reach them;
  putting the map there makes the unifier the single per-meta metadata authority next to the `metaStore`.
  `CheckState`'s `record*` methods are thin delegates into it, so call sites are unchanged.
- **`CalculatedReturn` is not a stored role, and "Carrier unsolved = error" overstated it.** The return meta is
  threaded directly (`PassContext.returnMeta`) and is solved-or-aborted *before* finalization, so it never needs a
  protected role. And an unsolved carrier is **not** an error: a legitimately higher-order carrier (`?F[A,B] ~
  Function[A,B]`) is intentionally defaulted to `Type` and backstopped when the callee is monomorphized — only the
  *wrong-kind* and *unsatisfiable-rigid-arity* cases error, and those are caught in `verifyCarrierKinds` before the
  finalizer. The finalizer's real win is **exhaustiveness**, not a new per-role outcome.

### D3 — Disambiguate `VTopDef(_, None, _)` ✅ DONE (small, removes F1)

Gave stuck native applications a distinct `SemValue.VStuckNative(fqn, spine)` head so the unifier never
injectivity-decomposes a non-injective native and the quoter fails loudly on a surviving one. The distinct head is
what does the work: `tryDecomposeApplied` decomposes only `VTopDef`/`VNeutral` rigid heads, so a `VStuckNative` rhs
postpones with **no change** there; `Quoter.quote` gains one arm that errors instead of minting a nonsense
`Structure`; `unifyForced` gains a same-FQN-spine definitional-equality arm; `metasOf` and `SemValuePrinter` gain
the obvious spine-recursion / rendering arms. `SystemNativesProcessor.stuck` and `StdlibNativesProcessor` produce
the new head. Suite green; covered by the new `unify/StuckNativeUnifyTest`.

**Refinement learned from building it** (see the F1 note above): making the quoter strict surfaced that
*intermediate* function types (a curried head reference / partial-application codomain) legitimately carry an
un-re-fired native, which the old lenient quoter hid as a discarded nonsense `Structure`. The fix is a `deep`
descent-under-`VPi` mode on `Evaluator.renormalize` used **only post-drain in `PostDrainQuoter`** (where all metas
are solved, so the descent is safe); the shallow check-time `renormalize` stays shallow so it does not collapse a
still-combinable result meta.

### D4 — Consolidate the refinement lattice (the structural one)

Move `Coerce` + `Combine` + `pendingUpperBounds` (~360 lines spread across `Checker`) into **one
refinement-bounds solver module** with a clear interface. This honestly separates *definitional equality* (the
Unifier) from *the refinement lattice* (the new module) — which is what the system actually is. The unifier's
`combinable` / `candidates` / taint logic moves there too, ending the current split where the data lives in the
unifier but the algorithm lives in the checker. Spike worth running first: a join may be expressible as "the
least `T` that both sides `Coerce` into," which would collapse `Combine` and `Coerce` into a single mechanism.

### D5 — Replace the `errors.size`-delta success protocol (low/medium)

`unify` success/failure is currently detected by diffing `errors.size` before/after, in ≥4 places
(`unifyOrCoerce`, `coercionExists`, the combinable interception in `Unifier.unify`, and `drain`'s progress
bookkeeping). Replace with `unify` returning an explicit success/contradiction result (or a non-committing
`tryUnify`). Removes a brittle hand-rolled speculative-transaction idiom over a mutable error log.

### D6 — Make staging/role explicit and thread it (aligns with keying B1)

Compute the per-binder role classification once on `SaturatedValue` and consume it in `PostDrainQuoter` and
`BindingProcessor` (and to seed `MetaRole.Carrier` kinds in D2), replacing `collectParamRefs` /
`valuePositionRefs` / `reifyingWrap`'s re-derivation. Shares the analysis with keying-plan B1; do it jointly with
that work so the classification has two consumers (checking + codegen dedup) and is exercised by both.

### Minor cleanups (opportunistic)

- `Evaluator.eval` and `SemExpressionEvaluator.eval` are near-duplicate traversals (the latter only reads
  pre-evaluated type args); fold toward a shared traversal.
- `Evaluator.semToGround`'s `case _ => GroundValue.Type` fallback (`Evaluator.scala:162`) is a residual lossy
  path the strict post-drain quoting work otherwise eliminated; route its callers (ability matching) through
  `Quoter.quote` so the lossy fallback can be deleted.

## What is explicitly *not* in scope

The NbE core, the single-evaluator/single-domain cornerstone, the uniform type-stack fold, and pure-definitional
`unify` all stay. The point of this plan is the opposite of a rewrite: the core is right, and the work is to stop
the *edges* from multiplying. D1–D3 are cheap and high-value; D4 and D6 are the structural fixes that stop new
features from multiplying against old ones.

## Validation

Each deliverable keeps the full suite green and adds a targeted test:

- D2: ✅ `unify/UnifierRoleTest` pins the role lifecycle (Plain → combinable `Instantiation`, candidate
  accumulation, taint, carrier/upper-bound/combine-resolved transitions) and the finalizer's protected set —
  `abstractAssocMetaIds` is *exactly* the `AbstractAssoc` metas, not the Plain/combinable/carrier ones — proving
  the catch-all default is gone.
- D3: ✅ `unify/StuckNativeUnifyTest` — the carrier-vs-stuck-native unification adversarial test (F1): a carrier meta
  decomposes against a real constructor (`?F := Box`) but **postpones** against a stuck native (`?F[a] ~ add(x, y)`
  does not solve `?F := add`); plus stuck-native definitional equality (same vs different FQN) and the loud quoter
  failure on a surviving stuck native.
- D4: existing `Coerce` / `Combine` coverage (`MonomorphicTypeCheckTest`) must pass unchanged through the new
  module boundary; add a join-as-coerce spike test if D4's collapse is pursued.
- D5: no behavioural test (pure refactor); rely on the suite.
- D6: shares the keying plan's `MonomorphizationVersioningTest` gate plus the reification end-to-end checks.

## Cross-refs

- `monomorphization-keying-plan.md` — B1 relevance analysis is the same explicit per-binder classification D6
  recommends; the two should share it.
- `backend-portability-plan.md` — its "reduce a feature to ordinary core terms before codegen" target is the
  downstream beneficiary of D4 (Coerce/Combine already reduced to leaf calls / type-only) and D6 (reification).
- `ide-type-hints.md` — error-recovery (Layers A/B) will lean on the checker; D1's named-pipeline shape and D2's
  total role match make a partial/error-recovering pass tractable.
