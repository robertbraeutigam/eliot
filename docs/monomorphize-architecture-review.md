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

### 2. Meta roles live in side-tables, not in the values

A `VMeta` is just an `Int`; *what role it plays* is recovered by membership in one of several `CheckState`
side-tables: `bindingCache`, `abstractTypeMetas`, `abilityResolutions`, `combineResolved`, `pendingUpperBounds`,
`typeStackValueParams`, `carrierKinds`, plus `combinable` / `candidates` inside the unifier. The post-check
section of `TypeStackLoop.processIO` is then an **implicit pipeline of ~8 ordered passes over this shared
blackboard**:

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

### F1 — `VTopDef(fqn, None, spine)` is overloaded for three incompatible things

The same physical shape represents:

1. **data / type constructors** — injective, rigid;
2. **abstract `def`s** (`some` / `none`, body-less platform signatures) — rigid, opaque, **not injective**;
3. **stuck native applications** — `add(?,?)`, `min(?,?)`, emitted as `VTopDef(fqn, None, spine)` by
   `SystemNativesProcessor.stuck` so `Evaluator.renormalize` can re-fire them — and **definitely not injective**
   (`add(1,3) == add(2,2)`).

But `Unifier.tryDecomposeApplied` (`Unifier.scala:186`) injectivity-decomposes **any** `VTopDef(_, None, _)`
head: `?F[a] ~ add(x, y)` would solve `?F := add` and unify the arguments pointwise, treating `add` as an
injective constructor. Likewise `Quoter.quote` (`Quoter.scala:47`) reads any `VTopDef(_, None, spine)` back as a
ground *type* `Structure(fqn, …)`, so a stuck native surviving to quote-time becomes a nonsense ground type
rather than an error. `renormalize` currently keeps those paths from being hit in practice, but the
representation makes "injective constructor," "opaque abstract def," and "non-injective stuck native"
indistinguishable to the unifier and the quoter.

### F2 — `defaultUnsolvedMetas` masks unresolved obligations as `Type`

Any meta not in the `abstractTypeMetas` protected set is solved to `VType` at the end of checking. A side-car
that fails to resolve its meta therefore produces a silently mistyped value, not a diagnostic. This is the
structural reason "gaps must be fail-safe" keeps needing manual enforcement here.

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

### D1 — Name the post-check resolution pipeline (low risk, do first)

Make `TypeStackLoop.processIO`'s tail an **explicit ordered list of named passes**, each with a documented
pre/postcondition, terminating in an assertion "every meta is resolved-or-explicitly-defaulted." No semantic
change. Payoff: the ordering becomes auditable, and the next feature has exactly one place to plug in instead of
being threaded by hand through a comment-documented sequence.

### D2 — Replace the meta side-sets with one `Map[MetaId, MetaRole]` ADT (highest structural payoff)

Introduce `MetaRole = Plain | Combinable(candidates) | Carrier(kind) | AbstractAssoc(fqn) | CalculatedReturn`
and fold `combinable` / `candidates` / `carrierKinds` / `abstractTypeMetas` / `pendingUpperBounds` /
`combineResolved` into role-keyed state. Then `defaultUnsolvedMetas` becomes a **total match on roles** — a
`Carrier` left unsolved is an *error by construction*, an `AbstractAssoc` stays, a `Plain` becomes `Type`. This
deletes F2 outright: an unhandled role cannot silently become `Type`. Medium effort; the single biggest
reduction in "non-obvious bug" surface.

### D3 — Disambiguate `VTopDef(_, None, _)` (small, removes F1)

Give stuck native applications a distinct head (or a discriminating flag) so the unifier never
injectivity-decomposes a non-injective native and the quoter fails loudly on a surviving one. Small, surgical,
removes the sharpest latent-unsoundness landmine. Pair with an adversarial test that unifies a carrier meta
against a stuck-native-headed value to confirm reachability before/after.

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

- D2: a test that an unresolved meta of each role produces the role-specific outcome (error vs default vs stays
  abstract) — proving the catch-all default is gone.
- D3: the carrier-vs-stuck-native unification adversarial test (F1).
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
