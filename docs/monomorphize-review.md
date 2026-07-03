# Monomorphize package review

Last performed: **2026-07-03, round 3** (tree at `5c9a7559` — the effect-lift-in-checker landing —
plus this round's quick-wins; round 2 was performed 2026-07-02 at `653bf006`; round 1 at
`60498a40`, its fixes landed as `b88a0c0f` and the four follow-up commits `43b6c6e2`, `1dfcbe3f`,
`68e8c584`, `653bf006`). Round 4 (2026-07-03) landed the R3-1 resolution-ladder dedup (§3.4a) as a
standalone behaviour-neutral commit.

A whole-package architecture review of `lang/src/com/vanillasource/eliot/eliotc/monomorphize/` —
the NbE type checker — after the compiler-as-platform rework (rounds 1–2) and the
effect-lift-in-checker landing (round 3). Three questions: is the NbE algorithm still faithfully
applied, do the language cornerstones still hold, and is the architecture sound / where can it be
simplified. This document records the method (so the review can be repeated), the open findings,
the resolved history, and the work that remains.

## 1. Scope and method (how to repeat)

1. **Read the package in its entirety** — every file under
   `lang/src/com/vanillasource/eliot/eliotc/monomorphize/` (`domain/`, `eval/`, `check/`,
   `refine/`, `unify/`, `lowering/`, `fact/`, `processor/`; ~48 files, ~6.6k lines). Do not
   sample: several findings (the silent `applyValue` fallback, the two-pool gap, the two-form
   ground injection) only show up by cross-referencing a comment in one file against code in
   another.
2. **Cross-check the upstream seams** the package consumes: the `Platform`-keying (and
   `= Platform.Runtime` defaults) of `SaturatedValue.Key`, `OperatorResolvedValue.Key`,
   `UnifiedModuleNames.Key`, `ModuleConstructors.Key`, `AbilityImplementation.Key`, and the
   `getFact`/`getFactOrAbort`/`activeFactKeys` semantics in `CompilerIO`. Since round 3 this
   includes the `effect/` phase (now verification-only): `EffectCheckProcessor` /
   `EffectUsageCollector` / `CalleeSignatures` must stay *accounting* (usedEffects +
   body-effectfulness diagnostics) — any bind/branch/machinery-position logic reappearing there
   is the shadow-type-system anti-pattern the effect-lift move removed, and `EffectCarriers` is
   the shared carrier-binder notion both the phase and `TypeStackLoop.recordAmbientCarriers`
   read.
3. **Verify the invariant checklist** below against the code as read — not against the skill or
   docs, which may be stale (each round has found drift).
4. **Check the `eliot-monomorphize` skill for drift** against what was read; a stale skill
   misleads every future session. (Round 2 found three stale spots even though the skill had been
   updated with every landed commit — symbol moves in refactors don't announce themselves.)
5. **Run the build**: `./mill __.compile && ./mill __.test` must be green before and after any
   fixes.

### Invariant checklist (the repeatable core)

NbE fidelity:

- The evaluator produces **only `VLam`** for `FunctionLiteral` (`NbeEvaluator.eval`); `VPi` is
  produced only checker-side — `Checker` (`infer`'s annotated-literal case, the fresh-`VPi`
  unification in `applyInferred`, `typeImmediateLambda`), the collaborators' type-slot splices
  (`EffectLifter`'s combinator/continuation types, `CalculatedReturnResolver`'s
  guarded-signature rebuild, `RefinementSolver`'s coercion-probe shape) — and the `Function`
  native. (`Evaluator.renormalize` rebuilds a forced `VPi`, it never mints one.)
- **No ORE substitution** anywhere; all binding is Scala-closure capture. (`reifyingWrap` wraps
  binders; it never rewrites a body.)
- Evaluators are **pure and synchronous** — no `CompilerIO` import under `eval/`; all bindings
  prefetched (`prefetchBindings` / `BindingClosure.collectBindings`).
- `unify` is **pure definitional equality**: no assignability arm, no `refinements` map, no
  `Coerce` logic in `unify/`. Directional widening lives only in
  `refine/RefinementSolver.unifyOrCoerce` (check mode). The only unification-time interception is
  combinable-candidate accumulation, covariant side only. The one sanctioned side-door is
  `solveAdopting` (the effect lift's Phase-B pass-through): equality-wise an ordinary bare-meta
  solve — occurs-check included — that merely skips `Combine`-candidate recording; it must never
  grow a relation `unify` itself does not have.
- **Effect-lift discipline** (round 3): the bind-lift / pure-wrap arms are *check-mode
  elaboration only* — consulted from the one shared ladder (`Checker.resolveLadder`, since the R3-1
  dedup), with the bind-lift arm gated by its `allowBindLift` flag (`true` only for argument slots,
  set by `checkArgumentSlot`; `typeImmediateLambda` carries its own `let`-bind rule) and never firing
  at a return boundary (stripping a carrier there would silently drop the effect — `checkAgainst`
  passes `allowBindLift = false`, and the doomed lift shape commits the eager mismatch instead). Both arms
  verify by *speculative* unification and commit only on success; the ladder runs the lift arms
  **before** the `Coerce` probe (its ability-fact generation fails builds as a side effect), with
  guards disjoint from every coercible shape. Carrier recognition reads only the
  `MetaRole.Instantiation.effectCarrier` flag and `CheckState.ambientCarriers` (meta heads
  re-forced at query time) — never a type name. No bind/`pure` decision may live outside the
  checker (grep the `effect/` phase for any resurrected bind-position logic).
- **No silent fallback on any path**: `applyValue` is exhaustive over `SemValue` and yields the
  loud `Reserved(BadApply)` neutral on non-applicable heads; the strict `Quoter` fails on every
  stuck form (`VNeutral`/`VMeta`/`VLam`/`VNative`/`VStuckNative`/unapplied cached `VTopDef`);
  `defaultUnsolvedMetas` is a **total** match on `MetaRole` (no catch-all); the
  `assertEveryMetaResolvedOrAbstract` postcondition is present.
- `VStuckNative` is never injectivity-decomposed (natives are non-injective) and never reads back
  as a ground type.
- **One ground→sem injection**: `Evaluator.groundToSem` is the only `GroundValue → SemValue`
  conversion — no local variant that maps a data value to an inert `VConst(Structure)` instead of
  its applicable constructor `VTopDef`. Grep `VConst(` construction sites: literals/`Direct`
  values only. (Two semantic forms for one ground value do not unify — a latent false "Type
  mismatch." — even though they read back identically; round 2 removed the last such variant.)

Architecture / cornerstones:

- **One evaluator traversal** (`NbeEvaluator`) with exactly three `decompose` adapters; no
  parallel compile-time interpreter anywhere (including the compiler track — `reduceSourced` is
  the same NbE).
- **Supplier/merger discipline**: `ContributedBinding` facts are total (answer `None`, never
  decline); `BindingMergerProcessor` is the single owner of `NativeBinding` and of its recursion;
  suppliers never read `NativeBinding` back.
- **Two-track acyclicity by construction**: no *processor* on the compiler track names
  `MonomorphicValue.Key` (grep it). One sanctioned shared-code exception:
  `CalculatedReturnResolver.readMonomorphicReturnGround` re-enters the runtime `MonomorphicValue`
  unconditionally — documented in-code as compiler-track-deferred (§3.3), fail-safe because no
  compiler-track value uses a calculated return. The native-leaf boundary hard-errors on
  runtime-concrete + compiler-absent names.
- **Pool-guarded platform reads**: leaf contributors (`DataTypeNativesProcessor`,
  `MatchNativesProcessor`) probe `DeclaringPool` before requesting a platform-keyed fact; no
  unconditional runtime-pool value request from a contributor serving both merges.
- **RoleHint reads** are constructor-shape only (`fieldCount` — `PostDrainQuoter.
  materialiseStructure`, `ModuleConstructors`); `typeParamCount` never drives a typing decision.
- **Hard rule 1, two-part scope**: the checker's fold never classifies binders; binder-structure
  reads exist only in the four sanctioned peripheral readers
  (`CarrierKindChecker.recordCarrierMetas`, `AbilityResolver.abilityArity`,
  `SaturatedValue.binderRoles` → `reifyingWrap`, and — since the effect lift —
  `TypeStackLoop.recordAmbientCarriers`) and none of them drives definitional equality.
- **No `platform match` in the checking core**: the per-track differences live in the four
  `check/Track` hooks (`settleReturnPosition`, `pinCarriers`, `implBindings`, `readBackBody`);
  fact keys read `track.platform`.
- Doc/skill hygiene: no scaladoc references to nonexistent `docs/*.md`; no scaladoc describing
  removed parameters/mechanisms; the skill matches the code (symbol names included).

## 2. Verdict and open findings

Verdict (round 3, after the effect-lift-in-checker landing; confirming rounds 1–2): NbE faithfully
applied; cornerstones stand with the documented erosions (E1/E2 below, both slightly *extended*
by the lift and re-accepted); architecture sound. The lift landed exactly on the established
collaborator pattern — `EffectLifter` is a genuine fifth collaborator (narrow injected primitives,
state via `CheckIO`, splices `SemExpression`s like `buildCoercedExpr`, resolved by the existing
`resolve-abilities` pass — no new evaluator, no new resolution machinery), the effect phase kept
only signature-derivable accounting, and every fail-safe surveyed is loud (doomed-postponement
mismatches committed eagerly at return boundaries, wrap-time flex core caught downstream, the
declared-pure diagnostic retained). Full suite green (986/986 targets). No fundamental
restructuring warranted; open items below.

| # | Finding | Status |
|---|---|---|
| C3 | `CalculatedReturnResolver` hosts two weakly-related concerns (calc-return back-edge + W2b guard discharge); guard recognition makes `Either` a language-reserved type by FQN (`isGuardCarrier` keys on `WellKnownTypes.eitherFQN`) | **Open, conditional** — §3.1; do when a second carrier appears |
| F1b | Two-arg HKT carrier over `Function` (`?F[A,B] ~ Function[A,B]`) postpones, `?F` defaults to `Type`, surfaces as a loud "Cannot resolve type." at the use site — an inference limitation, not a miscompile | **Open** — §3.2; needs a real client |
| R2-1 | The calc-return back-edge re-enters the **runtime** `MonomorphicValue` on both tracks; the compiler track should re-enter `CompilerMonomorphicValue` (and its recursion guard watch compiler keys) | **Open, deferred** — §3.3; documented in-code (`readMonomorphicReturnGround` NOTE), fail-safe: no compiler-track calc-return client exists, and a compiler-pool-only callee yields no fact and errors rather than mistypes |
| R3-1 | Ladder-skeleton duplication: `Checker.checkAgainst` (return boundaries) and `Checker.checkAgainstSlot` (argument slots) duplicated ~60 lines each — identical combinable-meta upper-bound deferral, identical instantiation, near-identical pre-arm + failure ladder; the W2b guard-kind acceptance was likewise duplicated between `check`'s fallback and `checkArgumentSlot` | **Resolved** — §3.4a; the shared `resolveLadder` (+ `resolveGuardedLadder` front, `resolveFailureLadder`, `commitMismatch`) with the `allowBindLift` position flag; `checkAgainstSlot` deleted, `checkAgainst` now a thin unwrap. Behaviour-neutral (986/986 green) |
| E1 | Cornerstone erosion, "no generic parameters": **four** peripheral binder-structure readers exist (`CarrierKindChecker.recordCarrierMetas`, `AbilityResolver.abilityArity`, `binderRoles`→`reifyingWrap`, and since the lift `TypeStackLoop.recordAmbientCarriers`) | **Accepted & documented** — round 3 re-accepted the fourth reader (sanctioned in the effect-lift design as "a peripheral binder read, like `binderRoles`"); none drives equality |
| E2 | Cornerstone erosion, "no constraint set": `MetaRole.Instantiation.candidates`/`upperBounds` are a small role-scoped constraint store — extended in round 3 by the `effectCarrier` flag (elaboration bookkeeping, not a constraint) and by `Unifier.solveAdopting` (a bare-meta solve variant that skips candidate recording, not a new relation) | **Accepted & documented** — the rule is scoped to *equality* constraints |

### Resolved findings (history)

Round 1 (`b88a0c0f` + follow-ups):

- **F1** — silent `applyValue` fallback on non-applicable heads → loud `$bad-apply` stuck neutral
  (`b88a0c0f`; head made structural `Reserved(BadApply)` by F3).
- **F2** — dependent Π checked non-dependently; env conflated Γ and ρ → `CheckState` splits
  `gamma`/`rho`, `check` binds a fresh neutral and checks against `codomain(neutral)`;
  `typeStackValueParams` + the `monoEnv` `VConst`-rewrite deleted (`68e8c584`).
- **F3** — convention-held neutral identity → structural `NeutralHead` sum
  (`Param`/`Fresh(Origin, depth)`/`Reserved(Marker)`), identity by constructor (`653bf006`).
- **G1** — two-pool gap in leaf contributors → `DeclaringPool` membership probe (`b88a0c0f`).
- **G2/G3** — stale scaladoc doc-refs; stale skill → refreshed (`b88a0c0f`).
- **C1** — `TypeStackLoop` god-object → ability resolution extracted to `check/AbilityResolver`
  (`43b6c6e2`), compiler-track carrier pinning moved onto `Track.Compiler` (`1dfcbe3f`).
- **C2** — platform leaked as scattered conditionals → `check/Track` strategy (two case objects,
  four hooks); the `resolveAbility` third `Platform` param deleted (`1dfcbe3f`). Deviation kept:
  `reduceSourced` physically stays in `PostDrainQuoter` (shares its machinery; the `readBackBody`
  hook dispatches).

Round 2 (this round's quick-wins):

- **R2-2** — *two semantic forms for one ground value*: `TypeStackLoop.applyTypeArgs` kept a
  value-level argument as an inert `VConst(Structure)` in the signature closure while ρ held its
  `groundToSem` constructor-`VTopDef` form. The forms read back identically but do **not** unify —
  a latent false "Type mismatch." for a data-value type argument occurring in a type position
  (`def f[P: Person](x: Tagged[P])`: Γ's `paramType` evaluates via ρ, the expected domain came
  from the closure). Not exercised by any test (the common dependent case, `Int` bounds, uses
  `Direct` literals, where the forms coincide) and fail-safe (rejects, never mistypes) — fixed by
  deleting the local `toSemArg` and injecting every argument through the one canonical
  `groundToSem`. The rationale the old comment gave ("the reification gate recognises the
  `VConst(ground)` form") was stale: the gate materialises from *quoted ground* values, which both
  forms produce identically.
- **R2-3** — dead code: `Env.lookupByLevel` (no caller anywhere); `Env`'s scaladoc described
  by-level lookup and "name-to-level resolution at Checker time," neither of which exists (lookup
  is by name, last-bound-wins). Removed/corrected.
- **R2-4** — stale scaladoc: `PostDrainQuoter`'s class doc and `structuralQuote` still described
  the `runtimeParams` set that F2's fix removed (runtime-ness is read off ρ). Corrected.
- **R2-5** — skill drift: `TypeStackLoop.abilityArity` (moved to `AbilityResolver` in C1), the
  `Env` line, and the `applyTypeArgs` description (pre-R2-2 two-form behaviour). Corrected.

Round 3 (this round's quick-wins — all documentation; no code path changed):

- **R3-2** — stale scaladoc: `EffectLifter`'s class doc stated the resolution ladder as
  "unify → coerce → bind-lift → pure-wrap → mismatch" and "both arms fire only after
  `tryUnifyOrCoerce` failed" — the *plan's original* ordering, superseded during step 4 by
  lift-arms-before-`Coerce` (recorded in docs/effect-lift-in-checker.md's status notes) and by
  the checker composing the arms itself around `tryUnifyCommitting`/`tryCoerce`. Also claimed
  "exactly one checker primitive" while two are injected. Corrected to the implemented order.
- **R3-3** — stale scaladoc: `RefinementSolver.tryUnifyOrCoerce` claimed to be the seam "the
  checker consults the effect-lift arms 3–4" around — the checker never calls it; its sole
  caller is `unifyOrCoerce` (the `let`-level resolution). Doc corrected; the method stays (it
  names the non-committing equality+`Coerce` pair `unifyOrCoerce` needs).
- **R3-4** — skill drift: hard rule 1 still said *three* peripheral binder readers (missing
  `recordAmbientCarriers`), and the `TypeStackLoop` step list omitted the ambient-carrier
  recording step. Corrected; checklist above updated to match (four readers, broader `VPi`
  producer wording, the effect-lift discipline block, the `solveAdopting` side-door note).

Round 4 (the R3-1 dedup — its own commit, behaviour-neutral):

- **R3-1** (§3.4a) — resolution-ladder dedup: the two ~60-line ladders `checkAgainst` (return
  boundaries) and `checkAgainstSlot` (argument slots) collapsed into one shared `Checker.resolveLadder`
  (combinable-meta upper-bound deferral → instantiate → pre-arms → failure ladder), with the single
  position difference carried by an `allowBindLift: Boolean` — `true` consults the bind-lift arm (arm 3)
  as a pre-arm and in the failure ladder and can return `SlotOutcome.Bound`; `false` omits it and turns
  the doomed `mustLiftBeforeUnify` shape into the eager mismatch. The failure path is `resolveFailureLadder`
  and the exact-mismatch commit is `commitMismatch`. The W2b guard-kind acceptance (duplicated between
  `check`'s fallback and `checkArgumentSlot`) folded into a thin `resolveGuardedLadder` front that both
  fresh-check sites share; `checkAgainst` is now `resolveGuardedLadder(allowBindLift = false)` unwrapping
  `Resolved`. `checkAgainstSlot` deleted. **One deliberate deviation from the §3.4a sketch** (kept the
  refactor provably neutral): the deferred-slot re-entry (`resolveDeferredSlot`) calls the bare
  `resolveLadder` — *not* the guarded front — because that path never ran a guard check before (a Deferred
  slot is always effect-carrier-headed, never a `Type`-kind guard carrier), so routing it through the
  guard would have been a (fail-safe-direction, but real) behaviour change on the compiler track. Full
  suite green (986/986).

## 3. Open work (in recommended order)

### 3.1 Guard-discharge split + the `Either` decision (conditional — do when a second carrier appears)

1. If/when effectful signatures grow beyond the `Throw[String]`/`Either[String,_]` carrier, split
   the W2b hooks (`isGuardCarrier`, `dischargeGuardedReturn`, `dischargeGuardedSignature`,
   `extractGuardMessage`) out of `CalculatedReturnResolver` into a `check/GuardDischarge`
   collaborator.
2. Independently of the split, make the reservation decision explicit: today any `Either`-headed
   value in kind position is treated as a guard (`isGuardCarrier` keys on
   `WellKnownTypes.eitherFQN`). Either bless `Either` as *the* reserved compile-time carrier
   (document in CLAUDE.md + the skill), or key recognition off the pinned `Throw`-carrier
   instead of the type name. (The same reservation exists on the producer side:
   `Track.Compiler.pinCarriers` hardcodes `throwAbilityFQN` — decide both together.)

### 3.2 HKT-over-`Function` inference (F1b — only if a client appears)

Two approaches were tried and rejected during the round-1 fixes — decomposing `VPi` in the
unifier (reverses the deliberate `CarrierKindChecker` design for non-rigid heads) and defaulting
the carrier to an arity-matched former (works applied, fails when `?F` is read back unapplied; no
default is both applicable and quotable-unapplied without masking spine mismatches). A real
solution needs a quotable carrier value for partially-applied `Function` (e.g. a first-class
read-back for `VLam`-shaped carriers), not a unifier tweak.

### 3.3 Compiler-track calc-return re-entry (deferred — do when a compiler-track client appears)

`CalculatedReturnResolver.readMonomorphicReturnGround` re-enters `MonomorphicValue` (runtime)
unconditionally, and its recursion guard scans the active chain for runtime keys only. When a
compiler-platform value first uses a calculated return, thread the track: re-enter
`CompilerMonomorphicValue` and guard on its key. Until then the gap is fail-safe (a compiler-pool
callee yields no runtime fact ⟹ the caller errors; never a silently wrong type).

### 3.4a Resolution-ladder dedup (R3-1 — **DONE**, round 4)

Landed as its own behaviour-neutral commit. `checkAgainst` and `checkAgainstSlot` were the same
algorithm twice — (combinable-meta upper-bound deferral) → (instantiate) → (pre-arms for the
doomed-postponement shapes) → (unify → …arms… → `Coerce` → committed mismatch) — differing only by
*position*: the bind-lift arm exists only at argument positions, and the outcome type (`SlotOutcome`
vs a plain `SemExpression`).

Now one shared `Checker.resolveLadder(tm, expr, inferred, forcedExpected, expected, allowBindLift)`
returning `SlotOutcome`. `allowBindLift = false` (return boundary) omits the lift arm and turns the
doomed pre-arm into the eager mismatch; `checkAgainst` calls it (via the guard front) and unwraps
`Resolved` (a `Bound` is unreachable by construction). The failure path is `resolveFailureLadder`;
the exact-mismatch commit is `commitMismatch`. The W2b guard-kind acceptance folded into a thin
`resolveGuardedLadder` front shared by `check`'s fallback (through `checkAgainst`) and
`checkArgumentSlot`. `checkAgainstSlot` deleted.

**Deviation from the sketch above** (`bindArm = None`): the guard front is a *separate* method rather
than folded into `resolveLadder` itself, because the deferred-slot re-entry (`resolveDeferredSlot`)
must call the *bare* `resolveLadder` — it never ran the guard check before, and a Deferred slot is
always effect-carrier-headed (never a `Type`-kind guard carrier), so folding the guard into the one
entry would have changed that path's behaviour (in the fail-safe accept direction, but still a change).
A `Boolean allowBindLift` replaced the sketch's `Option` bind-arm — the arm's call is identical at
both positions, so a flag suffices. Pinned by the existing suite + step-5 matrix (986/986 green).

### 3.4 Minor / opportunistic

- Replace the per-reference `AbilityResolver.abilityArity` marker read with a small `AbilityArity`
  fact (or a field forwarded on an existing ability fact, per the lean-fact-flow rule) computed
  once per ability. (Round-1 §3.1 step 4, still open.)
- **Pattern-rule scope check** (noted, not scheduled): the empty-spine meta solve has an
  occurs-check but no scope check, so a solution can capture a `Fresh(Unify, depth)` probe var
  that escapes its binder (classic skolem escape). Within one check session the `Unifier.depth`
  counter is globally monotone (threaded through the state), so no two live binders share a head,
  and an escaped fresh var fails read-back loudly ("Cannot quote neutral value") — fail-safe,
  worst case a spurious rejection. Add the scope check only if a real program hits it.

## 4. Considered and rejected (do not re-litigate without new evidence)

- **Merging `SemExpression` and `MonomorphicExpression` into one `Expression[T]` ADT** (they are
  shape-identical up to the type-slot type). Rejected: the two types *are* the phase boundary —
  `SemExpression` may carry unsolved metas, `MonomorphicExpression` is guaranteed fully ground —
  and the compiler currently enforces that invariant through the type distinction. A merge would
  trade that guarantee for deduplicating a handful of small traversals, and would churn every
  backend consumer (`used`, `uncurry`, `jvm`).
- **Merging `BindingClosure` and `ReducedBindingClosure`**. The shared core (activeFactKeys-guarded
  dependency fetch) is ~15 lines; the two differ in IR, evaluator, and the `reifyingWrap` step, so
  a shared abstraction would exceed the duplication. Reconsider if a third closure builder appears.
- **Folding `Function`/`VPi` into an ordinary `data` declaration** — forbidden by the standing
  cornerstone guardrail (`VPi` is the one primitive Π-former on principle).
