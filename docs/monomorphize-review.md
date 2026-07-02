# Monomorphize package review

Last performed: **2026-07-02, round 2** (tree at `653bf006` + this round's quick-wins; round 1 was
performed at `60498a40`, its fixes landed as `b88a0c0f` and the four follow-up commits `43b6c6e2`,
`1dfcbe3f`, `68e8c584`, `653bf006`).

A whole-package architecture review of `lang/src/com/vanillasource/eliot/eliotc/monomorphize/` —
the NbE type checker — after the compiler-as-platform rework. Three questions: is the NbE
algorithm still faithfully applied, do the language cornerstones still hold, and is the
architecture sound / where can it be simplified. This document records the method (so the review
can be repeated), the open findings, the resolved history, and the work that remains.

## 1. Scope and method (how to repeat)

1. **Read the package in its entirety** — every file under
   `lang/src/com/vanillasource/eliot/eliotc/monomorphize/` (`domain/`, `eval/`, `check/`,
   `refine/`, `unify/`, `lowering/`, `fact/`, `processor/`; ~47 files, ~5.7k lines). Do not
   sample: several findings (the silent `applyValue` fallback, the two-pool gap, the two-form
   ground injection) only show up by cross-referencing a comment in one file against code in
   another.
2. **Cross-check the upstream seams** the package consumes: the `Platform`-keying (and
   `= Platform.Runtime` defaults) of `SaturatedValue.Key`, `OperatorResolvedValue.Key`,
   `UnifiedModuleNames.Key`, `ModuleConstructors.Key`, `AbilityImplementation.Key`, and the
   `getFact`/`getFactOrAbort`/`activeFactKeys` semantics in `CompilerIO`.
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
  produced only by the `Checker` (`infer`'s annotated-literal case and the fresh-`VPi` unification
  in `applyInferred`) and the `Function` native.
- **No ORE substitution** anywhere; all binding is Scala-closure capture. (`reifyingWrap` wraps
  binders; it never rewrites a body.)
- Evaluators are **pure and synchronous** — no `CompilerIO` import under `eval/`; all bindings
  prefetched (`prefetchBindings` / `BindingClosure.collectBindings`).
- `unify` is **pure definitional equality**: no assignability arm, no `refinements` map, no
  `Coerce` logic in `unify/`. Directional widening lives only in
  `refine/RefinementSolver.unifyOrCoerce` (check mode). The only unification-time interception is
  combinable-candidate accumulation, covariant side only.
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
  reads exist only in the three sanctioned peripheral readers
  (`CarrierKindChecker.recordCarrierMetas`, `AbilityResolver.abilityArity`,
  `SaturatedValue.binderRoles` → `reifyingWrap`) and none of them drives definitional equality.
- **No `platform match` in the checking core**: the per-track differences live in the four
  `check/Track` hooks (`settleReturnPosition`, `pinCarriers`, `implBindings`, `readBackBody`);
  fact keys read `track.platform`.
- Doc/skill hygiene: no scaladoc references to nonexistent `docs/*.md`; no scaladoc describing
  removed parameters/mechanisms; the skill matches the code (symbol names included).

## 2. Verdict and open findings

Verdict (round 2, confirming round 1): NbE faithfully applied; cornerstones stand with the two
documented erosions (E1/E2 below); architecture sound. The round-1 fixes pulled their weight —
`TypeStackLoop` is down from ~680 to ~440 lines with four symmetric collaborators plus the `Track`
seam, the Γ/ρ split (F2) and structural `NeutralHead` (F3) removed the two convention-held
fragilities, and the invariant checklist verified clean against the code. No new fundamental
restructuring is warranted; the remaining open items are below.

| # | Finding | Status |
|---|---|---|
| C3 | `CalculatedReturnResolver` hosts two weakly-related concerns (calc-return back-edge + W2b guard discharge); guard recognition makes `Either` a language-reserved type by FQN (`isGuardCarrier` keys on `WellKnownTypes.eitherFQN`) | **Open, conditional** — §3.1; do when a second carrier appears |
| F1b | Two-arg HKT carrier over `Function` (`?F[A,B] ~ Function[A,B]`) postpones, `?F` defaults to `Type`, surfaces as a loud "Cannot resolve type." at the use site — an inference limitation, not a miscompile | **Open** — §3.2; needs a real client |
| R2-1 | The calc-return back-edge re-enters the **runtime** `MonomorphicValue` on both tracks; the compiler track should re-enter `CompilerMonomorphicValue` (and its recursion guard watch compiler keys) | **Open, deferred** — §3.3; documented in-code (`readMonomorphicReturnGround` NOTE), fail-safe: no compiler-track calc-return client exists, and a compiler-pool-only callee yields no fact and errors rather than mistypes |
| E1 | Cornerstone erosion, "no generic parameters": three peripheral binder-structure readers exist (`CarrierKindChecker.recordCarrierMetas`, `AbilityResolver.abilityArity`, `binderRoles`→`reifyingWrap`) | **Accepted & documented** — hard rule restated two-part in the skill |
| E2 | Cornerstone erosion, "no constraint set": `MetaRole.Instantiation.candidates`/`upperBounds` are a small role-scoped constraint store | **Accepted & documented** — the rule is scoped to *equality* constraints |

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
