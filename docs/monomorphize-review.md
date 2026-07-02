# Monomorphize package review

Last performed: **2026-07-02** (tree at `60498a40`; quick-win fixes landed as `b88a0c0f`).

A whole-package architecture review of `lang/src/com/vanillasource/eliot/eliotc/monomorphize/` —
the NbE type checker — after the compiler-as-platform rework. Three questions: is the NbE
algorithm still faithfully applied, do the language cornerstones still hold, and is the
architecture sound / where can it be simplified. This document records the method (so the review
can be repeated), the findings with their status, and the suggested implementation steps that
remain open.

## 1. Scope and method (how to repeat)

1. **Read the package in its entirety** — every file under
   `lang/src/com/vanillasource/eliot/eliotc/monomorphize/` (`domain/`, `eval/`, `check/`,
   `refine/`, `unify/`, `lowering/`, `fact/`, `processor/`; ~34 files, ~5.5k lines). Do not
   sample: several findings (the silent `applyValue` fallback, the two-pool gap) only show up by
   cross-referencing a comment in one file against code in another.
2. **Cross-check the upstream seams** the package consumes: the `Platform`-keying of
   `SaturatedValue.Key`, `OperatorResolvedValue.Key`, `UnifiedModuleNames.Key`,
   `ModuleConstructors.Key`, `AbilityImplementation.Key`, and the `getFact`/`getFactOrAbort`/
   `activeFactKeys` semantics in `CompilerIO`.
3. **Verify the invariant checklist** below against the code as read — not against the skill or
   docs, which may be stale (they were).
4. **Check the `eliot-monomorphize` skill for drift** against what was read; a stale skill
   misleads every future session.
5. **Run the build**: `./mill __.compile && ./mill __.test` must be green before and after any
   fixes.

### Invariant checklist (the repeatable core)

NbE fidelity:

- The evaluator produces **only `VLam`** for `FunctionLiteral` (`NbeEvaluator.eval`); `VPi` is
  produced only by the `Checker` (literal checked against `VType`) and the `Function` native.
- **No ORE substitution** anywhere; all binding is Scala-closure capture. (`reifyingWrap` wraps
  binders; it never rewrites a body.)
- Evaluators are **pure and synchronous** — no `CompilerIO` import under `eval/`; all bindings
  prefetched (`prefetchBindings` / `BindingClosure.collectBindings`).
- `unify` is **pure definitional equality**: no assignability arm, no `refinements` map, no
  `Coerce` logic in `unify/`. Directional widening lives only in
  `refine/RefinementSolver.unifyOrCoerce` (check mode). The only unification-time interception is
  combinable-candidate accumulation, covariant side only.
- **No silent fallback on any path**: `applyValue` is exhaustive over `SemValue` and yields the
  loud `$bad-apply` neutral on non-applicable heads; the strict `Quoter` fails on every stuck
  form (`VNeutral`/`VMeta`/`VLam`/`VNative`/`VStuckNative`/unapplied cached `VTopDef`);
  `defaultUnsolvedMetas` is a **total** match on `MetaRole` (no catch-all); the
  `assertEveryMetaResolvedOrAbstract` postcondition is present.
- `VStuckNative` is never injectivity-decomposed (natives are non-injective) and never reads back
  as a ground type.

Architecture / cornerstones:

- **One evaluator traversal** (`NbeEvaluator`) with exactly three `decompose` adapters; no
  parallel compile-time interpreter anywhere (including the compiler track — `reduceSourced` is
  the same NbE).
- **Supplier/merger discipline**: `ContributedBinding` facts are total (answer `None`, never
  decline); `BindingMergerProcessor` is the single owner of `NativeBinding` and of its recursion;
  suppliers never read `NativeBinding` back.
- **Two-track acyclicity by construction**: nothing under the compiler track names
  `MonomorphicValue.Key` (grep it); the native-leaf boundary hard-errors on runtime-concrete +
  compiler-absent names.
- **Pool-guarded platform reads**: leaf contributors (`DataTypeNativesProcessor`,
  `MatchNativesProcessor`) probe `DeclaringPool` before requesting a platform-keyed fact; no
  unconditional runtime-pool value request from a contributor serving both merges.
- **RoleHint reads** are constructor-shape only (`fieldCount` — `PostDrainQuoter.
  materialiseStructure`, `ModuleConstructors`); `typeParamCount` never drives a typing decision.
- **Hard rule 1, two-part scope**: the checker's fold never classifies binders; binder-structure
  reads exist only in the three sanctioned peripheral readers
  (`CarrierKindChecker.recordCarrierMetas`, `TypeStackLoop.abilityArity`,
  `SaturatedValue.binderRoles` → `reifyingWrap`) and none of them drives definitional equality.
- Doc/skill hygiene: no scaladoc references to nonexistent `docs/*.md`; the skill matches the
  code.

## 2. Findings and status

| # | Finding | Status |
|---|---|---|
| F1 | `Evaluator.applyValue` silently returned the argument on non-applicable heads (`VConst`/`VType`) — silently collapsed `F[A]` to `A` | **Fixed** in `b88a0c0f`: loud `$bad-apply` stuck neutral; exhaustive match; `EvaluatorApplyValueTest` |
| F1b | The F1 fix exposed a masked miscompile: two-arg HKT carrier over `Function` (`?F[A,B] ~ Function[A,B]`) postpones (a `VPi` is deliberately not injectivity-decomposed), `?F` defaults to `Type`, and the old fallback minted the nonsense type `BigInteger[String]` with no error | **Surfaced** in `b88a0c0f`: now a loud "Cannot resolve type." at the use site; test renamed and documents the inference limitation. Real HKT-over-`Function` inference is open (§3.5) |
| F2 | Dependent Π checked non-dependently: body checked against `codomain(paramType)`, not `codomain(neutral)`; the env conflates "binding = its type" (runtime params) with "binding = its value" (erased params), patched by `typeStackValueParams` + the `monoEnv` `VConst` rewrite | **Fixed** (§3.4 landed): `CheckState` splits Γ (`gamma`, name→type) from ρ (`rho`, name→value); `check` binds a fresh neutral in ρ and checks the body against `codomain(neutral)`; `infer` reads Γ; `typeStackValueParams` + the `monoEnv` rewrite deleted |
| F3 | Neutral identity (`VVar(level, name)`) is convention-held: env levels, unifier depth, and quote depth are independent counters; collisions avoided only by reserved names | **Fixed** (§3.5 landed): `NeutralHead` is now structural — `Param(level, name)` (bound vars) / `Fresh(Origin, depth)` (unifier+quoter probes) / `Reserved(Marker)` (bad-apply, guard-probe, coerce-arg, match); heads compare by constructor, `name` is display-only |
| G1 | Two-pool gap: leaf contributors read runtime-pool facts unconditionally while their `ContributedBinding` serves both platform merges | **Fixed** in `b88a0c0f`: `DeclaringPool` membership probe, compiler-pool fallback, `CompilerOnlyDataNativesTest` |
| G2 | Stale scaladoc references to deleted `docs/monomorphize-d1-design.md`, `docs/effectful-signatures.md`, `docs/block-syntax.md` | **Fixed** in `b88a0c0f` |
| G3 | `eliot-monomorphize` skill significantly stale (pre-rework: `forceAndConst`, `VMeta.expected`, `nameLevels`, no supplier/merger, no tracks) | **Fixed** in `b88a0c0f`: rewritten against sources |
| C1 | `TypeStackLoop` god-object (~680 lines): inlines ability resolution (~200 lines) and compiler-track carrier pinning beside the fold + pipeline runner | **Partly fixed**: ability resolution extracted to `check/AbilityResolver` (the fourth collaborator, §3.1 landed); compiler-track carrier pinning remains inline (folds into §3.2's `Track` seam) |
| C2 | Platform leaked into the checking core as ~6 scattered `platform match` conditionals; the `resolveAbility` seam's third parameter is hardcoded `Platform.Runtime` at some call sites and ignored at others | **Fixed** (§3.2 landed): `check/Track` seam (`Track.Runtime` / `Track.Compiler`) carries the platform + the four hooks; `TypeStackLoop`/`Checker` take the `Track`; the `resolveAbility` third `Platform` param is deleted. One deviation: `reduceSourced` stays in `PostDrainQuoter` (the `readBackBody` hook dispatches to it) rather than being physically extracted |
| C3 | `CalculatedReturnResolver` hosts two weakly-related concerns (calc-return back-edge + W2b guard discharge); guard recognition makes `Either` a language-reserved type by FQN | **Open** — §3.3, conditional |
| E1 | Cornerstone erosion, "no generic parameters": three peripheral binder-structure readers exist | **Accepted & documented** — hard rule restated two-part in the skill |
| E2 | Cornerstone erosion, "no constraint set": `MetaRole.Instantiation.candidates`/`upperBounds` are a small role-scoped constraint store | **Accepted & documented** — the rule is scoped to *equality* constraints |

Overall verdict at review time: NbE faithfully applied (strengthened by the rework in read-back
strictness and evaluator unification); cornerstones stand with the two documented erosions;
architecture sound — the supplier/merger design, two-track acyclicity, and the collaborator
pattern (D4/D7/D8) are pulling their weight.

## 3. Suggested implementation steps (open work, in recommended order)

### 3.1 Extract `check/AbilityResolver` (medium, behavior-preserving) — **DONE**

Made ability resolution the fourth collaborator, symmetrical with `solver`/`calcReturns`/
`carriers`:

1. **Done.** Moved `collectAbilityRefs`, the `resolveAbilities` saturation-round body,
   `tryResolveOne`, `abilityArity`, and `injectForImpl` from `TypeStackLoop` into
   `check/AbilityResolver`, constructed with the primitives it actually uses (`resolveAbility`,
   `fetchBinding`, `platform`; state access stays via `CheckIO`). The `AbilityRef` type alias
   moved to its companion. `AbilityResolver` is built in `Checker` (`checker.abilityResolver`),
   exactly like `solver`/`calcReturns`/`carriers`.
2. **Done.** `TypeStackLoop.processIO` seeds refs via `checker.abilityResolver.collectAbilityRefs`
   and the `resolve-abilities` saturation pass delegates to
   `checker.abilityResolver.resolveAbilities`; `PassContext` is unchanged.
3. **Done.** Pure move — no behavior change. The whole suite is green (`CompilerAbilityResolutionTest`,
   the ability/`Dep`/higher-kinded cases in `MonomorphicTypeCheckTest`); no mock-state unit tests added.
4. Optional follow-up (**not done**): replace the per-reference `abilityArity` marker read with a
   small `AbilityArity` fact (or a field forwarded on an existing ability fact, per the
   lean-fact-flow rule) computed once per ability.

### 3.2 Track strategy object (medium) — **DONE**

Removed the scattered `platform match` conditionals from the checking core:

1. **Done.** Introduced `check/Track` (a sealed trait with two case objects `Track.Runtime` /
   `Track.Compiler`) carrying the `Platform` value plus four hooks, extracted 1:1 from the former
   conditionals:
   - `settleReturnPosition` — the calc-return / guard-discharge / pass-through switch (the shared
     `calcReturn` branch is a `final` method on the trait; the platform-specific `settleGuardedReturn`
     is the abstract hook);
   - `pinCarriers` — compiler-track `{Throw[E]}` → `Either[E]` pinning (no-op on runtime); the
     `throwCarrierErrorType` / `pinCarrierToEither` helpers and the `throwAbilityFQN` constant moved
     with it onto `Track.Compiler`;
   - `implBindings` — the compiler track's resolved-impl binding fetch (empty on runtime);
   - `readBackBody` — `reduceSourced` (compiler) vs `quoteSourced` (runtime).
   - **Deviation:** `reduceSourced` was *not* physically moved out of `PostDrainQuoter`. It is a
     read-back variant that shares all of the quoter's machinery (`semEvaluator`, `monoEnv`,
     `materialise`, `quoteSourced` fallback), and `PostDrainQuoter` still legitimately keys facts by
     `platform` (`constructorRole`), so extracting it would expose the quoter's internals for no net
     removal of a platform branch. The `readBackBody` hook dispatches to it instead. The stated goal
     — no `platform match` in the checking core — is met.
2. **Done.** `TypeStackLoop` and `Checker` take the `Track` instead of a bare `Platform`; fact keys
   read `track.platform` (a private `val platform = track.platform` in `Checker`, so the four
   collaborators keep their bare-`Platform` construction unchanged).
3. **Done.** Deleted the third `Platform` parameter from the `resolveAbility` seam
   (`TypeStackLoop`/`Checker`/`RefinementSolver`/`AbilityResolver` and both processors'
   `resolveAbilityImpl`). The deferred properly-typed `Position` reintroduction is still future work,
   gated on a real position-routed client.
4. **Done.** Behavior-preserving; the full suite is green, with the compiler-track tests
   (`CompilerEitherCarrierTest`, `CompilerNativeLeafBoundaryTest`,
   `CompilerMonomorphicTypeCheckProcessorTest`, `CompilerAbilityResolutionTest`) covering the
   compiler hooks.

### 3.3 Guard-discharge split + the `Either` decision (conditional — do when a second carrier appears)

1. If/when effectful signatures grow beyond the `Throw[String]`/`Either[String,_]` carrier, split
   the W2b hooks (`isGuardCarrier`, `dischargeGuardedReturn`, `dischargeGuardedSignature`,
   `extractGuardMessage`) out of `CalculatedReturnResolver` into a `check/GuardDischarge`
   collaborator.
2. Independently of the split, make the reservation decision explicit: today any `Either`-headed
   value in kind position is treated as a guard (`isGuardCarrier` keys on
   `WellKnownTypes.eitherFQN`). Either bless `Either` as *the* reserved compile-time carrier
   (document in CLAUDE.md + the skill), or key recognition off the pinned `Throw`-carrier
   instead of the type name.

### 3.4 Split the typing context from the evaluation environment (fundamental) — **DONE**

The one redesign that removes a *category* of special cases (closes F2). The former single `Env`
served as both Γ (runtime param → its **type**) and ρ (erased param → its **value**), discriminated
by the `typeStackValueParams` name-set and patched by the `monoEnv` `VConst`-rewrite. The textbook
NbE-checker shape now keeps them separate:

1. **Done.** `CheckState` carries `gamma` (Γ: name → type) and `rho` (ρ: name → `SemValue`; erased
   params bound to values, runtime value params bound to **fresh neutrals** via `paramNeutral`,
   peeled instantiation metas to the meta). Both are `Env`s and grow in lockstep. `bind` split into
   `bindValueParam` / `bindTypeStackParam` / `bindTypeParam`.
2. **Done.** `Checker.check` (`FunctionLiteral` vs `VPi`): captures `paramNeutral` (fresh neutral),
   binds it in ρ and the param type in Γ (`bindValueParam`), checks the body against
   `codomain(paramNeutral)` — genuine dependent Π. (Every `VPi` codomain in current Eliot is
   constant, so this is a behavioural no-op today and correct once dependent types land.)
3. **Done.** `infer`'s `ParameterReference` reads `gamma` directly; `typeStackValueParams` and the
   `VConst`-vs-type special case deleted.
4. **Done.** `TypeStackLoop.applyTypeArgs` binds the `groundToSem` form into ρ and the arg's type
   into Γ (computed from the ground arg — a *type* arg is its own type slot, a *value* arg its
   declared `valueType`); `monoEnv` is now just `inspect(_.rho)` and the post-hoc `VConst → groundToSem`
   rewrite is deleted.
5. **Done.** `PostDrainQuoter` derives runtime-ness from ρ via `isRuntimeParam` (a name bound to a
   neutral) instead of threading a separate `runtimeParams: Set[String]`.
6. Evaluators unchanged (still name-based lookup in ρ, which is the env now passed to `eval`).
7. Risk was in call sites relying on "env binding = type" flowing into a codomain; the full suite
   (incl. `ReificationTest`, `ComputedTypeArgumentReadbackTest`, the dependent-bounds
   `Int[add(L,R),…]` cases) is green, confirming behaviour preservation.

### 3.5 Small hardening (opportunistic)

- **F3 — DONE**: `NeutralHead` (`domain/SemValue.scala`) is now a structured sum, so neutral identity
  is by *constructor*, not by convention-held reserved names / magic levels:
  - `Param(level, name)` — genuine bound variables (runtime value params, unresolved param refs,
    read-back binders) and the throwaway named placeholders diagnostics/printing substitute.
  - `Fresh(origin, depth)` — the binder-descending probes: `Origin.Unify` (identity-bearing — both
    codomains receive the same one) and `Origin.Quote` (a throwaway probe). Replaces the former
    `$unify<n>` / `$quote<n>` reserved-name convention.
  - `Reserved(marker)` — the four scope-less markers `Marker.BadApply` / `GuardProbe` / `Coerce` /
    `Match`, recognised by constructor (the `Coerce` marker was formerly matched by the `"$coerceArg"`
    string). `name` on the base trait is display-only (`SemValuePrinter`, error messages); the `tag`s
    preserve the former display strings. Behaviour-preserving; full suite green.
- **HKT-over-`Function` inference** (F1b): only if a client appears. Two approaches were tried
  and rejected during the review fixes — decomposing `VPi` in the unifier (reverses the
  deliberate `CarrierKindChecker` design for non-rigid heads) and defaulting the carrier to an
  arity-matched former (works applied, fails when `?F` is read back unapplied; no default is both
  applicable and quotable-unapplied without masking spine mismatches). A real solution needs a
  quotable carrier value for partially-applied `Function` (e.g. a first-class read-back for
  `VLam`-shaped carriers), not a unifier tweak.
