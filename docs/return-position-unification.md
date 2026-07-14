# Return-position unification — remove `calculatedReturn`, never add `guardedReturn`

**Status:** Stage 1 landed; **Stage 2 (§4) SUPERSEDED by `type-levels-as-values.md`** (2026-07-14) — the
TypeLevel plan generalizes Stage 2's goal (levels are named values compiled by the ordinary pipeline) and its
Attempt-1 findings below are the evidence record for *why*. This doc also supersedes the earlier paused
`if(cond) T else raise(…)` WIP (`wip/if-else-guard-idiom`) — see
[Relation to the paused WIP](#relation-to-the-paused-wip).

## 0. Principle (Robert, 2026-07-14)

> There should be **no structural flags on the return position**. A value's return type is *always just a
> return expression*, checked by the one ordinary checker. What happens falls out of *what the expression is* and
> *where it sits in the type stack* — never a stored boolean.

A "pure type" (`Int[0,255]`, `IO[Unit]`) is the primitive/leaf case; a bare under-applied constructor (`Int`) is the
holes-solved-by-the-body case (today's "calculated return"); an effectful expression (`raise(…)`,
`if..else..raise`, `orError(…)`) is the elaborated-and-reduced case (today's "guard"). None is a category; the
discriminator is the return's forced **shape/kind** or its inferred **effect row**.

Two flags contradict this and are removed / never introduced:

- `calculatedReturn` — a real field today (`OperatorResolvedValue.scala:23`). It is a flag *recovering information the
  representation threw away*: `SaturatedValueProcessor.saturate` (`:84`,`:92`,`:100`) detects a bare under-applied
  return, **overwrites it with a literal `Type` placeholder** (`withReturnType(typeRef(pos))`), and sets the boolean to
  remember what it destroyed.
- `guardedReturn` — proposed by the paused WIP (a `detectGuardedReturn` syntactic scan for a `Throw`-referencing
  return). The identical mistake for the effectful case. **Not introduced.**

## 1. Target model

The return position is the return expression of type-stack level 0 (`TypeStack.signature`, peeled by
`SignatureView.returnType`). One resolution rule, three behaviours that fall out structurally:

| Return expression, forced | Behaviour | Mechanism |
|---|---|---|
| ground, kind `Type` (`Int[0,255]`, `IO[Unit]`, applied `Either[String,Int]`) | use directly | ordinary check |
| under-applied type constructor, kind `K→…→Type` (`Int`, grown `Counter`) | its omitted slots are **metas the body solves** | return meta, installed **structurally** |
| effectful (inferred row ≠ `{}`, e.g. `{Throw[String]}`) | elaborated, **reduced on the compiler track**, read by consumers | compiler-track resolution + cross-track back-edge |

No `calculatedReturn`, no `guardedReturn`, no `sawGuardReturn`, no `isGuardCarrier` category. The caller likewise
decides "read the callee's *resolved* return vs. its source return" from the **forced shape** of the callee's source
return, not a flag.

## 2. Current state (anchored)

**Calculated-return machinery** (removed in Stage 1):
- Producer: `SaturatedValueProcessor.scala:84` `detectCalculatedReturn`, `:92` flatten-to-`Type`, `:100`
  `.copy(calculatedReturn=…)`. Field at `OperatorResolvedValue.scala:23`. **One producer, zero test constructors.**
- Reads: `TypeStackLoop.scala:93` (gate `installReturnMeta`), `:233` (`failOnAbstractCalculatedReturn`),
  `Checker.scala:458` (`resolveCompleteCalculatedReturn` gate), `CalculatedReturnResolver.scala:102`
  (`resolveCalculatedReturn` confirm).
- Back-edge: `CalculatedReturnResolver.readMonomorphicReturnGround:197` reads `MonomorphicValue(callee,args).signature`
  (runtime-only — see the `:182-185` NOTE).
- Callee-side hole: `installReturnMeta:62` (replace return with a fresh meta the body solves), dispatched by
  `Track.settleReturnPosition:49`.

**Guard machinery** (reworked in Stage 2). No flag on master; recognised by value-shape:
- `isGuardCarrier:229` (accept an `Either`/`Bool` head where `Type` is expected), `dischargeGuardedSignature:287`
  (callee side), `dischargeGuardedReturn:246` (read `Right(t)`→payload / `Left(msg)`→reject). `sawGuardReturn` in
  `CheckState:63`, set at `Checker.scala:241`.
- Runtime-side reduction relies on a **precompute-and-merge** path that only covers *nullary-reducible named
  combinators*: `CompilerNativesProcessor` reduces a runtime-abstract, ability-performing value via
  `CompilerMonomorphicValue(v, ∅)` and publishes it as `ContributedBinding(v,"compiler",Leaf)`;
  `BindingMergerProcessor` prefers that native Leaf so `NativeBinding(v, Runtime)` carries the compiler-reduced body;
  the runtime checker's `evalExpr`→`bindingCache` then inlines it and `dischargeGuardedSignature` reads `Right`/`Left`.
  **This does not generalise to `if..else..raise`**: `else` is runtime-*concrete* (reduces to a stuck
  `flatMap(runAbort(…),…)`), `runAbort` has no runtime compile-time reduction, so the return stalls as
  `flatMap((o => match(o)), match(String))` — verified.

**Elaboration path already present** (reused by both stages): `walkTypeStack` (`TypeStackLoop.scala:362-371`) already
computes `checked = checker.check(level, expected)` — the *elaborated* `SemExpression` that ran `EffectLifter` + carrier
recording — but **discards it**, taking the signature value from `checker.evalExpr(level)` (pure NbE; no lift, no
carrier, no discharge). `check` vs `evalExpr`: `check` drives `resolveGuardedLadder`→`resolveLadder` with the lift arms
and carrier-role recording (`Checker.scala:143,228,257`); `evalExpr` is `makeEvaluator.eval` over `bindingCache` only
(`Checker.scala:102`). **The elaborated form we need is computed and thrown away today.**

## 3. Stage 1 — remove `calculatedReturn` (✅ DONE)

**Landed.** Full suite green (1249 tests, 0 failures) and the W3 example runs end-to-end. The implementation differs
from the sketch below in two ways, both simplifications discovered during implementation:

- **Detection is one field read, no oracle relocation.** `growTypeConstructor` already bakes W2 growth into
  `inferableArity` (`SaturatedValueProcessor.scala:427`), so post-saturation the omittable arity of any head is
  `SaturatedValue(head).inferableArity`. Detection is therefore `argCount < that` —
  `CalculatedReturnResolver.arityShortfall` + `isCalculatedReturn` (SemValue form, the read sides) /
  `isCalculatedReturnExpr` (signature form, the callee side). No growth-oracle move, no ladder change.
- **The kind check is satisfied by a *transient* flatten, not a relaxation.** `saturate` leaves the real return; the
  checker (`TypeStackLoop.processIO`) computes `isCalc` on demand and, only when true, replaces the return with a `Type`
  placeholder *for its own kind check* (`flattenReturnToType`) — a checker-internal transient, never persisted. The
  existing `installReturnMeta`/`settleReturnPosition` flow then runs unchanged. This keeps the delicate check ladder
  untouched (lowest regression risk).

The consumer read-sites (`resolveCalculatedReturn`, `resolveCompleteCalculatedReturn`, `Checker.inferValueReference`)
now trigger on `isCalculatedReturn` (the return's under-applied *shape*) instead of `VType`-placeholder + flag — which
is *more* robust: it distinguishes a bare `Int` from a genuine `Type` return without any marker. `CACHE_VERSION` bumped
21→22. Note the real stdlib `Int` is nullary (range in the refinement channel), so its returns are ordinary, not
calculated; the W3 machinery fires only for genuinely under-applied omittable constructors (the test-stub `Int[MIN,MAX]`,
W2-grown records).

### Original sketch (for reference)

Goal: delete the field and make the under-applied return a structurally-detected hole. No guard behaviour changes.

1. **Saturate stops flattening.** In `SaturatedValueProcessor.saturate`: delete `detectCalculatedReturn`, the
   `calculatedReturn` local, the `withReturnType(typeRef(pos))` rewrite (`:92`), and the `.copy(calculatedReturn=…)`
   (`:100`). The bare under-applied return (`Int`) is left **as the real expression** in level 0. Delete
   `detectCalculatedReturn` (`:126`). Remove the field from `OperatorResolvedValue.scala:23`.

2. **Relax the return-level kind check to tolerate under-application** (the crux of Stage 1). Today the return level is
   kind-checked against strict `VType`; a bare `Int` (kind `BigInteger→…→Type`) fails — which is *why* saturate
   flattened it (see the `:85-90` comment). Change `walkTypeStack`'s handling of the **top** level so that when the
   return's inferred kind is an arrow chain ending in `Type` (an under-applied type constructor), it is **replaced by a
   fresh return meta** (holes the body solves) instead of unified with `Type`. This is exactly `installReturnMeta`, now
   triggered **structurally** (return kind ≠ `Type`) rather than by the flag. Reuse the existing
   `peelLams`/`instantiateRemaining` "under-applied ⟹ metas" semantics; the return meta feeds the same post-drain
   fail-safe (`failOnUndeterminedCalculatedReturn`, kept).

3. **`settleReturnPosition` loses its `calcReturn` arg** (`Track.scala:42-52`). The install-meta branch is now driven by
   the structural detection in step 2 (either inside `walkTypeStack`, or `settleReturnPosition` inspects the forced
   return shape). `TypeStackLoop.scala:93` `calcReturn = …` deleted.

4. **Consumers detect structurally, not by flag.** Because saturate no longer flattens, a calculated callee's *source*
   return is the real bare `Int` (kind ≠ `Type`), not `VType`:
   - `resolveCalculatedReturn` (`:88`): change the trigger from "forces to `VType` **and** `sv.calculatedReturn`" to
     "forces to an under-applied type constructor (kind `K→…→Type`)". Drop the `:102` flag re-check.
   - `resolveCompleteCalculatedReturn` (`:117`): same — trigger on under-applied shape, not `VType`.
   - `Checker.scala:458`: drop the `if (sv.value.calculatedReturn)` gate; call the (now structural)
     `resolveCompleteCalculatedReturn` whenever the applied source return is under-applied.

5. **`failOnAbstractCalculatedReturn`** (`:232`) becomes: a **body-less** value whose return kind ≠ `Type` (an
   under-applied return with no body to solve it) errors "must state its return type explicitly." Structural, no flag.

6. **Back-edge unchanged** (`readMonomorphicReturn`): still reads `MonomorphicValue(callee,args).signature`. The
   runtime-only NOTE (`:182-185`) is addressed in Stage 2 (generalise to `CompilerMonomorphicValue`).

**Invariant:** behaviour identical to today; `./mill __.test` green; the implicit-generics / W3–W4 tests unaffected
(the source return now *is* the hole, so the same meta is solved from the body). Delete `sawGuardReturn`? No — guard
code is untouched in Stage 1.

## 4. Stage 2 — the "guard" is just an effectful return (larger; the research half)

> **Status: SUPERSEDED — do not resume this sketch.** Attempt 1 paused on the deep-reduction wall (WIP branch
> `wip/return-position-unification-stage2`, master left green); the 2026-07-14 architecture review concluded the
> wall is structural (the in-place approach must re-implement the body pipeline feature-by-feature and still dies
> at `Reserved(Match)`, which only the full monomorphization pipeline reduces). The replacement is
> **`type-levels-as-values.md`**: every type level becomes a named value (`TypeLevel` dimension) compiled by the
> ordinary pipeline. The [Attempt-1 findings](#attempt-1-findings-what-works-what-blocks) below remain the
> evidence record; the branch is never merged (only the `EffectLifter.underApplied` `VType` fix and `Abort.els`
> are salvaged, per the new plan's Step 0).

Goal: an effectful return (`raise`, `if..else..raise`, `orError`) is elaborated as an ordinary expression and
**resolved on the compiler track**, its resolved return read by consumers. Removes `isGuardCarrier`/`sawGuardReturn`/the
runtime-side `dischargeGuardedSignature` precompute reliance. `if..else..raise` and `orError` become one path.

1. **Elaborate the return level.** In `walkTypeStack`, take the return level's value from the **elaborated** `check`
   result (evaluate the elaborated `SemExpression`) rather than `evalExpr`. This infers the effect row and creates the
   carrier meta — fixing the root cause (return expr was pure-evaluated, never elaborated). Pure return ⟹ empty row ⟹
   identical to Stage 1's ordinary path.

2. **Compiler track pins the inferred return carrier.** Extend `Track.Compiler.pinCarriers` (`Track.scala:140`) to pin
   the carrier of a **non-empty inferred return row** (not only *declared* `{Throw[E]}` binders in `paramConstraints`)
   to the compile-time carrier — `{Throw[String]}` ⟹ `Either[String]`, its `{Abort}` sub-layer ⟹ `AbortCarrier[…]`.
   This is "a non-empty row pins its carrier to `Either[String]` as a platform *definition*, like `main` pins `IO`."
   The compiler track then reduces the return through the compile-time `Effect`/`Throw`/`Abort` instances to `Right(t)`
   (accept) or `Left(msg)` (reject), and publishes the resolved return in `CompilerMonomorphicValue(v,args).signature`.

3. **Consumers read the resolved return via the cross-track back-edge.** Generalise `readMonomorphicReturn`
   (`CalculatedReturnResolver.scala:186-199`, the `:182-185` NOTE) to read **`CompilerMonomorphicValue`** for an
   effectful/guarded return (the compiler track is the authority on the *type*), and `MonomorphicValue` for a
   body-solved calculated return. The caller discharges the read `Right(t)`→`t` / `Left(msg)`→reject
   (`dischargeGuardedReturn`, kept). This **subsumes** the runtime-side `dischargeGuardedSignature` reduction — delete
   it and its `precompute-and-merge` dependency for guards (the `CompilerNativesProcessor` precompute may stay for other
   nullary compile-time natives, but guards no longer rely on it). Both `orError` (nullary-reducible) and
   `if..else..raise` (per-instantiation) resolve by the same read.

4. **Salvage from the WIP:** the compile-time `Abort` carrier overlay `stdlib/eliot-compiler/eliot/effect/Abort.els`
   (`data AbortCarrier` + `Effect`/`Abort` instances) — the missing sibling of the `Either`/`Option` overlays, needed so
   `runAbort`/`else` reduce on the compiler track. Confirmed correct and needed.

### Why compiler-track resolution, not the WIP's runtime-side approach

The WIP pinned the inferred carrier on the **runtime** track (`pinGuardCarrier`) to reduce `else`/`runAbort` via
compiler-pool bindings pulled onto the runtime path — hitting B1 (a kind-check on the lifted `String[]` arm off the
`isGuardCarrier` path) and B2 (bare `raise` not pinned). Both are symptoms of forcing a *compiler-platform computation*
(the type) onto the *runtime* track, where the carrier representations aren't present and `else` reduces to a stuck
`flatMap` before any discharge can see it. Resolving on the compiler track (where `Track.Compiler.pinCarriers` and the
overlays already live) is the principled version of what the WIP hand-rolled — and it is the item the code itself
defers (`CalculatedReturnResolver.scala:182-185`).

### Hard points (Stage 2, to validate during implementation)

- **Pinning an *inferred* (not declared) carrier.** Today `pinCarriers` keys off `paramConstraints`; the inline guard's
  carrier arises from `else`/`raise` instantiation. Needs: after elaboration, find the unsolved effect-carrier meta of
  the return row (`Unifier.effectCarrierMetaIds` — salvage from the WIP) and pin it.
- **The Abort/Throw carrier tower.** `if..else..raise` is `{Abort}` (from `if..else`) + `{Throw[String]}` (from
  `raise`); `else` discharges `Abort`, residual `{Throw[String]}` on `Either[String]`. Confirm the tower reduces:
  `AbortCarrier[Either[String]]` discharged to `Either[String]` = `Right(String)`/`Left(msg)`.
- **Does the compiler track monomorphize a runtime value's signature on demand?** The runtime caller reading
  `CompilerMonomorphicValue(greeting,[true])` must *drive* the compiler-track monomorphization of `greeting`'s type
  stack. Verify the demand-driven fact graph does this (it should: the read is a `getFactIfProduced` on the compiler
  key).
- **Ability-implementation markers stay on their own path.** `MarkerGuardSignature` / the `where`-clause ability guards
  (`GuardChannel`, Stage-4 `reduceGuardSubValues`) are a *different* feature and must be untouched — they legitimately
  keep the `Bool`-verdict-in-return mechanism. Only the `eliot.lang.Guard` return-type guard is reworked.

### Attempt-1 findings (what works, what blocks)

All on branch `wip/return-position-unification-stage2` (commit message has the file-level summary). The fixture used:
`def greeting[COND: Bool]: if(COND, String[]) else raise("greeting unavailable") = "hello"`, `main` calls `greeting[true]`.

**Solved — landed green (no regression across the full lang+jvm suite):**

- **B1 (the WIP's unsolved kind-check on the lifted arm) — SOLVED, and it was *two* independent bugs:**
  1. *`EffectLifter.underApplied` did not recognise `VType`.* `if(COND, String[])`'s pure arm `String[]` is a **type**
     (kind `Type`) flowing into `if`'s carrier value slot `{Abort} T = ?F[T]`. `mustPureWrapBeforeUnify` calls
     `underApplied(actual = VType, arity)`, which only matched `VTopDef(None)`/`VNeutral` — so `VType` returned `false`,
     the pure-wrap pre-arm never fired, and unification **postponed** `?F[T] =?= Type`. That postponed constraint then
     hard-mismatched the instant the carrier pinned to `Either[String]` (`AbortCarrier[Either[String], Type] =?= Type`),
     which is exactly the WIP's B1 error. Fix: add `case VType => 0 < arity` to `underApplied`. *This is a genuine,
     guard-independent `EffectLifter` correctness fix* (any pure type into a carrier slot).
  2. *The return-level `= Type` kind-unify itself.* At the return boundary `check(sig, VType)` the guard's inferred type
     is a still-open carrier meta `?G[Type]` (pinning happens *after* `walkTypeStack`), which `isGuardCarrier` (concrete
     `Either`/`Bool` only) does not accept, so it too postponed `?G[Type] =?= Type` and failed post-pin. Fix:
     `Checker.isGuardKind` — at a `Type` boundary, also accept an **effect-carrier-meta-headed** inferred type
     (`lifter.effectCarrierSplit`) as a guard, and on the compiler track **pin its carrier to `Either[String]` right
     there** (`pinGuardCarrierToEither`). This is the "carve the effectful return out of the `= Type` kind-unify" the
     2026-07-14 assessment called for, and it structurally kills B1.

- **Settle split** — `Track.settleReturnPosition` now returns `(bodyCheckSig, publishSig, returnMeta)`. The compiler
  track checks a guard body against a **lenient fresh meta** (a compile-time guard value's *type* is what matters, not
  its body) while *publishing* the carrier verdict; every non-guard case keeps `bodyCheckSig == publishSig`.
- **Runtime cross-track back-edge** — `CalculatedReturnResolver.settleEffectfulReturn` + `resolveEffectfulReturn`: a
  **stuck** inline guard (detected by `isStuckEffectfulReturn`: forced head ∈ `{flatMap, map, pure}`) reads
  `CompilerMonomorphicValue(v, args)` and discharges `Right(t)`→`t` (body checks against the payload — sound) /
  `Left(msg)`→report+abort. A **combinator** guard (`orError`/`fold`/`requireOr`, which already reduces to `Right`/`Left`
  here via the precompute-merge) keeps the ordinary `dischargeGuardedSignature` — untouched, no regression.
- Salvage landed: `stdlib/eliot-compiler/eliot/effect/Abort.els`, `Unifier.effectCarrierMetaIds`.

**BLOCKED — the deep-reduction wall (the actual research half):**

Reducing the inline guard's carrier tower `else(if(COND, String[]), raise(…))` to `Right(String)`/`Left(msg)` **on the
compiler track, before the body check** does not work with the in-place approach tried (`reduceGuardSubValues` reduces
the bodied `else`/`if` per-instantiation via `CompilerMonomorphicValue`, seed them, then re-evaluate the elaborated
`checkedSig` + `renormalize`). It bottoms out at a **`VNeutral(Reserved(Match))`** — a stuck pattern-match (from
`else`'s inlined body: `flatMap`→`foldEither`/`foldOption`→`match`) whose discriminee becomes concrete only after
inlining, but **`renormalize` has no rule to re-fire a `Reserved(Match)` neutral** (grep: `Match` appears only as a
`SemValue.Marker` definition; pattern-match reduction lives solely in the full monomorphization pipeline — which is
exactly how `orError`'s `foldOption` reduces to `Right`/`Left`). So the signature reduction needs the **full
body-reduction pipeline** (`reduceSourced`/`MonomorphicEvaluator`), not `renormalize`. Two viable next moves:
  - **Route the guard signature through the body reducer.** Reduce the elaborated `checkedSig` (COND applied) via the
    same `reduceSourced` path the compiler track already uses for bodies, then read the result back as the published
    signature. Domain mismatch to resolve: `reduceSourced` yields a `MonomorphicExpression`, the signature wants a
    `GroundValue`/`SemValue`; and the ordering (the `PostDrainQuoter` is built later in `processIO`).
  - **Synthetic value (Option A, previously demoted).** Reify the guard expression as a compile-time value `v^guard`
    whose *body* is the guard, monomorphize it via the full pipeline (which reduces the tower correctly — this is
    literally why `orError`-the-named-value works), read `CompilerMonomorphicValue(v^guard, args)`. Heavier (needs a
    synthesized `SaturatedValue`), but it *is* the mechanism that already works.

**Other findings:**

- **`evalSemExpr(checkedSig)` is NOT a drop-in for `evalExpr(level)` in `walkTypeStack`.** Sketch step 1 says take the
  return level's *value* from the elaborated `check`. Doing so uniformly **regressed `fold`-based guards** (and would
  any `fold` sig): the elaborated `SemExpression` carries an **extra explicit type-arg meta** (`fold[?R](cond, …)`) that
  the raw ORE `evalExpr` does not, and that extra arg leaves the `fold` native **stuck** (`instantiated` =
  `VStuckNative(fold, [?R, false, Right, Left])` instead of the reduced `Left`), so the runtime `dischargeGuardedSignature`
  never saw a `Left` to report — 3 green tests broke. Resolution: `walkTypeStack` keeps `evalExpr` for the signature
  *value*; the elaborated `checkedSig` is captured in `levelExprs` (its last entry) and used **only** by the
  compiler-track guard reduction. So step 1 should read "capture the elaborated return level for the *compiler-track
  reducer*", not "take the signature value from it".
- **Reject reporting site.** The existing tests expect the reject at the guard *definition* (`"empty" at "head"`) for a
  direct monomorphize, and at the *use* reference for a caller — both already textually "head"/the callee name. The
  runtime settle must therefore still *report* `Left` (not silently defer it), matching the direct-monomorphize convention.

## 5. Cleanup enabled once Stage 2 lands

- Delete `eliot.lang.Guard` (`when`/`orError`) — both `stdlib/eliot/eliot/lang/Guard.els` and
  `stdlib/eliot-compiler/eliot/lang/Guard.els` — replaced by `if..else..raise` / bare `raise`.
- Rewrite `GuardSignatureIntegrationTest` to the `if..else..raise` and bare-`raise` forms.
- Remove `isGuardCarrier`, `sawGuardReturn`/`recordGuardReturn`, runtime-side `dischargeGuardedSignature`.
- Update `Expression.scala` return-type-parser doc comments and `ASTParserTest` guard cases referencing `when`/`orError`.

## 6. Test & migration strategy

- Stage 1 lands behind the full suite unchanged (no `.els` changes; identical behaviour). Add a regression: a bare-`Int`
  calculated return still type-checks and the abstract-return error still fires.
- Stage 2: keep `GuardSignatureIntegrationTest` on the `orError` forms until the compiler-track path passes them, then
  add `if..else..raise` + bare-`raise` fixtures (the WIP's TRUE/FALSE cases), then flip the suite and delete `Guard.els`.
- `CACHE_VERSION` bump (fact shapes change: `OperatorResolvedValue` field drop; `CompilerMonomorphicValue` read on the
  runtime guard path).

## 7. Risks

- **Stage 1 kind-check relaxation** touches the checker's core walk; the risk is over-accepting an ill-kinded return.
  Mitigate: only under-application with codomain `Type` instantiates; anything else still errors.
- **Stage 2 changes how the *working* `orError` guard resolves** (runtime-side ⟹ compiler-track-read). Regression risk on
  the green `GuardSignatureIntegrationTest`; stage the switch and keep both paths until the new one is green.
- **Cross-track back-edge** re-enters `CompilerMonomorphicValue` from a runtime check; guard against a producer cycle
  with the existing `activeFactKeys` recursion guard (already in `readMonomorphicReturnGround`).

## Relation to the paused WIP

`wip/if-else-guard-idiom` remains the reference for the fixtures, the `Abort.els` overlay, and
`Unifier.effectCarrierMetaIds`. Its *mechanism* (flag + signature-slot hand-reimplementation of the body pipeline) is
**not** carried forward — this draft routes the return through the one ordinary elaboration + compiler-track resolution
instead. See `[[project_if_else_guard_idiom]]`.
