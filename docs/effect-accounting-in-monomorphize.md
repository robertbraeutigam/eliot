# Effect accounting in the monomorphize phase (plan)

**Status:** COMPLETE — Steps 1–5 landed. The monomorphize-phase `EffectResidualChecker` is the sole effect authority;
the pre-mono `effect/` verification phase, the `-E` syntax, and the `dischargedEffects` field are all gone.

## Progress

- **Step 5 — done.** Docs/skills/examples updated. `docs/effect-row-tails.md` drops the `-E` output markers (pinned
  positive rows unchanged); the effect section (item 9) of `.claude/CLAUDE.md` is rewritten around the residual check;
  `.claude/skills/eliot-monomorphize/SKILL.md` gains the `EffectResidualChecker` collaborator + its post-drain step;
  the eliot-code skill drops the "discharge only via a direct call" caveat (dot-chaining now works) and the `{…, -E}`
  authoring paragraph; the `IfDemo`/`DischargeDemo` examples repoint to this doc. Note: `docs/effect-discharge-accounting.md`
  and `docs/effect-lift-in-checker.md` (which the plan named as fold-in targets) had already been removed from `docs/`
  before this work, so there was nothing to retire or fold — their surviving conceptual content lives in item 9 and
  this file.
- **Step 4 — done.** Deleted the discharge machinery and the `-E` syntax. `EffectfulType.negativeEffects` and the `-`
  parser branch are gone (a brace entry is now just an ability reference); `EffectSugarDesugarer` no longer records
  negatives, has no "negatives-only pass-through" case, and no pinned-negative row error; the `dischargedEffects` field
  is removed from the whole fact chain (`FunctionDefinition` → `NamedValue` → `MatchDesugaredValue` →
  `BlockDesugaredValue` → `ResolvedValue` → `OperatorResolvedValue`) and every processor that threaded it, plus the
  layer-merge union in `UnifiedModuleValueProcessor`. The stdlib discharger signatures dropped their `{-E}` **output**
  markers (kept the `{E | G}` input rows). Verified the two invariants held: the raw-accessor merge
  (`runStateCarrier`/`runThrow`/`runAbort` abstract-vs-generated) now matches on the plain structure, and the
  compiler-track `Either`/`Throw` discharge (`CalculatedReturnResolver`) recognises the carrier by `Left`/`Right` FQN,
  never `-E`. Full suite green; `DischargeDemo` builds and runs (declares only `{Console}`).

- **Step 3 — done.** Deleted the pre-mono `effect/` verification phase: `EffectCheckProcessor`,
  `DeclaredEffectChecker`, `EffectUsageCollector`, `EffectAccounting`, `CalleeSignatures`,
  `EffectDischargeSummaryProcessor`, and the `EffectDischargeSummary` / `EffectCheckedValue` facts, and dropped the two
  processors from `LangProcessors` (the value chain is now `RecursionCheckedValue → SaturatedValue`, no effect gate
  between them). The `effect/` package now holds only the two helpers the mono residual read reuses:
  - **`EffectMachinery`** — kept whole (`isMachineryAbility`, `abilityNameOf`; both used by `EffectResidualChecker`).
  - **`EffectCarriers`** — kept `isHktBinder` / `carrierBinders` / `declaredEffects` (used by `TypeStackLoop` and
    `EffectResidualChecker`); deleted `carrierHead` / `carrierHeaded`, which only the retired
    `EffectUsageCollector`/`EffectAccounting`/`CalleeSignatures` consumed.
  - **`Effect`/`Suspend` machinery FQNs** in `WellKnownTypes` (`effectFlatMapFQN`/`effectMapFQN`/`effectPureFQN`) are
    untouched — the `EffectLifter` uses them.
  - **Tests.** Deleted `EffectCheckProcessorTest` and `EffectDischargeAccountingTest` — both drove the now-gone
    `EffectCheckedValue` / `EffectDischargeSummary` facts. Their diagnostics (declared-pure, `Log` subset, `Inf`
    propagation) are already pinned end-to-end through the *mono* path by the jvm `ExamplesIntegrationTest1` and
    `TerminationIntegrationTest`, so no coverage was lost; the stale `EffectCheckProcessorTest` cross-reference in
    `MonomorphicTypeCheckTest` now points at `TerminationIntegrationTest`.
  - **Not touched (Step 4).** The `dischargedEffects` field and `-E` surface syntax remain — the field is now
    unconsumed (comment updated to say so). Full suite green (lang/jvm/eliotc/ide.lsp), all modules compile.

- **Step 2 — done.** `SaturatedValueProcessor` now keys off `RecursionCheckedValue` (the pre-mono effect gate is
  dropped); the monomorphize-phase `EffectResidualChecker` is the sole effect authority. Two things the gate flip forced,
  beyond the plan's sketch:
  - **Transitive propagation** (soundness). The initial residual check only saw *direct* ability methods, so a leak
    *through a called value* (`caller : {Console} = doLog`, `doLog : {Log}`) compiled silently — and `Log[IO]` resolves,
    so ability resolution did not catch it either (the same would let an undeclared `Inf` through, breaking termination
    soundness). Fixed by walking *all* value references and reading each callee's declared effects
    (`EffectCarriers.declaredEffects` off its `OperatorResolvedValue`), counted iff its carrier rides the ambient — so a
    discharged callee (carrier = inner `StateCarrier`, not the ambient) still drops out. No `-E`, no discharge summary.
  - **Declared-pure fail-safe.** The subset check skips a carrier-less value, so `def helper: String =
    printLine(readLine)` degraded to a raw `Type mismatch` exposing the internal `$bad-apply` marker. Restored the
    friendly "performs an effect but is declared pure" message as the residual checker's ambient-carrier-less branch,
    discharge-aware by construction: it fires only for a nullary (can't-host-effects) return whose body-check left a
    committed mismatch *and* performs an effect — so a fully-discharged `sign(f) = if(f,"+") else "-"` (reconciled to
    `Id`, no mismatch) is accepted.
  - Fallout: 3 fixtures. The transitive-leak test now passes via the residual check; the two declared-pure tests via the
    restored message (one needed its harness to monomorphize the carrier-less value at *no* type args, not `[ioCarrier]`
    — previously masked by the pre-mono gate). `Blocks.els` + a new integration test now exercise dot-chained discharge
    in a `{Console}` body. Full suite green.
  - Diagnostics division of labour is unchanged from Step 1: `Console`/`Log`/`Inf` leaks get the friendly residual
    message; `State`/`Throw`/`Abort` leaks still fall to `AbilityResolver`'s "No ability implementation" (sound, cryptic).
    `EffectCheckProcessor` still exists but is now dead (its fact undemanded) — deleted in Step 3.

- **Step 1 — done.** `monomorphize/check/EffectResidualChecker` computes the residual (effect abilities whose
  ability-reference carrier argument forces to an ambient-carrier head, machinery excluded) and errors on
  `residual ⊄ declared`; wired into `TypeStackLoop.runPostDrainResolution` after the final drain, skipped for the
  signature twin. Verified a **no-op on the whole suite** (lang/jvm/eliotc/ide.lsp green). Positive control (with the
  pre-mono check temporarily neutered): it fires with the right message on an undeclared `Inf` — the load-bearing case,
  where `Inf[IO]` *resolves* so ability resolution does not abort and the residual check is the sole catcher.
- **Division of labour observed.** An undeclared effect whose instance *resolves* at the base carrier (`Console`, `Log`,
  `Inf`) is caught by the residual check (friendly, def-attributed). One that needs a transformer carrier
  (`State`/`Throw`/`Abort`) fails earlier at `AbilityResolver` ("No ability implementation … State") — sound but
  cryptic. `Inf` is why the residual check is load-bearing, not merely nicer: without it an undeclared `Inf` would
  *compile*.
- **Step 2 prerequisite — found and fixed.** Discharge inside an effect-declaring body
  (`printLine(runStateToValue("init", counter))` / the `.` form, under `{Console}`) did **not** monomorphize even with
  the pre-mono gate off: it failed at ability resolution with `No ability implementation found for ability 'State' with
  type arguments []`. Root cause (isolated, not `.`-specific — the direct form failed identically): a bare polymorphic
  **nullary** reference in tail position (`counter`'s block result `state : [S, F] F[S]`) was `infer`red but never
  instantiated in `typeImmediateLambda`'s effectful `let` branch, so its `[?S, ?F]` implicit type args were never
  allocated and its ability resolved at empty arguments (and it was even wrongly classified pure→`map` instead of
  effectful→`flatMap`). Fix: instantiate the inferred continuation there, exactly as the `let` argument already is
  (`Checker.typeImmediateLambda`). With it, both the direct and dot forms compile and run at mono (verified with the
  gate temporarily neutered). A **separate, pre-existing** issue remains out of scope: the *non-effectful* `let` branch
  (expected `None`) also infers a polytype tail without instantiating — a block ending in a bare polymorphic nullary in
  argument position (`useIt({ val x = "a"; none })`) fails with "Function not implemented"; unrelated to effects, not
  touched here.

## Summary

Move effect *verification* from the pre-monomorphize `effect/` phase into the monomorphize
checker, where the carrier flow is already exact. The pre-mono phase is a syntactic,
definition-local over-approximation that runs *before* instantiation, so it cannot see a
discharge that is reached through any indirection — the `.` operator, a wrapper function, any
higher-order combinator — and reports a false *"performs the effect 'X' but does not declare it."*
(see `docs/effect-discharge-accounting.md` for the mechanism being retired, and the "call the
discharger directly, not dot-chained" caveat in the eliot-code skill for the user-facing symptom).

Monomorphization already computes the exact answer: the checker (`EffectLifter`) threads carriers,
inserts `flatMap`/`pure`/`map`, and defaults a residual carrier to `Id` at a pure boundary. A
genuine leak already fails at `resolve-abilities`. So mono is already the sound authority; the
pre-mono phase is a shadow. This plan makes mono the *reported* authority and deletes the shadow.

Coverage becomes use-site (a definition is checked at each concrete manifestation, not once
abstractly). That is the language's standing "sound, not modular" stance — already accepted
everywhere else — so it is not a new trade-off here.

## The core mechanism

Define a value's **residual effect set** = the effect abilities *demanded on that value's own
ambient carrier* during checking (the carrier binder(s) the value is polymorphic over — the
`{...}` row / `[F[_] ~ …]`, tracked today as `CheckState.ambientCarriers`).

Two properties make this exact and make discharge fall out for free:

- **Discharge is structural, not declared.** A discharger's type maps an input carrier that
  *carries* `E` to an output carrier that does not: `runStateToPair(initial: S, p: {State[S] | G} A):
  G[Pair[A, S]]`. When mono unifies the caller's argument against `{State[S] | G}` and reads the
  result `G[Pair]`, `State` is demanded on the *inner* `StateCarrier`, never on the caller's ambient
  `G`. So a discharged effect is simply absent from the residual — no annotation required. (This is
  why the examples already run correctly today: the discharge happens in mono; `-State` never
  participates in codegen.)

- **`Inf` falls out.** `forever` demands `Inf[F]` on the ambient carrier like any effect, so `Inf`
  appears in the residual and is checked by the same subset test — no special mechanism. Note this
  is captured by *what is demanded on the ambient carrier*, independent of whether the instance
  resolves (`Inf[IO]` does resolve; the violation is `Inf ∉ declared`, not a missing instance).

The check: at each value's return boundary (post-drain, once carrier metas have settled — next to
`CarrierKindChecker`/`PostDrainQuoter`), compute `residual`, map the declared `{...}` row to ability
FQNs, and require `residual ⊆ declared`. On failure, emit a friendly, definition-attributed
diagnostic (the offending operation's `Sourced` lands in the body), deduplicated by
`(definition, effect)` so an N-times-instantiated def does not spam N identical errors.

## The change chain (ordered; green at each step)

### Step 1 — Add the mono residual check, behind the existing gate

Implement the residual readout + subset check in the checker (a new collaborator, e.g.
`monomorphize/check/EffectResidualChecker`, invoked from `MonomorphicTypeCheckProcessor`'s
return-boundary handling). Wire it to **error**.

Because the pre-mono gate still blocks effect-invalid code, mono only ever sees pre-mono-*valid*
bodies, so the new check must be a **no-op on the entire existing test suite**. That is the
acceptance criterion for this step: it proves the mono check agrees with the pre-mono check on all
currently-valid code (no false positives, discharge-through-direct-call correctly excluded). Any
firing here is a bug to fix before proceeding.

### Step 2 — Flip the authority

**Prerequisite — done** (see Progress): `Checker.typeImmediateLambda` now instantiates the inferred `let` continuation,
so discharge against a concrete ambient carrier monomorphizes (both direct and dot forms verified with the gate off).

Re-point `SaturatedValueProcessor` from `EffectCheckedValue.Key` to `RecursionCheckedValue.Key`
(drop the effect gate). Now every recursion-valid body reaches mono and the residual check is the
sole authority. Add/convert fixtures:

- dot-chained and wrapper-reached discharge now **compile** (e.g. `p.runStateToValue(s0)` inside a
  `{Console}` body) — the current documented limitation is *gone*;
- genuine leaks now error via the mono message, attributed to the definition;
- undeclared `Inf` (a `{Console}` body calling `forever`) still errors, now via the residual check;
- flip the existing "dot-chain fails accounting" tests to assert success.

`EffectCheckProcessor` still exists but its fact is no longer demanded — dead, not yet deleted.

### Step 3 — Delete the pre-mono `effect/` phase

Remove `EffectCheckProcessor`, `DeclaredEffectChecker`, `EffectUsageCollector`, `EffectAccounting`,
`CalleeSignatures`, `EffectDischargeSummaryProcessor`, and the `EffectDischargeSummary` /
`EffectCheckedValue` facts; drop them from `LangProcessors`. Audit `EffectMachinery` /
`EffectCarriers`: mono has its own carrier detection (`CheckState.ambientCarriers`,
`Unifier.isEffectCarrier`), so keep only whatever the residual readout genuinely reuses and delete
the rest. Keep the `Effect`/`Suspend` machinery FQNs in `WellKnownTypes` (`effectFlatMapFQN`,
`effectMapFQN`, `effectPureFQN`) — the `EffectLifter` uses them.

### Step 4 — Delete the discharge machinery and the `-E` syntax

The `dischargedEffects` field is now unconsumed. Remove it from the whole fact chain and every
producer that threads it: `ast/fact/FunctionDefinition`, `ast/fact/Expression`,
`ast/fact/ImplementBlock` → `core/fact/NamedValue` (+ `EffectSugarDesugarer`,
`CoreExpressionConverter`) → `matchdesugar/fact/MatchDesugaredValue` →
`block/fact/BlockDesugaredValue` → `resolve/fact/ResolvedValue` (+ `ValueResolver`) →
`operator/fact/OperatorResolvedValue` (+ `OperatorResolverProcessor`). Remove the layer-merge
*union* of `dischargedEffects` in `UnifiedModuleValueProcessor` and its slot in
`NamedValue.signatureEquality`.

Remove the `-E` surface syntax: the negative branch of the effect-row parser in `ast/fact/Expression`,
and the negative recording + "negatives-only set = pure pass-through" handling in
`EffectSugarDesugarer`. Update the stdlib discharger signatures to drop their `{-E}` **output**
markers (`stdlib/eliot/eliot/effect/State.els`, `Abort.els`, `Throw.els`, `Dep.els`/`provide`,
etc.) — the discharge is now read structurally from the input row vs. the plain output carrier.
Keep the discharger **input** rows (`{State[S] | G} A`) — those still carry the effect.

Verify two things still hold:
- the raw-accessor merge (`runStateCarrier`/`runThrow`/`runAbort` abstract-vs-generated) — it now
  matches on the plain structure with no `-E` union to reconcile;
- the compiler-track effectful-signatures discharge (`CalculatedReturnResolver`, `Either`/`Throw`
  reflected at the return boundary) — it recognizes the carrier by `Left`/`Right` FQN, never by
  `-E`, so it is unaffected.

### Step 5 — Docs, skill, examples

Retire `docs/effect-discharge-accounting.md` (the whole mechanism is deleted); fold the surviving
conceptual content into `docs/effect-lift-in-checker.md`, which now also owns *verification*, not
only the lift. Update `docs/effect-row-tails.md` (drop `-E` output markers; **pinned positive rows
are unchanged** — see "What stays"). Rewrite the effect section (item 9) of `.claude/CLAUDE.md` and
extend `.claude/skills/eliot-monomorphize/SKILL.md` with the residual-accounting responsibility.
Remove the "call the discharger directly, not dot-chained" caveat from the eliot-code skill
(dischargers and `provide` now dot-chain uniformly), and optionally re-showcase dot-chained
discharge in the examples.

## What goes away

- **`-E` negative-row syntax** and the parser branch that accepts a leading `-` in an effect row.
- **Negative recording + the "negatives-only set = pure pass-through" special case** in
  `EffectSugarDesugarer`, and the **"negatives cannot be pinned"** rule.
- **The `dischargedEffects` field** on the entire fact chain (AST → core → matchdesugar → block →
  resolve → operator) and its slot in `NamedValue.signatureEquality`.
- **`EffectDischargeSummary`** — fact *and* processor: both the **declared** discharge and the
  **inferred** discharge (the `survivingParamEffects` provenance / Step-3 "entered via a carrier
  parameter, did not survive the body" machinery). It was the syntactic estimate of what mono now
  computes exactly.
- **`CalleeSignatures.dischargedEffects`** and the shallow arg-union discharge subtraction in
  `EffectUsageCollector`.
- **The layer-merge union of `dischargedEffects`** for the raw data-field accessors in
  `UnifiedModuleValueProcessor`.
- **The entire pre-mono `effect/` verification phase**: `EffectCheckProcessor`,
  `DeclaredEffectChecker`, `EffectUsageCollector`, `EffectAccounting`, `CalleeSignatures`,
  `EffectDischargeSummaryProcessor`, and the `EffectCheckedValue` fact — plus its **gate edge**
  (`SaturatedValue` re-points to `RecursionCheckedValue`).
- **The `{-E}` output markers** on every stdlib discharger signature.
- **The user-facing restriction** "discharge only via a direct call, never a dot-chain or wrapper"
  — it stops being a limitation and becomes ordinary working code.

## What stays (do not conflate)

- **Pinned *positive* rows** (`{Throw[E] | Id} A` — concrete carrier stacks for stored fields) are a
  different mechanism from discharge negatives and are **unaffected**.
- **Discharger input rows** (`{State[S] | G} A`) stay — that is what encodes the effect's presence
  that the structural readout subtracts against.
- **`EffectLifter`** (the auto-lift: `flatMap`/`pure`/`map` insertion) is unaffected and *gains* the
  residual readout as a sibling responsibility. `tryIdDefault` (pure-boundary `Id` defaulting) is
  already mono-side and already structural.
- **`CalculatedReturnResolver`** (compiler-track `Either`/`Throw` discharge) is mono-side and reads
  the carrier structurally — unaffected.
- **`RecursionCheckProcessor` / termination** is unaffected: it gates recursion *origin* (no cycles);
  only the `Inf` *declaration* check moves (from the pre-mono subset test into the mono residual
  test).

## Risks and open questions

- **Readout timing.** The residual must be read after the ambient carrier's constraints have fully
  settled (post-drain); reading a still-flex carrier would under-report. Slot it next to
  `CarrierKindChecker`.
- **Attribution.** Point at the responsible *definition*, not the use-site instantiation chain. The
  offending effect operation's `Sourced` is in the body, so it is recoverable, but the wiring needs
  care.
- **Dedup.** Deduplicate diagnostics by `(definition, effect)` to avoid one error per instantiation.
  A genuinely instantiation-*specific* residual (a calculated/dependent effect that violates at some
  instantiations only) is correctly reported per use site — that is the use-site cornerstone, not a
  bug.
- **Cascade quality.** Removing the pre-mono gate means mono now attempts effect-invalid bodies that
  were previously rejected early; ensure the residual error fires cleanly rather than behind an
  unrelated mono mismatch.
- **Coverage confirmation.** Confirm the intended behavior for an uninstantiated / library-only
  definition (checked only when manifested). This is the accepted use-site trade-off; make it
  explicit in the docs so a library author knows totality comes from tests, not a def-site pass.

## Relation to existing docs

- Supersedes `docs/effect-discharge-accounting.md`.
- Extends `docs/effect-lift-in-checker.md` (the checker gains verification alongside the lift).
- Touches `docs/effect-row-tails.md` (output `-E` markers removed; pinned positive rows kept).
