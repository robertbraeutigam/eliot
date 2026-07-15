# Signature split — the signature becomes a named value (`Runtime`/`Signature` role)

**Status: IN PROGRESS** (updated 2026-07-15) — Steps 0–7 landed (Step 7, the payoff feature, complete this arc); Steps
8–10 (checks reach signature bodies, the checker deletion sweep, the `Guard.els` removal + closeout) remain. This plan
supersedes and replaces two retired documents:

- `docs/return-position-unification.md` — Stage 1 (structural calculated-return detection, no persisted flag) landed
  and stays; Stage 2 (effectful returns) is realized by this plan instead. Its Attempt-1 evidence is absorbed in §8.
- `docs/type-levels-as-values.md` — COMPLETE (the `TypeStack` removal, `38305b75` + `8da08c1c`); its durable results
  (front-end uniformity, the derived-kind insight, the dead-end measurements) are absorbed in §1 and §8.

## 0. Goal — read this first

Two sentences the whole plan serves:

1. **The goal is the execution model for the signature.** A value's signature becomes an ordinary named value — a
   *unit of execution* on the compiler platform — so a type-level expression is elaborated, effect-accounted, and
   per-instantiation reduced by the **same pipeline as any body**. Today the signature is walked *in place* by the bare
   β/δ+native kernel (`TypeStackLoop.walkTypeStack` → `checker.evalExpr`), which is exactly why effectful or `match`-y
   type expressions stall: the kernel legitimately cannot re-fire a stuck `Reserved(Match)` neutral, and pattern-match
   reduction lives only in the full monomorphization pipeline — which only *named values* pass through.
2. **We expect this to *simplify* the type checker, not add branches to it.** Every piece of signature-position
   special-casing in `monomorphize/check` exists *because* the signature is not a value (§6 enumerates them). The stop
   rule: the deletion ledger in §6 must actually realize; a change that adds a new signature-specific branch to
   `Checker`/`Track` is going the wrong way — stop and re-derive.

**Payoff feature** (not itself the goal, but the acceptance test of the goal): effectful return expressions —
`raise(…)`, `if(COND, T) else raise(…)` — work in signature position through ordinary compiler-track reduction. Three
prior attempts failed on exactly the "signature is not a body" wall (§8).

This continues the cornerstone line: "types are values" already holds *semantically* (one evaluator, one `SemValue`
domain) and, since the `TypeStack` removal, *representationally* (a signature is a plain expression through the whole
front-end). This plan closes the third axis — *executionally* (a type expression becomes a named value's body, so the
platform runs it).

## 1. The model

A new **role** dimension on value identity — `Runtime | Signature` — carried as a field on `QualifiedName` beside
`qualifier` (`QualifiedName.scala`), explicitly **not** a new `Qualifier` case: the role is orthogonal to the qualifier,
because *every* qualifier (`Default`, `Type`, `Ability`, `AbilityImplementation`) carries both slots today
(`NamedValue.runtime` + `NamedValue.signature`). A `Type`-qualified value's "runtime" is its alias body and its
"signature" is its kind; a `Default` value's "runtime" is its term body and its "signature" its type — same shape,
different qualifier.

Every named value splits **at birth, in `core`** (`CoreProcessor` mints both twins from the one `FunctionDefinition`):

| Twin           | Body                                             | Its own signature                                  | Can be abstract?        | Monomorphizes on   |
|----------------|--------------------------------------------------|----------------------------------------------------|-------------------------|--------------------|
| `v@Runtime`    | today's runtime expression                       | none of its own — its type *is* its sibling        | yes (`None` body)       | runtime + compiler |
| `v@Signature`  | today's signature expression (**always** present) | the **derived kind** (projection of its binders)   | never                   | compiler only      |

Key properties:

- **The kind is never minted or stored** (the B0 insight of the retired TypeLevel plan is preserved). The split is
  *binary*, not a level tower: `v@Signature`'s own signature is the kind derived from its binders on demand
  (`SignatureView.of(body).binders` → `Function[K1, …, Type]`), and kinds are simple arrows over `Type` the kernel
  checks in place — so no `v@Signature@Signature` demand ever exists and the tower bottoms immediately.
- **Binder scope is shared.** The generic binders are baked into the signature expression; the runtime body's
  type-parameter references resolve against them. `core` stamps the same binder list into both twins (it mints both
  from one definition, so this is one stamp, not a cross-fact read in `resolve`).
- **Abstractness lives only on the runtime twin.** A signature twin always has a body (a declaration always has a
  signature); "abstract" means "the runtime twin has no body". The layer merge recasts accordingly:
  - `@Signature` twins from all layers must **agree** — this *is* today's `signatureEquality`, relocated to ordinary
    body agreement of signature twins (the lexical character-exact rule unchanged);
  - `@Runtime` twins keep prefer-the-bodied / "Has multiple implementations." exactly as today.
- **Track alignment is natural.** The signature twin is inherently compiler-platform (types are compile-time), so the
  `Signature` role only ever monomorphizes on the compiler track; the runtime twin monomorphizes on both tracks as
  today. The role and the existing `platform` dimension stay orthogonal but compose.
- **Front-end facts become single-bodied.** From `core` through `operator`, each value fact carries **one** expression
  (its body) instead of the `runtime`/`signature` pair — each phase processes one expression per value with the path it
  already has. (This is the same uniformity the `TypeStack` removal bought, taken to its conclusion: not only is the
  signature a plain expression, it is a plain *value*.)

## 2. The read boundary — the one new mechanism

Checking `v@Runtime` at concrete type arguments reads **`CompilerMonomorphicValue(v@Signature, args)`** — the reduced,
ground signature — instead of walking the signature expression in place. A callee reference inside a body reads the
callee's signature twin the same way. That single read is what routes every type-level expression through elaboration
(`EffectLifter`), ability resolution, carrier pinning, and full per-instantiation body reduction (`reduceSourced`,
where match reduction actually lives).

Consequences to design deliberately, not discover:

- **The acyclicity argument (written down and verified, Step 6).** Today `TypeStackLoop` names no monomorphic fact,
  which keeps the two tracks acyclic *by construction* (see `TypeStackLoop.Result`'s doc). Under the flip,
  monomorphization demands monomorphization, so acyclicity is a property of the **demand DAG**, argued in three parts:
  1. **A value mono depends only on its own signature twin, at depth 1.** `MonomorphicValue(v@Runtime, args)` (and its
     compiler analogue) reads `CompilerMonomorphicValue(v@Signature, args)` for its signature; the signature twin's mono
     is `signatureOnly`, so it **checks no body** and issues no further mono demand of its own. The demand bottoms
     immediately — there is no `v@Signature@Signature` (the kind is derived, never a fact, §1).
  2. **The callee flip must not fire while a signature twin computes itself.** A signature twin's own mono walks its
     signature, and a *self-in-signature* type constructor (`Function`'s kind mentions `Function`, `Type : Type`) would
     then read `CompilerMonomorphicValue(Function@Signature, …)` while computing exactly that fact — a genuine cycle
     (it surfaced in implementation as `Cyclic fact demand: Function@Signature <- Function@Signature`). The fix is
     structural, not a backstop: the callee flip is **gated off inside `signatureOnly` monos** (`Checker(signatureOnly)`
     → `flippedCalleeSignature` returns `None`), so a twin's walk resolves its callees in place (`evalExpr`) and never
     re-enters the twin fact. Callee flips therefore fire only in **body** checks (a runtime/compiler *value* mono),
     whose callee twin then bottoms per part 1. Depth stays ≤ 1.
  3. **No recursion closes it.** Eliot has no recursion (the body reference graph is checked acyclic), and the callee of
     a flip is a *different* value than the one being checked, so the finite body-reference DAG cannot cycle.
     `activeFactKeys` remains the backstop for the W3 back-edge (`readMonomorphicReturnGround`, which re-enters the own
     track's value mono and would otherwise re-enter an in-progress fact on a genuinely recursive calculated return).
- **Caching win, realised.** A callee's signature is now the computed-once, cached `CompilerMonomorphicValue(callee@Signature,
  args)` fact instead of re-walked per reference per instantiation — but only for a **fully-applied, all-ground-args,
  non-marker** callee reference; a partial application (remaining binders inferred by this call), a not-yet-ground
  argument, or a marker keeps the in-place `evalExpr` (which handles inference metas the ground read cannot).
- **Granularity / overhead — measured (Step 6).** On the examples workload the flip's overhead is **negligible**: a
  checker-heavy example (`EffectsMulti`) compiles in ~2.82 s pre-flip vs ~2.92 s post-flip (best of 3), a ~3 % wall delta
  that is within noise dominated by the ~2.8 s fixed JVM/mill startup — the signature-twin monos are largely computed on
  the compiler track anyway, and the added reads are fact-cache lookups. The `ide.lsp` suite (which drives the same mono
  machinery) stays green; it was not separately profiled. Decision: **keep** both the own-signature and the (gated)
  callee flip.

## 3. The two information-flow inversions become explicit dataflow

The return position has exactly two cases where information flows in a non-default direction. Today both are in-walk
special cases; under the split both become explicit reads between two ordinary values:

- **Guard (W2b): the signature computes without the body.** `v@Signature`'s body (e.g. `if(COND, String[]) else
  raise("…")`) reduces on the compiler track to `Right(t)` / `Left(msg)`; consumers — `v@Runtime`'s own check, and any
  caller — discharge at the read (`Right(t)` ⤳ check/publish `t`; `Left(msg)` ⤳ report at the established site: the
  guard definition for a direct monomorphize, the use reference for a caller).
- **Calculated return (W3): the body computes the signature.** A bare under-applied return (`Int`, a grown `Counter`)
  is solved *by the body* — so `v@Signature`'s mono cannot stand alone for W3 values; it reads back from
  `MonomorphicValue(v@Runtime, args)` (the existing `readMonomorphicReturnGround` back-edge, with the authority
  explicitly inverted). W3 was one of the two reasons the earlier graft measurement failed (§8); this plan *owns* the
  inversion instead of pretending signature-first always holds. The body-side mechanism (`installReturnMeta`, the
  return meta the body solves, `failOnUndeterminedCalculatedReturn`) survives behind this edge.

Note the symmetry: guard = signature without body; calculated return = body computing signature. The split makes both
*visible* as dataflow between twins — which is the sense in which it removes exceptional handling rather than merely
relocating it.

## 4. Remaining feature work on top (the split is groundwork, not the feature)

The split delivers *reduction machinery access*. For effectful returns to actually work, the Stage-2 carryover must
land on top — **none of it is on master today** (the Attempt-1 salvages were all lost in the hard revert; verified
2026-07-14):

- **`EffectLifter.underApplied` must recognise `VType`** — but **only in the pure-wrap direction** (`allowType`
  parameter). A genuine, guard-independent correctness fix (any pure *type* flowing into a carrier value slot —
  `if(COND, String[])`'s pure arm); confirmed by Attempt 1. Scoping is essential: recognising `VType` in the *shared*
  arm also enables the *bind-lift* direction, where `expected = VType` is an effectful carrier-headed term meeting a
  type/return position (a guarded signature reducing to `Either[String, A]`), and bind-lifting there strips the carrier
  and collapses a satisfied guard to `Left`. Landed scoped in Step 0.
- **Pin the *inferred* return row's carrier.** `Track.Compiler.pinCarriers` today keys off declared `paramConstraints`
  only; an inline guard's carrier arises from `else`/`raise` instantiation. Needs the unsolved effect-carrier-meta
  lookup (`Unifier.effectCarrierMetaIds` — rebuild; it was WIP-branch-only). The signature twin is a compile-time
  value, so this lands exactly where CP-D pinning already operates.
- **Re-add the compile-time Abort overlay** `stdlib/eliot-compiler/eliot/effect/Abort.els` (`data AbortCarrier` +
  `Effect`/`Abort` instances) — the missing sibling of the `Either`/`Option` overlays, needed so `else`/`runAbort`
  reduce on the compiler track. Confirmed correct and needed by Attempt 1; deleted in the revert; currently absent.
- **Derived effect row for signature twins.** A signature twin's own signature (the kind) is compiler-derived, so its
  effect row is derived too: signature twins are exempt from the user-facing `declared ⊇ used` check — their row is
  *inferred* and published for the consumer's discharge. Principled, because the kind was never user-written.
- **Discharge at the read.** `dischargeGuardedReturn` (the `Right`/`Left` consumer read, `GuardChannel` protocol)
  survives, relocated to the twin-read boundary.

## 5. Checks that newly reach signatures — verify, don't assume

Making the signature a body means body-checks now cover it. Each must be confirmed not to newly reject legitimate
programs:

- **Recursion check** (today deliberately body-only so covariant `data Tree(left: Tree, right: Tree)` is not flagged).
  Under the split the constructor's signature *is* a body — but the exemption should fall out structurally: that body
  references the *type constructor* `Tree`, whose own twins have no body referencing back, so there is no cycle.
  Verify this, plus the monad-transformer lift. Upside: a genuinely cyclic type alias is now caught by the ordinary
  recursion check — a fail-safe win.
- **Strict positivity**: untouched (a property of `data` shape, not of signature execution).
- **Purity / effect accounting** on signature bodies: runs with §4's derived-row exemption.
- **Saturation / W2 growth** (`inferableArity`): derived from the signature twin's fact instead of the combined value.

## 6. Deletion ledger — the machinery this work retires

This is the measure of the plan (goal 2): each item below exists **only because a signature is not a value**. If an
item does not become deletable, the design has drifted.

**Monomorphize / checker — the guard machinery:**

- `CheckState.sawGuardReturn` + `recordGuardReturn` (`CheckState.scala:63,69`) — the last "structural flag on the
  return position" (Stage 1 removed the persisted `calculatedReturn`; this checker-state boolean is the survivor).
- `CalculatedReturnResolver.isGuardCarrier` (`:263`) and the `case VType => isGuardCarrier` carve inside the kind
  check (`Checker.scala:238`).
- `CalculatedReturnResolver.dischargeGuardedSignature` (`:321`) — the runtime-track callee-side discharge
  (`Track.Runtime.settleGuardedReturn`, `Track.scala:99`) — subsumed by compiler-track reduction + the consumer read.
- `Track.settleReturnPosition`'s three-way switch collapses: the calc branch becomes the explicit W3 back-edge (§3),
  the guard branch deletes; ability-implementation *markers* keep their publish-undischarged behaviour (their verdict
  is interpreted per candidate by `AbilityImplementationProcessor` — a separate feature, untouched).
- The Stage-4 ability-guards workaround in `TypeStackLoop`: `reduceGuardSubValues`, `reevaluateGuardReturn`,
  `collectValueRefs` (`TypeStackLoop.scala:191–237`) — it exists to reach the full pipeline for *named sub-values* of a
  where-guard signature and then re-evaluate in the kernel; under the split the guard signature is itself a
  compiler-track value reduced by the pipeline, so the workaround is unnecessary.
- The guards' reliance on the **precompute-and-merge** path (`CompilerNativesProcessor` nullary reduction feeding
  `BindingMergerProcessor`): the precompute stays for genuine compile-time natives, but guards stop depending on it —
  today it is exactly why `orError` (nullary-reducible named combinator) works while inline `if..else..raise`
  (per-instantiation) cannot.
- `walkTypeStack` and the whole in-place signature walk, including its `levelExprs` plumbing
  (`TypeStackLoop.scala:394–415`): the kind check moves into the signature twin's own mono; ability refs embedded in
  type positions are discovered by that mono's ordinary body walk. `TypeStackLoop` shrinks to "read the reduced
  signature, check the body against it" — and finally earns its long-deferred rename.
- `flattenReturnToType` (`TypeStackLoop.scala:248`) — the W3 kind-check transient — dissolved by the §3 inversion (the
  signature twin's W3 mono defers to the back-edge; no placeholder rewrite of the return position).

**Front-end — the dual-slot threading:**

- The `NamedValue.runtime`/`signature` two-slot shape and every phase's own-signature arm (the resolver's signature
  resolution, matchdesugar's own-signature path — the `a18c1e2f` fix class, operator's signature handling, the effect
  phase's runtime-only read): each fact carries **one** body; each processor processes one expression per value.
- `signatureEquality` as a bespoke comparator — becomes the ordinary all-layers-agree merge rule for `@Signature`
  twins.

**Stdlib / tests — once the payoff feature lands:**

- `eliot.lang.Guard` (`when`/`orError`) in both layers (`stdlib/eliot/eliot/lang/Guard.els`,
  `stdlib/eliot-compiler/eliot/lang/Guard.els`) — replaced by `if..else..raise` / bare `raise`.
- `GuardSignatureIntegrationTest` rewritten to the `if..else..raise` / bare-`raise` forms; the `Expression.scala`
  return-parser doc comments and `ASTParserTest` guard cases updated.

**What explicitly survives** (so nobody "cleans up" too far): `GuardChannel` (the `Right`/`Left` verdict protocol —
both its interpreters remain); `dischargeGuardedReturn` as the consumer read; ability-implementation `where` guards
end-to-end (`MarkerGuardSignature` semantics, `AbilityImplementationProcessor` interpretation — a different feature);
`installReturnMeta` / return-meta solving (the W3 body-solve, now behind the explicit back-edge);
`CompilerNativesProcessor` precompute for genuine compile-time natives; `SignatureView` (binder/return peeling and
kind derivation).

## 7. Execution plan — steps that build on each other

Each step is one committable unit: it compiles, the full suite + examples are green at its gate, and the next step
assumes it. Fact-shape changes bump `CACHE_VERSION`. Steps 1–4 use **transient join adapters** (a processor that
reconstitutes the old dual-slot view for the phases not yet converted) exactly as the `TypeStack` removal did — each
adapter is deleted by the step that converts its consumer, and none survive past Step 4. Lesson from Step A's fate:
the role is born with consumers in the same arc (Steps 5+6 are one arc; do not land 5 without starting 6).

- **Step 0 — prep: the guard-independent `EffectLifter` fix.** Teach `EffectLifter.underApplied` to treat `VType` (the
  type of types — a rigid nullary head) as under-applied against a carrier meta, but **only in the pure-wrap direction**
  (`mustPureWrapBeforeUnify`, `allowType = true`): a pure *type* flowing into a carrier *value* slot
  (`if(COND, String[])`'s pure arm — Attempt 1's B1(1)) must be `Effect.pure`-wrapped. The **bind-lift** direction
  (`mustLiftBeforeUnify`, `allowType = false`) must **not** recognise `VType`: there the rigid head is the *expected*
  slot, and `expected = VType` is an effectful carrier-headed term meeting a type/return position (a guarded signature
  reducing to `Either[String, A]`) — bind-lifting there strips the carrier and collapses a satisfied guard to `Left`
  (verified: the unscoped shared-arm change reddens all five satisfied-guard `GuardSignatureIntegrationTest` cases; the
  scoping keeps the suite green). Regression tests: the pure-wrap arm fires on `VType` and declines against a concrete
  carrier; the bind-lift arm does not fire on `VType`-expected. No split dependency.
  *Gate:* suite green.

- **Step 1 — the role is born: identity, `core`, `module`.** *(landed 2026-07-15.)* Add `Role = Runtime | Signature`
  as a field on `QualifiedName` (default `Runtime`, invisible in `Show`/mangling — a runtime twin renders and mangles
  byte-identically, so diagnostics, jvm class names, and test expectations don't churn). `CoreProcessor`
  (`transformFunction`, so every `DataDefinitionDesugarer` output too) mints both twins: signature twin body = the
  signature expression (always present, never abstract), binders stamped on both, abstractness = runtime twin body
  `None`. The signature twin's own signature (the kind) is **not** minted/stored — its `signature` slot repeats the body
  as an inert placeholder until Step 5.
  `UnifiedModuleValueProcessor` merges **per `(name, role)`** (the role is part of the `vfqn` key): signature twins
  **all-agree** (`hasSameSignatures`; no prefer-the-implementation, so two co-located *concrete* layers merge cleanly
  where a runtime twin would be "Has multiple implementations."), runtime twins prefer-the-bodied (unchanged). The
  `dischargedEffects` layer-union stays on the runtime twin for now (the effect phase reads it there; it relocates to
  the signature twin when the effect phase converts, Step 4).

  **Realization notes (differs from the original sketch above):**
  - *No join adapter is needed.* The runtime twin stays the dual-slot `NamedValue`, so it **is** the reconstituted fact
    `resolve` reads — byte-identical. The single-bodying of the runtime twin is deferred to Steps 2–4 as planned; the
    "adapter" only becomes non-degenerate once a downstream phase drops the signature slot.
  - *Signature twins stay out of the **surface name set**.* `ModuleNamesProcessor` extracts `Runtime`-role names only.
    The name set / dictionary is enumerated and qualifier-decoded by many consumers (imports/shadowing, the ability &
    constructor indices, `resolve`'s ability search, LSP completion, and the JVM backend — where a signature twin
    *collides* onto its runtime twin's class name, a hard `ZipException`), and nothing ever *resolves into* a signature
    twin. So the twins flow through the **value** machinery instead: `ModuleValueProcessor` registers each signature
    twin's `ModuleValue` beside its runtime twin, and `UnifiedModuleValueProcessor` locates a value's files by its
    `Runtime`-role surface name, then fetches the role-bearing `ModuleValue`s. A signature twin is thus a first-class,
    demandable `UnifiedModuleValue` fact (what Step 5 needs) without polluting name resolution. The only qualifier
    decoders touched are the four whose 2-arg `QualifiedName(_, _)` patterns the new field forces open — each now
    matches `Role.Runtime` (self-documenting: signature twins are not ability methods).
  *Gate:* zero behaviour change (full suite + examples + `ide.lsp` green, `HelloWorld` builds & runs);
  `SignatureTwinMergeTest` exercises the recast merge rules; `CACHE_VERSION` 24 → 25.

- **Step 2 — `resolve` on twins.** *(landed 2026-07-15 — verification-only, no production change.)* Each twin resolves
  as its own value through the *same* `ValueResolver` — no role-specific path. The signature twin's body is its
  (resolved) signature expression, and the generic binders baked into that expression are in scope while it resolves
  (the cross-twin scope point). **Why no code change:** the Step-1 realization keeps the runtime twin dual-slot, so its
  binder scope still comes from its own `.signature` (`collectGenericParamsFromExpr`, unchanged), and the signature twin
  — a well-formed `NamedValue` whose `.signature` placeholder equals its body (both = the curried type, which carries
  the binders) — resolves through the identical path. The original sketch's "runtime twin's scope takes the stamped
  binder list" + "join adapter moves post-resolve" were artifacts of the progressive single-bodying mechanic Step 1 did
  not adopt; the runtime twin's `.signature` slot survives until the deletion sweep (Step 9, after Step 6 makes the
  signature twin authoritative). `SignatureTwinResolveTest` pins it.
  *Gate:* zero behaviour change (full suite + examples green); new `SignatureTwinResolveTest`.

- **Steps 3–4 — `matchdesugar` + `operator` + `saturate` on twins.** *(landed 2026-07-15 — verification-only, no
  production change; folded into one front-end-confirmation unit.)* The whole front-end is keyed by `(vfqn, platform)`
  and role-agnostic, so the signature twin flows through `matchdesugar` → `operator` → `saturate` unchanged: each phase
  produces the signature twin's fact with its body preserved and zero errors, and the saturated signature carries the
  twin's `inferableArity` (§5). There is no "single-bodied end-to-end" conversion and no migrating join adapter to do
  (the runtime twin stays dual-slot; `mono` reads it as today). Effect/recursion checks are never *demanded* for
  signature twins in the real pipeline (nothing references them), so they stay runtime-only for free until Step 8.
  `SignatureTwinFrontEndTest` pins matchdesugar/operator/saturate. `used`/`uncurry`/backend and the LSP/apidoc indices
  key off runtime twins (unchanged — signature twins are not in the surface name set).
  *Gate:* full suite + examples + `ide.lsp` green; zero behaviour change. (The mechanical split is complete — the real
  divergence-work is all Step 5+.)

- **Step 5 — the signature twin gets its own mono (consumer-first).** *(landed 2026-07-15.)*
  `CompilerMonomorphicValue(v@Signature, args)`: check the signature body against the **derived kind**, elaborate +
  reduce on the compiler track. W3 values (under-applied return) *decline* here — their back-edge lands in Step 6. The
  consumer landing in the same arc is an **equivalence test**: for representative fixtures (generic, ability-constrained,
  guarded, W3-declined), the twin mono's ground signature equals what the in-place walk produces.

  **Realization notes:**
  - *The signature twin's mono reuses the runtime twin's, minus the body.* `TypeStackLoop` gains a `signatureOnly` flag;
    when set it (a) ignores the value's (placeholder) `.runtime` slot — walking the signature *is* the whole job, so the
    ordinary body-less path runs (no body check, `reduced`/`body` `None`) — and (b) declines a calculated return (`isCalc`)
    with an explicit, error-free abort instead of the abstract-calc-return error. Nothing else changes: `walkTypeStack` +
    `applyTypeArgs` + carrier-pinning + post-drain resolution are the same calls, so equivalence with the runtime twin's
    in-place walk is nearly by construction (only the body-driven return — calc / guard — could diverge, and W3 declines).
  - *No new fact, no new processor, no shape change.* The role rides `CompilerMonomorphicValue`'s key `vfqn`, so
    `CompilerMonomorphicTypeCheckProcessor` branches on `key.vfqn.name.role` (its `TransformationProcessor` already reads
    the role-appropriate `SaturatedValue`). For a `Signature`-role key, `signature` carries the sibling's *reduced ground
    signature* (what §2's consumer reads) and `reduced` is `None`. `CACHE_VERSION` unchanged (25) — a new producer branch
    for an existing fact type at new keys, no persisted shape churn.
  - *The read is not yet flipped* (that is Step 6): nothing in the real pipeline demands a signature-twin mono; only the
    equivalence test (`SignatureTwinMonoTest`) does, comparing both twins on the *same* compiler track (apples-to-apples,
    so the guard producer path matches too — pinCarriers runs identically with no body).
  *Gate:* equivalence holds (`SignatureTwinMonoTest`); full suite + all modules + examples green.

- **Step 6 — flip the read.** *(landed 2026-07-15 as 6a/6b/6c.)* `v@Runtime`'s mono reads the twin mono instead of
  walking in place, and callee references read the callee's twin the same way. Write down the §2 acyclicity argument.
  *Gate:* full suite + examples green; overhead measured on examples + the LSP workload; acyclicity note added to §2.

  **Realization notes (differs from the original sketch — the flip lands, but as a *gated* flip beside a retained
  in-place path, not a wholesale replacement):**
  - *6a — own-signature flip.* `TypeStackLoop.establishSignature` reads the value's own reduced ground signature from
    `CompilerMonomorphicValue(v@Signature, args)` (injected by both mono processors) and re-inflates it to a checkable
    `VPi` via the new `Evaluator.groundToSemPi`, binding the type arguments to their binders in ρ/Γ — no in-place walk.
    Scoped to the behaviour-preserving case (Step 5's equivalence backs it): **fully-applied, non-W3, non-marker**.
    Markers are excluded *and their twin is not even read* — a `where` guard rides the return slot and reduces only in
    the in-place guard machinery (else a stuck `Bool::&&` signature).
  - *6b — callee flip.* `Checker.inferValueReference` reads `CompilerMonomorphicValue(callee@Signature, groundArgs)` when
    the reference **fully applies** the callee and **every argument is ground**; otherwise the in-place `evalExpr`
    (partial application whose remaining binders this call infers, or an inference-meta argument, needs it). The flip is
    **gated off inside `signatureOnly` monos** (`Checker(signatureOnly)`) — the load-bearing acyclicity fix (§2 part 2):
    without it, self-in-signature `Function`/`Type` cause `Cyclic fact demand`. `groundToSemPi` is the shared
    re-inflation (a quoted `Function` type lost its Π structure).
  - *6c — W3 back-edge inversion.* `readMonomorphicReturnGround` re-enters the **own track's** value mono
    (`MonomorphicValue` on runtime, `CompilerMonomorphicValue` on compiler) instead of the runtime one unconditionally —
    the callee flip now lets a compiler-track value reach a calculated-return callee whose reduced return lives in the
    compiler pool. The recursion guard covers both key shapes.
  - *Deliberate deviations from the sketch, all so each commit stays green:*
    - **`walkTypeStack` + `levelExprs` are NOT deleted.** The kind-check-and-eval of a signature is irreducible, and the
      signature twin's own mono still needs it (as does the in-place fallback for W3 / partial application / markers /
      non-ground callee refs). What Step 6 achieves is removing the in-place walk from the *common runtime-value path*
      (it reads the twin); the walk survives, now reached only through `signatureOnly` monos and the fallbacks. Its
      deletion, if ever, is not Step 6.
    - **The W3 form is "twin declines, `v@Runtime` computes in place," not "twin reads back."** A W3 signature twin still
      declines (Step 5); `v@Runtime` solves its own calculated return from its body via the in-place path; the back-edge
      serves *caller* references to W3 callees. This sidesteps the `v@Runtime ↔ v@Signature` self-cycle a read-back would
      create.
    - **Guards are not routed through the twin read** (that is Step 7's job with `dischargeGuardedReturn` at the read).
      Markers stay in-place; W2b effectful-signature guards are partial-applied (inferable carrier) → in-place. So the
      guard discharge is untouched by Step 6.

- **Step 7 — the feature (Stage-2 carryover).** *(COMPLETE 2026-07-15 — Part 1 `ff5fa879`, the crux this commit.)* The
  inline `if(cond, T) else raise(msg)` and bare `raise(msg)` guards now reduce on the signature twin's compiler mono to
  their `Right(t)`/`Left(msg)` verdict, discharged at the consumer's twin read — the same path `orError` already takes.
  *Gate met:* new `if..else..raise` + bare-`raise` `GuardSignatureIntegrationTest` fixtures green (accept runs as the
  bare type, reject fails the build with the author message) **and** the untouched `orError` cases still pass; full
  suite + all examples green.

  **Realization map (what landed, and the two mechanisms the sketch under-anticipated):**
  - **Part 1 — DONE** (`ff5fa879`): `stdlib/eliot-compiler/eliot/effect/Abort.els` (`data AbortCarrier` + `Effect`/`Abort`
    over any base `G`). `CompilerAbortCarrierTest` pins its extraction.
  - **`Unifier.effectCarrierMetaIds`** — derived from `carrierRoles.collect { case (id, CarrierRole(_, true)) => id }`,
    as planned.
  - **`Track.Compiler.pinCarriers` pins the inferred carrier** (`pinInferredReturnCarriers` → `pinMetaToEither`): every
    still-unsolved effect-carrier meta is fixed to the base `Either[String]`. The `{Abort}` layer
    (`AbortCarrier[Either[String]]`) is *not* pinned — it is already solved *structurally* by the `else` signature's
    `computation: AbortCarrier[G, A]` unification, so only the base metas (`else`'s `G`, `raise`'s carrier) remain to pin.
  - **`isGuardCarrier` recognises an effect-carrier *meta* head** (not in the sketch — the load-bearing kind-check fix).
    At kind-check time the inline guard's return type is `?G[?A]` (carrier still a meta), so the concrete-`Either`/`Bool`
    recognition missed it; without acceptance the kind check postponed `?G[?A] ~ Type`, which the carrier pin then turned
    into a hard `Either[String, _] ~ Type` mismatch. Accepting an ability-constrained (effect) meta head as a guarded
    return closes that — and does **not** over-fire, because a normal effectful return (`{Console} Unit`) never reaches
    this ladder with a `Type` expectation (verified against the effect examples). `sawGuard` is *not* the gate for the
    reduction — it is `false` for `orError` too; the reduction is driven by `signatureOnly && sawGuard && !isMarker`.
  - **Deep-reduce to the verdict, uniformly** (`TypeStackLoop.quoteSignature` → `PostDrainQuoter.reduceSemExprToGround`):
    the checked return position (peeled of its binders) is re-evaluated with the resolved impls / reduced sub-values
    folded in — the one form that yields `Right(t)`/`Left(msg)` for *all* shapes (the shallow signature value is the
    verdict only for `orError`; the carrier *type* for bare `raise`; a stuck `flatMap(match,…)` for inline `if..else`).
    The raw signature ORE is *not* usable here — it lacks the checker's effect-lift `pure`; the **checked** expression
    (in `levelExprs`) carries it.
  - **The stacked-carrier recursive reduction** (not in the sketch — the true crux, the Attempt-1 wall). An inline
    `if..else..raise` builds `AbortCarrier` *over* the `Throw[String]` base `Either[String]`, so an impl body performs a
    *nested* base ability call (`Effect[AbortCarrier[G]]::pure` = `a -> AbortCarrier(pure(Some(a)))`; `AbortCarrier::abort`
    = `AbortCarrier(pure(None))`). Reducing an impl / sub-value at one instantiation leaves that inner call abstract; it
    must be resolved **recursively**. `ReducedBindingClosure.reduceInstance` gains a `recursive` flag: when set,
    `collectBindings` takes each dependency **reduced at its own instantiation** (recursively) instead of its raw
    `NativeBinding` — the base-carrier impls bottom out non-nested. The guard path (`reduceGuardSubValues` for `else`/`if`;
    `reduceResolvedImpls` for the surface `raise`/`pure` impls) uses `recursive = true`, each binding wrapped to *absorb*
    the reference's type arguments (`absorbLeadingArgs` — the reduced body is already instantiated; `MonomorphicEvaluator`
    drops a reference's type args, but the twin-read quoter applies them). The flag defaults `false`, so the
    precompute-and-merge and `where`-marker readers keep the one-hop raw closure — reducing every dependency there tripped
    spurious per-instantiation resolutions (a `Compare` dep at a defaulted `Type` arg), the regression that scoped this.
  - **Still to sweep (Steps 8–10):** the old runtime-side discharge path is now redundant for these guards but not yet
    deleted; the derived-effect-row exemption for signature twins was not needed for the inline guard to reduce and is
    left to Step 8. `CACHE_VERSION` unchanged (no persisted fact-shape change).

- **Step 8 — checks reach signatures (verify, don't assume).** Extend the recursion check to signature-twin bodies:
  covariant `data Tree(left: Tree, right: Tree)` and the monad-transformer lift must stay accepted (the §5 structural
  argument, now tested); add the type-alias-cycle regression (newly caught — a fail-safe win). Purity/effect accounting
  over signature bodies with the derived-row exemption.
  *Gate:* full suite + all examples compile — no legitimate program newly rejected.

- **Step 9 — deletion sweep, checker.** Delete the §6 monomorphize items: `sawGuardReturn`/`recordGuardReturn`,
  `isGuardCarrier` + the `VType` kind-check carve, `dischargeGuardedSignature` + the `Track.Runtime.settleGuardedReturn`
  guard branch, the `settleReturnPosition` switch collapse, the Stage-4 `reduceGuardSubValues`/`reevaluateGuardReturn`/
  `collectValueRefs` workaround, `flattenReturnToType`, and the guards' precompute-and-merge reliance.
  *Gate:* suite green; net line count of `monomorphize/check` decisively negative — goal 2 realized.

- **Step 10 — deletion sweep, stdlib/tests + closeout.** Delete `eliot.lang.Guard` (`when`/`orError`) from both
  layers; rewrite `GuardSignatureIntegrationTest` to the `if..else..raise`/bare-`raise` forms; update the
  `Expression.scala` return-parser doc comments and `ASTParserTest` guard cases. Mark this document **COMPLETE** with
  the final net-line ledger.
  *Gate:* full suite + all examples green.

## 8. Evidence record (absorbed from the retired docs — why this design and not the others)

- **The deep-reduction wall (Attempt 1, branch `wip/return-position-unification-stage2`, never merged).** Reducing the
  guard tower in place bottoms out at `VNeutral(Reserved(Match))` — a stuck pattern match whose discriminee becomes
  concrete only after inlining, and `renormalize` has no rule to re-fire it; match reduction lives solely in the full
  monomorphization pipeline. Structural: any in-place approach must re-implement the body pipeline feature-by-feature
  inside the checker (the second-evaluator anti-pattern). This is *the* reason the signature must become a body.
  Attempt 1's B1 fixes (the `underApplied` `VType` arm; accepting a carrier-meta-headed return at the `Type` boundary)
  were validated there but are **not on master** — the revert took everything; §4 rebuilds what is still needed.
- **The graft measurement (retired TypeLevel doc §3).** Routing the *unsplit* value's signature through a level-1 mono
  demand was measured to net-ADD and to break the tracks' acyclicity-by-construction as a bolt-on — with the guard
  machinery explicitly out of scope of that measurement. It is not a verdict on *this* plan: the split changes the
  ledger (single-slot facts across the front-end plus the entire §6 guard machinery), and the demand is a structural
  read between two values born separate, not a graft onto a combined one.
- **Step A's fate (retired TypeLevel doc §2).** A `typeLevel` dimension minted at the *saturate* boundary with no
  consumers was scaffolding, deleted wholesale. Hence the staging rule: the role is born in `core` and its consumers
  (merge, resolve scope, the checker read) land in the same arc.
- **`evalSemExpr` is not a drop-in for `evalExpr` on the unsplit walk** (Attempt 1: taking the signature *value* from
  the elaborated check regressed `fold`-based guards — the elaboration's extra explicit type-arg meta left the `fold`
  native stuck). Moot under the split: the signature body is elaborated *and reduced as a body* — the same path that
  makes `orError`'s body reduce today — never evaluated-then-patched.
- **Reject-site convention** (existing green tests): a guard's `Left(msg)` is reported at the guard *definition* for a
  direct monomorphize and at the *use reference* for a caller. The twin read must preserve this.

## 9. Guardrails (stop rules)

- **Net-simplify the checker.** The §6 ledger must realize. A new signature-specific branch in `Checker`/`Track`, or
  any "the checker learns to force this type-level computation to reduce" move, is the wrong direction — the split
  exists precisely so the *ordinary pipeline* runs type-level code.
- **One evaluator, one pipeline.** No kernel reduction re-implementation: no sub-value binding composition, no
  deep-`renormalize` read-back, no carrier synthesis in the walk. (The full list of forbidden moves is §8's first
  bullet — they were all tried.)
- **The kind is never minted or stored.** Binary role, derived kind, no level towers.
- **Gaps must be fail-safe.** A signature that does not reduce hard-errors (the `PostDrainQuoter` "Cannot resolve
  type." convention), and a guard rejection is always reported (`GuardChannel.fallbackRejectionMessage`) — never a
  silent `Type` fallback, never silent acceptance.
