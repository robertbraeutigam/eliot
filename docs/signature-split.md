# Signature split — the signature becomes a named value (`Runtime`/`Signature` role)

**Status: PLANNED** (2026-07-14). This plan supersedes and replaces two retired documents:

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

- **The acyclicity argument changes.** Today `TypeStackLoop` names no monomorphic fact, which keeps the two tracks
  acyclic *by construction* (see `TypeStackLoop.Result`'s doc). Under the split, monomorphization demands
  monomorphization — acyclicity becomes a property of the demand DAG: Eliot has no recursion (the reference graph of
  bodies is checked acyclic), and the derived kind bottoms the tower (a signature twin's own check demands no further
  signature mono). `activeFactKeys` remains the recursion backstop. This argument must be **written down and verified**
  during implementation, not assumed.
- **Caching win.** Today a callee's signature is re-walked per reference, per instantiation, inside every caller's
  check. It becomes a computed-once, cached fact per `(value, args)`.
- **Granularity risk.** Every callee reference becoming a fact read is a real overhead-profile change; measure on the
  examples + LSP workload before declaring the increment done.

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

- **`EffectLifter.underApplied` must recognise `VType`** (`case VType => 0 < arity`). A genuine, guard-independent
  correctness fix (any pure *type* flowing into a carrier value slot — `if(COND, String[])`'s pure arm); confirmed by
  Attempt 1, currently absent (`EffectLifter.scala:131` has only the `VTopDef`/`VNeutral` arms).
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

## 7. Staging (each increment lands green; lesson from Step A: split at birth, consumers in the same arc)

1. **Mechanical split at birth.** `core` mints the twins; roll the role through module (merge recast per §1) →
   resolve (shared binders) → matchdesugar → operator → effect → termination → saturate. The checker still walks the
   signature twin's body in place — behaviour identical to today. Full suite + examples green; `CACHE_VERSION` bump.
   *Gate:* front-end fact shapes are single-bodied; zero behaviour change.
2. **Flip the read.** `v@Runtime`'s check and callee references read `CompilerMonomorphicValue(v@Signature, args)`;
   the kind check moves into the signature twin's own mono; the W3 inversion lands (back-edge). The in-place
   `walkTypeStack` is deleted. *Gate:* suite green; the §2 acyclicity argument written down; overhead measured on the
   examples + LSP workload.
3. **Stage-2 carryover** (§4): the `underApplied` `VType` arm, inferred-row pinning, the Abort overlay, discharge at
   the read. *Gate:* `if..else..raise` and bare-`raise` fixtures green **alongside** the existing `orError` forms.
4. **Deletion sweep.** The §6 monomorphize items, then `Guard.els` + test rewrites. *Gate:* the ledger realized; the
   net line count of `monomorphize/check` decisively negative.

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
