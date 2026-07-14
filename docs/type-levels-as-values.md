# Type levels as named values — uniform front-end processing (levels born in `core`)

**Status:** IN PROGRESS. **Step A landed** (`614da60a`); the **genArrow divergence fix** (`a4e64b62`) and the
**match-in-signature fix** (`a18c1e2f`) landed 2026-07-14. The goal was **reframed** (Robert, 2026-07-14, `9f586f07`):
this is not a `monomorphize` line-count exercise (that framing, §3, is measured dead) — it is about *uniformity*,
realized by making higher type levels **ordinary named values born in the `core` phase**.

**B0 audit DONE (2026-07-14).** The read-only measurement (§4 "B0 — Audit results") is in: the net-delete is real,
but it lives in **de-stacking the six *nested* single-level `TypeStack` positions (Change A)**, not in **levels born in
core (Change B)**. Change A passes the gate (~80–120 lines, no new machinery, provably lossless); Change B is
line-break-even (its per-phase traversal delete is offset by the core mint + a resolve scope-threading blocker + the
`typeLevel` identity plumbing) and its headline *uniformity* win is **already banked by `a18c1e2f`**. See §4.

## Goals — read this first

1. **Uniformity (the primary goal).** A type-level expression must go through **the whole front-end pipeline
   automatically** — match desugaring, operator resolution, effect checking, ability resolution, per-instantiation
   reduction — *exactly like a value body*. Today it does **not**, and inconsistently so: operators and abilities work
   in signatures, but a `match` in a signature crashed until `a18c1e2f`. The mechanism that delivers uniformity is
   making each type level an **ordinary named value**, so no phase needs type-level-specific code.
2. **Net-delete across the front-end (the measurable proxy).** `TypeStack` is threaded through **~35 files**
   (matchdesugar ~71 refs, core ~28, resolve ~26, operator ~24 …). Every front-end phase individually traverses it;
   all of that traversal *deletes* once a level is a value the phase already knows how to process. **This — not
   `monomorphize` — is where the lines come off.**
3. **Non-goal restated: not the `monomorphize` walk.** Dissolving `walkTypeStack` (§3) was measured and does *not*
   net-delete; it is a dead end as written. A level `n ≥ 1` is **not special in any way** — checked, reduced, and read
   like every other named value.

**Hard non-goals (this is where the first attempt went wrong — do not repeat it):**

- **Do not extend the compiler / the checker / the kernel to make any particular type-level computation reduce.**
  Whether a value reduces at compile time is a property of *that value and its layer bodies*, never something the
  checker should be grown to force.
- **Do not chase making effectful guards reduce at compile time.** `if..else..raise`, `when`/`orError`, `{Throw}` /
  `{Abort}` signature computations — none of these are a target here. They are ordinary values; if one does not reduce,
  that is a pre-existing reducibility property of the values on its path, out of scope for this plan. (The old
  "effectful returns read a guard verdict" framing has been **deleted** from this plan — it lured an implementer into
  re-implementing reduction machinery in the checker. Do not resurrect it.)
- **Do not re-implement reduction in the kernel.** No sub-value-binding composition, no type-arg-absorbing binding
  wrappers, no deep-`renormalize` read-back, no carrier synthesis on level signatures. §0 explains why: re-implementing
  the body pipeline inside the kernel bottoms out at `Match` and is the anti-pattern the whole "named values" idea
  exists to avoid.

**Guardrail, restated as a stop rule:** the win is a phase *deleting* its type-stack-specific code because it now reads
a level as an ordinary value — so a change that makes a phase (or the checker) *learn* new level-specific behaviour is
going the wrong way; route the expression through the ordinary value machinery, or don't do it. Each staged step must
net-delete (see §4's per-step gates and §5).

## 0. Principle

> The unit of execution on every platform is a **named value**, not an expression. The NbE `Evaluator` is the
> *kernel*, not the platform: an expression runs by becoming (part of) a value's **body**. "Types are values" must
> therefore hold *representationally*, not just semantically — the type of a value is a named value one **TypeLevel**
> up, compiled by the same pipeline as every other value.

Only a named value passes through saturation → the effect phase → check-mode elaboration → ability resolution →
per-instantiation match reduction. A signature slot that is walked *in place* gets none of that — it gets only the bare
β/δ+native kernel, and match reduction in particular **cannot** be added to the kernel (`MatchNativesProcessor.stuck`
mints `VNeutral(Reserved(Match), …)`, dropping the reducer; adding a dispatch rule to `renormalize` would duplicate the
match native — the second-evaluator anti-pattern). So the way to make a higher-level expression behave *exactly* like
runtime code is to make it a **named value** and let the ordinary pipeline run it — not to grow the kernel to reduce it
in place.

## 1. The model

Named-value identity gains a **TypeLevel** dimension, parallel to `Qualifier`:

| TypeLevel | Content            | Today's representation            | Track    |
|-----------|--------------------|-----------------------------------|----------|
| 0         | runtime body       | `NamedValue.runtime`              | runtime  |
| 1         | the signature      | `TypeStack.levels(0)`             | compiler |
| n ≥ 2     | kind levels        | `TypeStack.levels(n-1)`           | compiler |
| top       | literal `Type`     | implicit                          | —        |

A value at level `n` has **body** = the level-`n` expression and **signature** = the level-`n+1` expression. Level `n`
is checked as an ordinary value **against the evaluation of level `n+1`** — the same recursive "check a value's body
against its signature", applied up a finite tower that bottoms at `Type`.

**The payoff — front-end uniformity (not the `monomorphize` walk).** Once a level is an ordinary named value born in
`core`, every front-end phase (matchdesugar, operator, effect, ability) processes it with the value path it already
has — no phase threads `TypeStack`, and a type-level expression gets match/operator/effect/ability resolution *for
free*. The measured dead-end (§3) tried to cash this out inside `monomorphize`'s `walkTypeStack`; the real dividend is
deleting the per-phase type-stack traversal across the ~35 files that carry it (§4). A level is still "an ordinary value
checked against the eval of the level above" — that model (§0) is unchanged; what moves is *where the level value is
born* (core, early), so the front-end sees it.

## 2. What's landed — Step A (`614da60a`, `CACHE_VERSION` 23)

The naming at the saturate/monomorphize boundary, with **zero consumer changes** and the full suite green:

| Symbol | File | Role |
|---|---|---|
| `SaturatedValue(value, typeLevel = 0)` + `.Key(vfqn, platform, typeLevel = 0)` | `saturate/fact/SaturatedValue.scala` | level dimension on the saturate-boundary fact |
| `CompilerMonomorphicValue(…, typeLevel = 0)` + `.Key(vfqn, typeArguments, typeLevel = 0)` | `monomorphize/fact/CompilerMonomorphicValue.scala` | level dimension on the compiler-mono fact |
| `TypeLevelSaturatedValueProcessor` | `saturate/processor/TypeLevelSaturatedValueProcessor.scala` | derives `SaturatedValue.Key(v, Compiler, n≥1)` from the host level-0 value |
| `SaturatedValueProcessor` (level-0 guard) | `saturate/processor/SaturatedValueProcessor.scala` | `generateFact` declines `typeLevel != 0` |
| `CompilerMonomorphicTypeCheckProcessor` (rides `key.typeLevel`) | `monomorphize/processor/CompilerMonomorphicTypeCheckProcessor.scala` | maps the level onto the `SaturatedValue.Key` fetch + the produced fact |
| `TypeLevelEquivalenceTest` | `lang/test/…/monomorphize/processor/` | pins level-1-reduced == level-0-signature |

Both processors match `SaturatedValue.Key` by type; the fact engine (`SequentialCompilerProcessors`) runs **both** per
key, and each `abort`s (declines) the levels it does not own — so the level partition needs no new dispatch mechanism.

**The synthetic level-value construction (`TypeLevelSaturatedValueProcessor`).** For a requested level `n`, let
`levelExpr = host.typeStack.levels(n-1)` and `view = SignatureView.of(levelExpr)`:

- **runtime body** = `view.copy(binders = Seq.empty).toExpression` — the type expression with its leading generic
  `FunctionLiteral` binders stripped, so those references are *free* and resolve to the ρ bindings.
- **synthetic signature** = `view.withParameters(Seq.empty).withReturnType(Type).toExpression` — the generic binders
  wrapping `Type`, so `applyTypeArgs` peels each binder and the body checks against kind `Type`.
- **upper levels** = `host.typeStack.levels.drop(n)`; `n` beyond the stack top ⟹ the literal `Type`.

This value is **an ordinary compiler-track value with a body and a signature** — that is the entire point. It carries
no guard-specific carrier, no synthesised effect channel: keep it that way.

**Step-A finding — RESOLVED (prerequisite for §3, landed ahead of the dissolution).** A level **body** is checked
through the *value* path (`Checker.infer` → Γ), whereas today's **signature** walk evaluates through the *type* path
(`walkTypeStack` → `evalExpr` → ρ). They used to resolve a generic-parameter reference differently: in a level body,
`applyTypeArgs` bound a *type* argument's Γ slot to the instantiated **value** (`Γ(X) = A`), not its **kind** (`Type`),
so `def genArrow[X]: Function[X, X]` — whose level-1 body is the arrow spine `Function[X, X]`, each `X` checked as a
value-path spine argument against `Function`'s domain kind `Type` — reported a spurious `Type mismatch` at `X`. **Fix
(two lines of substance):** (1) `applyTypeArgs` now binds Γ(param) to the argument's *kind* uniformly —
`groundToSem(head.valueType)`, i.e. `Type` for a type argument (`Type : Type`), not the argument value `A` — so
`infer(X)` reports `X : Type` and kind-checks against `Function`'s domain; ρ still binds `argVal` so a *type-position*
use (`evalExpr`) reads the denoted value `A`, unchanged. (2) A level body reduces to a **type**, which the runtime
staging gate (`PostDrainQuoter`) declines as "not a runtime constant"; the compiler track's `reduceSourced` now reads a
reduced ground *type* (`valueType === Type`) back structurally (`groundTypeToMono`) — types are values, so a type is a
legitimate compile-time constant here. `TypeLevelEquivalenceTest.genArrow` now asserts equivalence (was: asserted the
divergence). Level checking is thereby *one* path: a type-parameter reference resolves consistently whether reached from
a body or a signature — which is exactly what makes the §3 dissolution (route the signature through the level-1 mono)
safe. This was the substance of the work — not any effect/guard concern.

**Test-harness recipe (compiler-pool level tests).** A leaf `ProcessorTest(LangProcessors(systemModules = Seq.empty)*)`
demanding a `CompilerMonomorphicValue` at a non-trivial signature must **declare `Type` and `Function` in the compiler
pool** (`type Type` at `eliot.compiler.Type`; `type Function[A, B]` + `def apply` at `eliot.lang.Function`) and the test
module must `import eliot.lang.Function`. To assert equivalence, read a pure type-denoting `MonomorphicExpression` back
to a `GroundValue` (`TypeLevelEquivalenceTest.denote`) and compare to the host's level-0 `.signature`.

## 3. The simplification — MEASURED, and it does **not** net-delete (do not attempt as written)

The intended step: make a value's signature come from its **level-1 monomorphized value**, deleting the in-place stack
walk — in `TypeStackLoop.processIO`, replace `walkTypeStack(rv.typeStack)` with a demand for
`CompilerMonomorphicValue((v, 1), typeArgs)` and check `v`'s body against it, then delete `walkTypeStack` /
`flattenReturnToType` / the per-level `= Type` kind-unify.

**The mandated measurement was done (2026-07-14, by tracing every consumer, not by a throwaway implementation — the
check ladder is too delicate to churn for a line count I was already confident of). The result is that this step
net-ADDS and increases coupling. Concrete reasons:**

- **`instantiated` (the checkSig) is already fully produced by `evalExpr(levels(0))` + `applyTypeArgs` +
  `instantiateRemaining`.** `walkTypeStack`'s *only* unique contribution is the inline per-level kind-check plus
  `levelExprs`. So the demand replaces ~10 lines of an already-lean fold — not a bespoke walker re-implementing
  caching / a recursion guard (the plan's premise; the fold does neither).
- **The guard-signature machinery does not collapse into the sub-mono.** `reduceGuardSubValues` /
  `reevaluateGuardReturn` / `collectValueRefs` (~45 lines) exist because a guard reaches its ability (`Eq.equals`)
  *through an operator body* (`!=`), which ordinary post-drain resolution — collecting refs from the *un-inlined*
  checked signature — never sees. A level-1 sub-mono checks the identical un-inlined `E1 != E2`, so it has the same
  blind spot; the machinery would have to be kept, not deleted.
- **Calculated returns (W3/W4) force keeping the real-signature path anyway.** `installReturnMeta` needs the
  un-reduced return position as a live meta the body solves; the level-1 reduced result has it defaulted. So
  `applyTypeArgs` / `instantiateRemaining` / `flattenReturnToType` stay regardless.
- **It adds a level-0→level-1 `CompilerMonomorphicValue` fact edge**, breaking the current *"`TypeStackLoop` names no
  mono fact ⟹ the two tracks are acyclic by construction"* invariant, and needs level threading + a recursion base
  case + a new callback.

Net: removes ~10, adds ~20+ plus cross-track coupling. Per the stop-rule below, **the approach is wrong; it was not
committed.** The real, concrete improvement in this thread was the §2 genArrow divergence fix (landed `a4e64b62`):
level-checking is now genuinely one path, which also removed a "known divergence" carve-out from the test. If a future
session still wants a *structural* simplification here, look at §4 (does moving `TypeLevel` into core identity and
dropping the `TypeStack` carriage net-delete? — that is a different measurement) or leave `walkTypeStack` alone: it is
already lean.

- **The stop-rule that fired:** if the step does not remove more than it adds from the check ladder, the approach is
  wrong — reconsider before committing.

**Accidental complexity to re-examine as the walk dissolves** — remove it only if the dissolution genuinely makes it
unnecessary, never by special-casing: the `= Type` kind-unify carve-outs live in the same code as the effectful-return
special-casing (`sawGuardReturn` / kind-position guard-carrier acceptance and the like). If the ordinary level check
subsumes them, they go with the walker; if it does not, **leave them alone** — do not build new machinery to preserve or
extend them. The measure of success is *fewer* lines and *fewer* special cases in `monomorphize`, full stop.

## 4. The actual target: levels are named values born early, so every phase processes them uniformly

**This is where the win is** (reframed 2026-07-14 — see "The point"). The `TypeStack` structure is pervasive (~35
files carry it through core → resolve → matchdesugar → operator → effect → ability → saturate), and every one of those
phases has to *individually* thread and traverse it — inconsistently (the `a18c1e2f` match-in-signature bug is the proof
that a phase can silently skip it). The goal: **extract each type-stack level as its own named value at (or near) the
`core` phase**, so a value's signature becomes a *reference* to a level-value, and drop the `TypeStack` carriage. Then:

- Each front-end phase processes a level value with the **same code path it already uses for any value** — no
  type-stack-specific traversal, no `convertTypeStack`/`desugarInTypeStack`/`traverseStack` special methods. That is the
  net-delete: across ~35 files, not in `monomorphize`.
- A type-level expression **automatically** gets match desugaring, operator resolution, effect checking, ability
  resolution — gaps like `a18c1e2f` become structurally impossible, not one-off patches.
- Step-A's `TypeLevelSaturatedValueProcessor` (which derives level-n at the *saturate* boundary — too late for the
  front-end phases) is superseded: the derivation moves into core, *before* matchdesugar/operator/effect/ability, which
  is exactly what lets those phases see the level.

### B0 — Audit results (DONE 2026-07-14): the net-delete is *de-stacking nested positions*, not *levels born in core*

**Two structural facts dominate the whole measurement** (confirmed by reading every `TypeStack` site — the top-level
value field *and* the six positions the structure is woven into inside the expression AST):

1. **The top-level value `typeStack` is at most 2 levels** — `levels(0)` = the signature, `levels(1)` = the kind — and
   the 2-level form is built in exactly one place (`CoreProcessor.transformFunction:101-104`) and *only* for values with
   generic parameters (non-generic values get a 1-level stack). There is no arbitrary tower; the implicit top is `Type`.
2. **Every *other* `TypeStack` is single-level.** `TypeStack` is nested into six AST positions —
   `FunctionLiteral.parameterType`, `FunctionLiteral.body`, `FlatExpression.parts`, `MatchExpression.scrutinee`,
   `MatchCase.body`, `BlockLine.binderType` — each *always* built with `TypeStack.of` (one level) and *only ever* read
   via `.signature` (level 0). The only multi-level reads in the whole codebase (`.levels.tail`, `levels.drop`) are on
   the **top-level** value stack (`walkTypeStack`, `flattenReturnToType`, `MarkerGuardSignature`, the two saturate
   processors) — never on a nested one.

**So the refactor decomposes into two independent changes with very different economics:**

- **Change A — de-stack the six nested positions** (`Sourced[TypeStack[E]]` → `Sourced[E]`). This is the **bulk** of the
  ~150 refs and a clean, uniform, *provably lossless* net-delete: the nested stacks are single-level and
  `.signature`-only, so the plain-expression traversal each phase already has subsumes them. Deletes
  `traverseStack`/`convertTypeStack`/`wrapExpr`, the `.levels.map` arms of `substitute`/`containsVar`/
  `foldValueReferences`/`mapChildrenM`, the nested call-sites of `resolveTypeStack`/`desugarInTypeStack`, the effect/
  ability nested `.signature` reads (→ identity), and — the largest single win — nearly all of the match-desugarer's
  plumbing (`MatchDesugarContext`, `DataMatchDesugarer`, `TypeMatchDesugarer`, `MatchDesugarUtils` thread
  `Sourced[TypeStack[Expression]]` for scrutinee/body throughout; ≈50 of matchdesugar's 71 refs are this). Requires
  **no new machinery** — no minting, no `typeLevel` identity, no cross-level scope threading, no cross-track fact edge.
  Estimated **~80–120 lines** deleted.

- **Change B — the top-level level(s) born as named values in `core`** (the plan's original B1/B2/B3). Its economics are
  **break-even-to-negative on lines**: it deletes the per-phase *own-value* stack traversal — measured (A) counts:
  core 0, block ~6, resolve ~14, operator ~4, matchdesugar ~5–12 — but that is *offset* by the core mint (+15–30), the
  **resolve cross-level scope-threading blocker** (levels are resolved top→bottom in one `withLocalScope` so upper
  binders stay visible below; a level-value with its own scope must reconstruct that), and migrating the `typeLevel`
  dimension into core identity (~25 lines relocated, threaded through the ~35-file fact chain). Its one large distinctive
  delete is **superseding `TypeLevelSaturatedValueProcessor`** (−85 in saturate) — but that removes Step-A scaffolding
  *this plan added*. `walkTypeStack` stays either way (degenerates to one iteration; §3-dead, not the target).

**The headline motivation for Change B is already banked.** §4's Goal 1 ("a type-level expression must go through the
whole front-end pipeline automatically — today it does NOT") is *no longer true*: the per-phase
`.levels.traverse(ordinaryTransform)` already routes every level through match desugaring
(`desugarInTypeStack`→`desugarExpression`), operator resolution (`resolveInTypeStack`→`resolveInExpression`, why
`Box[I+1]` works), and name resolution; abilities/effects in type positions resolve in the checker (`walkTypeStack`'s
`levelExprs`). The last gap — matchdesugar skipping the *own-value* signature — was closed by `a18c1e2f`. Change B would
make uniformity a *structural* guarantee (a phase cannot forget to traverse when there is nothing to traverse) rather
than a per-phase convention — real, but qualitative, not a net-delete.

**Gate verdict.** Change A **passes** the B0 gate (~80–120 clean delete, no offset, low risk). Change B **fails** it as a
line exercise (net break-even once mint + scope-threading + `typeLevel` plumbing are counted; its −85 is self-scaffolding
removal). This matches Robert's reframed goal exactly — *"the net-delete lives across the ~35 files carrying TypeStack"*
— and identifies the mechanism: it is **Change A**, doable directly, not **Change B**.

**What a level-value must expose** (union across all consumers — needed only if Change B is pursued): body expression
(the level-n type expr, host binders stripped); the host generic binders it closes over (its own leading generic prefix
over `Type`); its kind (the level above; `Type` at the apex); `paramConstraints`; `inferableArity`; `roleHint`;
`platform`; source position/name; and the `typeLevel` identity dimension. `SignatureView` (in
`OperatorResolvedExpression`) already models the binder-strip + synthetic-`… → Type` signature at the *saturate*
boundary; Change B reproduces it in `core`.

### Revised next steps

- **A (recommended — the real net-delete): de-stack the six nested positions.** Turn `Sourced[TypeStack[E]]` →
  `Sourced[E]` at `FunctionLiteral.parameterType`/`body`, `FlatExpression.parts`, `MatchExpression.scrutinee`,
  `MatchCase.body`, `BlockLine.binderType` across the core/resolve/matchdesugar/operator facts; delete
  `traverseStack`/`convertTypeStack`/`wrapExpr`/the nested `xInTypeStack` calls and the match-desugarer's stack
  plumbing; collapse the effect/ability `.signature` reads to identity. **Keep the top-level 2-level value stack as-is**,
  so `walkTypeStack` is untouched. This IS §4's "drop the TypeStack structure" (old B3), but the audit shows it needs
  **none** of B1/B2's levels-born-in-core prerequisite. Spike **matchdesugar** first (heaviest, ~50 nested refs); gate on
  net-negative line count, `examples/TypeLevelMatch.els` + full suite green.

- **B (optional, deferred): levels born in core.** Pursue only if the *structural* guarantee against future
  `a18c1e2f`-style gaps is judged worth the machinery. After Change A the top-level ≤2-level stack is the *only*
  remaining traversal, so turning its `levels(1)` (kind) into a named value is small and clearly scoped, and it lets
  `TypeLevelSaturatedValueProcessor` be deleted. Line-neutral at best — do it for the invariant, not the count.

**Immediate next action:** decide A vs B (see verdict). Recommended: execute **Change A**, starting with a matchdesugar
spike.

## 5. Guardrails (the stop rules)

- **Net-delete — but across the front-end, not just `monomorphize`.** The win is removing per-phase `TypeStack`
  traversal (§4), not shrinking the checker walk (§3, which does not net-delete). A change that moves a level into an
  ordinary value should let a phase *delete* its type-stack-specific code, not add more.
- **Uniformity is the goal; still no level-specific behaviour in the kernel.** A level value must go through the
  *existing* phase machinery, never a new level-aware branch inside `Checker` / `renormalize` / a phase. Route through
  the platform as an ordinary value, or don't. (This is how "capability" — match/ability/effect in types — is
  obtained: by reuse, not by teaching each phase about levels.)
- **No kernel reduction re-implementation.** No sub-value binding composition, no read-back renormalize tricks, no
  carrier synthesis on level signatures. The named-value pipeline reduces; the kernel stays the kernel.
- **Higher levels are not special.** A level `n ≥ 1` value is checked, reduced, and read exactly like a level-0 value.
  If a task starts treating it specially, that task has drifted from the goal.

## Appendix — reference branches

`wip/return-position-unification-stage2` (`77c2ed43`), `wip/if-else-guard-idiom`, `failed/if-else-guard` are
reference-only historical attempts at the (now-abandoned) guard-reduction direction; do not mine them for machinery.
