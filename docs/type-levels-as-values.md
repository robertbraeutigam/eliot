# Type levels as named values — uniform front-end processing (levels born in `core`)

**Status:** IN PROGRESS. **Step A landed** (`614da60a`); the **genArrow divergence fix** (`a4e64b62`) and the
**match-in-signature fix** (`a18c1e2f`) landed 2026-07-14. The goal was **reframed** (Robert, 2026-07-14, `9f586f07`):
this is not a `monomorphize` line-count exercise (that framing, §3, is measured dead) — it is about *uniformity*,
realized by making higher type levels **ordinary named values born in the `core` phase**.

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

### Next steps (sequenced; each gates the next)

The full extraction is large (~35 files) and unvalidated, so it is staged so the cheap, high-information work comes
first and any stage can stop the effort if the net-delete isn't there.

- **B0 — Audit & measure (do this first; low-risk, no product code).** For each front-end phase (`core`, `resolve`,
  `matchdesugar`, `operator`, `effect`, `ability`) classify its `TypeStack` handling into: **(a) level-traversal that
  would vanish** — code that walks `.levels` and applies the phase's ordinary value-expression transform to each level
  (e.g. matchdesugar's `desugarInTypeStack`, operator's per-level resolution), versus **(b) structural carriage that
  stays** — constructing/passing the stack, and the binder/kind-chain *shape* a phase needs regardless. Output: a
  per-phase deletable-line estimate **plus** the list of what a level-value must expose for a phase to treat it as an
  ordinary value (its body expression, the host binders it closes over, its kind).
  **Gate:** if the deletable total is small (≲ ~100 lines), the net-delete isn't there — stop and reconsider, exactly
  as §3's stop-rule fired.

- **B1 — Design the core extraction (design only, no code).** Decide and write down:
  - **Identity/keying** — carry Step A's `typeLevel` dimension into core `NamedValue` identity, so `(v, level)` is a
    distinct named value and a signature slot is a *reference* to `(v, level+1)`; no synthetic FQN.
  - **Binder closure** — the signature (`Function[X, X]`) references the host's own generic binders, so the level value
    is parametric in them. Step A's `SignatureView` binder-strip + synthetic-`… → Type` sig already models this at the
    *saturate* boundary; the task is to reproduce it in `core`, where the binders are still syntactic.
  - **Acyclicity** — the level body references only the host's binders and other top-level values, never the host's
    runtime body, so host → its level value is acyclic; confirm no fact cycle is introduced (the invariant §3 nearly
    broke).
  - **Fetch point** — each phase, instead of walking `host.typeStack`, produces/reads the level value's fact and
    processes its body with its ordinary value path.

- **B2 — Spike matchdesugar end-to-end (the go/no-go).** Implement the minimal core extraction for the *signature*
  level (level 1) only — enough that matchdesugar consumes the level value as an ordinary value body — and **delete**
  matchdesugar's own-signature handling (the `a18c1e2f` `desugarInTypeStack`-of-own-signature call and the
  `convertTypeStack` of the own signature).
  **Acceptance:** matchdesugar's own-signature type-stack code is gone; `examples/TypeLevelMatch.els` + the full suite
  stay green; matchdesugar's net line count is negative.
  **Gate:** if the extraction infrastructure needed to let matchdesugar drop its code is *larger* than what it deletes,
  §4 does not net-delete either — report and stop, don't force it.

- **B3 — Roll out & remove the carriage.** Extend the extraction to the kind levels and the remaining phases
  (`operator`, `effect`, `ability`, `resolve`); supersede `TypeLevelSaturatedValueProcessor` (its saturate-boundary
  derivation now lives in `core`); drop/minimize the `TypeStack` structure across the ~35 files — keeping only the
  binder/kind *shape* where a phase genuinely still needs it.
  **Acceptance:** overall net-delete across the front-end; full suite + all examples green; `CACHE_VERSION` bumped.

**Immediate next action: B0** — it is a read-only audit that either produces the net-delete case that justifies B1–B3
or kills the approach cheaply.

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
