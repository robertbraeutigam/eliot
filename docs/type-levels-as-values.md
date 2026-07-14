# Type levels as named values — uniform front-end processing (levels born in `core`)

**Status:** IN PROGRESS. **Step A landed** (`614da60a`); the **genArrow divergence fix** (`a4e64b62`) and the
**match-in-signature fix** (`a18c1e2f`) landed 2026-07-14. The goal was **reframed** (Robert, 2026-07-14, `9f586f07`):
this is not a `monomorphize` line-count exercise (that framing, §3, is measured dead) — it is about *uniformity*,
realized by making higher type levels **ordinary named values born in the `core` phase**.

**COMPLETE (2026-07-14).** The plan collapsed to **one change: remove `TypeStack` entirely** (B0 audit), executed in two
increments: nested positions (`38305b75`) then the top-level field + kind-derivation (`8da08c1c`). The kind is a *derived
projection of the signature's binders* (`SignatureView.of(sig).binders`), so it is neither stored nor minted — the
signature and every nested annotation are now plain expressions. `TypeStack.scala`, `TypeLevelSaturatedValueProcessor`,
and the `typeLevel` dimension are deleted; ~−141 net lines; all 1249 tests green + 8 examples run; `CACHE_VERSION` 24.
Uniformity is now structural (no stack to forget to traverse). The earlier "Change A vs Change B" framing was wrong
(minting the kind as a level value was over-engineering). See §4 for the full record.

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

A value's type is a value one level up. The finite tower:

| Level | Content        | Representation (as shipped)                                  | Track    |
|-------|----------------|-------------------------------------------------------------|----------|
| 0     | runtime body   | `NamedValue.runtime`                                        | runtime  |
| 1     | the signature  | `NamedValue.signature` (a plain expression)                | compiler |
| 2     | the kind       | *derived* from the signature's generic binders, never stored | compiler |
| top   | literal `Type` | implicit                                                    | —        |

The signature is checked against its kind — the same recursive "check a value's body against its signature", up a finite
tower that bottoms at `Type`. Because the kind of a type constructor `[A: K1, B: K2] -> …` is exactly
`Function[K1, Function[K2, Type]]`, it is a *projection* of the signature's binders and need not be stored; `walkTypeStack`
derives it on demand (§4). **This is what shipped** — the earlier idea of representing each level as a distinct named
value with its own `TypeLevel` identity (§2/§3) was measured to over-complicate and was dropped.

**The payoff — front-end uniformity.** With no `TypeStack` structure, every front-end phase (matchdesugar, operator,
effect, ability) processes a signature (and every nested annotation) with the plain-value path it already has — a
type-level `match`/operator/ability gets resolution *for free*, and a phase can no longer silently skip it (the
`a18c1e2f` bug class is structurally impossible). See §4 for the delivered change.

## 2. Historical — Step A (`614da60a`), later deleted

Step A added a `typeLevel` key dimension to `SaturatedValue`/`CompilerMonomorphicValue`, a
`TypeLevelSaturatedValueProcessor` that derived a value's level-*n* type expression as a synthetic named value, and a
`TypeLevelEquivalenceTest`. This was scaffolding for the abandoned "levels born as named values" direction. **All of it
was deleted in increment 2 (`8da08c1c`)** — the B0 audit found the kind is a *derived projection* of the signature's
binders, so it is never minted as a level value (see §4). One genuine fix from this thread survives: the `a4e64b62`
genArrow divergence, which made the signature kind-check resolve type-parameter references consistently (it is about
checking the signature, which stays).

## 3. Historical — the `walkTypeStack`→level-1-demand measurement (dead)

An intermediate idea was to make a value's signature come from a *demand* for its own level-1 `CompilerMonomorphicValue`,
deleting the in-place `walkTypeStack`. Measured 2026-07-14 and found to net-ADD (a cross-track fact edge that breaks the
tracks' acyclicity, the guard machinery doesn't collapse into the sub-mono, and calc-returns force keeping the
real-signature path). **Superseded by §4**: increment 2 removed the stored stack a *different* way — by dropping the
stored kind and deriving it from the signature's binders inside `walkTypeStack` itself, with no cross-track demand.

## 4. The actual target: remove `TypeStack` entirely, so every phase processes one expression

**This is where the win is** (reframed 2026-07-14; corrected by the B0 audit below). The `TypeStack` structure is
pervasive (~35 files carry it through core → resolve → matchdesugar → operator → effect → ability → saturate), and every
one of those phases has to *individually* thread and traverse it — inconsistently (the `a18c1e2f` match-in-signature bug
is the proof that a phase can silently skip it). The goal: **drop the `TypeStack` structure entirely** — a value's
signature and every nested type annotation become plain expressions, and the kind (the only genuinely higher level) is
*derived from the signature's binders on demand*, never stored or minted. Then:

- Each front-end phase processes the signature (and every nested annotation) with the **same code path it already uses
  for any value expression** — no type-stack-specific traversal, no `convertTypeStack`/`desugarInTypeStack`/
  `traverseStack` special methods. That is the net-delete: across ~35 files, not in `monomorphize`.
- A type-level expression **automatically** gets match desugaring, operator resolution, effect checking, ability
  resolution — gaps like `a18c1e2f` become structurally impossible (there is nothing to forget to traverse).
- Step A's `typeLevel` dimension + `TypeLevelSaturatedValueProcessor` are **deleted, not superseded**: the audit found
  the kind is a projection of the signature (see B0 below), so no level-*n* value is ever minted, and the machinery that
  derived one at the *saturate* boundary is unnecessary.

### B0 — Audit results (DONE 2026-07-14): one change — *remove `TypeStack` entirely*; the kind is a derived projection, not a minted value

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

**The key insight (Robert, 2026-07-14): the kind (level 1) is a *derived projection of the signature*, not separate
content.** `buildKindExpression` folds `function.genericParameters` into `Function(K1, …, Type)`; the signature
(`curriedFunctionType`) bakes those *same* generic params in as binders. So `SignatureView.of(signature).binders` already
reconstructs the kind — `[Binder(A, K1), Binder(B, K2)]` folded into `Function(K1, Function(K2, Type))` **is** level 1.
The kind therefore does not need to be *stored* (as a stack level) or *minted* (as a named value): it can be **derived on
demand** from the signature's binders in the one consumer that needs it (the monomorphize kind-check, `walkTypeStack`).

**So there is one change, not two: remove `TypeStack` everywhere.** An earlier framing of this doc split it into "de-stack
the nested positions" versus "levels born in core (named values with a `typeLevel` identity)" — but that split was
wrong. The top-level stack removes the *same way* the nested ones do: keep the signature as a plain expression, derive
the kind. Minting the kind as a level-*n* named value (Step A's direction) was an over-engineering of "the type of a
value is a value one level up" — representationally the kind is not a distinct value, it is a projection of the
signature.

- **What it deletes.** `Sourced[TypeStack[E]]` → `Sourced[E]` at all six nested positions *and* the top-level value
  field (`NamedValue.typeStack` / the four downstream fact `typeStack` fields become the plain signature expression).
  Removes `traverseStack`/`convertTypeStack`/`wrapExpr`/`resolveTypeStack`/`desugarInTypeStack`/`resolveInTypeStack`, the
  `.levels.map` arms of `substitute`/`containsVar`/`foldValueReferences`/`mapChildrenM`, the match-desugarer's whole
  `Sourced[TypeStack[Expression]]` plumbing (`MatchDesugarContext`/`DataMatchDesugarer`/`TypeMatchDesugarer`/
  `MatchDesugarUtils` — ≈50 of matchdesugar's 71 refs), the `a18c1e2f` own-signature special path (the signature is now
  just an expression, processed like the body), `TypeStack.scala` itself, the saturate 2-level rebuilds, **and** Step A's
  `typeLevel` dimension + `TypeLevelSaturatedValueProcessor` (−85) — *deleted, not migrated*, since no level-*n* value is
  minted. Estimated well over **~150 lines** net.
- **The structural guarantee is free.** Once a phase gets one expression instead of a stack, it runs its ordinary path;
  there is nothing to forget to traverse, so `a18c1e2f`-class gaps become structurally impossible. (Uniformity was
  *already* achieved functionally by the per-phase `.levels.traverse(ordinaryTransform)` + `a18c1e2f`; removing the stack
  makes it structural *and* deletes the traversal — both wins, no trade-off.)
- **The two "costs" the old framing attributed to the top-level change evaporate.** There is no core mint. The **resolve
  cross-level scope-threading blocker disappears** — with no separate kind level to resolve, `resolveTypeStack` collapses
  to resolving one expression, no shared cross-level scope. No `typeLevel` identity to thread.

**The one spot with genuine (non-mechanical) work.** `walkTypeStack` (`TypeStackLoop`) today folds the stored
`[kind, signature]` stack top-down, kind-checking the signature against the evaluated kind and emitting one
kind-check `SemExpression` per level (`levelExprs`, consumed by the drain loop for ability refs embedded in type
positions). After the stack is gone it must **derive the kind from the signature's binders** (`SignatureView.of(sig)`)
instead of reading `levels(1)`, and still emit those `levelExprs`. Everything else across the ~35 files is mechanical
`TypeStack[E] → E`. This is the piece the spike must validate; the genArrow signature-check fix (`a4e64b62`) is about
checking the *signature*, which survives unchanged.

### Next steps

- **DONE — Increment 1: de-stack the six nested positions** (`38305b75`). All six nested `TypeStack` positions →
  plain `Sourced[Expression]` across the four chained expression types (core → resolve → matchdesugared → operator).
  matchdesugar's whole `Sourced[TypeStack]` plumbing (`MatchDesugarContext`/`DataMatchDesugarer`/`TypeMatchDesugarer`/
  `MatchDesugarUtils`) vanished; `TypeStack` refs across the five front-end phases went **151 → 42** (core 21→7,
  resolve 24→9, matchdesugar 71→15, operator 22→9, block 13→2). All 1253 tests green; 7 examples build + run. The
  transient boundary adapters are all gone; the top-level value `typeStack` field stays a stack (so `walkTypeStack` is
  untouched). Net −30 lines (understated — the big deletes are in increment 2).
- **DONE — Increment 2: drop the top-level stack + derive the kind** (`8da08c1c`). The five value facts now carry a
  plain `signature: Sourced[Expression]` instead of `typeStack: TypeStack[…]`; `walkTypeStack` **derives the kind** from
  `SignatureView.of(sig).binders` (a binderless signature checks directly against `Type`). Deleted `TypeStack.scala`,
  `TypeLevelSaturatedValueProcessor` + the `typeLevel` dimension (Step-A scaffolding — the kind is derived, never minted
  as a level value), `buildKindExpression`, the top-level `xInTypeStack` helpers, and the saturate 2-level rebuild.
  `flattenReturnToType`/`MarkerGuardSignature.stripSignature` simplified to signature transforms (both preserve binders,
  so the derived kind is unchanged). Net −111 lines + 3 files deleted; **`TypeStack` is fully gone** (only the
  `TypeStackLoop` class *name* remains). `CACHE_VERSION` 23 → 24. All 1249 tests green; 8 examples run.

**Status: COMPLETE.** The `TypeStack` structure is removed end-to-end (increment 1 + 2 = ~−141 net lines). Every
front-end phase now processes plain expressions with its ordinary value path, and the kind is a derived projection of the
signature — never stored or minted. Uniformity is now structural: a `match`/operator/ability in a type position can no
longer be silently skipped, because there is no stack to forget to traverse. Optional cosmetic follow-up: rename
`TypeStackLoop` (the class name is the last vestige of the removed structure).

Historical: the original spike framing below is superseded by the two increments above.

- **Spike matchdesugar (the mechanical proof + go/no-go).** De-stack matchdesugar's nested positions and its own-signature
  handling: `MatchDesugaredExpression.FunctionLiteral.parameterType`/`body` and `FlatExpression.parts` → plain
  `Sourced[MatchDesugaredExpression]`; `Expression` (resolve) mirror positions likewise as far as matchdesugar reads
  them; delete `desugarInTypeStack`/`convertTypeStack`/`traverseStack`/`wrapExpr` and the `MatchDesugarContext`/
  `DataMatchDesugarer`/`TypeMatchDesugarer` `TypeStack` threading; the value's own signature is desugared as an ordinary
  expression. **Acceptance:** matchdesugar's `TypeStack` code is gone, its net line count is negative,
  `examples/TypeLevelMatch.els` + full suite green. **Gate:** if the infrastructure needed is larger than what deletes,
  stop and report.
- **Roll out to the remaining phases + the top-level field.** Extend `TypeStack[E] → E` across core/resolve/operator/
  block facts + processors and the four fact `typeStack` fields; derive the kind on demand in `walkTypeStack`; delete
  `TypeStack.scala`, `TypeLevelSaturatedValueProcessor`, and the `typeLevel` dimension. Full suite + all examples green;
  `CACHE_VERSION` bumped.

**Immediate next action:** the matchdesugar spike.

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
