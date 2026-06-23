# Recursion & Termination — Total Core, `Inf` as a Platform Effect

Status: **Design note + implementation plan.** Captures the model, its theoretical grounding, its
preconditions, and a sequenced milestone plan grounded in a codebase investigation.

**Supersedes the earlier `Rec[N]` measure design.** An earlier version of this note proved termination
with a per-function *measure* `Rec[N]` that had to strictly decrease across recursive calls, discharged by a
symbolic descent check (`N-1 < N`) in the refinement solver. That is all **gone**. There is no measure, no
descent check, no symbolic `<`, no `RefinementSolver` involvement, and no language-mandated `fold` primitive.
The model below is strictly simpler and rests on a single idea: **the language cannot express recursion at
all**, so termination is the default and the only thing to track is the opt-out.

## The model in one paragraph

**Eliot user code cannot express recursion or loops — full stop.** Every loop in a running program lives
inside a **platform-provided native function** (whether the platform implements one master fold or many
specialized loops is the platform's business; the language does not care and does not mandate any particular
primitive). With recursion unavailable in the pure fragment and the preconditions enforced, **every program
terminates by default.** The sole opt-out is **`Inf`**, an effect meaning "may not terminate." Because a
recursion-free typed core cannot itself introduce divergence, an `Inf` effect can **only originate on a
platform native**; from there it propagates virally to every caller. Bounded execution (`withFuel`-style)
discharges `Inf` back to a total `Option`. That is the entire design.

## Why this is sound — and where the literature puts it

The soundness is not hand-waved; it is forced by a standard normalization result, and the design lands on a
well-mapped point.

**The forcing argument.** A typed lambda calculus with no recursion is **strongly normalizing** (STLC:
Tait; System F: Girard's reducibility). The preconditions below eliminate every *operational* source of
recursion, so the pure user fragment is SN. Natives are **δ-constants** — opaque values with
externally-supplied reduction rules. Adding constants whose δ-rules terminate **preserves** SN; this is
exactly **Gödel's System T** (STLC + a primitive recursor constant is still SN). Therefore, if the pure
fragment cannot diverge, *any* divergence in a program must live in a constant's δ-rule. **Divergence can
only come from a native — that is a theorem, not a rule we impose.** The diverging constant has a name too:
it is **PCF**'s fixpoint `Y` (STLC + `Y` is Turing-complete and non-total). So the design is precisely
*System T by default (a total core plus terminating constants), with opt-in PCF constants, where the opt-in
is a type-level effect.*

**Literature alignment.**

- **Turner, *Total Functional Programming* (2004)** — total by default, general recursion unavailable. We go
  *further* than Turner: he keeps structural-recursion operators in the language; we push every loop into
  platform constants.
- **System T / PCF** — the terminating-recursor-constant vs diverging-`Y`-constant dichotomy above.
- **Capretta, the partiality / delay monad (2005)** — the principled type for divergence. `{Inf} A` is the
  delay monad `D A`; the bounded-execution discharge (`withFuel : {Inf} A -> Nat -> Option[A]`) is its
  standard "run for *n* steps" eliminator.
- **Koka's `div` effect (Leijen)** — the closest existing system: Koka's effect row tracks `div`
  (may-diverge), and a `total` function is one whose row lacks it. That is exactly `Inf`. The difference is
  strength: Koka *infers* `div` for user-written recursive functions; Eliot's core cannot write recursion at
  all, so `Inf` is **never inferred from a cycle — only ever declared on a native and propagated.** Our
  guarantee on the pure part is strictly stronger.

**The caveat — Eliot is λ\*, not System F.** The SN theorems above are for STLC/System F. Eliot is **λ\* /
`Type : Type`** (see the Cornerstone), and `Type : Type` is *operationally* inconsistent: **Girard's paradox
(Hurkens' construction) yields a closed, non-normalizing term in the pure fragment — with no explicit
recursion, no recursive data, and no mutable cell.** This is a *fourth* source of non-termination, internal
to the type system, that the preconditions do **not** block (only a universe hierarchy would, and the
Cornerstone deliberately rejects one). Therefore the honest claim is:

> The preconditions block all three of Turner's **operational** recursion sources, making the pure fragment
> total *for every program a person would actually write*. They do **not** close the **type-theoretic**
> source (Girard). "Eliot cannot recurse, full stop" holds **modulo the already-accepted `Type:Type`
> trade-off.** Divergence is confined to `Inf` natives, except for the residual `Type:Type`
> non-normalization, which this model does not close.

This is not a regression introduced here — it is the same trade-off the Cornerstone already takes
("`Type : Type` is logically inconsistent … totality handled separately") and the reason the checker's
compile-time `Coerce` evaluation "Girard's paradox allows to diverge." In practice it is irrelevant: nobody
encodes Hurkens' paradox by accident, and every *practical* loop must still come from an `Inf` native. The
doc states it rather than overclaiming strong normalization, which is simply false for λ\*.

## What the compiler does — and does not

**Does:**

1. **Forbids recursion in user code** — rejects any self/mutual reference cycle in the resolved value graph.
   This is the one behavioural rule, and (given the preconditions) it is *complete* — see below.
2. **Enforces the preconditions** (occurs-check, strict positivity, purity) that make "no recursion" mean
   what it says.
3. **Tracks `Inf`** as an effect: read from a native's declaration, propagated by union to callers.

**Does not:** no measures, no `Rec[N]`, no descent check, no symbolic bound comparison, no `RefinementSolver`
participation, and **no language-mandated recursion primitive.** The platform supplies whatever loops it
needs; the language only refuses to write any itself.

## Preconditions — "in a pure language, recursion cannot hide"

Recursion is something a normalizing calculus *adds back*, and there are exactly **three operational
sources** (plus the λ\* caveat above). Two are blocked by the type system; the third by purity.

1. **Explicit recursive bindings** (`fix` / `letrec` / a name referring to its own binding, incl. mutual).
   *Visible by construction* → caught by the name/fact-cycle detector. This is the source the no-recursion
   rule directly polices.

2. **Recursive types in negative position** (the Y-combinator route). `Y` needs `x x`, i.e. a type
   `T = T -> A`. Two guards, blocking different doors:
   - **Occurs-check in unification** — rejects an *inferred* infinite type (a bare `x x`). Standard HM;
     Eliot's `unify` must enforce it (it currently does not — see findings).
   - **Strict positivity on `data`** — rejects a *nominally* introduced negative recursive type, e.g.
     `data Loop(f: Function[Loop, A])` (the defined type appears left of an arrow). The occurs-check does not
     catch this. **Note its role here:** positivity is *not* needed to make any fold "total" (folds are
     platform natives, trusted to terminate); it is needed purely to stop the user *manufacturing*
     recursion via a negative-recursive datatype that would bypass natives entirely. Still the concrete new
     `data`-checker obligation. (Coq/Agda enforce exactly this.)

3. **The store (mutable cells)** — Landin's knot: allocate a cell, store a function that reads and calls the
   cell, backpatch. Recursion through the heap, no syntactic cycle, no recursive type. **Blocked by purity**
   — see *No mutable cells*.

4. **(`Type : Type` / Girard)** — *not blocked.* The accepted trade-off above. Listed so the boundary of
   the guarantee is explicit.

> With (1) policed, (2) blocked by occurs-check + strict positivity, and (3) blocked by purity, **every
> *operational* instance of recursion is witnessed by an explicit syntactic recursive binding** — which the
> compiler rejects in user code. That is what makes "deny recursion" mechanically decidable.

### Detection is complete — data cannot hide recursion

The cycle detector operates on the definition's **lexical reference graph** (resolved free variables), not on
runtime data flow. A data structure is only a *courier* for a function value; it cannot launder a lexical
reference. The classic worry — "`f` builds a lambda, stores it in a structure, and that lambda calls `f`" —
is caught at `f`'s definition: the lambda's free variables are computed lexically and include `f`, so `f`
transitively references itself regardless of where the data flows. The only way to route recursion *purely*
through data (a stored lambda that reaches itself without naming `f`) requires either a mutually-recursive
`let` (a syntactic cycle, policed) or mutating already-built data to point at the new lambda (Landin's knot,
forbidden by purity). Immutable data is built bottom-up from values that already exist, so it cannot
manufacture a cycle that was not written down. **Conclusion: with the preconditions, recursion is always
detected.**

Theory references: Tait / Girard (strong normalization); Gödel (System T); Plotkin (PCF); Pierce *TAPL*
(recursive types derive `fix`); Coquand–Paulin (strict positivity); Landin's knot (references break
normalization); Hurkens (`Type:Type` paradox); **Turner, *Total Functional Programming* (2004)**; Capretta
(partiality/delay monad); Leijen (Koka effect system, the `div` effect).

## No mutable cells

Decision: **Eliot ships no general user-facing mutable cell.** An unrestricted cell knocks over all three
guarantees Eliot exists to make (termination via the knot, resource bounds via aliasing, race-freedom under
interrupts). The legitimate uses are covered otherwise:

- **logical mutable variable** → the **State effect** (built — [[project_state_revived]]);
- **in-place update for performance** → a future **linearity / uniqueness analysis** (mutate when provably
  unaliased — Clean / Rust / Linear Haskell / Idris 2), as a compiler optimization under a pure surface;
- **hardware registers / pins / timers** → **platform effects at the edge** (registers hold bytes, not
  closures, so they never tie a call-knot);
- **shared / interrupt state** → a *restricted* capability, not a free cell;
- **cyclic data (graphs)** → **index-based representations** (arrays + indices), which bounded-memory targets
  favour anyway and which do not tie call-knots.

**Graceful fallback if mutation is ever added:** the latent-effect machinery prices cells correctly. A cell
holding **plain data** contributes no effect (terminating); a cell holding a **function** defaults to `Inf`
(you cannot optimistically infer termination for mutable contents). "No callables in a `Ref`" falls out as
"function-holding cells are `Inf`" — no special rule.

**Honest cost:** until linearity exists, "no cells" means some algorithms **copy**, which hurts more on a
2 KB-SRAM part. Mitigate with State + monomorphization now, linearity later.

## `Inf` as an effect: declaration, propagation, discharge

There is **no `Terminating` effect.** Termination is the *default* — simply the absence of `Inf` from a
computation's effect row, exactly as "does no console IO" is the absence of `Console`, not a `NoConsole`
token. `Inf` is an ordinary member of the existing `{…}` row (`{Inf} A`); the row is a *set* of effects and
`Inf` is one possible element. So there is nothing to write on a terminating function, no two-point lattice,
and no invisible bottom value to carry — `Inf` is just another capability effect, and termination is its
absence.

**Declaration — terminating by default; `Inf` is the opt-in.** A body-less native is terminating unless it
declares `{Inf}`. The platform author writes `Inf` precisely on the natives whose implementation contains an
unbounded loop (the event-loop driver, `forever`, an unbounded `iterate`) and nothing on the straight-line
rest (`add`, `println`, `intToString`). The trusted-divergence surface stays greppable — `grep Inf` over the
platform layer lists every native that delegates totality.

> Trade-off (a deliberate reversal of an earlier "every native must declare, no default" sketch): defaulting
> to terminating means a platform author who writes a loop but forgets `Inf` would *silently* over-claim
> termination — a fail-open gap, in tension with [[feedback_gaps_must_be_failsafe]]. It is accepted because
> (a) it is the same irreducible platform trust the language already places in every native's *correctness*
> (a buggy `add` is no different), (b) the only natives that can loop are ones whose author visibly wrote a
> loop, and (c) "write `Inf` only when you loop" is worth far more than forcing a meaningless `Terminating`
> onto every leaf. The compiler cannot inspect a native's body (it is opaque platform code), so requiring a
> declaration would not actually *close* the gap — it only relocates the same trust to an annotation.

**Propagation — set union, inferred, implicit.** A function's effect row is the union of its callees' rows
(plus its own). `Inf` present in *any* callee's row ⟹ present in yours; absent everywhere ⟹ the function is
terminating. This is ordinary effect-row union — the *same* mechanism as capability effects, not a special
termination lattice. The user writes no effect variables; propagation reuses the existing effect pipeline
(`DirectStyleDesugarer` / `CalleeSignatures` / `DeclaredEffectChecker`).

**Higher-order propagation rides the carrier, like every effect.** `Inf` is reflected in a function value's
type (its effect row / carrier), so it cannot be laundered through a lambda — and higher-order functions are
**effect-polymorphic** over their argument arrows exactly as they are for `{E}`:

```
map : (Function[A, B] ! e) -> List[A] -> List[B] ! e
```

A platform `fold`/`map` native carries no `Inf` *in its own loop*, but its result row is the union with its
**step's** row: fold an `Inf` step ⟹ the fold is `Inf`; fold a terminating step ⟹ the fold is terminating.
This is the **function-coloring win** (Nystrom, *What Color is Your Function?*): one combinator serves pure
and effectful arguments alike, instead of the Haskell/cats-effect `map`/`mapM`, `fold`/`foldM` duplication —
and here it is the *same* carrier-polymorphism the rest of the effects use, not a parallel mechanism. For a
language where iteration is a *platform* feature, a single effect-transparent `fold` is load-bearing.

**Discharge — execute it.** `Inf` is a genuine effect with discharge functions that *run* the computation —
that is what makes it a real effect and not a mere marker:

```
withFuel : {Inf} A -> Nat -> Option[A]      -- run for up to N steps; None if it did not finish
```

`withFuel` is itself a terminating (`Inf`-free) native: it drives the computation under a step budget and
returns a total `Option`, "birthing" an `Option` at the edge exactly as `runAbort` births `Option` and
`runState` births `Pair` (M5). At the bare-metal edge a trusted event-loop driver runs an `{Inf}` computation
*unboundedly* (never returns); that is the one place divergence is executed for real, and a discharge does
not propagate `Inf` further (a `main` that calls a discharge stays terminating). For a discharge to *bound*
execution it must be able to stop between steps, so an `{Inf} A` is a **steppable** computation (a reified
delay/iteration structure, à la Capretta's delay monad) rather than a raw non-returning loop. That
representation — and its fusion to a tight counted loop under monomorphization, so the embedded cost is a
loop and not a heap of thunks — is the one mechanism still to pin (deferred).

**`main` and the event loop.** A reactive `main` runs forever, so the top-level event loop is an `{Inf}`
native run by the trusted edge driver — exactly where unboundedness belongs (mirroring cats-effect: the
runtime *is* the loop). Individual callbacks/handlers are terminating (`Inf`-free); only the scheduler is
`{Inf}`, and it is consumed by the edge driver. A bare-metal target must ship that event-loop runtime — the
one trusted unbounded loop.

## How you actually iterate

Recursion being absent from the language does not make iteration awkward — it relocates it:

- **Structure traversal** (`map`, `filter`, `foldLeft`, `sum`, `reverse`, …) is built on a platform-provided
  `fold`/`repeat` native. Because platform collections expose `size` + indexed access, the most fundamental
  loop is **counted iteration** ("do this `size` times"); a cons-structural fold is the special case for
  genuinely-inductive data, which Eliot mostly does not have (its `List` is a platform `ArrayList`). The
  platform decides which loops to ship.
- **Numeric recursion** (`power`, `gcd`, factorial) folds over an available bounding `Nat` (the exponent,
  `b`, the input) — a loose bound is fine for *termination*; only cost wants it tight.
- **Divide-and-conquer and mutual recursion** (quicksort, mergesort, mutually-recursive walks) are the
  ergonomic pressure points: phrase them as a fold over a fuel count plus an explicit worklist. Expressible
  and asymptotically faithful, but clunkier — absorbed once by the library author, never seen by users, with
  `Inf` + `withFuel` always available as the escape hatch.

**Size-indexing is cost-only and optional.** Termination needs *nothing* in the type — a platform fold reads
the runtime `size`. A type-level `List[SIZE, A]` survives purely for the WCET/resource-bound story (the fold
visits `SIZE` elements). Without it, termination still holds; only the static cost bound is lost. This is the
clean split: **soundness from "no recursion + `Inf` natives"; cost from optional size-indexing.**

## Implementation plan

### What the codebase dictates (investigation findings)

(file references under `lang/src/com/vanillasource/eliot/eliotc/` unless noted)

1. **Function-valued parameters stay runtime closures — no per-argument specialization.** `map(inc, xs)`
   keeps `f(h)` as a `ParameterReference` invoked through an indirect `Function.apply`
   (`jvm/.../classgen/processor/ExpressionCodeGenerator.scala`), and `UsedNamesProcessor` does not follow
   parameter references. The post-monomorphization call graph is incomplete for indirect calls → **the
   `Inf` effect must sit on the arrow** for sound higher-order propagation, not merely diagnostics.
2. **No occurs-check.** `monomorphize/unify/Unifier.scala` (`solveMeta`) binds a metavariable without
   checking it occurs in the RHS; a cyclic meta then loops `Evaluator.force`. Precondition #2 lands here.
3. **No strict-positivity check.** `data` receives only syntactic validation; `DataDefinitionDesugarer`
   passes constructor field types through untouched. Precondition #2 (positivity) must be built.
4. **The effect pipeline is reusable wholesale.** `{E} A` desugars in `EffectSugarDesugarer`;
   `EffectDesugaringProcessor` (after `operator`, before `saturate`) propagates effects via
   `DirectStyleDesugarer`, `CalleeSignatures`, and `DeclaredEffectChecker` (the ⊆ subset check). `Inf` is
   just another member of the capability set and threads through the identical shape — no special lattice.
5. **Recursion can only form among top-level values.** No local `let`/`letrec`; lambda parameters are
   non-recursive. Every cycle is a self/mutual reference among top-level named values, visible in the
   resolved `ValueReference` graph. The `activeFactKeys` pattern (already used for recursive-return
   detection in `monomorphize/check/CalculatedReturnResolver.scala`) catches the self-cycle during effect
   inference; the `used/UsedNamesProcessor.scala` non-convergence backstop is the existing safety net.

### Milestones

Each milestone is independently landable and leaves the compiler sound.

**M0 — Preconditions (soundness gate; no surface change).**
- *Occurs-check* in `Unifier.solveMeta`: before solving, walk the RHS (through solved metas) for the id; on
  occurrence, emit "cannot construct infinite type." Tests: `x x` rejected; legitimate higher-order metas
  still solve.
- *Strict-positivity processor*: walk each `data` constructor's field types with variance; reject the data
  type's own type-constructor FQN in a contravariant (left-of-arrow) position. Tests:
  `data Loop(f: Function[Loop, A])` rejected; `data Box(content: Function[A, B])` and
  `data Tree(left: Tree, right: Tree)` accepted.
- *Confirm purity*: verify no mutable-cell primitive exists; record as a guard.

**M1 — No recursion + `Inf` effect: declare, propagate, discharge.**
- *Reject recursion in user code*: any self/mutual cycle in the resolved `ValueReference` graph → hard error
  ("Eliot cannot express recursion; use a platform loop, or mark `Inf`"). Reuse `activeFactKeys` for the
  self-cycle; a reference-graph SCC pass covers mutual cycles.
- *`Inf` declaration (terminating by default)*: a body-less native is terminating unless it declares `{Inf}`;
  there is no `Terminating` annotation to write. (See the *Declaration* trade-off above.)
- *`Inf` effect fact + propagation*: a value's effect row = set-union of its callees' rows (read from native
  declarations for leaves), via the existing effect-pipeline shape. `Inf` present anywhere taints; its
  absence is termination — no `Terminating` token.
- *Discharge*: `withFuel : {Inf} A -> Nat -> Option[A]` as a terminating (`Inf`-free) native.
- Tests: a user-written cycle rejected; an `Inf` native propagates to its callers; a program with no `Inf` in
  any reachable row is total; `withFuel` tames an `Inf` computation to `Option`.

**M2 — Higher-order propagation (the function-coloring piece).**
- `Inf` rides the existing effect-row / carrier in a function value's type (the same channel as `{E}`
  capability effects), so it propagates through unification as ordinary set-union and is erased before
  codegen — *not* a separate two-point lattice slot on the `Function` representation.
- Higher-order functions are effect-polymorphic over their argument arrows; a call through a parameter incurs
  that parameter's row. `fold`/`map` become `Inf`-iff-their-step-is.
- Tests: a platform `fold` over a terminating step is terminating; the same `fold` over an `Inf` step is
  `Inf`; an `Inf` lambda stored in data then called → its `Inf` reaches the caller.

**Deferred (explicitly):** WCET / resource bounds and the optional size-indexing that feeds them; linearity
for mutation; CFA to recover indirect calls the coarse arrow bit over-rejects; the exact steppable carrier
for a richer `{Inf} A` discharge.

## Relationship to existing work

- Effects machinery (rows, `{E}` sugar, the subset check, carrier-polymorphism, M5 discharge) — the
  substrate `Inf` rides on. See [[project_effects_plan]] / [[project_effects_m4]] / [[project_effects_m5]].
- W4 recursion detection (`activeFactKeys`) + the `used` non-convergence backstop — the existing cycle
  detectors the no-recursion rule reuses.
- `Coerce` / compile-time evaluation already "Girard's paradox allows to diverge"
  ([[project_coerce_replaces_typerefinement]]) — the same residual `Type:Type` leak this model does not close.
- Cornerstones: Types Are Values (λ\*, and the accepted `Type:Type` trade-off); Platform-Independence via
  Layers (loops are platform natives); Use-Site Verification.
