# Recursion & Termination — Total Core, `Inf` as a Platform Effect

Status: **Design note + implementation plan — milestones M0–M2 ✅ COMPLETE.** Captures the model, its
theoretical grounding, its preconditions, and a sequenced milestone plan grounded in a codebase
investigation. The preconditions (M0), the no-recursion rule plus the `Inf` effect (M1), and higher-order
propagation (M2) are all built and verified end-to-end; only the explicitly-deferred resource/time work
remains (see *Deferred*).

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
platform native**; from there it propagates virally to every caller. `Inf` is not discharged — left on
`main` it denotes a deliberately non-terminating program (a server / firmware super-loop), executed by the
runtime on the `IO` carrier; *bounding* it (a timeout) is deferred until a time type exists. That is the
entire design.

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
- **Capretta, the partiality / delay monad (2005)** — non-termination framed as an effect, the conceptual
  ancestor of `{Inf}`. Eliot does **not** reify the delay structure, though: there is no step-budget
  eliminator; an `{Inf}` computation is *run* (forever, on `IO`), not *stepped*.
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

## `Inf` as an effect: declaration, propagation, execution

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

**`Inf` is run, not discharged.** Unlike a capability, `Inf` is **not discharged** — there is no terminating
discharge for it today, and it does not need one. You **run** it: realize it on the `IO` carrier (the
`forever` primitive becomes a JVM `while (true)`) and let the runtime execute the resulting `IO[Unit]`. `Inf`
is the **one effect that may legitimately reach `main` undischarged.** A capability must be handled for the
program to mean anything (`println` needs an implementation); `Inf` needs no handler, because "runs forever"
is already its complete meaning. An undischarged `{Inf}` at the top is therefore not
an error — it *is* a non-terminating program, exactly what a server or firmware super-loop should be. The
guarantee stays honest and conditional: a program terminates iff its row lacks `Inf`; a server opts in,
visibly.

**Endless loops (servers, firmware super-loops, the event loop).** You cannot *write* `loop { handle() }` —
that is recursion, rejected. You hand a **terminating step** to a platform unbounded driver, exactly as
bounded iteration hands a step to `fold`:

```
forever : (Unit -> {e} Unit) -> {Inf, e} A      -- run the step endlessly; never returns normally
```

The step (`handle(accept())`, `toggle(led)`) is `Inf`-free — it terminates each iteration, so its WCET is
analyzable — while the driver carries `{Inf}`. So a server's `main` is `{Inf, Console}` (or `{Inf, Gpio}`,
…): the capability effects discharge the usual way, and `Inf` rides out to the top, meaning "runs until
killed." This is the embedded super-loop, and it is the *simplest* `Inf` case — `forever` is a plain
`while (true) { step() }` platform native, with no reified delay/step structure anywhere.
The reactive/event-loop variant is the same shape with the driver supplied by the runtime scheduler
(cats-effect style: finite handlers, the runtime *is* the loop), where the handlers stay `Inf`-free and only
the scheduler is `{Inf}`. Either way the **per-iteration bound** — each request/tick terminates — comes for
free, which is exactly what a responsive embedded system needs. (Two shapes for the top level: in the
reactive model the *runtime* owns the loop, so `main` stays a finite description; in a user-written
super-loop the driver is in the program, so `main` is honestly `{Inf}` — both are correct, neither is a
defect.)

**Bounding an `{Inf}` computation is deferred.** There is no step-budget `withFuel`/fuel discharge — it is not
needed, and reifying every `{Inf}` computation as a steppable structure to support it is cost the embedded
target should not pay. When bounding is wanted, it will be a **timeout** (race the computation against a
clock and cancel), which is the honest mechanism but needs a *time type* first; it is therefore deferred to
the resource/time work, not part of this plan. Until then the only fates of `{Inf}` are: run it (forever, or
until the OS kills it) or don't reach it.

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
  ergonomic pressure points: phrase them as a fold over a bounding count plus an explicit worklist. Expressible
  and asymptotically faithful, but clunkier — absorbed once by the library author, never seen by users, with
  marking it `{Inf}` (and running it) always available as the escape hatch.

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
   parameter references. The post-monomorphization call graph is incomplete for indirect calls → **`Inf`
   must be carried in the function's type** (its effect row / carrier), so it propagates through an indirect
   call; it cannot be recovered from the call graph after the fact. (This is the *existing* carrier channel,
   not a new arrow slot — see M2.)
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

**M0 — Preconditions (soundness gate; no surface change). ✅ DONE.**
- *Occurs-check* in `Unifier.solveMeta` — **DONE.** `occursIn` walks the RHS through solved metas (forcing, so an
  *indirect* cycle `?id := … ?other …` with `?other := … ?id` is caught too); the empty-spine direct solve refuses
  and emits "Cannot construct infinite type.", and `decomposeSpines` refuses (postpones, fail-safe) on an injective
  solution that would be cyclic. Tests: `monomorphize/unify/OccursCheckTest` — `x x` (`?A ~ ?A -> ?B`) rejected,
  indirect cycle rejected, legitimate higher-order metas still solve.
- *Strict-positivity check* — **DONE.** `core.processor.StrictPositivityChecker` (a pure
  `DataDefinition => Seq[Sourced[String]]`) invoked from `CoreProcessor` for every loaded file: walks each `data`
  constructor's field types tracking polarity (only `Function` flips, its last generic arg is the covariant codomain),
  reporting the data type's own unqualified name in any contravariant position. Tests:
  `core/processor/StrictPositivityCheckerTest` + end-to-end `jvm/TerminationIntegrationTest` —
  `data Loop(f: Function[Loop, String])` rejected; `data Box(content: Function[A, B])` /
  `data Tree(left: Tree, right: Tree)` accepted (the latter compiles and runs). Mutual negative recursion across two
  data types is left to M1's value-reference-graph cycle detection; other type constructors are conservatively
  covariant (sound — over-rejects, never under).
- *Confirm purity* — **DONE.** Confirmed no mutable-cell primitive exists today (system natives = Function/Type/Bool +
  bound-arithmetic; jvm `Intrinsics` = intToString/nativeWiden/native{Add,Subtract,Multiply}*; no `.els` cell type/def).
  Recorded as the guard `termination/PurityGuardTest`, which scans every layer's `.els` for the mutable-reference
  vocabulary and fails (with a pointer back to the graceful-fallback note) if a cell is ever added.

**M1 — No recursion + `Inf` effect: declare, propagate, run. ✅ COMPLETE.**
- *Reject recursion in user code* — **✅ DONE.** `termination.processor.RecursionCheckProcessor` (running
  `termination.processor.RecursionChecker`) is a standalone gate phase placed after `OperatorResolverProcessor`
  and before `EffectDesugaringProcessor` — the per-value gate the whole compiled program passes through *before*
  monomorphization. It rejects any self/mutual cycle in a value's **runtime-body** value-reference graph with the
  hard error "Value 'X' is defined recursively." + the `fold`/`forever` pointer. It is a bounded reachability
  search over *resolved* `ValueFQN`s in the body only (never the type signature), so it is precise: a covariant
  `data Tree(left: Tree, right: Tree)` (constructors are body-less leaves) and the monad-transformer lift
  (`EitherT.pure` calling the inner carrier's *abstract* `pure`, a different FQN) are not flagged. A clean value
  is certified as a `termination.fact.RecursionCheckedValue` (the operator-resolved value carried through
  untouched); a reported cycle trips `registerFactIfClear` so that fact is never registered. Because
  `EffectDesugaringProcessor`'s sole input is repointed to `RecursionCheckedValue`, a recursive value never
  reaches effect desugaring, saturation or monomorphization — the gate is fail-safe by construction, and it
  preempts the two pre-existing recursion fail-safes (the calculated-return guard and the `used` non-convergence
  backstop), which survive only for residual type-level (Girard) divergence. There is **no** "mark it `Inf`"
  escape for a user cycle: `Inf` originates only on a native, so a recursive user function is rejected outright
  (it becomes `{Inf}` only by *calling* an `Inf` native like `forever`). Demand-driven (fires for values reachable
  from `main`), consistent with use-site verification. Implementation note: it was originally folded into
  `EffectDesugaringProcessor` to avoid wiring a new producer into the ~10 custom-pipeline test harnesses; it is
  now its own processor + fact (those harnesses each register `RecursionCheckProcessor` before
  `EffectDesugaringProcessor`), keeping the recursion concern entirely out of the effect phase. Tests:
  `jvm/TerminationIntegrationTest` (direct cycle, mutual cycle, deep non-recursive chain compiles+runs); the
  pre-M1 recursion-proxy tests across `MonomorphicTypeCheckTest`/`MonomorphizationVersioningTest`/etc. were
  retargeted to assert the rejection.
- *`Inf` as an effect — declare, propagate, run* — **✅ DONE.** `Inf` is an ordinary effect *ability*
  (`stdlib/.../Inf.els`: `ability Inf[F[_]] { def forever(step: F[Unit]): F[Unit] }`, import-required like `Abort`/
  `State`), so it rides the **existing** effect machinery with no new termination lattice — exactly the doc's intent:
  - *Declaration / origination.* `Inf` originates on `forever`, the sole operation of the `Inf` ability. A body-less
    native is terminating by default; `forever` is the one that loops, and its `{Inf}`-ness is carried by being an
    `Inf`-ability method (its owning ability *is* `Inf`). There is no `Terminating` token.
  - *Propagation is the existing subset check.* Because calling an ability method performs its owning ability
    (`CalleeSignatures.effectAbilitiesOf` → `Inf`), and `DeclaredEffectChecker` already enforces *used ⊆ declared* for
    every carrier-polymorphic value, a `{Console}`-only function that calls `forever` is rejected ("performs the effect
    'Inf' but does not declare it") — i.e. `Inf` propagates to its callers as a hard requirement, for free, through the
    *same* pipeline as `Console`/`Log`. A value declaring `{Inf}` (or `{Inf, Console}`) is accepted. A concrete-carrier
    value (`main : IO[Unit]`) has no declared set, so `Inf` is simply absorbed into `IO` and reaches the top — a server.
  - *Run, not discharged.* There is no `runInf`. The jvm layer (`jvm/.../Inf.els`) provides `implement Inf[IO]` whose
    `forever(step) = IO(_ -> foreverInternal(block(step)))`, where `foreverInternal` is a `private` leaf native emitting
    a `while (true) { thunk.apply(unit) }` loop (`NativeImplementation.eliot_lang_Inf_foreverInternal`, marked `impure`
    so the backend enforces its `private`). Reaching it via `main : IO[Unit]` runs the step endlessly; an `{Inf}` `main`
    is valid and runs forever. No step-budget discharge (`withFuel` dropped); a timeout-based bound is deferred to the
    time/resource work.
  - *Deviation from the sketch above.* `forever` is typed `F[Unit] -> F[Unit]`, not the `… -> {Inf, e} A` of the
    *Higher-order propagation* sketch: the result is `Unit` (a never-returning loop produces no value; `Unit` covers the
    server/super-loop case) and the step's own effect polymorphism `{e}` is **M2**. In M1 the step is a concrete carrier
    value (`println("tick") : IO[Unit]` at `main`), which is all the end-to-end tests need; carrier-row `{e}` on the step
    is the M2 refinement.
- Tests — **✅ done.** A user-written cycle rejected (recursion half); `Inf` propagation rejected/accepted by the subset
  check (`EffectDesugaringProcessorTest`, lang); end-to-end `compileForErrors` rejection + a bounded-subprocess
  "runs endlessly" test for both a concrete-`IO` `main` and a carrier-polymorphic `{Inf, Console}` super-loop pinned to
  `IO` (`TerminationIntegrationTest`, jvm — the loop's infinite-loop bytecode also verifies under `COMPUTE_FRAMES`).

**M2 — Higher-order propagation (the function-coloring piece). ✅ COMPLETE.**

This milestone required **no new production code**: it is the architectural payoff of M1's decision to model
`Inf` as an ordinary effect *ability* riding the carrier rather than a bespoke termination lattice. Because
the carrier `F` is shared across a computation and unified at every call (including indirect/higher-order
calls), `Inf` propagates through a higher-order combinator *for free*, exactly as a capability effect does.
The milestone is therefore a **verification + test** step confirming the behaviours below, each now covered
end-to-end in `jvm/TerminationIntegrationTest`:

- *`Inf` rides the existing carrier, no new lattice.* Confirmed — there is no two-point slot on the
  `Function` representation; the effect lives in the carrier's ability constraints and is erased before
  codegen (the `Inf[IO]` instance is resolved and the dictionary erased, so the programs below run as plain
  bytecode).
- *Function-coloring win — one combinator, `Inf`-iff-its-step-is.* A single effect-transparent combinator
  `def runStep[F[_]](step: Function[Unit, F[Unit]]): F[Unit] = step(unit)` *terminates* over a terminating
  step (`runStep(_ -> println("done"))` prints once and exits) and *loops* over an `Inf` step
  (`runStep(_ -> forever(println("loop")))` runs endlessly) — the **identical definition**, the colour coming
  entirely from the argument. This is Nystrom's function-coloring win and the load-bearing reason a language
  whose iteration is a platform feature needs only one effect-transparent `fold`/`map`/`runStep`. (No `List`
  or platform `fold` native exists yet, so `runStep` — apply the carrier-returning step — stands in as the
  minimal higher-order combinator; the propagation it exercises is identical.)
- *The step's own `{e}` unions with `Inf`.* An `{Inf, Console}` driver over a `{Console}` step
  (`def driver(step: {Console} Unit): {Inf, Console} Unit = forever(step)`) unions both effects through the
  shared carrier and runs — closing the M1 deviation that deferred the step's `{e}` carrier-row polymorphism
  to M2 (it needed no change; M1's `F[Unit]` step already carries `{e}` because the carrier is a *set* of
  effects).
- *`Inf` survives a round-trip through data.* An `Inf` action stored in `data Box[F[_]](action: F[Unit])`,
  pulled back out through its field accessor and run, still loops — data is only a courier for the
  carrier-typed value and cannot launder the effect.
- *Propagation is the same subset check, fail-safe through a higher-order driver.* The driver above declaring
  only `{Console}` (omitting `Inf`) is rejected with the clean "performs the effect 'Inf' but does not declare
  it" — the M1 used-⊆-declared check, unchanged. The dual carrier-pin case (a `{Console}`-carrier combinator
  handed an `Inf` step) is likewise rejected, at monomorphization ("Function not implemented." — the use-site
  backstop, sound but less precise; surfacing it at the definition is deferred IDE work, not a soundness gap).

**Deferred (explicitly):** WCET / resource bounds and the optional size-indexing that feeds them; a
**timeout-based bound on `{Inf}`** (needs a time type; the honest replacement for a step-budget discharge);
linearity for mutation; a definition-site (vs. use-site/monomorphization) error when an `Inf` step is handed
to a carrier that cannot host it (IDE work — sound today, just less precise). The investigation's "coarse
arrow bit over-rejects indirect calls" concern did **not** materialize: M2 carries `Inf` in the *carrier*,
not a per-arrow bit, so propagation is precise and no CFA recovery is needed.

## Relationship to existing work

- Effects machinery (rows, `{E}` sugar, the subset check, carrier-polymorphism, M5 discharge) — the
  substrate `Inf` rides on. See [[project_effects_plan]] / [[project_effects_m4]] / [[project_effects_m5]].
- W4 recursion detection (`activeFactKeys`) + the `used` non-convergence backstop — the existing cycle
  detectors the no-recursion rule reuses.
- `Coerce` / compile-time evaluation already "Girard's paradox allows to diverge"
  ([[project_coerce_replaces_typerefinement]]) — the same residual `Type:Type` leak this model does not close.
- Cornerstones: Types Are Values (λ\*, and the accepted `Type:Type` trade-off); Platform-Independence via
  Layers (loops are platform natives); Use-Site Verification.
