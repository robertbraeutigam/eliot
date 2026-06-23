# Recursion & Termination — `Rec[N]` / `Inf` as Effects

Status: **Design note + concrete implementation plan.** Captures the model, its preconditions, and a
sequenced milestone plan grounded in a codebase investigation (see *Implementation plan*). Nothing is
built yet. Supersedes the sketch in the `project_recursion_as_effect` memory.

## Goal

Recursion is too easy to get wrong and — more importantly for Eliot's targets — defeats the
compile-time resource guarantees (memory, stack, WCET) that are the language's whole point. So:

> **Disallow unbounded recursion by default.** A function may recurse only if it either *proves
> termination* (`Rec[N]`) or *explicitly opts out* (`Inf`). Both are **effects**, tracked and
> propagated by the existing capability-row machinery, and both are **phase-erased** before codegen.

This makes termination a *typing property*, consistent with the cornerstones (Types Are Values;
Use-Site Verification; effects as capability rows). The accepted trade-off mirrors the rest of the
language: soundness is total, the per-definition certificate comes from **tests**, not a proof, and a
latent partiality surfaces as a hard error at the use site, never as a silent miscompile.

## The two effects

- **`Inf`** — "may not terminate." The honest escape hatch. Viral like any effect: calling an `Inf`
  thing makes you `Inf`. Expected to be **rare in user code** (see *Who writes `Rec`?* and *`main`
  needs no `Inf`*).
- **`Rec[N]`** — "terminates, with a well-founded measure `N`." `N` is a non-negative
  (`Int[0, MAX]`-style) measure that **strictly decreases** on every recursive call. By
  well-foundedness, strict descent over ℕ *is* a complete termination proof — "exits at 0" falls out
  for free (you cannot decrease below 0). `N` is **unreified / phantom**: it exists only for the
  compile-time proof and is erased, so it never affects monomorphization (it collapse-erases under the
  monomorphization-keying classification — see [[project_generic_multifield_codegen_fix]] and the
  keying work in `saturate/fact/BinderRoles.scala` / `used/CodegenProjection.scala`).

We are **not building a theorem prover.** Instead of proving `expr < N` for arbitrary `expr`, we
**restrict the vocabulary** of how a measure may decrease to a fixed, safe set of operations the
compiler recognises syntactically:

- **measure descent**: `dec(N)`, `half(N)`, … — each is a *partial* operation with precondition
  `N > 0` (see below);
- **structural descent**: recurse on a structurally-smaller sub-term of the scrutinee (the FP
  bread-and-butter — tree/list traversal). The measure is the structure's size bound.

## The crux: descent operations are *partial*, and that partiality is the whole proof

This is the one technical core. The decrease operation carries a precondition:

```
dec : {N > 0} Int[1, M] -> Int[0, M-1]
```

To type the recursive call `f[dec(N)]` without underflow, the checker must discharge `N > 0`
**at that call site, from the condition guarding the recursive branch**. For `List[SIZE]` the
recursive branch is "the list is `cons`", and the type-level invariant `cons ⟹ SIZE > 0` is what
discharges it. **That discharge is the proof** — it is the only place the measure `N` is consulted,
and it is *ordinary refinement reasoning* already implemented in
`monomorphize.refine.RefinementSolver` (see [[project_coerce_replaces_typerefinement]]). No new
solver, no theorem prover.

Important consequence of erasure: because `N` is erased (no per-`N` monomorphization), there is **no
use-site monomorphization underflow to fall back on** — the definitional descent check is the *sole*
guarantee for an erased measure. It must be sound on its own.

## Detection & denial: a latent effect on the arrow, not pointer-tracking

Recursion is "allowed" syntactically (you can call any function), so we need to detect and deny it.
The naive route — statically tracking which function value flows where (CFA / defunctionalization) —
is expensive and imprecise. Instead:

**Put the termination effect on the function arrow itself.** For *propagation* purposes the effect is
a coarse two-point lattice `{Terminating, Inf}` (the measure `N` stays local to each recursive
definition; it never needs to thread through higher-order composition — finite ∘ finite = finite, so
the bit suffices for termination; `N` matters only for the local descent check and, later, WCET):

```
Function[A, B]            -- today
Function[A, B] ! Term     -- Term ∈ {Terminating, Inf}, latent effect of *calling* it
```

- A **lambda** gets its effect from its body.
- A **function value** (param / local / field) carries the effect in its type; calling it incurs it.
- A function **stored in data** is carried by the data's *type parameter*
  (`List[Function[A,B] ! Inf]`): the slot's declared effect is the worst-case bound, checked **at
  insertion** (all at compile time, on static types — *nothing runtime*) and incurred **at the call**.
  This is the "worst-case compile-time info about what a structure can contain," formalised as just a
  type parameter.

Higher-order functions become **effect-polymorphic** (same shape as the existing carrier-polymorphism
for `{E}`):

```
map : (Function[A,B] ! e) -> List[n, A] -> List[n, B] ! (Rec[n] ⊕ e)
```

`map`'s own recursion contributes `Rec[n]` (direct structural self-call, fully visible); it inherits
`f`'s effect `e`. At `map(inc, xs)` with `inc` terminating, `e := Terminating`.

**Detection = the effect-inference fixpoint hitting a cycle.** A function whose latent effect can't be
assigned `Terminating` without referencing its own effect *is* the recursive one; the compiler then
demands an explicit `Rec[N]` (verified by the local descent check) or `Inf`. The existing machinery
already finds the direct case: a recursive self-call re-requests the same fact key, caught by W4's
`activeFactKeys` chain (`FactGenerator` / `CompilationProcess.activeFactKeys`) and the
non-convergence backstop in `used/UsedNamesProcessor.scala`. The latent bit's job is to carry the
verdict across *indirect* calls (function pulled from data / chosen at runtime) that the fact-cycle
detector cannot see.

### Detection is reliable — storing functions in data does not hide recursion

**Detection is not the hard part of this design.** It operates on the definition's **lexical
reference graph** (resolved free variables), *not* on runtime data flow. A data structure is only a
*courier* for a function value; it cannot launder a lexical reference. The repeatedly-raised worry —
"`f` builds a lambda, stores it in a structure, and that lambda calls `f`" — is caught at `f`'s
definition, before any reasoning about where the data flows:

```
f(d: Tree) =
  let g  = (x) -> f(prune(d))     -- lambda calls f BY NAME, capturing d
  let d2 = store(d, g)            -- g now lives inside the data structure
  (lookup(d2))(unit)             -- pull g back out and call it  → calls f
```

The lambda `g` is part of `f`'s source; its free variables are computed lexically and include `f`. So
`f`'s definition transitively references `f` — `f` is self-recursive, flagged by the *same*
name/fact-cycle detector as a plain `f(...) → f(...)`. The `store`/`lookup` dance is invisible to
detection. It generalises to any number of hops (`f → h → f` through couriers is an SCC in the
reference graph) and is **conservative**: a recursive lambda that is stored but never called still
flags `f` — a harmless over-rejection (you annotate or restructure), never a miss.

The only way to make recursion flow *purely through the data* (a stored lambda that does **not** name
`f` yet reaches itself) is impossible to construct purely:

```
f(d) =
  let g  = (x) -> (lookup(d))(x)  -- g calls whatever is in d, but captured d (pre-store)...
  let d2 = store(d, g)            -- ...and d does NOT contain g, so g can never reach g.
```

To make `g` reach itself it must close over `d2` (`let g = (x) -> (lookup(d2))(x)` with
`let d2 = store(d, g)`) — a **mutually recursive `let`**, i.e. a syntactic name cycle, caught and
policed (source #1 in *Preconditions*); without recursive `let`, `d2` isn't even in scope inside `g`.
The remaining option is to build `g`, then **mutate `d`** to contain it (Landin's knot) — forbidden by
*No mutable cells*. Immutable data is built bottom-up from values that already exist, so it cannot
manufacture a cycle that was not written down.

> **Conclusion: recursion is always detected.** Storing functions in data changes nothing about
> detection. The genuinely hard part is not *detecting* the recursion but *proving it terminates* when
> the recursive call is deferred inside a stored lambda — addressed next.

### Proving termination through a stored lambda

Once flagged, the function must be `Inf` or carry a `Rec[N]` whose descent holds at *every* recursive
call — including one buried in a lambda fired later. Two tiers:

- **Tractable — measure on the lambda's own argument.** If the lambda descends on what it is *applied
  to* (`g = (x) -> f(smaller(x))`), each call of `g` decreases `x`; this is ordinary `Rec[N]` descent
  checked at the lambda's definition, exactly like any function. Storage/retrieval is irrelevant — the
  measure rides in as the argument at call time. This is the common, clean case.
- **Hard — measure threaded through closure *capture*.** If termination depends on each created lambda
  capturing a strictly-smaller measure than the activation that built it (`f[N]` builds `g` capturing
  `m < N`; `g` calls `f[m]`; …), the descent is mediated by closure capture across store/retrieve and
  the syntactic check cannot establish it. **Rule: if descent can't be shown syntactically, the
  function is forced to `Inf`** — rejected as `Rec`, never silently accepted (fail-safe; the latent
  partiality is a hard demand to mark `Inf` or move the measure onto an argument).

## Effects on every arrow: propagation, the function-coloring win, and effect algebra

The latent effect must sit on **every** function arrow, lambdas included — otherwise an `Inf`
computation could be laundered through a lambda and lose its effect. The annotation burden, however,
falls only where inference genuinely cannot proceed:

- **Lambdas and acyclic functions: the effect is *inferred*** from the body, bottom-up. No syntax.
- **Recursive (cyclic) functions must *declare*** `{Rec[N]}` / `{Inf}` — inference hits the cycle and
  cannot close it without the user's measure. The annotation lands precisely and only at the cycle.

### Storing an effectful lambda in data

Inference doesn't lose the effect; the **type captures it at the storage boundary**. Storing
`a -> {Abort} b` surfaces its effect in the containing data type, two ways:

- **fixed field** — `data Box(f: Function[A,B] ! {Abort})` — constrains the box to functions whose
  effect ⊑ `{Abort}`; written when you want to *restrict*;
- **effect-parametric (default)** — `data Box[e](f: Function[A,B] ! e)` — the box is polymorphic over
  its content's effect, inferred at construction and carried as a (phantom, erased) type parameter,
  just like an element type.

So the effect becomes *explicit* only at the data **declaration**, and even there it is usually an
implicit/`auto` parameter, not a hand-written annotation. You annotate to *narrow* (e.g. "this list
holds only `Terminating` functions" for a resource guarantee), never just to propagate.

### Propagation is union, inferred and implicit

A function parameter `g: Function[A,B]` carries a **fresh, polymorphic effect variable** `e` — not
"any" effect (which would read as "ignore it"), but a variable instantiated per call site. Calling `g`
incurs `e`; the enclosing function's effect is the **union** of its callees' effects plus its own:

```
f(g, h, a) = g(h(a))     ⟹     effect(f) = effect(g) ∪ effect(h) ∪ (f's own)
```

`f` is generalised over `e…` at its definition and instantiated at each call. The user writes **no
effect variables** — propagation is implicit, matching the `auto` / inferable-generics ethos. The cost
of implicitness (a function's true effect is *computed*, not written) is paid by **tooling**: the IDE
surfaces the inferred effect on hover (the `TypeHintIndex` machinery), and the fail-safe design does
the rest (a propagated `Inf` is caught at the first `Rec` use site).

**The Java lesson.** Checked exceptions are the one mainstream latent-effect system. They propagate
fine through direct calls but broke on lambdas, because the functional interfaces are
effect-*monomorphic* (`Function.apply` declares no `throws`); the fix — throws-polymorphism — was
rejected as too complex, compounded by a deferred-firing problem (a lazy `Stream` holds an exception
that fires at the terminal op, with no type to express it). Eliot is positioned differently: it has
already committed to effect polymorphism (`{E}` → carrier rows, M1–M5 — exactly the transparency Java
declined), and termination is a **purely static** effect that never "fires" at runtime, so the
deferred-firing objection does not apply. Eliot therefore *must* do what Java refused — effect-transparent
higher-order functions, since `map`/`fold` being terminating-iff-their-argument-is is the whole point of
recursion being a library feature — and it can, because it paid for the machinery and avoids the worst
pitfall.

### The function-coloring win (a primary motivation)

Because every arrow is effect-polymorphic, **one combinator serves pure and effectful arguments
alike.** Haskell and cats-effect duplicate the entire combinator zoo — `if`/`ifM`, `map`/`mapM`,
`filter`/`filterM`, `(.)`/`(<=<)`, `foldr`/`foldM` — *because* `a -> b` and `a -> m b` are different
types, so a combinator written for one rejects the other. With an inferred effect on each arrow,
`if` / `map` / `compose` / `fold` accept both and just propagate the effect — a single definition.

This is the cure for **"function coloring"** (Nystrom, *What Color is Your Function?* — the async/await
form of the same disease), delivered by Koka, Unison, Frank, and Eff. For a language where **recursion
is a library feature**, a single effect-transparent `fold` rather than `fold`/`foldM` is not a nicety —
it is load-bearing. This is a primary reason to put effects on every arrow, not merely a side benefit.

### Effect algebra: additive now, typestate later

For everything in this document, **union is total — there is no compatibility check at a composition
site:**

- *termination* is a two-point lattice; `∪` is join and `Inf` absorbs (`Terminating ∪ Inf = Inf`);
- *capabilities* (`{Console, State, Abort}`) union as **sets**.

So `g(h(a))` always type-checks its effect. What *can* fail lives downstream and is deferred there:
**discharge** (is a handler present, and in what order — the M5 ordering-at-the-edge) and **coherence**
(is the ability resolvable uniquely for the concrete carrier). Neither is a propagation concern.

This totality holds **because these effects are additive / idempotent.** It will **not** hold for a
**typestate / linear** effect — and the roadmap already has one: *"you cannot set a pin `high` without
first configuring it as an output"* (TODO, microcontroller target). That effect carries **ordering and
linearity** (configure-then-use, possibly use-once); unioning two functions that each touch the pin is
*not* free — it must check the protocol. So the effect row is not one uniform thing:

> **Additive effects** (termination, capabilities) — union always defined, no check, order-independent
> for propagation. **Typestate / linear effects** (pin configuration, use-once resources) — union and
> sequencing carry a real obligation; *same row syntax, genuinely different algebra*. The additive kind
> is what this design relies on; the typestate kind is future work that must **not** be assumed to
> union freely.

## Preconditions — "in a pure language, recursion cannot hide"

The detection story is **complete only under three conditions**, which are theorems, not hopes. A
typed pure lambda calculus is *strongly normalizing* (STLC — Tait; System F — Girard): it has **no
recursion at all**. Recursion is something a language *adds back*, and there are exactly **three
sources**. Two must be blocked by the type system; the third by purity.

1. **Explicit recursive bindings** (`fix` / `letrec` / a name referring to its own binding, incl.
   mutual). *Visible by construction* → caught by the name/fact-cycle detector. This is the only
   source we want to keep, and it is exactly the one we police.

2. **Recursive types in negative position** (the Y-combinator route). `Y` needs `x x`, i.e. a type
   `T = T -> A`. Two guards required, because they block different doors:
   - **Occurs-check in unification** — rejects an *inferred* infinite type (a bare `x x`). Standard
     HM; confirm Eliot's `unify` enforces it (it must, per the cornerstone — `unify` is pure
     definitional equality).
   - **Strict positivity on `data`** — rejects a *nominally* introduced negative recursive type, e.g.
     `data Loop(f: Function[Loop, A])` (the defined type `Loop` appears **left of an arrow**). The
     occurs-check does *not* catch this; without the positivity check, this single declaration is a
     backdoor that reintroduces `Y` and hides recursion in data + lambdas, *even in a pure language*.
     **Rule:** in every constructor field, the type being defined may appear only in **strictly
     positive** positions (never to the left of a function arrow). This is what Coq/Agda enforce; it
     preserves strong normalization. **This check does not exist yet and is the concrete new
     obligation this model places on the `data` desugarer / checker.**

3. **The store (mutable cells)** — Landin's knot: allocate a cell, store a function that reads and
   calls the cell, backpatch. Recursion through the heap, no syntactic cycle, no recursive type.
   **Blocked by purity** — see *No mutable cells* below.

> **Therefore:** with (1) policed, (2) blocked by occurs-check + strict positivity, and (3) blocked
> by purity, **every instance of recursion is witnessed by an explicit syntactic recursive binding.**
> That is what makes "deny recursion by default" mechanically decidable.

Theory references: Tait / Girard (strong normalization); Pierce *TAPL* (recursive types derive
`fix`); Coquand–Paulin (strict positivity for inductive definitions); Landin's knot (references break
normalization); **David Turner, *Total Functional Programming* (2004)** — manifesto for exactly this
discipline; and the *partiality / delay monad* (Capretta) — non-termination framed as an effect,
which is precisely `{Rec[N], Inf}` in finitary, resource-flavoured form.

## No mutable cells

Decision: **Eliot ships no general user-facing mutable cell.** This is not a concession to purity
aesthetics — an unrestricted cell knocks over all three of the guarantees Eliot exists to make
(termination via the knot, resource bounds via aliasing, race-freedom under interrupts). "Even
Haskell has it" cuts the other way: Haskell has cells *because* it gave up those guarantees.

The legitimate uses of mutation are covered otherwise:

- **logical mutable variable** → the **State effect** (already built — [[project_state_revived]]);
- **in-place update for performance** → a future **linearity / uniqueness analysis** (mutate when
  provably unaliased — Clean / Rust / Linear Haskell / Idris 2), as a *compiler optimization* under a
  pure surface. This is the principled embedded answer and fits monomorphization + resource
  accounting; it can land later without changing the surface language;
- **hardware registers / pins / timers** → **platform effects at the edge** (already on the TODO);
  registers hold bytes, not closures, so they never tie a call-knot;
- **shared / interrupt state** → a *restricted* capability (you want it restricted for race safety),
  not a free cell;
- **cyclic data (graphs)** → **index-based representations** (arrays + indices), which bounded-memory
  targets favour anyway and which don't tie call-knots.

**Graceful fallback if mutation is ever needed:** the latent-effect machinery already prices cells
correctly. A cell of a type with latent effect `e` contributes `e`; you cannot optimistically infer
`Terminating` for mutable contents, so a cell holding **plain data** costs nothing (empty `e`) while a
cell holding a **function** defaults to `Inf`. "No callables in a `Ref`" thus falls out as
"function-holding cells are `Inf`" — no special rule, guarantee preserved.

**Honest cost:** until linearity exists, "no cells" means some algorithms **copy**, which hurts more
on a 2 KB-SRAM part, not less. Watch this; mitigate with State + monomorphization now, linearity
later.

## Secondary design points (settled in discussion)

- **Termination ≠ WCET (for now).** Strict descent proves *termination*; a *cost* bound additionally
  needs the measure's `MAX` to be statically known. Resource-bounded targets should reject a `Rec`
  whose measure has no static `MAX`. Whether the two later share the measure is open. (See TODO:
  WCET / `strictlyEvery`.)
- **`main` needs no `Inf`.** Like cats-effect, the **runtime is the event loop**; `main : IO[Unit]`
  is a finite description and the looping lives in the scheduler. Unboundedness is *discharged at the
  runtime edge*, mirroring `run*` effect discharge — each callback/handler stays `Rec`-bounded. The
  per-iteration bound therefore comes for free; no productivity/coinduction machinery needed. (A
  bare-metal target must actually *ship* that event-loop runtime; that runtime is the one place a
  trusted unbounded loop lives.)
- **Tail vs. non-tail is an optimization, not a restriction.** `Rec[N]` already bounds non-tail
  **stack depth** (`N` frames). Tail-position recursion is the special case the backend can loop-ify
  for O(1) stack; detecting tail position is an easy local syntactic check. Do *not* restrict to
  tail-only — it just relocates the bound into explicit accumulator structures that need their own
  bounds.
- **Who writes `Rec`?** Almost no one. Recursion is a *library* feature (`map`/`fold`/`Seq`).
  Authors discharge `Rec` once, parameterized by the collection's length bound; users over
  length-indexed structures inherit it **transitively, with zero annotation**. The `Rec`/`Inf` tax
  is paid only by the rare hand-written recursion. This is the desired ergonomic gradient.
- **Structural recursion on real data always terminates at runtime** regardless of measure
  correctness (the data is genuinely finite). A wrong `SIZE` there corrupts the *bound*, not
  termination. The hang risk is confined to *pure measure* recursion (`half(N)`, numeric iteration)
  with no underlying finite structure — exactly where the partial-`dec` discharge does its work.

## Open questions / not yet decided

Placement, positivity, mutual recursion, CFA, and surface syntax are now resolved (surface syntax:
ride the `{…}` row — see the plan). What genuinely remains open:

- **WCET / resource bounds** — whether they later share the termination measure (deferred).
- **Linearity-for-mutation** — the eventual performance escape hatch under the no-mutable-cells
  decision (deferred; see *No mutable cells*).

## Implementation plan

### What the codebase dictates (investigation findings)

Five facts from the current compiler shape the plan (file references under
`lang/src/com/vanillasource/eliot/eliotc/` unless noted):

1. **Function-valued parameters stay runtime closures — no per-argument specialization.** `map(inc, xs)`
   keeps the inner `f(h)` as a `ParameterReference` invoked through an indirect `Function.apply`
   (`jvm/.../classgen/processor/ExpressionCodeGenerator.scala:88-109`), and `UsedNamesProcessor` does
   not follow parameter references (`used/UsedNamesProcessor.scala:160-166`). **The post-monomorphization
   call graph is incomplete for indirect calls** → the latent effect on the arrow is required for *sound*
   higher-order propagation, not merely diagnostics.
2. **No occurs-check.** `monomorphize/unify/Unifier.scala:248-272` (`solveMeta`) binds a metavariable
   without checking it occurs in the RHS; a cyclic meta then loops `Evaluator.force`
   (`monomorphize/eval/Evaluator.scala:150-163`). Self-application `x x` is not cleanly rejected today.
   **Precondition #2 lands here.**
3. **No strict-positivity check.** `data` receives only syntactic validation; `DataDefinitionDesugarer`
   passes constructor field types through untouched. **Precondition #3 must be built.**
4. **The effect pipeline is reusable wholesale.** `{E} A` desugars in `EffectSugarDesugarer`
   (CoreProcessor); `EffectDesugaringProcessor` (after `operator`, before `saturate`) propagates effects
   via `DirectStyleDesugarer` (one bottom-up walk threading `usedEffects`), `CalleeSignatures`
   (callee→effect via `Qualifier.Ability`), and `DeclaredEffectChecker` (the ⊆ subset check). The
   termination effect threads a *termination lattice* through the identical shape.
5. **Recursion can only form among top-level values; the reference graph is cheap, and detection is
   complete.** There is no local `let`/`letrec` (`ast/fact/Expression.scala`) and lambda parameters are
   non-recursive (`resolve/processor/ValueResolver.scala:215-224`), so **every cycle is a self/mutual
   reference among top-level named values**, visible in the resolved `ValueReference` graph. With the M0
   preconditions (occurs-check blocks self-application; positivity + purity block the other two recursion
   sources), static cycle detection is **complete**. The `activeFactKeys` pattern — already used for
   recursive-return detection (`monomorphize/check/CalculatedReturnResolver.scala:176-180`) — catches the
   cycle during effect inference (re-entry on the same `vfqn`), so no separate SCC pass is required for
   the self/mutual case.

### Milestones

Each milestone is independently landable and leaves the compiler sound.

**M0 — Preconditions (soundness gate; no surface change).**
- *Occurs-check* in `Unifier.solveMeta`: before `metaStore.solve(id, rhs)`, walk `rhs` (through solved
  metas) for `id`; on occurrence, emit "cannot construct infinite type" instead of binding. Tests: `x x`
  rejected cleanly; legitimate higher-order metas still solve.
- *Strict-positivity processor*: consume each `data`'s resolved value-constructor, walk every parameter
  type with variance, reject the data type's own type-constructor FQN in a contravariant (left-of-arrow)
  position. Tests: `data Loop(f: Function[Loop, A])` rejected; `data Box(content: Function[A,B])` and
  `data Tree(left: Tree, right: Tree)` accepted.
- *Confirm purity*: verify no mutable-cell primitive exists; record as a guard.

**M1 — Core termination effect: detect, deny, `Inf`, structural descent (first-order).**
- New fact `TerminationEffect(vfqn) ∈ {Terminating, Inf}`, inferred from the body by unioning callees'
  effects (reusing the `DirectStyleDesugarer`/`CalleeSignatures` shape). `activeFactKeys` breaks cycles:
  a value whose inference re-enters itself is *recursive* and must carry an explicit annotation; an
  annotated value's effect is **read from the annotation** (not inferred), which breaks the cycle.
- *Deny-by-default*: a cyclic value with no `Rec`/`Inf` annotation → hard error.
- *`Inf` opt-out*: legal; effect = `Inf`; propagates to direct callers via the subset-check shape.
- *Structural descent* proof for `Rec`: syntactic check that every recursive call's argument is a
  pattern variable bound to a proper sub-component of the matched scrutinee — no measure, no solver.
- *Scope limit (lifted in M2)*: a call **through a function parameter** conservatively incurs `Inf`
  (sound but restrictive), so first-order recursion (length/sum/tree-walk) works while higher-order
  combinators are temporarily `Inf`.
- Tests: structural recursion = Terminating; unannotated non-structural cycle rejected; `Inf` loop
  accepted; `Inf` propagates to a direct caller.

**M2 — Arrow effect + higher-order propagation (the load-bearing type-system piece).**
- Add a `{Terminating, Inf}` effect slot to the function type (`VPi` / the `Function` representation),
  inferred for every lambda from its body, carried through unification (join on the 2-point lattice), and
  erased before codegen. (An extension of the one-primitive-Π, *not* a fold of `Function` into `data`.)
- Higher-order functions become effect-polymorphic over their argument arrows; a call through a parameter
  incurs that parameter's arrow effect (lifts M1's conservative `Inf`). `fold`/`map` become
  Terminating-iff-their-argument-is. Unlocks the function-coloring win.
- Effect-parametric data: a `Function`-typed field carries its arrow effect as an (erased) type
  parameter, checked at construction.
- Tests: terminating `fold` over a terminating function = Terminating; the same `fold` over an `Inf`
  function = Inf; a recursive lambda stored in data then called → its `Inf` reaches the caller.

**M3 — Numeric measure `Rec[N]` (counting / non-structural recursion).**
- Partial `dec`/`half` (`dec : {N>0} Int[1,M] -> Int[0,M-1]`); descent on a numeric measure; discharge
  the `N>0` precondition at the recursive branch via `RefinementSolver` (`monomorphize/refine/`). Needs
  measure-bound refinement from the guard — start with explicit numeric guards (`if N > 0`) before
  indexed-data refinement (the one genuine dependent-typing touchpoint).
- Tests: countdown via `dec`, bisection via `half` accepted; missing base case / non-decreasing measure
  rejected.

**M4 — Mutual recursion, capture, tooling.**
- Mutual-recursion measures: a shared/lexicographic measure decreasing around the SCC (here an explicit
  reference-graph SCC pass over `ValueReference`s complements the `activeFactKeys` self-cycle case).
- Stored-lambda whose termination rides closure capture → forced `Inf` (fail-safe) with a clear message.
- IDE: surface the inferred termination effect on hover (`TypeHintIndex`) so implicit propagation is
  visible (mitigates the "invisible effect" cost of implicit propagation).

**Deferred (explicitly):** CFA to recover indirect calls the coarse bit over-rejects; WCET/resource
bounds and whether they share the measure; linearity-for-mutation.

### Surface syntax (decided): ride the `{…}` row

`Rec`/`Inf` are written in the existing effect row — `{Rec(n)} A` / `{Inf} A` — uniform with capability
effects, a minimal parser change (the brace row already exists). The crucial distinction is in the
**desugaring**: a capability entry desugars to a carrier constraint (`F[_] ~ Eff`, dischargeable at a
`run*` edge), whereas `Rec`/`Inf` desugar to the **arrow-effect channel** — there is no carrier and no
`runRec`. So `EffectSugarDesugarer` must split brace entries into two kinds: capability entries (existing
path) and termination entries (new path that annotates the arrow effect rather than adding a carrier
binder). The measure binds by **named argument** — `{Rec(n)}` references the in-scope parameter `n` it
decreases (matters at M3) — not positionally.

## Relationship to existing work

- Effects machinery (rows, `{E}` sugar, M4 subset check, carrier-polymorphism) — the substrate this
  rides on. See [[project_effects_plan]] / [[project_effects_m4]].
- `RefinementSolver` / `Int[MIN, MAX]` — discharges the `dec` precondition.
- Monomorphization keying (`BinderRoles` / `CodegenProjection`) — erases the phantom `N`.
- W4 recursion detection (`activeFactKeys`) + the `used` non-convergence backstop — the existing
  cycle detectors for the direct case.
- Cornerstones: Types Are Values (λ\*); Use-Site Verification (Sound, Not Modular).
</content>
</invoke>
