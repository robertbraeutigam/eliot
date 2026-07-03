# Effect Auto-Lift in the Checker (Type-Directed Elaboration)

Status: **IMPLEMENTED — all five steps done.** `DirectStyleDesugarer` (incl. the dot-inline) is
deleted, the effect phase is verification-only (`EffectCheckProcessor`/`EffectCheckedValue`, fed by
the `EffectUsageCollector` accounting walk), and the checker owns the lift (`EffectLifter`, the
fifth collaborator). All step-4 tests pass — the `81485de9` regression, the five new-functionality
programs — plus the step-5 extended matrix (deferral order, emergent branch rule, bind-of-a-bind,
pass-through-then-parent-lift, storage slots, annotated-vs-unannotated `let` binders, the
fail-safes, Coerce non-interference, and a compiler-track guard through a *user-defined* pipe and
guard combinator). Full parity: 986/986 suite, every example module builds, `ide.lsp` green. Docs
synced: CLAUDE.md pipeline list (effect = verification; monomorphize hosts the lift) and the
`eliot-monomorphize` skill (EffectLifter collaborator, spine/Phase-A/B, `solveAdopting`, the "no
bind decisions from declared signatures outside the checker" anti-pattern). One diagnostics
improvement beyond the plan: a doomed carrier-vs-rigid postponement at a *return boundary* now
commits an exact "Type mismatch." immediately instead of surfacing as a late carrier-kind error.
Implementation notes discovered en route:

- **Post-landing (monomorphize-review R3-1 / §3.4a, 2026-07-03)**: the two check-mode ladders
  `checkAgainst` (return boundary) and `checkAgainstSlot` (argument slot) were merged into one shared
  `Checker.resolveLadder` (+ `resolveFailureLadder`, `commitMismatch`) with the position difference
  carried by an `allowBindLift: Boolean` — `true` consults the bind-lift arm (arm 3), `false` omits it
  and turns the doomed lift shape into the eager mismatch. `checkAgainstSlot` no longer exists; the arms
  read the same, just from one place. The W2b guard-kind acceptance moved into a thin
  `resolveGuardedLadder` front (the deferred re-entry uses the bare `resolveLadder`). Behaviour-neutral;
  see docs/monomorphize-review.md round 4.
- Step-4, **callee-side carrier notion is UNFILTERED**: the plan's "true iff the binder carries
  ability constraints" was wrong for callee results — today's `CalleeInfo.resultEffectful` uses
  *unfiltered* `carrierBinders`, and that is load-bearing: `runState[S, G[_], A]`'s deliberately
  unconstrained `G` must still make a bound `val outcome = runState(...)` sequence. So
  `effectCarrier` is flagged on *every* HKT binder; the `∩ paramConstraints` filter is ambient-only.
- Step-4, **Phase-B pass-through is adoption, not contribution** (`Unifier.solveAdopting`): the
  still-flex slot is solved *directly* to the carrier-headed type — the reverse orientation only
  postpones (not a pattern), letting the spine's wrap type wrongly pin the slot ("map instead of
  flatMap" on the Abort branch shape) — and *without* recording a `Combine` candidate, which would
  reroute the enclosing result through the post-saturation upper-bounds check and starve ability
  resolution (the guard combinators' `pure` impl never folds into the reduced native).
- Step-4, **finalization can now solve metas**: `resolveUpperBounds` commits its
  definitional-equality successes (a deferred upper bound may be the only constraint grounding the
  solution's residual metas), and the post-drain pipeline drains once more after finalization so
  constraints postponed against those metas resolve instead of defaulting.
- Step-4, **lift arms are consulted before the Coerce probe** on unification failure: probing
  `Coerce` generates ability facts whose missing-implementation diagnostic is a build-failing side
  effect (the `combinePair` caveat), so `"..." : String` under a concrete `AbortCarrier[IO, String]`
  expectation must pure-wrap before any probe. The arms' guards are disjoint from every coercible
  shape (an `Int` range is neither carrier-headed nor ambient), so widening behaviour is untouched
  — the "Coerce stays before the lift" ordering note is superseded by this refinement.
- Step-4, **the let-bind rule infers its continuation**: pushing the carrier-typed expected into the
  bound body lets a still-flex tail type (`old : ?S`) wrongly unify with the whole carrier type
  (`?S := IO[String]`, corrupting `State[S]`); the body is inferred, the wrap picks `map`/`flatMap`
  from its shape (the former desugarer's continuation rule), and the wrap's result resolves against
  the expected type at the let level.

Earlier notes:

- Step-2: an ability method's own carrier binder (`printLine`'s `F`) carries *no* explicit
  `paramConstraints` entry — the owning ability is the constraint — so `CarrierKindChecker` flags
  `effectCarrier` for explicit constraint entries *or* the leading owning-ability binders (arity
  read off the ability marker, like `AbilityResolver.abilityArity`).
- Step-3, **pre-arms**: the ladder cannot literally run "unify first" for the canonical lift shape
  — `?F[String] ~ String` (a carrier-*meta* application against an under-applied rigid head) does
  not *fail* unification, it **postpones** (doomed — no injective solution; surfacing only as the
  post-drain carrier-kind error). So `EffectLifter.mustLiftBeforeUnify` /
  `mustPureWrapBeforeUnify` detect exactly that unsatisfiable shape (mirroring
  `CarrierKindChecker.unsatisfiableApplication`) and consult the arm *first*; a `Coerce` cannot
  fire on the shape either (the carrier meta cannot quote), so ladder semantics are preserved.
  Concrete carrier heads (`IO[String]` vs `String`) mismatch properly and use the ordinary
  failure-path arms.
- Step-3, **carrier connection**: `bindWrap` must `doUnify` the bind's carrier with the core's
  carrier (the one-`C` constraint `flatMap[C](f: T' -> C[R], a: C[T'])` implies) — without it the
  spliced combinator's carrier meta dangles, defaults, and dies at quoting as a `$bad-apply`.

Supersedes the signature-heuristic auto-lift in
`effect/processor/DirectStyleDesugarer.scala`, including the `.`-operator special case added in
commit `81485de9`.

## Motivation

The effect auto-lift decides, per argument, whether an effectful sub-term (type `C[T]` for an
effect carrier `C`) flowing into a callee's parameter slot must be **bound** (sequenced with
`Effect.flatMap`/`map`) or **passed through** (a carrier storage slot). Today this decision is
made *before type checking*, in `DirectStyleDesugarer`, from the callee's **declared** signature
(`CalleeSignatures.isBindPosition`).

For a generic parameter slot this is **undecidable from the declaration**: the same slot needs the
opposite answer at different instantiations. Proof by probe pair, both on `.`'s `a: A` parameter
(`def .[A, B](a: A, f: Function[A, B]): B = f(a)`):

```eliot
// A := F[String]  — must NOT bind (readLine flows into flatMap's carrier storage slot)
def echo: {Console} Unit = readLine.flatMap(line -> printLine(line))

// A := String     — MUST bind (readLine's result flows into the plain function f)
def call(f: String => String): {Console} Unit = printLine(readLine.f)
```

No pre-typechecking rule can get both right, so the phase has accreted per-shape patches, each a
structural approximation of the missing instantiation:

- `isBranchPosition` (eliminator branches, compiler-as-platform increment F),
- `isAuthorMachineryCall` (hand-written `pure`/`flatMap`/`map`/`sync` sniffing),
- the `.`-operator FQN-keyed inlining (commit `81485de9`) — which fixed the first probe and
  **regressed the second** (verified: the second probe compiles at `81485de9~1`, errors at
  `81485de9`), and does not cover a user-defined pipe with the identical shape:

```eliot
// fails today with "Higher-kinded type parameter mismatch." — same bug as `.` had, not FQN-covered
infix left below apply def |>[A, B](a: A, f: A => B): B = f(a)
def echo: {Console} Unit = readLine |> flatMap(line -> printLine(line))
```

`CalleeSignatures` is a shadow type system — signatures without instantiations — which is the
same species of anti-pattern the single-evaluator rule bans. The fix is the one the codebase
already uses for directional `Int` widening: **the decision moves into the NbE checker's check
mode, as type-directed elaboration** — the `Coerce` slot (`RefinementSolver.unifyOrCoerce`),
extended with an effect-lift arm. This is also the Use-Site Verification cornerstone verbatim: an
obligation not dischargeable abstractly (the probe pair) is deferred to the concrete use site,
where the one checker decides it exactly, per instantiation.

Verified-failing-today programs that this design makes work (the "new functionality" tests of
Step 4): the `|>` probe above, the `readLine.f` regression probe, and

```eliot
// author-lifted machinery into a pure slot — isAuthorMachineryCall leaves it alone today,
// monomorphization then rejects it; under the lift it binds and prints "lifted"
def echo: {Console} Unit = printLine(pure("lifted"))
```

## Target design

### One resolution ladder in check mode

When the checker resolves an inferred type against an expected type (the `checkAgainst` /
argument-position path), the ladder is:

1. **Unify** — pure definitional equality (unchanged). Covers all storage positions:
   `readLine` into `flatMap`'s `F[A]` slot, `.`'s `A := F[String]`, carrier-to-carrier.
2. **Coerce** — the existing `RefinementSolver.tryCoerce` (unchanged, stays before the lift so
   all current `Int`-widening behaviour is untouched).
3. **Effect-lift (bind)** — *argument positions only*: if the inferred type forces to `C[T']`
   where `C` is an **effect carrier** (see below) and `T'` unifies with the expected type,
   record a bind: the argument slot receives a fresh variable `x : T'`, and the enclosing
   application spine is wrapped `flatMap(x -> <spine>, action)` (`map` when the spine core is
   pure). The effect is never dropped — it rides the wrapping combinator's result type.
4. **Pure-wrap** — the dual: if the *expected* type forces to `C[T]` headed by the **ambient**
   carrier and the inferred pure type unifies with `T`, wrap with `Effect.pure`. (Subsumes
   today's body-level `pureWrap`; see Semantic deltas for the small extension this implies.)
5. Otherwise: the ordinary "Type mismatch." (unchanged).

The bind-lift arm never fires at a **return boundary** (a lambda body against a pure codomain,
the def body against a pure declared return) — there is no spine to hoist into, and stripping
would silently drop the effect. Those remain hard mismatches; the friendly "declared pure"
diagnostic stays in the effect phase (below).

### What counts as an effect carrier

Exactly today's notion, read at the right time:

- **Callee-side**: an instantiation meta peeled from a callee's higher-kinded,
  ability-constrained binder (`F` in `printLine : [F[_] ~ Console] String -> F[Unit]`).
  `Checker.instantiatePolymorphic` → `CarrierKindChecker.recordCarrierMetas` already gives these
  metas a `carrierKind`; it additionally records an `effectCarrier: Boolean` on
  `MetaRole.Instantiation` (true iff the binder carries ability constraints — the same
  `carrierBinders ∩ paramConstraints` filter `EffectDesugaringProcessor` uses today). A bare
  unconstrained HKT binder (`C[_, _]`) is *not* liftable, as today.
- **Ambient**: the value-under-check's own carrier binders (HKT + ability-constrained, read once
  per check from its `SignatureView` + `paramConstraints` — a sanctioned peripheral binder read,
  like `binderRoles`). After `applyTypeArgs`/`instantiateRemaining`, `TypeStackLoop` looks each
  carrier binder up in ρ and records the forced head (a `VTopDef` FQN for a concrete
  instantiation like `IO`, a meta id for a peeled one) as `CheckState.ambientCarriers`.

`isEffectCarrierHeaded(t)`: force `t`; it is `C[T']` iff the head is a meta whose role says
`effectCarrier`, or a `VTopDef`/meta in `ambientCarriers`, with a non-empty spine.

### Spine-level argument resolution (flex-slot deferral, no backtracking)

The one genuinely new mechanism. ORE applications are curried, so today `applyInferred` checks
one argument per node, committing metas left-to-right. That order is what breaks the
`readLine.f` case: checking `readLine` against `.`'s flex `?A` would greedily solve
`?A := F[String]` before `f`'s type is seen. Instead, application checking is restructured to
operate on the **whole spine** at once (`inferSpine`, decomposing nested `FunctionApplication`s
exactly like `DirectStyleDesugarer.sourcedSpine` did), with two-phase argument resolution:

- **Phase A** (left to right): infer each argument. If the parameter slot's domain forces to a
  *bare flex meta* AND the argument's type is effect-carrier-headed, **defer** it (no
  unification yet). Every other argument runs the ladder immediately.
- **Phase B** (after all args, left to right): re-force each deferred slot's domain. Now rigid →
  run the ladder (unify / coerce / bind-lift). Still flex → **prefer pass-through**: unify the
  slot with the carrier-headed type; the effectful result propagates upward and the *parent's*
  slot decides (this is how `identity(readLine)` and eliminator branches resolve — the
  `isBranchPosition` behaviour falls out with no special case).
- **Wrap**: rebuild the core application with bound slots replaced by fresh
  `ParameterReference($eff$N)` nodes, then fold the recorded binds around it, innermost `map` if
  the core's (forced) result type is pure / `flatMap` if carrier-headed, outer binds always
  `flatMap` — exactly today's `buildArguments`/`wrapBinds`, transplanted and fed exact types.
  A core result still flex at wrap time defaults to `map`; a wrong default surfaces as a loud
  type error downstream, never a silent miscompile.

Worked through the probes: `readLine.flatMap(g)` — `g`-side solves `?A := F[String]` in Phase A,
Phase B unifies `readLine` against it, no lift. `readLine.f` — `f` solves `?A := String` in
Phase A, Phase B hits the rigid mismatch, bind-lift fires. `printLine(readLine)` — rigid
`String` slot, lifts in Phase A. Hand-written `flatMap(g, readLine)` — the domain `F[A]` is an
*applied* meta, not bare-flex, so it unifies in Phase A (no deferral, no lift).

Per-argument extras of today's `applyInferred` (calc-return resolution, codomain
renormalisation, `dischargeGuardedReturn`) are preserved per-slot inside the spine loop. For a
lifted argument the dependent codomain is applied to the fresh binder's neutral, not the action
value (today all codomains are constant, so this is future-proofing, not a behaviour change).

### Machinery node assembly

Lift and pure-wrap splice `SemExpression` nodes directly (the `buildCoercedExpr` precedent — no
ORE is ever rewritten):

- `flatMap`/`map`/`pure` FQNs move from `EffectMachinery`'s privates to `WellKnownTypes`
  (`WellKnownTypes.dotOperatorFQN` is deleted — nothing else uses it).
- The combinator reference is
  `SemExpression.ValueReference(fqn, typeArguments = [C, T', R])` (ability binder first, matching
  the `[abilityParams ++ methodParams]` order `AbilityResolver.abilityArity` slices), the
  continuation a `SemExpression.FunctionLiteral($eff$N, T', core)` under a
  `VPi(T', _ => coreType)`, applied to the action.
- Because insertion happens **during** the body check, `TypeStackLoop`'s existing
  `collectAbilityRefs` walk finds the inserted ability-qualified refs and the ordinary
  `resolve-abilities` saturation pass resolves them to the carrier's `Effect` impl — the same
  path today's pre-inserted machinery takes. No new resolution machinery.
- Fresh binder names come from a `liftCounter` on `CheckState` (the established `$eff$N`
  convention; `$` is not a user identifier character).

### Let / block statements (`typeImmediateLambda`)

Blocks lower to immediately-applied lambdas upstream, and today's `desugarImmediateLambda` binds
an effectful `arg` unless the binder is annotated carrier-typed. The same rule moves into
`Checker.typeImmediateLambda`: run the ladder between the argument's inferred type and the
binder's (annotated or inferred) type; a bind rewrites to `flatMap/map(param -> body, arg)` with
the binder at `T'`; an annotation that unifies with `C[T']` keeps the plain `let` (deliberate
storage). This is what threads effects through `{ ... }` blocks — it must land with the lifter,
not after.

### The effect phase becomes verification-only

`effect/` stops transforming and keeps its two *diagnostics*, both definition-local and
signature-derivable (independent of bind decisions — sequencing never changes *which* effects a
body performs):

- **Declared-effects subset check** (`DeclaredEffectChecker`, unchanged) + **`Inf` propagation**,
  fed by a new small `EffectUsageCollector` walk (the accounting half of today's
  `CalleeSignatures`/`DirectStyleDesugarer`: per spine, callee FQN + applied count →
  `effectAbilities` gated on `resultEffectful`; carrier-headed parameter references). No
  bind/branch/machinery-sniffing logic survives.
- **The "declared pure but performs effects" fail-safe** (same walk provides body
  effectfulness), keeping today's friendly message rather than a raw mono mismatch.

The processor is renamed `EffectCheckProcessor`, its fact `EffectDesugaredValue` →
`EffectCheckedValue` (still the gate `SaturatedValueProcessor` keys off — a value failing the
checks keeps failing the build, fail-safe by construction). `DirectStyleDesugarer` and the
bind-position half of `CalleeSignatures` are **deleted**; `EffectCarriers` stays (used by both
the collector and, conceptually, the checker-side carrier read); `EffectMachinery` shrinks to
recognition (`isMachineryAbility`, `abilityNameOf`) — construction moves to the lifter.

### What deliberately does not change

- `unify` stays pure definitional equality — the lift is check-mode elaboration, never a unifier
  arm (same law as Coerce).
- `EffectSugarDesugarer` (core): the `{E...}` → `[F[_] ~ E...]` signature sugar is untouched.
- `RecursionCheckProcessor` placement and inputs: unchanged (inserted lambdas create no named
  cycles).
- Both tracks get the lift for free — it lives in the shared `Checker`/`TypeStackLoop`;
  the compiler track's pinned `Either[E]` carrier is just an ambient carrier like any other
  (the guard combinators' branch behaviour, increment F, must reproduce through the flex-slot
  deferral — pinned by tests).
- Codegen/uncurry/used are untouched: the monomorphic output for today-compiling programs is the
  same resolved-impl `flatMap`/`map`/`pure` application shape.

## Semantic deltas (intentional, all *more* accepting or better-diagnosed; nothing silently changes meaning)

1. Programs the heuristics wrongly rejected now compile: the three probe programs above, and any
   user-defined subject-last/pipe/eliminator combinator over effectful subjects.
2. `isAuthorMachineryCall` is gone: an author-lifted value (`pure(x)`, a hand-written `flatMap`
   chain) flowing into a *pure* slot now binds (correct semantics) instead of erroring downstream.
3. Pure-wrap fires wherever the expected type is ambient-carrier-headed — today only the body
   return position was wrapped. Net effect: a pure value can now be passed where the def's own
   carrier type is expected (e.g. a helper taking `F[Unit]`). Sound (the elaboration is
   type-checked), small widening; pinned by a test either way.
4. Elaboration output *shapes* can differ from today's for already-working programs (e.g.
   `printLine(identity(readLine))` binds the whole `identity(...)` call rather than `readLine`
   inside it). Behaviour-equivalent; integration tests assert behaviour, not shape.
5. Error sites improve: a genuine mismatch reports exact instantiated types at the exact
   argument, instead of "Higher-kinded type parameter mismatch." on downstream machinery.

## Implementation steps

Each step compiles and is fully green before the next; none is a workaround — steps 1–3 are the
destination's own load-bearing pieces, landed in dependency order.

### Step 1 — Spine-level application checking (behaviour-neutral restructure)

- `monomorphize/check/Checker.scala`: replace the per-curried-node `applyInferred` path with
  `inferSpine` — decompose the full application spine at the root (`infer`'s
  `FunctionApplication` case), instantiate the head once, then fold arguments through the
  existing per-argument logic (check against domain, calc-return, renormalise, guard-discharge)
  in left-to-right order. Immediately-applied-lambda heads keep routing to
  `typeImmediateLambda` (including the multi-argument `(x -> body)(a)(b)` shape).
- No ladder change, no deferral yet. Every existing test stays green — this step is pure
  enablement and must produce byte-identical diagnostics on the existing suites
  (`lang.test`, `jvm.test`, `eliotc.test`, examples).

### Step 2 — Effect-carrier bookkeeping (behaviour-neutral)

- `WellKnownTypes`: add `effectFlatMapFQN` / `effectMapFQN` / `effectPureFQN` (moved from
  `EffectMachinery`'s privates); **delete `dotOperatorFQN`** is deferred to Step 4 (its one user
  dies there).
- `MetaRole.Instantiation`: add `effectCarrier: Boolean`;
  `CarrierKindChecker.recordCarrierMetas` sets it for ability-constrained HKT binders (it
  already reads the callee `SignatureView`; it additionally consults the callee's
  `paramConstraints`).
- `CheckState`: add `ambientCarriers` (forced carrier heads) + `liftCounter`; `TypeStackLoop`
  computes ambient carriers right after `instantiateRemaining` (carrier binder names from
  `SignatureView` + `paramConstraints`, values looked up in ρ).
- Unit tests: `unify/UnifierRoleTest`-style coverage that the role flag lands on the right metas
  (e.g. `printLine`'s `F` yes, a bare `C[_,_]` no), and a `TypeStackLoop`-level assertion of
  `ambientCarriers` for a `{Console}` value instantiated at `IO`.

### Step 3 — The `EffectLifter` collaborator (lands inert)

- New `monomorphize/check/EffectLifter.scala`, the fifth checker collaborator (constructed like
  `RefinementSolver`, with exactly the primitives it needs: `force`, `freshMeta`, `doUnify`,
  `tryUnify` access, `liftCounter` state access). Owns:
  - `isEffectCarrierHeaded` (role/ambient read),
  - the argument ladder arms 3–4 (bind-lift decision incl. the speculative `T' ~ expected`
    unification, pure-wrap decision),
  - `SemExpression` assembly (`bindWrap`, `pureWrap`) per the machinery-node spec above.
- `Checker.inferSpine` gains Phase A/B deferral + bind accumulation + wrapping, all routed
  through the lifter; `Checker.checkAgainst` gains the pure-wrap arm; `typeImmediateLambda`
  gains the let-bind rule.
- **Inert by construction while `DirectStyleDesugarer` still runs**: the desugarer's output is
  already monadic, so unification succeeds at every slot and no lifter arm fires on any program
  the current pipeline accepts. The full suite stays green *unchanged*.
- Tests at this step are **pure unit tests** (no pipeline): `AnyFlatSpec` over directly
  constructed `SemValue`s/`SemExpression`s, like `unify/`'s tests — ladder decision tables
  (rigid mismatch + carrier-headed ⟹ bind; flex ⟹ defer; return-boundary ⟹ never strip;
  `Box[String]` vs `String` ⟹ plain mismatch, no lift), wrap shape (`flatMap` vs `map`
  innermost selection, type-argument order `[C, T', R]`, fresh-name threading).

### Step 4 — The pivot: checker owns the lift, the effect phase verifies

The single behaviour-changing step; everything red-to-green lands here together.

- `effect/`:
  - **Delete `DirectStyleDesugarer.scala`** (including the `.`-operator inlining) and the
    bind/branch half of `CalleeSignatures`; extract the accounting walk into
    `EffectUsageCollector` (usedEffects + body-effectfulness, per the spec above).
  - `EffectDesugaringProcessor` → `EffectCheckProcessor`; `EffectDesugaredValue` →
    `EffectCheckedValue` (body passes through untransformed; the two diagnostics stay verbatim).
    Rekey `SaturatedValueProcessor`, update `LangProcessors`, fix the doc-comment references
    (`RecursionCheckedValue`, CLAUDE.md's phase list).
  - `WellKnownTypes.dotOperatorFQN`: delete.
- Tests, all in this step:
  - **The regression test for commit `81485de9`** (verified broken on HEAD today, must pass
    here): `jvm` `ExamplesIntegrationTest` — *"bind an effectful subject dotted into a
    function-typed parameter"*:
    `def call(f: String => String): {Console} Unit = printLine(readLine.f)` with
    `main = call(s -> s)`, asserting the echoed line.
  - **New-functionality tests that fail on today's HEAD** (each was probe-verified failing):
    1. user-defined pipe chaining an ability method:
       `infix left below apply def |>[A, B](a: A, f: A => B): B = f(a)` +
       `def echo: {Console} Unit = readLine |> flatMap(line -> printLine(line))`;
    2. the dotted function-parameter regression above;
    3. author machinery into a pure slot: `def echo: {Console} Unit = printLine(pure("lifted"))`;
    4. the non-infix twin of (1) (`def pipe[A, B](a: A, f: A => B): B = f(a)`, called
       `pipe(readLine, flatMap(...))`) — proves the fix is shape-generic, not operator-plumbing;
    5. a pipe into a concrete slot (`readLine |> shout`, `shout(s: String): String`) — the bind
       direction through a user combinator.
  - **Parity**: the two `ExamplesIntegrationTest` cases from `81485de9` stay verbatim (dot-chain
    no-bind + dotted-concrete-slot bind, now via the checker); every existing effect example
    (direct-style echo, hand-written `flatMap` idempotence, blocks/`val` sequencing, `Throw`/
    `Abort`/`State` discharge helpers, `Inf`, guard combinators on the compiler track) must pass
    unmodified.
  - **Migrated shape tests**: `EffectDesugaringProcessorTest`'s rewrite-shape assertions
    (`flatMap(x -> printLine(x), readLine)` on ORE) die with the rewrite; their replacements are
    `MonomorphicTypeCheckTest` cases asserting the *monomorphic* body (resolved `Effect` impl
    call present / absent), plus the diagnostics tests kept verbatim in the renamed
    `EffectCheckProcessorTest` (subset check, `Inf` propagation, declared-pure fail-safe,
    accept-all-declared).

### Step 5 — Extended regression matrix + docs sync

- `MonomorphicTypeCheckTest` additions (checker-level, full pipeline):
  - flex-deferral order: `pair(readLine, "x")` against `(A, A)` — lift on the left arg once the
    right rigidifies the meta;
  - eliminator branch via deferral: `foldOption(o, error(msg), pure-branch)`-shaped user
    eliminator with an effectful branch (the increment-F behaviour, now emergent);
  - nested effectful arguments: `log(url(get))` (bind-of-a-bind, today's `synthesizedBind`
    parity);
  - pass-through-then-parent-lift: `printLine(identity(readLine))`;
  - carrier storage stays unbound: `runState`/`catch`/`orElse`-style helper slots
    (`p : Carrier[G, A]` positions);
  - annotated carrier `let` binder stores, unannotated binds (the block rule);
  - **fail-safes**: `printLine(box("x"))` (`Box` not a carrier — plain mismatch, no lift);
    effectful body under pure declared return still rejected with the friendly phase message;
    effectful lambda body under a rigid pure codomain still a mismatch (no strip at return
    boundaries); `{Console}`-declared body calling `forever` still rejected (`Inf` subset).
  - Coerce-before-lift ordering: an `Int`-widening argument that also happens to sit under a
    carrier-polymorphic callee still coerces (no lift interference).
  - compiler track: a compile-time guard written with a user pipe (`o |> orError(msg)`-shape)
    reduces correctly (new functionality on the compiler track).
- Docs: update `CLAUDE.md` (pipeline list: effect phase = verification; monomorphize hosts the
  lift; retire the `isBranchPosition`/dot-inline guardrail notes), the `eliot-monomorphize`
  skill (new collaborator `EffectLifter`, the spine/Phase-A-B rule, new anti-pattern: "no bind
  decisions from declared signatures outside the checker"), and this document's status line.

## Risks and mitigations

- **Phase-order sensitivity of the flex deferral**: a slot rigidified only *after* its spine
  completes misses the lift and errors late. By design this is loud (a type error naming exact
  types), never silent; genuinely ambiguous programs stay rejected. The Phase-B pass-through
  default resolves the common cases (`identity`, branches) upward.
- **Error-message churn**: argument evaluation order changes can reorder/reword existing
  failing-path diagnostics. Step 1 is required to be diagnostics-identical; Step 4 may re-pin
  expected messages in tests where the new site is strictly better.
- **Wrap-time flex core**: defaulting to `map` when the core's result is still flex can produce
  `C[C[T]]` on pathological inputs — caught by the quoter/unifier loudly. Documented; test
  pinned.
- **IDE/LSP fixtures**: `ide.lsp` embeds position-sensitive snippets; inserted nodes reuse the
  action's `Sourced` positions (as today's desugarer did), but hover/`TypeHintIndex` fixtures
  need a re-run (`./mill ide.lsp.test`).
- **Compiler track (increment F) parity**: the guard combinators' branch behaviour must
  reproduce through deferral; `CompilerMonomorphicTypeCheckProcessorTest` +
  `CompilerEitherCarrierTest` gate this in Step 4.

## Verify recipe (per step, full at Step 4/5)

```bash
./mill __.compile
./mill __.test
./mill examples.run jvm exe-jar examples/src/ -m HelloWorld && java -jar target/HelloWorld.jar
./mill ide.lsp.test
```

Plus the three probe programs from Motivation as compile-and-run checks (they are the Step-4
integration tests).
