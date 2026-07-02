# Compiler as a Full Platform

## Goal

Make the **compiler platform** a first-class, fully-checked participant in the whole compilation
pipeline — resolution, the complete ability cycle (dispatch + overlap + completeness), and reduction
of resolved method bodies — so that *compile-time code is just code*, with the same language features
and the same soundness guarantees as runtime code.

The forcing client is **effectful / ability-using type-level code**: a guard like

```eliot
def someFn(A: Type): {Throw[String]} Type = raise("…")          -- compile-time, discharged by the compiler
def head[A, MIN, MAX](xs: Seq[A, MIN, MAX]): someFn(A)
```

which requires `raise`/`pure`/`flatMap` (ability methods) to *reduce* at compile time, and requires
the `Throw[String]`/`Effect` instances the compiler resolves to be overlap- and completeness-checked
like any other ability. Neither happens today.

## Current state: the platform is half-built

> **Status note.** This section describes the *starting* state that motivated the work. As of the
> current session, stages **1a**, **A**, **B**, **C** (fold + reduction + fail-safe), and **D** (carrier
> pinning) have landed — the compiler track now *is* driven through the checker in `Platform.Compiler`, a
> `pure`/`raise` in a compiler-platform value **resolves in the compiler pool** to the compile-time
> `Effect`/`Throw[Either[String]]` instances **and reduces** (`raise("boom") ⤳ Left("boom")`, `pure(x) ⤳
> Right(x)`) via the compiler backend (`PostDrainQuoter.reduceSourced`), and a `{Throw[String]} Type`
> **effect-sugar** guard now works too — its inferable carrier is **pinned to `Either[String]`** on the
> compiler track (`TypeStackLoop.pinCompilerCarriers`), the `main`-pins-`IO` analogue. The claims below
> ("nothing ever passes `Platform.Compiler`", "compiler-platform abilities are never checked") are the
> problem statement, no longer literally true. What remains is the runtime track *consuming* that reduced
> native at type positions (deferred with **E**, since no current consumer needs it — the shipped guard
> client already reduces via the preferred-native `compilerLabel` path) and the monadic `when`/`orError`
> rewrite (**E**). See **Staging**.

The infrastructure is platform-parameterized, but only the **runtime** platform is ever *driven*
through the resolution-and-check cycle. Concretely:

- **Facts carry a `platform`.** `AbilityImplementation.Key`, `AbilityImplementationCheck.Key`,
  `ModuleAbilityOverlapCheck.Key`, `SaturatedValue`/`EffectDesugaredValue`/`OperatorResolvedValue`
  keys all take a `Platform` (default `Runtime`).
- **But ability resolution during checking is hard-wired to runtime.** The monomorphize checker
  resolves every ability through one call:

  ```scala
  // MonomorphicTypeCheckProcessor.resolveAbilityImpl
  getFact(AbilityImplementation.Key(vfqn, typeArgs))   // no platform arg → defaults to Platform.Runtime
  ```

  `AbilityImplementation.Key`'s own doc notes the compiler pool's `Effect`/`Throw[Either[String]]`
  instances are "reachable only by querying under `Platform.Compiler`" — and **nothing ever passes
  `Platform.Compiler`**. Those instances are wired up (W2a) but never resolved.

- **So compiler-platform abilities are never checked.** The overlap and completeness passes are
  triggered as a side effect of `AbilityImplementationProcessor`, which only runs when someone
  requests an `AbilityImplementation` — which only happens at `Runtime`. Compiler-platform ability
  instances get **no overlap check and no completeness check**.

- **The evaluator can't reduce ability methods at all.** `Checker.evalExpr` is `prefetchBindings +
  pure Evaluator.eval`; the pure NbE `Evaluator` does no dispatch. Ability resolution lives only in
  the post-drain `resolve-abilities` pass, which **records** method resolutions (for the backend to
  emit a call) and **injects associated *types*** (why `Combine` reduces) — it never folds a resolved
  *method* body back into a value. `pure`/`raise` are methods, so they stay stuck as `pure(x)`.

It "kind-of works" only because the compiler platform is currently used for a handful of bespoke
intrinsics that *bypass* the ability cycle: native routing for `add`/`Bool.fold`, the check-mode
`Coerce` reduction (`RefinementSolver.tryCoerce` — which *does* resolve an instance and evaluate its
`coerce` body, but only for one ability, triggered by a unify failure), `Combine`'s associated-type
injection, and the W2b discharge reading a *directly constructed* `Right`/`Left`. Each is a partial,
one-off participation of the compiler platform in the ability cycle. `Either`/effects would be hook
number five. **The accretion of these hooks is the symptom of the general mechanism being absent.**

## Architecture: two monomorphize tracks

Split the single `MonomorphicTypeCheckProcessor` into **two processors over two distinct fact types**:

- `MonomorphicValue` — the **runtime** track (as today).
- `CompilerMonomorphicValue` — the **compiler** track: the same checker, resolving in the compiler
  platform, whose output is a *reduced compile-time value* (see "The compiler backend").

Both share essentially all code — the same `TypeStackLoop`, `Checker`, `Evaluator`, `Unifier`,
collaborators — parameterized by the platform threaded into `fetchBinding` and `resolveAbility`. The
difference is only *which pool they resolve in* and *which fact each produces/consumes*.

### Why this cuts the cycle

The one-way edge is a property of **facts**, not of source visibility (see the next subsection):
compiler-track code freely *accesses* runtime-defined Eliot but always compiles it into its own
`CompilerMonomorphicValue`, so it depends only on other compiler-track facts, never a runtime one. So
the dependency edges are:

```
runtime-mono   → compiler-mono     (one way: a runtime value's type-level work)
compiler-mono  → compiler-mono     (self, different key: nested compile-time work)
compiler-mono  → runtime-mono      NEVER
```

The self-edge is **not** a new hazard: it is the same shape as today's calculated-return re-entry
into `MonomorphicValue` (`CalculatedReturnResolver.readMonomorphicReturn`), bounded by the
no-recursion cornerstone (an acyclic compiler-platform value graph) and guarded against same-key
deadlock by `activeFactKeys`. The termination gate already runs per-platform on the path to
`SaturatedValue`, so the compiler-platform value graph is guaranteed acyclic — which is exactly what
makes the `compiler-mono → compiler-mono` fixpoint terminate.

### The native-leaf boundary: access runtime Eliot, forbid runtime natives

The one-way *fact* edge is not a *source-visibility* wall. Both tracks resolve over the same shared
Eliot — the abstract base **and the user program** (application code carries no platform
representation, and, types-being-values, any `def` in it may be forced at the type level). Only the
**platform layer** stacked on that shared source differs, and with it the **native leaves** at the
bottom:

- **Shared (both tracks):** the abstract base (`lang` + `stdlib`) and the user program.
- **Compiler layer (compiler track only):** compile-time bodies + compiler native leaves — `add`,
  `Bool.fold`, the compile-time `Either`, the `Coerce`/`Combine` reductions.
- **Runtime layer (runtime track only):** the jvm target's representations + runtime native leaves —
  `printlnInternal`, the bytecode ops, `data IO`.

So compiler-track reduction **may freely access runtime-defined Eliot** — a user-provided `Coerce`
instance, a value invoked from a return type, any user function — but it **compiles that source as
compiler code**: re-resolved and re-reduced in the compiler platform (into a `CompilerMonomorphicValue`),
never borrowing the runtime track's reduction. Access is unrestricted; treatment is compiler-platform.

The single hard boundary is at the **leaves**. Every compiler-track reduction must bottom out in a
compiler-platform leaf — a compiler native, a compiler-layer body, or a fully-reduced pure value. A
name that is **concrete on the runtime platform but abstract on the compiler platform** (its only
realization a jvm-layer body or a runtime native) is a *runtime-only value*, and reaching it in a
forcing position is a **hard compiler error** — "cannot use runtime-only value `X` at compile time."
The native merge already supplies the *positive* half: it prefers the compiler-platform body/native
where one exists (`BindingMergerProcessor`'s native precedence + the `compiler`-labelled contributor
reading the compiler pool). This rule adds the missing *negative* half.

The trigger is precise — **not** "any body-less `VTopDef`": an abstract **type** constructor (`Int`,
body-less on *both* platforms) is a legitimate compile-time normal form, and an unresolved generic
binder (`VNeutral`) is a use-site-deferred obligation. The error is specifically *runtime-concrete,
compiler-abstract*.

**This is fail-safe, not cosmetic.** Today the evaluator, on a name with no reducible binding, falls
back to a stuck `VTopDef(fqn, None, …)` and continues (`NbeEvaluator.scala:37`). That is **correct for
the runtime track** — the backend emits the stuck native as a call — but in the compiler track there
is no backend, so a stuck runtime native at compile time would silently corrupt the type it was meant
to compute (a "gap that silently accepts wrong typing," which the cornerstone forbids). The compiler
track must turn that same leaf into the error above; the runtime track keeps the fallback.

Two consequences, both currently unwired:

1. **The user program must join the compiler source pool.** Today the positional program args fold
   into the *runtime* path only (`LangPlugin.scala:60`; `--compiler-path` carries only base + compiler
   layer). For compiler-mono to reach user type-level code, the program must be scanned in the
   compiler pool too — matching the stated design that each platform "unifies the base + the user
   program + its own layer independently." Only the platform *layers* stay pool-exclusive; the
   application is shared like the base.
2. **The stuck-native fallback becomes track-specific** (runtime: emit the call; compiler: the error).

### Why two fact types, not one platform-keyed fact

A single `MonomorphicValue.Key(vfqn, args, platform)` with one processor would leave "no reverse
edge" as a *discipline* — the same code could, by mistake, request the runtime key from a
compiler-platform context and reintroduce the cycle. With **two distinct fact types**, the compiler
processor's code *cannot even name* `MonomorphicValue.Key`; acyclicity is enforced **by
construction**. The shared logic lives in a platform-parameterized helper both processors call.

## The compiler backend: the end of the compiler pipeline

The runtime pipeline ends in a code generator (`used → uncurry → JVM backend`, producing bytecode).
The compiler platform's analogue does **not** generate code — its "backend" produces a **fully
reduced compile-time value**: monomorphize **+ normalize**, with the ability cycle run in-platform.
No `used`/`uncurry`/bytecode.

Its output is the reduced `SemValue`/`GroundValue` that the runtime evaluator plugs in as a "native."
This is the key move that **keeps the evaluator pure**: all ability dispatch, method-body reduction,
and checks live in `compiler-mono` (which is `CompilerIO` — it can fact-lookup and raise errors), and
`runtime-mono`'s evaluator merely *consumes* the already-reduced compiler natives. No dispatcher is
ever added to the NbE `Evaluator`; there is no second interpreter. The instance bodies stay Eliot and
the one evaluator reduces them — exactly as `RefinementSolver.tryCoerce` already does for `Coerce`,
generalized.

### It is keyed per-instantiation, not one-shot

A compile-time ability call whose dispatch type is **concrete / platform-determined** (e.g. `pure` on
the pinned `Either[String]` carrier) reduces at the value's definition. One that dispatches on a
**generic parameter** can only reduce once that generic is instantiated — at a runtime use site. So
`CompilerMonomorphicValue` is keyed by `(vfqn, concreteTypeArgs)`, exactly like `MonomorphicValue`
today: `runtime-mono`, when it instantiates a generic that performs compile-time ability work, calls
`compiler-mono` **at the concrete type arguments**. Still strictly one-way; the compiler backend is
just a per-instantiation reduced form, not a single form per definition.

## The value/type-position rule (the two platforms never meet)

The split separates the two tracks; this rule answers *which references within one value's check
resolve in which track*. The key realisation: **the two platforms never meet.** No single reduction
ever mixes the two source pools — every evaluation resolves names *either* against {base + user
program + compiler layer} *or* against {base + user program + jvm layer}, never a blend. A name that
exists in both pools (`coerce`, `add`, `pure`) is simply **compiled twice, independently**, once per
platform, for two different purposes. There is no meeting point to reconcile — only the same source
reached by two separate compilations.

**The rule is definitional, by the position's λ\* level, applied recursively:**

- A position that computes a **value** (inhabits a runtime type) resolves in the **runtime** pool.
- A position that computes a **type** (inhabits `Type`) resolves in the **compiler** pool.

At the top level this reads as "the runtime body and value parameters below → runtime; the type-stack
levels, the signature, and the return-type expression above → compiler." But the rule **recurses**:
type positions are also entered *while checking the body*, and those are compiler-platform too. The
canonical example is the checker's own implicit `Coerce`/`Combine` — a coercion synthesised on a unify
failure reconciles two *types*, so it resolves in the compiler pool even though it fires inside
`check(body, …)`. So "everything above the runtime level" means **every type position, wherever it is
reached** — not the literal top stack slots.

Because the rule is positional, **no ability is pinned to a platform.** The user-supplied `Coerce`
needs no special handling: the checker's *implicit* coercion is a type position → compiler pool, while
a user's *explicit* `coerce(x)` written in a value position is an ordinary call → runtime pool (it
runs the runtime instance — identity on today's backend, but legal, e.g. for tests). These are already
**distinct code paths** — the synthesised `RefinementSolver` coercion vs. a source-level
`ValueReference(coerceFQN)` on the checker walk — so "route by position" keeps them apart for free. The
platform is read off the position, never the callee's identity.

### What the checker actually does

The checker already structurally distinguishes the levels: `walkTypeStack` processes the type levels;
`check(body, checkSig)` processes the runtime body. Routing follows the position it is at:

- `println(x)` in a body → resolve `Console[IO]` in the **runtime** pool (value position; the jvm
  instance is the one that exists).
- `someFn(A)` in the return type → resolve/reduce in the **compiler** pool.
- an implicit `Coerce`/`Combine` reconciling two types → **compiler** pool.

Crucially, the runtime body's *values* are never reduced during checking — they are type-checked and
emitted as a `MonomorphicExpression`; their real reduction happens later at codegen against the
runtime pool. So "the runtime level is evaluated in the runtime platform" lands exactly: the checker
resolves *runtime* abilities in the runtime pool *for type-checking*, and the value computation itself
is a runtime-pool reduction at the backend. Two runtime touches, zero compiler-pool contamination —
and symmetrically for the type side. The platforms never meet.

### The implementation delta

Today ability resolution runs as one post-drain pass (`TypeStackLoop.resolveAbilities`) over the
**combined** tree — `(runtime.toSeq ++ levelExprs).flatMap(collectAbilityRefs)`
(`TypeStackLoop.scala:101`) — every ref resolved through the default-`Runtime`
`AbilityImplementation.Key`. Because type positions occur *inside* the body (the recursive case
above), a flat "levels = compiler, body = runtime" tag is **insufficient**: the platform must be
routed by the checker's *current position* as it walks — the mode it is already in (checking against
`VType` ⇒ type position ⇒ compiler; against a runtime type ⇒ value position ⇒ runtime) — not attached
post-hoc to a flattened tree. The implicit `Coerce`/`Combine` paths in `RefinementSolver` route the
same way (always type positions ⇒ compiler), which is why they need no per-ability pin.

## The ability cycle for compiler code comes for free

Once `compiler-mono` resolves abilities through `AbilityImplementation.Key(vfqn, typeArgs,
Platform.Compiler)`, the existing machinery does the rest with **no new checking code**:

- `AbilityImplementationProcessor` requests `AbilityImplementationCheck.Key(…, Compiler)`, which runs
  **completeness** (all ability methods implemented, signatures match) and eagerly triggers
  `ModuleAbilityOverlapCheck.Key(…, Compiler)` for every candidate module — **overlap** checking.
- A missing compiler-platform instance produces the same "does not implement ability" diagnostic; an
  overlapping pair produces the same overlap diagnostic — now for compile-time code.

This closes the soundness gap: an unchecked compile-time dispatch could silently pick an overlapping
instance or fail inconsistently, violating *Use-Site Verification is total-sound* and *gaps must be
fail-safe*. After this change, compile-time ability use is checked exactly as runtime use is. (The
**native-leaf boundary** subsection closes a second, independent fail-safe gap on the same principle:
a compile-time reduction that reaches a runtime-only native.)

## First client: `Throw[String]` guards, end to end

1. **Desugar / carrier pinning. ✅ done (D).** For a `{Throw[String]} Type` return on the compiler
   platform, bind the effect carrier to the concrete `Either[String]` rather than an inferred `F` (the
   compiler platform *is* the runner; its `Throw[String]` carrier is `Either[String]`, the
   `main`-pins-`IO` analogue). Implemented as `TypeStackLoop.pinCompilerCarriers`: on the compiler track
   the inferable carrier `[F[_] ~ Throw[E]]` introduced by `EffectSugarDesugarer` is solved to `Either[E]`
   before the body is checked. So `someFn`'s body is `pure[Either[String]](A)` and its return is
   `Either[String, Type]`.
2. **Compiler backend reduction. ✅ done (B/C/D).** `compiler-mono` checks `someFn`, resolving
   `Effect[Either[String]]` / `Throw[String, Either[String]]` **in the compiler platform** and **reducing
   the method bodies**: `pure → Right`, `raise → Left`. `someFn`'s reduced compiler native is `A ->
   Right(A)` (or `Left(msg)`) — with the `Right`/`Left` fold performed once the payload is a concrete
   constructor (a neutral parameter payload keeps the resolved `Either::pure`/`Either::raise` ref, folded
   at the concrete use site).
3. **Use-site.** At `head`'s monomorphization (or `b: someFn(String[])`), the type-level evaluation of
   `someFn(A)` consumes the *reduced* compiler native → `Right(String)` / `Left(msg)` — a concrete
   `Either`, no stuck `pure`.
4. **Discharge (unchanged).** The existing W2b discharge (`CalculatedReturnResolver`) reads
   `Right(t) ⤳ t` / `Left(msg) ⤳ compile error`. This path is already proven for concrete `Right`/
   `Left` (verified: `Right(A)` → the type; `Left(msg)` → the diagnostic; `fold(cond, Right, Left)`
   → the taken branch).

`when`/`orError` are then rewritten as ordinary `{Throw[String]}` functions on this path (e.g.
`orError` as `foldOption(o, raise(msg), pure)`), instead of special forms — and they reduce because
the compiler backend reduces their `pure`/`raise`.

## What this retires

The bespoke compiler-platform hooks collapse into "the compiler backend reduced it":

- `RefinementSolver.tryCoerce` (resolve `Coerce` + eval `coerce` body) — the existing precedent,
  now the general case.
- `Combine`'s associated-type injection (`injectForImpl`) — a compiler-platform ability resolution.
- Native routing for `add`/`Bool.fold` — compiler-platform leaf natives (kept as leaves, but now
  siblings of a uniform mechanism rather than the only mechanism).

These need not be migrated all at once; the new mechanism sits beside them and subsumes them
incrementally.

## Open questions / risks

- **The value/type-position rule — settled** (see the section above), now a mechanical task rather
  than an open design question. The rule is definitional: value positions → runtime pool, type
  positions → compiler pool, applied recursively; the two platforms never meet. The former "hard case"
  (a runtime body whose expected type is compile-time-computed) dissolves — there is no meeting, only
  the same source compiled twice. The remaining work is *implementation*: route `resolveAbility` (and
  the `RefinementSolver` inline paths) by the checker's current position instead of the post-hoc flat
  `collectAbilityRefs` walk (`TypeStackLoop.scala:101`). The one thing still worth a written argument:
  confirm the checker never needs to *reduce a runtime value* (as opposed to type-check it) during
  checking — if it ever did, that reduction would be the sole place the rule would need a value-side
  route; the claim is that runtime values are only reduced at codegen.
- **Carrier pinning placement — settled to `compiler-mono` at instantiation** (Increment D, done; see
  Staging). *Which* carrier is settled by the position rule (a `{Throw[String]} Type` return is a type
  position → the compiler-platform `Either[String]`; a `{Throw[String]} Int` runtime return is a value
  position → its runtime carrier). The binding is written in `compiler-mono`, not the effect desugarer:
  `core`'s `EffectSugarDesugarer` is platform-agnostic (it runs once, before the platform split) so it
  cannot make the platform-scoped decision, whereas the compiler track already holds the carrier's
  instantiation meta and solves it in place (`TypeStackLoop.pinCompilerCarriers`). The carrier value
  `Either[E]` is derived from the binder's `Throw[E]` constraint, generalizing over the error type.
- **Cost.** The compiler track is demand-driven and cached, so only *reached* compiler values are
  processed — but the fixpoint (`compiler-mono → compiler-mono`) should be watched for pathological
  fan-out; the `activeFactKeys` guard and no-recursion bound it.
- **Fact-graph shape vs. the "no processor cycles" preference.** `runtime-mono → compiler-mono` and
  the compiler self-edge must read as a clean DAG plus a single-owner bounded recursion, not a
  mutual-fact cycle between two processors. *(Reinforced by the native-leaf boundary: compiler-track
  access to runtime-defined source still produces only compiler-track facts, so the DAG holds even
  though source visibility is shared.)*

## Staging

Status as of the current session (commits on `master`, not pushed).

**1a — the `resolveAbility` seam platform-aware. ✅ done** (`fa0699af`). Widened the injected
`resolveAbility` callback with a `Platform` parameter through `MonomorphicTypeCheckProcessor`,
`TypeStackLoop`, `Checker`, `RefinementSolver`; `resolveAbilityImpl` forwards it into
`AbilityImplementation.Key(vfqn, typeArgs, platform)`. Behavior-neutral — every site passes
`Platform.Runtime`.

**A — the compiler monomorphize track. ✅ done** (`d2123c49`; lang.test 728 / jvm.test 155 green). A
value can now be type-checked wholly in the `Platform.Compiler` pool.

- `CompilerMonomorphicValue` — a **distinct** fact type (not a platform-keyed `MonomorphicValue`), so
  the compiler processor cannot name `MonomorphicValue.Key` and the `compiler-mono → runtime-mono` edge
  is impossible by construction.
- `CompilerMonomorphicTypeCheckProcessor` — runs the shared `TypeStackLoop` over a value's
  `SaturatedValue.Key(vfqn, Platform.Compiler)`, resolving every ability in the compiler pool.
- The shared checker was made **output-fact-agnostic**: `TypeStackLoop.process` returns a
  platform-neutral `Result(signature, body)`; each track's processor wraps it into its own fact.
- **The binding subsystem is platform-keyed** — this was the real work. The checker's *own* dependency
  fetches were pervasively runtime-blind (`SaturatedValue.Key` at `Checker.scala:299`; `fetchBinding →
  NativeBinding`, which had no platform), so the compiler track was checking bodies against the
  *runtime* pool. Fixed by: `NativeBinding.Key` gains a `Platform`; `BindingMergerProcessor`'s compiler
  branch consults only the native/compiler-layer suppliers and **never falls through to the runtime
  `user` pool** — the native-leaf boundary, now structural; `BindingClosure` threads the platform into
  its recursive dependency fetches; and a track `Platform` is threaded through `Checker` +
  `RefinementSolver` + `CarrierKindChecker` + `CalculatedReturnResolver` so every signature / impl-body
  fetch targets the track's pool. One documented, fail-safe deferral: the calculated-return
  `MonomorphicValue` re-entry (`CalculatedReturnResolver`) stays runtime-only — no compiler-track client
  uses it yet, and a compiler-pool-only callee yields no fact (the caller errors) rather than a silently
  wrong type.

**B — ability resolution in the compiler pool. ✅ done** (lang.test 731 / jvm.test 155 green). A `pure`/`raise`
call in a compiler-platform value now **resolves in the compiler pool** to the compile-time
`Effect[Either[String]]` / `Throw[String, Either[String]]` implementations, so its reduced form carries the
concrete `Either::pure` / `Either::raise` reference instead of the abstract `Effect::pure` / `Throw::raise`
ability method. This uses the *existing* ability machinery unchanged — the checker's `resolveAbilities`
post-drain pass drives `AbilityImplementation`, and `PostDrainQuoter.resolveIfAbility` rewrites the resolved
call to the concrete-impl reference (both already track-parameterised from Increment A).

The one fix B needed was a platform-threading bug: `TypeStackLoop.abilityArity` read the ability *marker*
signature via `OperatorResolvedValue.Key(markerFqn)` at the **default runtime platform**, so on the compiler
track (empty runtime pool) the arity query returned nothing, the method reference's type arguments were never
sliced to the ability-level prefix, the sliced args failed to quote to ground, and resolution silently never
fired. It now reads the track's `platform`. Behaviour-neutral for the runtime track (`platform` was already
`Runtime` there).

**The actual reduction of the resolved call** (`Either::raise(m)` ⤳ `Left(m)`, `Either::pure(x)` ⤳ `Right(x)`)
is *not* a bespoke checker step — it is ordinary NbE evaluation of the concrete-impl body, which the impl
carries. It therefore lands with **consumption (C)**, where the runtime track plugs the compiler native into a
type-level evaluation and reduces it there, in `SemValue`/`GroundValue` shape. (An earlier version of B
pre-reduced it in the read-back into a `MonomorphicExpression` — the runtime-codegen IR, the wrong lane for a
type-level value — and was reverted: the reduction belongs at the consumption boundary, not the checker
read-back.)

**C — compiler-pool fold + compiler-backend reduction + native-leaf fail-safe. ✅ (a), (b-reduction), (c) done**
(lang.test 736 / jvm.test 155 green).

- (a) **✅ done.** The positional program args now fold into *both* source pools, not `runtimePathKey`
  only (`LangPlugin`), so `compiler-mono` can reach user type-level code. Validated non-breaking by the
  full jvm integration suite (which drives the real `LangPlugin` path config).
- (b) **The compiler-backend *reduction* is ✅ done; the runtime-track *routing/consumption* is deferred to D/E.**
  - *Reduction (done).* `CompilerMonomorphicTypeCheckProcessor` now produces a **fully reduced** body: a
    drain-resolved ability call has its concrete-impl *body* folded in by ordinary NbE evaluation
    (`PostDrainQuoter.reduceSourced` — rewrite the resolved ability ref to its impl ref, evaluate, then
    materialise the ground constructor value; fall back to the structural quote for a compiler *function*
    with runtime params). So `raise("boom")` reduces all the way to `Left("boom")` and `pure("hello")` to
    `Right("hello")` — the reduced compile-time value the runtime track will plug in
    (`CompilerAbilityResolutionTest`, now Increments B+C). Required threading the track `platform` into
    `PostDrainQuoter` so `constructorRole`'s `UnifiedModuleValue` lookup reads the *compiler* pool (else a
    compiler-pool constructor like `Either`'s `Left`/`Right` is missed and materialisation silently drops
    to the structural fallback). Runtime track behaviour-neutral (`quoteSourced` path, `Platform.Runtime`).
  - *Routing/consumption (deferred to D/E).* The runtime track does **not yet** consume
    `CompilerMonomorphicValue` per type position, and the `resolveAbility` call sites still pass `Runtime`.
    This is deferred deliberately: it has **no consumer yet** — the shipped guard client
    (`GuardSignatureIntegrationTest`) already reduces `when`/`orError` in the runtime track through the
    *preferred-native* `compilerLabel` bindings (`fold`/`foldOption`/`data`, no ability dispatch), so the
    two-track consumption is not on any current path. It is also entangled with **D** (carrier pinning) and
    with the empty-compiler-pool test harness (a global type-position→compiler-pool flip of the implicit
    `Coerce`/`Combine` resolution would break every `runGenerator` test until the base is registered on both
    pools, with no behavioural gain — the same source compiled twice). Wire it when a consumer (E's monadic
    `orError`/`raise`) needs it.
- (c) **✅ done.** `CompilerMonomorphicTypeCheckProcessor.fetchBinding` is now track-specific: a
  runtime-concrete / compiler-abstract leaf reached in the compiler track becomes the "Cannot use
  runtime-only value X at compile time" error (gated on runtime-pool membership so a compiler-only name is
  not probed and never triggers the runtime pool's "Could not find" abort); the runtime track keeps
  `VTopDef(fqn, None, …)` for the backend to emit (`CompilerNativeLeafBoundaryTest`).

**D — carrier pinning for `{Throw[String]} Type`. ✅ done** (lang.test 743 / jvm.test 155 green). A guard
written with the `{Throw[String]}` **effect sugar** on a `Type` return now type-checks and *reduces* on the
compiler track, exactly as the explicit-`Either` B/C forms do. `EffectSugarDesugarer` turns `{Throw[String]} Type`
into an inferable higher-kinded carrier `[F[_] ~ Throw[String]]` with return `F[Type]`; on the compiler track that
carrier has no runtime value to infer it from, so it is **pinned to the compile-time `Throw` carrier `Either[E]`**
(`TypeStackLoop.pinCompilerCarriers`, the `main`-pins-`IO` analogue). Then `raise`/`pure` dispatch to the
compile-time `Throw[String, Either[String]]` / `Effect[Either[String]]` instances and reduce (`raise("empty")` ⤳
`Left("empty")`; `pure(A)` resolves to `Either::pure`, folding to `Right` once the payload is a concrete
constructor). Test: the `Increment D` cases in `CompilerAbilityResolutionTest`.

- **Placement — settled to the compiler track (not the effect desugarer).** The pin lives in `compiler-mono`
  because `core`'s `EffectSugarDesugarer` is platform-agnostic (it runs once, before the platform split) and so
  cannot make the platform-scoped decision; the compiler track already holds the instantiation meta and can solve
  it in place. The carrier value `Either[E]` is derived from the binder's own `Throw[E]` constraint (`E` = the
  constraint's first argument), so it generalizes over the error type rather than hard-coding `String`.
- **Producer, not consumer — the discharge is skipped on the compiler track.** The guard *discharge* (`Right(t)` ⤳
  `t` / `Left(msg)` ⤳ compile error) is the runtime track's *use-site* job. The compiler track *produces* the
  reduced guarded value and publishes the **undischarged** carrier signature `Either[String, Type]` for a consumer
  to evaluate and discharge; running the discharge here would replace the return with a metavariable and starve the
  body's `pure`/`raise` of its concrete carrier, blocking the reduction. So `dischargeGuardedSignature` is now
  runtime-track-only; the compiler track checks the body against the carrier signature as-is.
- **Routing/consumption still deferred to E** (as planned — "wire it when a consumer needs it"). The runtime track
  does not yet consume `CompilerMonomorphicValue` at type positions; that lands with E's first client, together
  with the both-pools test-harness change it requires.

**E — first client end-to-end:** the runtime track *consuming* the compiler track's reduced native at a type
position (the deferred (b) routing/consumption), `Throw[String]` guard integration tests (pure path → type; `raise`
path → compile error), then rewrite `when`/`orError` onto the effect path.
