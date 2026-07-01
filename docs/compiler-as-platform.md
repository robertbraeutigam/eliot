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

## The per-position platform rule (the real crux)

The split cleanly separates the two tracks but does **not** by itself answer: *within one value's
check, which references resolve in which track?*

Because types-are-values, a runtime value's **type-stack levels** are compile-time while its
**runtime body** is runtime — and, more subtly, the body's *expected types* are compile-time even
though its *values* are runtime. Today, ability resolution runs as a single post-drain pass
(`TypeStackLoop.resolveAbilities`) over the **whole** SemExpression tree (levels **and** body
together), with no per-reference platform tag.

**Working rule:** the platform is a property of the *position*.

- A reference in a **type-computing position** (a type-stack level, or a type being evaluated to
  check against — anything whose result inhabits the "type" role) resolves in the **compiler** track.
- A reference in a **value-producing position** (the runtime body's actual values) resolves in the
  **runtime** track.

The checker already structurally distinguishes these: `walkTypeStack` processes the type levels;
`check(body, checkSig)` processes the runtime body. The design work is to **thread a platform onto
each ability reference** as it is collected (`collectAbilityRefs`) — tagging refs discovered in
type-computing positions as compiler-platform — and route `resolveAbilities` to the correct track per
ref. The hard case to specify precisely: a runtime body whose **expected type is itself computed by
compile-time abilities** (the guard case — the body checks against a type produced by reducing
`someFn(A)`); the type flows compiler-track, the value flows runtime-track, and they meet at the
`check`. Pinning this rule down is the core of the design, and everything else is plumbing.

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

1. **Desugar / carrier pinning.** For a `{Throw[String]} Type` return on the compiler platform, bind
   the effect carrier to the concrete `Either[String]` rather than an inferred `F` (the compiler
   platform *is* the runner; its `Throw[String]` carrier is `Either[String]`, the `main`-pins-`IO`
   analogue). So `someFn`'s body is `pure[Either[String]](A)` and its return is `Either[String,
   Type]`.
2. **Compiler backend reduction.** `compiler-mono` checks `someFn`, resolving `Effect[Either[String]]`
   / `Throw[String, Either[String]]` **in the compiler platform** (now overlap/completeness checked)
   and **reducing the method bodies**: `pure → Right`, `raise → Left`. `someFn`'s reduced compiler
   native is `A -> Right(A)` (or `Left(msg)`).
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

- **The per-position rule precisely.** Specify exactly which positions are compiler-track, especially
  a runtime body whose expected type is compile-time-computed. This is the one genuinely hard piece.
  The native-leaf boundary narrows it: since both tracks see the same source, the rule is purely
  *position → track*, and every type-position reduction must bottom out in compiler leaves (else the
  fail-safe error fires); the meeting point at `check` still needs pinning down.
- **Carrier pinning placement.** Whether the `{Throw[String]} Type → Either[String]` carrier binding
  happens in the effect desugarer (must then be platform-scoped, compiler-only, so runtime `parseBad`
  keeps its polymorphic carrier) or in `compiler-mono` at instantiation. The discriminator is stable:
  the carrier wraps `Type` (a compile-time-only payload).
- **Cost.** The compiler track is demand-driven and cached, so only *reached* compiler values are
  processed — but the fixpoint (`compiler-mono → compiler-mono`) should be watched for pathological
  fan-out; the `activeFactKeys` guard and no-recursion bound it.
- **Fact-graph shape vs. the "no processor cycles" preference.** `runtime-mono → compiler-mono` and
  the compiler self-edge must read as a clean DAG plus a single-owner bounded recursion, not a
  mutual-fact cycle between two processors. *(Reinforced by the native-leaf boundary: compiler-track
  access to runtime-defined source still produces only compiler-track facts, so the DAG holds even
  though source visibility is shared.)*

## Staging

1. Introduce `CompilerMonomorphicValue` + the platform-parameterized shared checker helper; make
   `resolveAbilityImpl` platform-aware. (No behavior change yet — runtime still resolves at runtime.)
2. **Native-leaf boundary wiring.** Fold the user program into the compiler source pool (`LangPlugin`
   — the positional program args currently land only in `runtimePathKey`), so compiler-mono can access
   user type-level code; and make the stuck-native fallback track-specific — a runtime-concrete /
   compiler-abstract leaf in the compiler-track evaluator becomes the "runtime-only value at compile
   time" error, while the runtime track keeps `VTopDef(fqn, None, …)` for the backend to emit.
3. Thread the platform onto ability references by position (`collectAbilityRefs` +
   `resolveAbilities`), implementing the per-position rule. Compiler-track refs now resolve in
   `Platform.Compiler` and trigger the checks.
4. Add method-body reduction: after a compiler-track resolution, fold the resolved instance body via
   the one evaluator into the value (generalize `tryCoerce`), to fixpoint via the existing drain.
5. Carrier pinning for `{Throw[String]} Type`.
6. First client end-to-end: `Throw[String]` guard integration tests (pure path → type; `raise` path
   → compile error), then rewrite `when`/`orError` onto the effect path.
