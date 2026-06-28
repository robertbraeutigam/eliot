# Effectful Signatures: Compile-Time `Throw` for Guarded & Calculated Return Types

Status: **Foundation + ability bridge landed; W2b–W5 remain.** W1 — the compile-time error carrier — is **done**: it
shipped as the compiler-platform `Either` layer when the compiler became a first-class platform (CP1–CP4; see the
"compiler is itself a platform" section of `.claude/CLAUDE.md` and `compiler/README.md`). **W2a is also done**: the
carrier's `Monad`/`Throw` instances are **compiler-pool-only**, and ability-instance resolution is now platform-aware —
`AbilityImplementation.Key` (and the `AbilityImplementationCheck`/`ModuleAbilityOverlapCheck` facts it gates) carry a
`platform` marker (default `Runtime`, a no-op for every existing ability), so querying under `Platform.Compiler` reaches
the compiler-pool instances (the ability-instance analogue of CP3). What remains is the discharge (W2b) and the surface
syntax (W3–W5). The motivating consumer is length-indexed
`Seq`/`Stack` (`Seq[A, MIN, MAX]`), where operations like `head`/`pop`/`get` are only valid on sufficiently-large
collections and must reject the empty case *with a readable message* at the concrete use site.

## Goal

Let a value's **return type be an ordinary Eliot expression** that may *reject* as well as compute. Concretely,
make this legal:

```
def head[A, MIN, MAX](xs: Seq[A, MIN, MAX]): if (MIN > 0) A else error("head requires a non-empty Seq")

// with normal library combinators, the same thing reads:
def head[A, MIN, MAX](xs: Seq[A, MIN, MAX]): A when (MIN > 0) orError "head requires a non-empty Seq"
```

At a concrete monomorphic use site the compiler evaluates the signature: a sufficiently-bounded `Seq` types as
`A`; an empty one aborts compilation with the author's message. There is **no solver and no theorem prover** — the
decision is *evaluation* of ordinary code by the one NbE evaluator.

There is deliberately **no dedicated guard/precondition syntax** (no `where` clause): once any expression is
allowed in the return position, standard-library combinators (`when`, `orError`, `if`/`else`, …) cover it as
ordinary Eliot. This also generalizes the existing *calculated return* (W3/W4 of implicit-generics).

## Cornerstone framing

This is a direct consequence of **Types Are Values (λ\*)**: a signature is already an expression evaluated by the
compiler before code generation. Today that expression must reduce to a bare type constant (`VType`-kinded). The
only change is to let it be a richer computation — one that can fail with a message.

It is also the **Use-Site Verification** cornerstone applied to preconditions: an obligation that cannot be
discharged abstractly (binders left neutral) is deferred to the concrete use site, where the one checker decides
it exactly. A guard on abstract bounds stays *stuck* (deferred); a guard on concrete bounds *evaluates* (accept or
reject). No modular per-definition proof is attempted or required.

## Architecture: one front-end, a fixed compiler backend

There is no separate "type pipeline." A signature is an ordinary `Expression`, so it flows through the **same
front-end as a runtime body** — and a signature and a body differ in exactly two things:

1. **The carrier the `effect` phase binds.** A body binds an *inferable platform* carrier (`F[_] ~ Monad`, realized
   as `IO`). A signature binds the *fixed* compile-time carrier (`Either[String, _]` with the **compiler platform's**
   `Monad`/`Throw` — ordinary Eliot in the compiler layer, not a Scala intrinsic) — *every* signature, uniformly; a
   non-guarded return type `T` is `pure(T)`, not a carrier-free special case. Same phase, position-dependent carrier.
2. **Its terminal fate.** A body survives to `used → uncurry → codegen` — the *platform* backend, run by the JVM. A
   signature is *forced by the NbE evaluator and discharged* (W2) — the *compiler* backend, run by the
   compiler-as-handler. Eval-and-discharge replaces codegen.

So a signature is "normal code compiled against a **fixed** backend": the NbE evaluator + the compiler-provided
carrier + the compiler-as-handler, invariant across platforms where the runtime backend is chosen by the platform
layer. This is the λ\* phase/erasure distinction made operational, and the same "reduce a feature to ordinary core
terms before the backend" philosophy `docs/backend-portability-plan.md` states for runtime code, applied to type
expressions. The fixedness is *why* guards are layer-independent (see "Platform-independence"): the evaluator and
carrier are always compiler-supplied, so a guard evaluates in the abstract-only LSP workspace exactly as in a
linked build.

**Phase ledger** — what already treats the signature as code, vs. what this plan extends:

| phase | applies to the signature today? | this plan |
|---|---|---|
| `resolve` (value resolver) | yes | — |
| `operator` | yes — this is why `Int[a + b]` already works | — |
| `matchdesugar`, block | body only | **extend to the signature position** (W4) |
| `ability` | resolved during monomorphize / discharge | the compile-time `Monad`/`Throw` instances exist (W1 ✓); ability resolution now sees the **compiler** pool via the `platform`-marked `AbilityImplementation.Key` (W2a ✓) |
| `termination` | runtime-body graph only | must also cover the (recursion-free) type program |
| `effect` | body only (`EffectDesugaredValue` copies `runtime`) | **extend to the signature position**, fixed carrier (W3) |
| `monomorphize` | shared — the checker + evaluator already run both | discharge the return computation (W2) |
| `used → uncurry → codegen` | runtime only | n/a — type code is evaluated, never emitted |

The terminal interpreter must be **total**: the type-level program is recursion-free (Total-by-Default) and the
non-convergence backstop in `used/UsedNamesProcessor.scala` catches the `Type:Type` residual, so the fixed backend
always terminates.

## The model — a compile-time effect, not a bottom

**Every return-type position has the same type, uniformly: `{Throw[String]} Type`.** There is no "pure type *or*
effectful type" split — a signature is *always* a `{Throw[String]} Type` computation, and a non-failing one is simply
`pure(T) = Right(T)`. The signature's type is invariant; a guarded and a non-guarded signature differ only in the
*value* that computation reduces to (`Right(T)` vs. `Left(msg)`), never in its type. So the signature is a **total**
function whose result is an inspectable value: a type or an error. We model this with the existing **`Throw[String]`**
effect discharged into **`Either[String, Type]`**. The compiler is the *handler*: it runs the computation and reacts to
the result.

The key analogy — it is the runtime effect story, lifted one stage:

| position | carrier | who provides `Monad`/`Throw` instances | who runs it | result inspected |
|---|---|---|---|---|
| runtime body `{Throw[String]} A` | `EitherT[String, IO]` | jvm layer | the JVM runtime | `Either[String, A]` |
| **signature `{Throw[String]} Type`** | `Either[String, _]` (pure base — used directly, no `EitherT`/`Id`) | **the compiler platform** (Eliot layer, like jvm) | **the compiler** | `Either[String, Type]` |

The compile-time carrier is the *pure* error monad `Either[String, _]` **directly** (no transformer, no `Id` base):
`pure = Right`, `flatMap` short-circuits on `Left`, `raise = Left` — exactly as the landed
`compiler/.../Either.els` instances define them. The compiler reads back the discharged result:

- `Right(T)` → the resolved return type is `T`.
- `Left(msg)` → `compilerAbort` with `msg` as the **primary** diagnostic (author-written, not a unification dump).
- still stuck (abstract bounds) → **defer** (keep the obligation; not an error).

### Why an effect (Either), not a bottom — rationale

An earlier sketch modelled `error` as a polymorphic bottom (`error : String -> A` producing a special unquotable
`VError` sentinel). The effect/`Either` model was chosen instead because the signature stays a **total function
returning a first-class value**, which buys three things the bottom cannot:

1. **Recovery is free and uniform.** A total wrapper `tryHead : Seq[A, MIN, MAX] -> Option[A]` is *just a signature
   that `runThrow`s / `foldEither`s the same computation* and yields an `Option` type instead of letting the
   compiler error. The bottom needed a bolt-on "catchable `VError`" extension for this.
2. **Composition.** Multiple guards and transformations sequence with ordinary `flatMap`/`map`/`foldEither`; a
   bottom does not compose monadically.
3. **No special sentinel** threaded through the evaluator / quoter / unifier. The result is an ordinary ground
   `Either` value that the compiler inspects after read-back, not a stuck value that must be specially recognised
   everywhere it could surface.

## Reuse — existing machinery this rides on

Almost everything exists; the feature is mostly wiring.

- **The effect itself:** `Throw[E, F[_]]` with `raise(err: E): F[A]`
  (`stdlib/.../Throw.els`, abstract base). `runThrow(p): G[Either[E, A]]` is the runtime handler-into-`Either`
  (`jvm/.../EitherT.els`). `Either[E, A]` is abstract in the base (`stdlib/.../Either.els`) and concrete *per platform*:
  `jvm/.../Either.els` (runtime) and `compiler/.../Either.els` (compile-time, **with** the `Monad`/`Throw[Either[String]]`
  instances — W1, **done**). The compile-time carrier is `Either[String, _]` **directly** (the pure error monad), so no
  `Id` base or `EitherT` transformer is needed at compile time.
- **Branching at type level already works:** `SystemNativesProcessor.boolFoldNative`
  (`SystemNativesProcessor.scala:84-91`) selects a branch on a concrete `Bool` and **goes stuck on an abstract
  condition** — that stuck-on-abstract behaviour *is* our deferral, for free.
- **The pure→effectful seam already exists:** `Quoter.quote` returns `Either[String, GroundValue]` (pure); a
  `Left` is lifted to `compilerAbort` in `PostDrainQuoter.quoteSem` (`PostDrainQuoter.scala:60-64`). The evaluator
  (`Evaluator`) is pure/synchronous, so a diagnostic must be produced at this read-back boundary — exactly where
  the discharge of `Either[String, Type]` happens.
- **The discharge has a home:** `CalculatedReturnResolver` already implements "the return type is the result of a
  compile-time computation read back as a SemValue" (the W3/W4 back-edge). The guard is the same back-edge with an
  `Either` unwrap instead of a monomorphized-body read (see "Generalizes the calculated return").

### Generalizes the calculated return

`CalculatedReturnResolver` (`monomorphize/check/CalculatedReturnResolver.scala`) computes a value's return from a
compile-time computation:

- **Calculated return (today):** the computation is the *body* (`double`'s `x + x`); the result is read off
  `MonomorphicValue(callee, args).signature.deepReturnType`.
- **Guarded return (this plan):** the computation is the *signature's `{Throw[String]}` expression*; the result is
  `runThrow`'d to a ground `Either[String, Type]` and unwrapped.

Both are "run a compile-time computation to obtain the return type." This is why the discharge slots into that
module rather than being a foreign mechanism. Note one difference in the not-ground case: a calculated return
*errors* when its args are not ground at a point that needs them (`reportUngroundCalculatedReturn`), whereas a
guard at an abstract definition site **defers** (a stuck guard is correct, not an error) — that is use-site
verification.

## Platform-independence: the compiler platform provides the carrier

Because the compile-time carrier and its `Monad`/`Throw[String]` instances live in the **compiler platform layer** —
on the `--compiler-path` of *every* type-checking entry point unconditionally (CP2), the same way the abstract base is —
type-level guards evaluate **regardless of which runtime platform layer is present**, including the abstract-only LSP
workspace. They do *not* depend on the jvm layer's `Either`/`EitherT` being on the path: the compiler layer is always
linked for the compile-time phase and never for codegen. This is the resolution of the layering hazard, and it is the
compiler-as-platform cornerstone doing exactly its job — a guard bottoms out in the compiler layer (`Either` + its
instances) plus the `boolFold` Scala leaf, all compiler-supplied; a guard routed through a *runtime* platform-layer
`data` would only reduce once that layer was linked.

(Deferral stays safe either way: a guard only fully reduces when its bounds *and* the combinators it uses are
concrete; otherwise it stays stuck and is deferred.)

## Work items

### W1 — Compile-time Error carrier — **done (the compiler-platform `Either` layer)**

> **Superseded mechanism — now landed.** The sketch below supplies the carrier + instances as **Scala** native
> reductions in `SystemNativesProcessor`. That reimplements in Scala what the one NbE evaluator already does for ordinary
> `data`/`match`/ability instances (a parallel evaluator — the single-evaluator anti-pattern). W1 is instead realized
> by making the compiler a first-class platform with its own **Eliot** source root (the *compiler-as-platform*
> architecture — see the "compiler is itself a platform" section of `.claude/CLAUDE.md` and `compiler/README.md`; this
> carrier is its first content, and is implemented). The carrier is now ordinary concrete
> Eliot (`data Either` + `foldEither` + `implement Monad[Either[String]]`/`Throw[String, Either[String]]`) in the
> compiler-platform layer (`compiler/resources/eliot/eliot/lang/Either.els`), reduced by the existing checker, with the
> abstract `type Either[E, A]` in the `stdlib` base; `WellKnownTypes` exposes `eitherFQN`/`leftFQN`/`rightFQN` as the
> only Scala surface, for W2 to inspect by name.
>
> **One consequence the Scala-intrinsic sketch hid:** because the carrier is now a marker-scoped Eliot *layer* rather
> than a globally-visible intrinsic, its `Monad`/`Throw[Either[String]]` instances live **only in the compiler source
> pool**, and ability-instance resolution is platform-blind today (it defaults to the runtime pool). Making those
> instances reachable from the checker is therefore the first concrete task of W2 (W2a below) — the ability-instance
> analogue of CP3's `CompilerNativesProcessor`, which does the same overlay for value *bodies*. The original sketch is
> retained below for rationale.

Provide, as compiler intrinsics (alongside the Bool primitives in `SystemNativesProcessor`):

- `Either[String, _]` value representation + `foldEither`, available at compile time (promote/mirror
  `jvm/.../Either.els` so it does not depend on the jvm layer being linked).
- `implement Monad[Either[String, _]]` — `pure = Right`, `flatMap` short-circuits on `Left`.
- `implement Throw[String, Either[String, _]]` — `raise = Left`.
- The pure base `Id` + `Monad[Id]` promoted out of `examples/src/EffectsTestable.els` into a place the compiler can
  always see (only needed if the carrier is expressed as `EitherT[String, Id]` rather than `Either[String, _]`
  directly; prefer `Either[String, _]` directly to avoid the transformer).

These are input-less compiler constants, wired exactly like `boolFoldFQN` et al.
(`SystemNativesProcessor.produceNativeBinding`).

### W2 — Reach the carrier instances, then discharge

**W2a — make the compiler-pool carrier instances reachable (the ability analogue of CP3). — done.** The carrier's
`Monad[Either[String]]` / `Throw[String, Either[String]]` instances live in the compiler pool, but ability-instance
resolution was platform-blind: `AbilityImplementation.Key` carried no marker and `AbilityImplementationProcessor`
queried `UnifiedModuleNames` at the default `Platform.Runtime`, so a checker running on a runtime-marker value could not
see them. The `platform` marker is now threaded through ability-instance resolution exactly as CP3 threaded it through
`ValueResolver` for value bodies: `platform` was added to `AbilityImplementation.Key` — *and* to the two facts it gates,
`AbilityImplementationCheck.Key` and `ModuleAbilityOverlapCheck.Key`, since the completeness/overlap checks must read the
same pool — all defaulting to `Runtime` (a no-op for every existing ability), and the three processors
(`AbilityImplementationProcessor`, `AbilityImplementationCheckProcessor`, `ModuleAbilityOverlapCheckProcessor`) now read
every `UnifiedModuleNames`/`OperatorResolvedValue` under `key.platform`. Querying under the **compiler** marker resolves
the carrier's instances; under `Runtime` they are absent. This is the ability-instance overlay that CP3's
`CompilerNativesProcessor` is for value bodies; without it the discharge below would have no `flatMap`/`pure`/`raise` to
run. *Alternative considered:* check the whole signature sub-expression under the compiler marker; rejected because the
signature shares binders with the body (checked under runtime), so a hard marker split is more invasive than overlaying
just the instances. Leaf test (`ability/PlatformScopedAbilityResolutionTest`): a compiler-pool-only `implement` resolves
under the compiler marker and is **absent** under the runtime marker (dual-pool, sharing the abstract ability + type,
exactly as the carrier shares the abstract `Throw`/`Either` base).

**W2b — the discharge/unwrap (handler) in `CalculatedReturnResolver`.** At the point the checker reads a value's return
type, if the return-type computation is on the compile-time Error carrier, force it to a ground `Either[String, Type]`
and:

- `Right(T)` → return `T` as the SemValue return type.
- `Left(msg)` → `compilerAbort(at.as(msg))` (author message is primary).
- not ground (stuck) → **defer**: leave the obligation for the use site (do **not** emit the calculated-return
  ungrounded error here).

Fail-safe: every monomorphic signature is read back; a guard that is still stuck where a concrete type is
*required* hard-errors via the existing `PostDrainQuoter` "Cannot resolve type." path. There is no silent `Type`
fallback (`PostDrainQuoter` header invariant).

### W3 — Route the signature position onto the compile-time carrier

The body of a function desugars its `{E}` row onto an *inferable runtime* carrier `F` (effect desugaring). The
**return-type position** must desugar onto the *fixed* compile-time `Either[String, _]` carrier instead. This is
the one genuinely new piece of wiring and the precise form of the "staging" separation:

- signature/return-type position → **CompileError** carrier (`Either[String, _]`), handled by the compiler;
- body position → runtime carrier `F`, handled by the platform runtime.

They are different expressions on different carriers by construction, so `{Throw[String]}` of the type computation
never enters the value's runtime effect row. This routing is **uniform**: the return-type position is *always*
`{Throw[String]} Type`, so a bare `Type` is `pure`-lifted (`Right(T)`) like any other computation — non-guarded
signatures are *observationally* unchanged (they still resolve to `T`), not a separate carrier-free path. (Until W3
lands, the W2-first step opts in per signature by writing an explicit `Either[String, Type]` return; W3 is what makes
the carrier universal, so *all* signatures — bare or guarded — flow through it.)

Touch points: the effect/`core` desugaring that handles signature effect rows (sibling of `EffectSugarDesugarer`),
and the checker's expectation for the return slot. **This is a bigger change than it looks:** effect desugaring
today rewrites only the body (`EffectDesugaringProcessor` does `value.copy(runtime = …)`; the signature is read
solely to pick the *body's* carrier). Extending it to the signature changes what `SignatureView.returnType` holds —
no longer a plain type but a discharged `Either[String, Type]` *computation* — which ripples through everything that
reads the return position (`SignatureView`, `BinderRoles`' representation/relevance analysis, `saturate`). W2's
discharge is what restores a plain `Type` for the rest of the checker; downstream readers must either run after the
discharge or tolerate the un-discharged return.

### W4 — Any expression in return position

Two parts, both realizing "the signature is just code" at the front-end:

1. **Parser.** Lift the restriction on `Expression.typeParser` so the return position parses as a normal expression
   (infix operators, string literals, `if`/`else`, blocks, `match`, application). This is the λ\* cornerstone at the
   grammar level: type position = value expression. Expect grammar-ambiguity wrinkles around the `[]`-vs-`()` call
   forms and operators inside type arguments — the reason the parser is restricted today — to be resolved here.
2. **Desugar phases on the signature position.** Extend `matchdesugar` and block desugaring to run on the
   signature/return-type sub-expression, not only the body, so a `match`/block that now parses in a signature lowers
   to core exactly as in a body (the phases must recurse into the type-annotation position). `resolve` and
   `operator` already do this; this item closes the gap for the remaining expression-desugaring phases. (`effect`
   desugaring of the signature is W3.)

There is **no dedicated guard syntax**. Guards/preconditions are expressed entirely with standard-library
combinators (W5) — `A when (MIN > 0) orError "…"`, `if (MIN > 0) A else error("…")` — so the parser change plus
uniform desugaring is all that is needed at the surface.

### W5 — Stdlib guard combinators

Ordinary total Eliot, built so they bottom out in **compiler-supplied primitives** (the `boolFold` Scala leaf + the
compiler-platform `Throw`/`Either` layer), keeping stdlib guards layer-independent:

- `error[A](msg: String): {Throw[String]} A` — `raise(msg)`.
- `when[A](a: A, cond: Bool): Option[A]` — `fold(cond, some(a), none)` (or an `Either`-direct variant; see the
  layer note — prefer the intrinsic-only path for stdlib).
- `orError[A](o: Option[A], msg: String): {Throw[String]} A` — `foldOption(o, error(msg), x -> x)`.
- `orElse`, etc., as needed.

These are dual-use: the same functions work on real `Option`/`Either` values at runtime, because types are values.

## Sequencing (incremental — validate the hard part first)

W1 (the compile-time carrier) and W2a (the ability-resolution platform bridge) are **done**. Sequencing picks up at W2b:

1. **W2b** — the discharge/unwrap (handler) in `CalculatedReturnResolver`, with `when`/`orError` written to return
   `Either[String, Type]` *explicitly* (plain total functions, no effect row, no signature desugaring). The carrier's
   `Monad`/`Throw[Either[String]]` instances are now reachable from the checker (W2a), so the discharge can `flatMap`/
   `raise`/`pure` over the compile-time `Either` by querying ability resolution under `Platform.Compiler`. This already
   gives erroring guards in near-normal code and exercises the whole discharge path end to end. **Leaf test:** a
   return-type expression evaluating to `Left("msg")` aborts with exactly `msg`; one evaluating to `Right(T)` types as
   `T`; one stuck on an abstract bound defers (no error).
2. **W3** — route the signature position through effect desugaring onto the fixed carrier, enabling direct-style
   `{Throw[String]}` signatures.
3. **W4** — parser unrestriction + extend `matchdesugar`/block to the signature position (any expression in return
   position).
4. **W5** — flesh out the combinator vocabulary.

## Guarantees

- **Deferral:** abstract bounds ⟹ `boolFold` stuck ⟹ the `Either` computation is not ground ⟹ deferred. Concrete
  bounds ⟹ grounds to `Right`/`Left`. Same shape as the existing use-site obligations.
- **Fail-safe:** a guard cannot be silently dropped — the signature is always read back, and a stuck guard at a
  required site hard-errors (no `Type` fallback). A `Left` always becomes a diagnostic.
- **Termination:** the carrier code is ordinary recursion-free Eliot (Total-by-Default), and the non-convergence
  backstop in `used/UsedNamesProcessor.scala` covers the residual `Type:Type`/Girard divergence.
- **No runtime leakage:** the `{Throw[String]}` lives only on the signature computation (a distinct expression on
  the compile-time carrier); the runtime body and its effect row are unaffected; the `Either` never reaches
  codegen (W2 discharges it to a plain `Type`, or aborts).

## Files

- `compiler/resources/eliot/eliot/lang/Either.els` — W1 (**done**): the compile-time carrier `data Either` +
  `foldEither` + the `Monad`/`Throw[Either[String]]` instances. Abstract `type Either` lives in `stdlib/.../Either.els`;
  the runtime carrier stays in `jvm/.../Either.els`.
- `module/fact/WellKnownTypes.scala` — `eitherFQN`/`leftFQN`/`rightFQN` (**done**); add an `error` FQN for W5 if pinned.
- `ability/fact/AbilityImplementation.scala`, `ability/fact/AbilityImplementationCheck.scala`,
  `ability/fact/ModuleAbilityOverlapCheck.scala` + their processors
  (`AbilityImplementationProcessor`/`AbilityImplementationCheckProcessor`/`ModuleAbilityOverlapCheckProcessor`) — W2a
  (**done**): `platform` marker threaded through ability-instance resolution; default `Runtime`, the carrier instances
  resolve under `Compiler`. Leaf test: `ability/PlatformScopedAbilityResolutionTest`.
- `monomorphize/check/CalculatedReturnResolver.scala` — W2b (the discharge/unwrap step).
- `effect/processor/EffectDesugaringProcessor.scala` + `core/processor/EffectSugarDesugarer.scala` — W3 (route the
  signature position onto the fixed carrier); ripples to `operator/.../OperatorResolvedExpression.scala`
  (`SignatureView`) and `saturate/fact/BinderRoles.scala`, which read the return position.
- `ast` parser (`Expression.typeParser`) + `matchdesugar`/block processors — W4 (full expressions in return
  position, and the desugar phases extended to the signature sub-expression).
- `stdlib/.../` — W5 (`error`, `when`, `orError`, `orElse`). The `Either`/`foldEither` carrier promotion is already
  done (W1); no `Id` base is needed since the compile-time carrier is `Either[String, _]` directly.

## Deferred follow-ons

- **Recovery wrappers** (`tryHead : Seq[A, MIN, MAX] -> Option[A]`) — already *enabled* by the model (signature
  `runThrow`s the computation and yields an `Option` type), to be exercised and tested once W2–W3 land.
- **Richer error payloads** than `String` — `Throw[E]` is already generic in `E`; a structured diagnostic type
  (with source spans / suggestions) could replace the bare message.
- **Definition-site surfacing** of latent partiality (the use-site-verification trade-off): the IDE surfaces a
  guard at the definition by *probing* the signature with sample bounds (generators + probing; see
  `docs/ide-type-hints.md`), since the precondition is computed, not declared.
