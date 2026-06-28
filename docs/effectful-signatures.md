# Effectful Signatures: Compile-Time `Throw` for Guarded & Calculated Return Types

Status: **Foundation + ability bridge + discharge landed; surface vocabulary/syntax (G1–G3) remain.** W1 — the compile-time error carrier — is
**done**: it shipped as the compiler-platform `Either` layer when the compiler became a first-class platform (CP1–CP4;
see the "compiler is itself a platform" section of `.claude/CLAUDE.md` and `compiler/README.md`). **W2a is also done**:
the carrier's `Monad`/`Throw` instances are **compiler-pool-only**, and ability-instance resolution is platform-aware —
`AbilityImplementation.Key` (and the `AbilityImplementationCheck`/`ModuleAbilityOverlapCheck` facts it gates) carry a
`platform` marker (default `Runtime`, a no-op for every existing ability), so querying under `Platform.Compiler` reaches
the compiler-pool instances (the ability-instance analogue of CP3). **W2b — the discharge — is now done**: a return-type
expression whose value is on the `Either[String, _]` carrier is recognised at the kind check and discharged by
`CalculatedReturnResolver` — `Right(t)` ⤳ the plain type `t`, `Left(msg)` ⤳ `compilerError(msg)`, a guard stuck on
abstract bounds is deferred to the body. This works with guards written explicitly against the carrier (e.g.
`fold(cond, Right(t), Left("msg"))`); what remains is the **surface vocabulary and syntax**, refolded (see "The
remaining work, folded") into **G1** — guard combinators (`error`/`when`/`orError`) on the carrier, usable in
application form with no auto-lift; **G2** — the parser/desugar unrestriction so guards read as
`A when (MIN > 0) orError "…"`; and **G3** — the deferred opt-in return auto-lift for raw `if/else` direct style. The
motivating consumer is length-indexed `Seq`/`Stack` (`Seq[A, MIN, MAX]`), where operations
like `head`/`pop`/`get` are only valid on sufficiently-large collections and must reject the empty case *with a readable
message* at the concrete use site.

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
| `matchdesugar`, block | body only | **extend to the signature position** (G2 — only for raw `if`/`else`/`match`/blocks; combinator guards need no new desugaring) |
| `ability` | resolved during monomorphize / discharge | the compile-time `Monad`/`Throw` instances exist (W1 ✓); ability resolution now sees the **compiler** pool via the `platform`-marked `AbilityImplementation.Key` (W2a ✓) |
| `termination` | runtime-body graph only | must also cover the (recursion-free) type program |
| `effect` | body only (`EffectDesugaredValue` copies `runtime`) | **opt-in** lift of the signature position onto the fixed carrier (G3, deferred — only raw `if`/`else`; a pure return and a combinator guard are left untouched) |
| `monomorphize` | shared — the checker + evaluator already run both | discharge the return computation (W2) |
| `used → uncurry → codegen` | runtime only | n/a — type code is evaluated, never emitted |

The terminal interpreter must be **total**: the type-level program is recursion-free (Total-by-Default) and the
non-convergence backstop in `used/UsedNamesProcessor.scala` catches the `Type:Type` residual, so the fixed backend
always terminates.

## The model — a compile-time effect, not a bottom

A return-type position that *can* fail is a **`{Throw[String]} Type`** computation on the `Either[String, _]` carrier: a
guarded signature reduces to `Right(t)` (the type) or `Left(msg)` (a rejection), and the compiler is the *handler* — it
runs the computation and reacts to the result. We model this with the existing **`Throw[String]`** effect discharged
into **`Either[String, Type]`**.

> **Lift is opt-in, not universal (corrected).** An earlier sketch made *every* return uniformly `{Throw[String]} Type`,
> `pure`-lifting even a bare `Int` to `Right(Int)`. That was rejected for two concrete reasons found while building it:
> (1) it is *not* how the runtime body is handled — `EffectDesugaringProcessor` pure-wraps a body **only** when the
> declared return already rides an effect carrier `F[...]` (`returnIsCarrierBinder`); a plain `def f: Int = 5` body is
> left untouched. So "handle the type-level return exactly like a runtime body" means a **conditional** lift, not a
> universal one. (2) A universal `Right(T)` makes *every* value hard-depend on the Either carrier being linked
> (a spike broke 107 tests with "Name not defined." because minimal sources don't link Either). So a return is on the
> guard carrier **iff it actually rides it** — i.e. it uses a `{Throw}`/guard combinator. A pure return stays the plain
> type `T`, which the discharge already passes through unchanged (`dischargeGuardedReturn` no-ops on a non-`Either`
> value). This is the same opt-in the body uses (`{E}` row ⟹ carrier; no row ⟹ plain), applied to the return.

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

**W2b — the discharge/unwrap (handler) in `CalculatedReturnResolver`. — done.** At the point the checker reads a value's
return type, if the return-type computation is on the compile-time Error carrier, force it to a ground
`Either[String, Type]` and:

- `Right(T)` → return `T` as the SemValue return type.
- `Left(msg)` → `compilerError(at.as(msg))` then abort (author message is primary).
- not ground (stuck) → **defer**: leave the obligation for the use site (do **not** emit the calculated-return
  ungrounded error here).

Landed in `CalculatedReturnResolver` (it hosts this beside the calculated return because both are "run a compile-time
computation to obtain the return type"), with four hook points threaded through the checker:

1. **Kind acceptance** (`isGuardCarrier`, called from `Checker.check`). A guard's *value* types as `Either[..]`, not
   `Type`, so the signature/return-position kind check would reject it (`Either[..]` ≠ `Type`). When a `Type` kind is
   expected and the inferred type is `eitherFQN`-headed, the check *accepts* it as a guarded type instead of unifying;
   the value is discharged separately. (A fully-applied type constructor `Either[String, Int]` has kind `Type`, so it is
   unaffected — only a guard *value* is recognised.) A `sawGuardReturn` flag on `CheckState` records that this fired.
2. **Callee signature discharge** (`dischargeGuardedSignature`, called from `TypeStackLoop.processIO`). The
   type-argument-applied signature's return is descended to (peeling value-parameter `VPi` arrows — a guard depends only
   on the now-concrete type parameters) and discharged: `Right(t)` rebuilds the arrows over the plain type `t` (so the
   body checks against `t` *and* the published `MonomorphicValue.signature` is `t` — the `Either` never reaches
   codegen), `Left` aborts, and a guard *stuck* on abstract bounds (recognised via `sawGuardReturn`) is **deferred to the
   body** — its return becomes a fresh metavariable the body solves, exactly the calculated-return treatment, fed to the
   same `failOnUndeterminedCalculatedReturn` post-drain fail-safe. This is what restores a plain `Type` return for the
   rest of the checker.
3. **Applied read** (`dischargeGuardedReturn`, called from `Checker.applyInferred`). When a *caller* applies a guarded
   function, the renormalised codomain is discharged before it becomes the call's type.
4. **By-name read** (`dischargeGuardedReturn`, called from `Checker.infer`'s `ValueReference`). A *complete* guarded
   value read by name (`def y: Bar = foo`) is discharged; a guarded function read unapplied stays a `VPi`/`VLam`.

Fail-safe: every monomorphic signature is read back; a guard that is still stuck where a concrete type is *required*
hard-errors via the existing `PostDrainQuoter` "Cannot resolve type." path. There is no silent `Type` fallback
(`PostDrainQuoter` header invariant). Leaf tests in `MonomorphicTypeCheckTest` ("effectful-signatures discharge (W2b)"):
`Right(t)` types as `t` (and publishes `t` as the signature); `Left("msg")` aborts with exactly `msg` (at the callee and
through a caller / generic intermediate); a guard stuck on an abstract bound defers with no error.

## The remaining work, folded: combinators first, the lift last

The original W3 (route the signature through effect desugaring) / W4 (parser) / W5 (combinators) split is **refolded**
around one realization: **a guard written as a combinator call needs no auto-lift at all.** A return like
`orError(when(A, MIN > 0), "…")` is an ordinary function application that the NbE evaluator *reduces* to `Right(t)` /
`Left(msg)`; the W2b discharge then handles it. The combinators encapsulate the carrier (`pure`/`raise`/`fold`); the
caller's signature is just a call. So the **combinators are the surface**, and the signature-position **auto-lift is
only needed for *raw* `if/else` direct style** — and is deferred (it has a real blocker, see G3). The folded steps:

### G1 — Guard combinator vocabulary on the carrier (the surface)

Ordinary total Eliot that bottoms out in the carrier primitives (`fold` leaf + the compiler-platform `Either` /
`Option`). These *evaluate* to `Right`/`Left`; **no auto-lift, no carrier pinning** — the return is a plain call whose
value the discharge reads.

- `error[A](msg: String): Either[String, A]` — `Left(msg)` (the carrier's `raise`).
- `when[A](a: A, cond: Bool): Option[A]` — `fold(cond, Some(a), None)`.
- `orError[A](o: Option[A], msg: String): Either[String, A]` — `foldOption(o, Left(msg), v -> Right(v))`.
- `orElse`, etc., as needed.

**Layering.** Guards are checked on the platform the checker reads its signatures from (currently the *runtime* pool —
`MonomorphicTypeCheckProcessor` keys `SaturatedValue` at the default `Runtime` marker), so the vocabulary must resolve
there *and* reduce at compile time. That is exactly the carrier's own layering: abstract `def` signature in `stdlib`
(signature-only is allowed in the base), concrete bodies in **both** `jvm` (runtime) and the **compiler** layer
(compile-time) — like `Either`/`foldEither`. `Option` is concrete only in `jvm` today, so the compiler layer needs its
own `Option` carrier (a CP4-style promotion, mirroring `Either.els`) for `when`/`orError` to reduce at compile time.
These are **dual-use**: the same functions work on real `Option`/`Either` at runtime, because types are values.

`error`'s return is written on the **fixed** carrier `Either[String, A]` directly, not `{Throw[String]} A`: the
compile-time carrier is fixed, so its `pure`/`raise` are the known `Right`/`Left` — no generic `F` to pin. A generic
`{Throw[E]}` form (which *would* need carrier pinning at the return slot) is a later generalization, not on this path.

### G2 — Parser + desugar in the return position (the syntax)

`fold(cond, Right(t), Left(m))` and `orError(when(t, c), m)` already *parse* (application + string literals — the W2b
tests prove it). What does **not** parse in the return position is **infix operators** (`MIN > 0`, and `when`/`orError`
used infix) and **`if`/`else`** — `Expression.typeParser` is the restricted `effectfulTypeParser or typeAtom`. G2 lifts
that toward a full expression so `A when (MIN > 0) orError "…"` reads, and extends `matchdesugar`/block to the
return-type sub-expression so `if`/`else`/`match`/blocks there lower to core exactly as in a body (`resolve`/`operator`
already recurse into it). The motivating `MIN > 0` also needs a compile-time integer-comparison leaf (a `<`/`>` native,
the analogue of the existing `add` leaf). Expect the `[]`-vs-`()` and operators-in-type-arguments ambiguity (the reason
the parser is restricted) to be the work here.

### G3 — The opt-in return auto-lift (raw `if/else` direct style) — deferred

Only `if (MIN > 0) A else error("…")` written *directly* (not via combinators) needs the signature-position auto-lift:
run the return-type expression through the **same** `DirectStyleDesugarer` as a body, **opt-in** (a pure return stays
`T`; only a return that rides the carrier is lifted) and **downstream-pinned** (the inserted machinery's carrier is
fixed by the return-slot expectation). Two blockers make this last, not first:

1. **A spurious-coupling trap (resolved by opt-in).** A *universal* `Right(T)` wrap broke 107 tests by making every
   value depend on the linked Either carrier. Opt-in (lift only an actual guard) avoids it — and matches the body lift,
   which is itself conditional (`EffectDesugaringProcessor` pure-wraps only a `returnIsCarrierBinder` return).
2. **The branch-lift gap (genuinely new).** `if/else` lowers to `fold(cond, thenE, elseE)`, a *selection*. The current
   `DirectStyleDesugarer` treats a `fold` branch as a `flatMap` **bind** position, so an effectful branch would be
   *sequenced* (both branches executed) instead of *lifted* (each branch placed on the carrier, one selected). This is a
   pre-existing limitation — effectful `if/else` in a **body** is equally unhandled (no test exercises it) — so closing
   it fixes the runtime auto-lift *and* unlocks type-level guards together, keeping the two identical. Until then,
   combinators (G1) cover every guard the language needs; the bare-`if/else` sugar is pure ergonomics on top.

## Sequencing (combinators first, the lift last)

W1 (the compile-time carrier), W2a (the ability-resolution platform bridge), and W2b (the discharge) are **done**.

1. **G1 — combinator vocabulary** (`error`, `when`, `orError`) on the compiler/runtime `Either`/`Option` carriers, with
   a compiler-layer `Option`. Testable now in **application form** (`def head[C: Bool]: orError(when(t, C), "…") = …`)
   — no parser change — exercising the discharge through real vocabulary, not the raw `Right`/`Left` the W2b leaf tests
   use. This is the first coded step.
2. **G2 — parser + desugar** in the return position (infix, `if`/`else`, `match`, blocks) + the compile-time
   `<`/`>` comparison leaf, so the guard surface reads as designed (`A when (MIN > 0) orError "…"`).
3. **G3 — the opt-in return auto-lift + branch-lift**, last, fixing the runtime body lift and the type-level return
   together so raw `if (MIN > 0) A else error("…")` works without combinators.

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
- `module/fact/WellKnownTypes.scala` — `eitherFQN`/`leftFQN`/`rightFQN` (**done**); add FQNs for G1 combinators if pinned.
- `ability/fact/AbilityImplementation.scala`, `ability/fact/AbilityImplementationCheck.scala`,
  `ability/fact/ModuleAbilityOverlapCheck.scala` + their processors
  (`AbilityImplementationProcessor`/`AbilityImplementationCheckProcessor`/`ModuleAbilityOverlapCheckProcessor`) — W2a
  (**done**): `platform` marker threaded through ability-instance resolution; default `Runtime`, the carrier instances
  resolve under `Compiler`. Leaf test: `ability/PlatformScopedAbilityResolutionTest`.
- `monomorphize/check/CalculatedReturnResolver.scala` — W2b (**done**): the discharge/unwrap step (`isGuardCarrier`,
  `dischargeGuardedReturn`, `dischargeGuardedSignature`). Hook points: `monomorphize/check/Checker.scala` (kind
  acceptance + applied/by-name reads), `monomorphize/check/TypeStackLoop.scala` (callee signature discharge),
  `monomorphize/check/CheckState.scala` (the `sawGuardReturn` guard-signature flag). Leaf tests:
  `MonomorphicTypeCheckTest` ("effectful-signatures discharge (W2b)").
- `stdlib/.../` (abstract `def` signatures) + `jvm/.../` + `compiler/.../` (concrete bodies) — **G1**: the combinator
  vocabulary (`error`, `when`, `orError`, `orElse`) on the carrier, layered like `Either`/`foldEither`. Needs a
  compiler-layer `Option.els` (CP4-style promotion of `jvm/.../Option.els`) so `when`/`orError` reduce at compile time.
- `ast` parser (`Expression.typeParser`) + `matchdesugar`/block processors + a compile-time `<`/`>` comparison leaf in
  `SystemNativesProcessor` (alongside `add`) — **G2** (infix / `if`/`else` / `match` / blocks in the return position, and
  the desugar phases extended to the signature sub-expression).
- `effect/processor/EffectDesugaringProcessor.scala` + `DirectStyleDesugarer.scala` (the branch-lift) — **G3**, deferred:
  the opt-in return auto-lift for raw `if/else`, plus fixing the `fold`-branch sequencing so an effectful branch lifts
  (selection) rather than binds (sequence) — for the runtime body and the type-level return together. Ripples to
  `operator/.../OperatorResolvedExpression.scala` (`SignatureView`) and `saturate/fact/BinderRoles.scala` only if the
  signature is rewritten; with opt-in, an un-lifted pure return leaves those readers untouched.

## Deferred follow-ons

- **Recovery wrappers** (`tryHead : Seq[A, MIN, MAX] -> Option[A]`) — already *enabled* by the model (signature
  `runThrow`s the computation and yields an `Option` type), to be exercised and tested once W2–W3 land.
- **Richer error payloads** than `String` — `Throw[E]` is already generic in `E`; a structured diagnostic type
  (with source spans / suggestions) could replace the bare message.
- **Definition-site surfacing** of latent partiality (the use-site-verification trade-off): the IDE surfaces a
  guard at the definition by *probing* the signature with sample bounds (generators + probing; see
  `docs/ide-type-hints.md`), since the precondition is computed, not declared.
