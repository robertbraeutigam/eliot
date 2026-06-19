# Effects (`{E} A`)

## Goal

Give Eliot a first-class effect system so real programs can be written: I/O, dependency
injection, logging, error, state — composed without monad-transformer boilerplate, without
`do`/for-comprehensions, and **without any runtime cost**.

**One developer-facing concept: an "effect."** A type may carry a curly-brace list of effects
*before* the type name — `def readLine: {Sync} String`. There is no second concept: a developer
never says "ability" or "transformer" — only "effect." Inside a function you write in direct style
and the compiler inserts the monadic plumbing:

```eliot
def greet: {Sync} Unit = println(readLine())
```

desugars to `flatMap(readLine, s -> println(s))`. `readLine` returns an *effectful* `String`, so it
can flow directly into `println`, which expects a plain `String`; the compiler binds it and lifts
the continuation automatically. The only thing checked is that the **effects a body uses are
declared in its signature** (and conversely that nothing is hidden).

The effect list is **ordered**, and the order is meaningful: it fixes how effects compose and it
gates discharge (you can only discharge the outermost effect). The **base monad defaults to
`Identity` (pure)** and becomes **`IO` only when `Sync` is present at the bottom** — `Sync` is not
*the* base, it is the effect that *makes* the base `IO`. So a function with **no effects is a plain
value `A`** (and the compile-time evaluator can fully evaluate it); `{Option} A` is `Option[A]` —
pure, no IO; `{Option, Sync} A` is `OptionT[IO, A]`. Effects are **freely combinable** over whatever
base: a pure `{Option}` value drops straight into an `{Option, Sync}` context. `main : IO[Unit]` has
`IO` at its base only because the *runtime* runs `IO`. Each effect, layer or base, is discharged in
order by its own `run*`; whether an effect is realized as a base ability or a stacked transformer is
an *implementation* detail the developer never sees.

## Design decisions (settled)

These were worked out in design discussion; this section records the *what* and *why* so the
implementation does not relitigate them.

1. **One developer-facing concept: an "effect." The brace list is an ORDERED effect list.**
   `{E1, E2, …} A` is an ordered monad stack, **leftmost = outermost (top), rightmost = base**, over
   **one shared carrier `F`** per signature. The developer only ever says "effect" — never "ability"
   or "transformer." `{Option, Sync} A` means "the Option effect over the Sync base," realized as
   `OptionT[F, A]` (= `F[Option[A]]`) with `F ~ Sync`. **There is no set reading**: order is
   authoritative, and an ill-formed order is **rejected** (see below).
   - **The base monad defaults to `Identity` (pure); `Sync` makes it `IO`.** `Sync` is not *the*
     base — it is the effect that, present at the bottom, sets the base to `IO`. No effects ⟹ a plain
     value `A` (compile-time evaluable). `{Option} A` ⟹ `Option[A]` (pure, no IO). `{Option, Sync} A`
     ⟹ `OptionT[IO, A]`. `main : IO[Unit]` has `IO` at its base only because the runtime runs `IO`;
     you never write `{IO}` — the base data type appears only in concrete type syntax, never in braces.
   - **Every effect is a layer** over the base (`Identity` or `IO`), and effects are **freely
     combinable**: in polymorphic form the base is an abstract `F` constrained *only by base-level
     effects present* (`{Option, Sync}` ⟹ `OptionT[F, A]`, `F ~ Sync`; `{Option}` ⟹ `OptionT[F, A]`,
     `F` unconstrained — runs over `Identity` standalone, unifies to `F := IO` when combined with
     effectful code). *Realization is invisible to the developer:* internally `Sync` is an ability
     constraint on `F` and the layers are concrete transformers over it, but the surface exposes
     neither term — only the ordered effect list.
   - **The order does two real jobs**, which is why it is imposed even though, as bare abilities,
     effects would commute: (1) it **fixes composition semantics** locally and visibly (does state
     survive an error? — answered by the written order, not hidden in whatever concrete monad the
     edge happens to pick); (2) it **gates discharge** (next bullet, and Decision 6).
   - *Base polymorphism survives:* the layers are parameterized over the abstract base `F ~ Sync`, so
     business logic stays generic in `F` (testable by running under a different base) while the order
     is fully explicit.
   - *Commuting effects (`Reader`/`Dep`):* some effects genuinely commute, so reordering them is
     harmless — but that is an *emergent property* of those effects, not a separate category the
     developer manages. Order-sensitive effects (`State`, `Error`, `Option`) do not commute, and
     getting their order wrong is a meaning change the type system catches.
   - *On ordering (the recurring question), resolved:* the order is **declared** in the ordered
     signature and is authoritative; the body is desugared **to conform** to it (insert the
     `lift`/`flatMap`/`pure` needed to hit the declared stack, or error). So two functions both
     written `{Option, Sync}` denote the **same** `OptionT[F]` type regardless of whether one
     *introduces* Option and the other *passes it through* — that distinction changes only the body,
     never the type. Combining sub-expressions whose declared stacks have *inconsistent* nesting
     (`OptionT[StateT[F]]` vs `StateT[OptionT[F]]`) has no join — that is the **effect-compatibility
     error**.
   - *`lift` is real, and auto-inserted.* Within a layer the desugarer inserts `flatMap`/`map`/`pure`;
     **across** layers (a base `F[A]` used in an `OptionT[F,_]` context) it inserts `MonadTrans.lift`.
     The nesting is read off the types (no order guessing); monomorphization erases all of it.

2. **Effects desugar in a phase *before* monomorphization.** The phase rewrites direct-style bodies
   into ordinary `Monad[F].flatMap`/`map`/`pure` ability-method calls on the abstract carrier `F`,
   then the existing checker type-checks straight types. Running before is *required*, not merely
   preferred: pinning `F` to a concrete monad and resolving `Monad[F].flatMap` to a concrete
   instance **is monomorphization** (the same "resolve an ability constraint at the concrete use
   site, flowing from `main`" it already does for `[A ~ Show]`). The effect phase produces the
   abstract, constraint-carrying form; monomorphize consumes it.
   - *Monomorphize stays the sole arbiter.* After desugaring, effects are ordinary `F[...]` types and
     ordinary `flatMap` calls. Monomorphize checks all of it with no effect-specific logic, and it
     **backstops** the effect phase: a mis-placed bind or a hidden effect (`def foo: Unit =
     println(readLine())`, body `F[Unit]` vs declared `Unit`) surfaces as a plain type mismatch.
   - *Why not after monomorphize (erase-then-reintroduce):* the lift decisions need **effect
     rows/capabilities** (from signatures), not concrete value types. Erasure throws away exactly
     the information the decisions use and keeps the information they don't; it is not type-preserving
     when effects appear in data/HOF positions; and it would force a *second* monomorphize pass over
     the inserted combinators.

3. **Zero runtime cost by monomorphization.** Tagless-final is "elegant but slow" elsewhere because
   the abstract `F` and its dictionaries don't erase. Eliot monomorphizes from `main`
   unconditionally, so `F := AppM` specializes every generic function, `Monad[F].flatMap` resolves
   to a concrete call, and the whole tower collapses to flat code. The abstraction is free by
   construction.

4. **Deterministic — no handlers.** Meaning is fixed at definition and resolved statically via
   ability resolution; there is no ambient handler that can reinterpret an effect from outside
   (contrast Kyo/algebraic-effect handlers). Testability is recovered *statically* by choosing a
   different carrier `F` (a `Test` monad), resolved at compile time — interpreter-swapping without
   dynamic reinterpretation.

5. **Declared at signatures, inferred inside.** A function declares its effect list; inside the
   body the effect phase **infers** which effects are used (union of callees' declared effects,
   carriers unified to the one `F`, transformer nestings joined) and checks that against the declared
   list, erroring on an undeclared effect or an inconsistent nesting. This reuses the "sound, not
   modular" cornerstone: a generic `something[F ~ Sync]` is verified at each concrete `F` that
   manifests, not proven for all `F`.

6. **`main : IO[Unit]` (concrete), discharge gated by order.** The runtime executes a concrete
   `data IO`, so `main`'s type is the concrete `IO[Unit]` (ordinary type syntax), and it is the
   **seed** that pins the polymorphic program: calling `{Sync}` logic unifies `F := IO`, and
   monomorphization specializes everything reachable at `IO`. Other effects are **discharged**
   (peeled from the top) before reaching the base. **Discharge gating falls out of ordinary typing,
   for free:** `runState : StateT[S, M, A] -> S -> M[(A, S)]` only applies to a value whose
   *outermost* effect is `State`; if `State` is not on top (e.g. `{Option, State, Sync}`, whose type
   is `OptionT[StateT[…]]`, not `StateT[…]`), `runState` simply does not type-check — you must clear
   the effects above it first. So "you can only discharge the top effect" needs no special machinery;
   it is the discharge function's signature meeting the ordered representation. Effects that need
   mocking should be **Deps (values you swap at the discharge site)**, not reinterpretable
   operations. See "Concrete carrier & discharge".

## How it compares (for the reader)

| System | Effect set | Who fixes meaning | Bind written as | Runtime cost |
|---|---|---|---|---|
| Haskell `do` + transformers | ordered tower | concrete monad | `x <- e`, manual `lift` | high |
| tagless-final / mtl-classes | unordered constraints | static, `m` at edge | `do`, lift hidden | medium |
| algebraic effects + handlers (Koka, Unison, OCaml 5, Kyo) | unordered row | **handler in scope (dynamic)** | direct / `do` | low–med |
| F# computation expressions | one builder/block | the builder object | `let!` / `do!` | builder-dependent |
| Idris `!`-notation | enclosing `do`'s monad | static | `f !e` (marked) | monad-dependent |
| **Eliot (this plan)** | **ordered stack: transformers over a capability base (caps commute)** | **static, monomorphized** | **inferred (`f e`)** | **erased to flat code** |

Eliot ≈ MTL's semantics + Idris's continuation-lift with the `!` *inferred* + F#'s builder chosen
automatically from the declared capabilities + monomorphized erasure. The bind is *fully implicit
inside a body* (no `!`, no `let!`), but effects are *explicit in signatures*, so a function is always
visibly effectful even though individual bind points are not marked.

## Current machinery (what already exists)

The system is built on more existing infrastructure than expected.

- **Curly braces already tokenize** as standalone symbols and are reserved
  (`token/TokenParser.scala:54`, `ast/fact/Primitives.scala:93`). No tokenizer change needed.
- **The type-position parser** is `Expression.typeParser` (`ast/fact/Expression.scala:131`), a
  single restricted hook used by every type annotation (`FunctionDefinition.scala:106`,
  `ArgumentDefinition.scala`, `GenericParameter.scala`). One place to add `{...}`.
- **Higher-kinded function params work, with tests.** `[F[_]]` parses and desugars to
  `F: Function[Type, Type]` (`ast/fact/GenericParameter.scala:79-99`).
  `def f[F[_]](x: F[BigInteger]): F[BigInteger]` type-checks and injectivity-infers `?F := Box` from
  `Box[BigInteger]` (`monomorphize/processor/MonomorphicTypeCheckTest.scala:436-498`).
- **Higher-kinded abilities work, with tests.** `ability Container[F[_]] { def wrap(s: String):
  F[String] }` + `implement Container[Box]`, with return-type-driven dispatch
  (`ability/.../AbilityImplementationCheckProcessorTest.scala:378-451`).
- **Variable application + injectivity unification** are first-class: `applyValue` appends to a spine
  for `VNeutral`/`VTopDef`/`VMeta` (`monomorphize/eval/Evaluator.scala:55-77`,
  `domain/SemValue.scala`), and the unifier decomposes `?F[A] ~ Box[A]` to `?F := Box`
  (`monomorphize/unify/Unifier.scala:159-198`).
- **Ability constraints** (`[A ~ Show]`) parse into `AbilityConstraint`
  (`ast/fact/GenericParameter.scala:30,42`) and flow `NamedValue → ResolvedValue →
  OperatorResolvedValue` as `paramConstraints`. The monomorphize `TypeStackLoop` already resolves
  constraint-covered ability references at use sites.
- **`matchdesugar` is the exact template** for the new phase: it rewrites a surface construct into
  ability-method calls, consuming `ResolvedValue.Key` and producing `MatchDesugaredValue.Key`
  (`matchdesugar/processor/MatchDesugaringProcessor.scala`), building curried ability calls with
  `MatchDesugarUtils.buildCurriedCall`/`findAbilityMethodImpl` and
  `DataMatchDesugarer.buildHandleCasesCall:231`.
- **`saturate` is the template for introducing a generic param + constraint** as a fact-level
  transform: `SaturatedValueProcessor` mints fresh binders and prepends them to the type stack
  (`saturate/processor/SaturatedValueProcessor.scala:70-145`), carrying `paramConstraints`,
  `inferableArity`, `calculatedReturn` on `OperatorResolvedValue` (`operator/fact/...:10-34`).
- **Pipeline order** (`plugin/LangPlugin.scala:62-99`):
  `… ValueResolver → MatchDesugaringProcessor → OperatorResolverProcessor → SaturatedValueProcessor
  → Ability* → SystemNatives* → MonomorphicTypeCheckProcessor → UsedNames → Uncurry`.

### The net-new pieces

1. **HKT *ability constraint* on a function param** — `[F[_] ~ Sync]`. The pieces (HKT params, HKT
   abilities, constraint plumbing) each exist but have never been wired *together*; no test exercises
   a constraint whose constrained param is higher-kinded.
2. **Kind awareness for a carrier meta** — `?F` must unify only with something of kind `Type →
   Type`. The cornerstone forbids kind metadata driving typing (`RoleHint.typeParamCount` is
   write-only). Solution that respects the cornerstone: encode the kind as the meta's **expected
   type** (`Function[Type, Type]`), not as side metadata — definitional equality then enforces it.
3. **Auto-carrier saturation** — inferring/inserting `F` and its constraints into a signature from
   `{...}` and threading `inferableArity`.
4. **The effect-desugar phase itself** — capability/carrier inference + bind insertion.
5. **The library** — `Monad`/`Applicative`/`MonadTrans` abilities; the **base monads `Identity`
   (pure) and `IO`** (`Identity` is the default base for effect-free / pure-layer code; `IO` the base
   when `Sync` is present); base abilities (`Sync`, `Dep`/`Has`, `Logging`); transformer effects
   (`OptionT`, `EitherT`, `StateT`, …) with their `Monad`/`MonadTrans` instances; and discharge
   functions. **This is the one real authoring cost of free combinability** — a transformer + its
   instances per effect — but it is paid *once in the effect library*, never by the developer (one
   concept: "effect"), and monomorphization erases all of it at runtime.

## Architecture

Two clearly separated parts, both strictly before `monomorphize`:

### Part A — signature sugar (frontend)

`{E1, E2} A` is pure, type-information-free sugar. Desugar it at the **core level**
(`core/processor/CoreProcessor.scala`, where `data` and `[F[_]]` arity already desugar) into the
already-supported constrained-generic form. Introduce **one** inferable higher-kinded carrier param
`F[_]` per signature (shared across all `{...}` occurrences), marked `auto` (counts toward
`inferableArity`); then classify each brace entry by whether the named thing is a **capability**
(an ability) or a **transformer effect** (a data type with a `MonadTrans` instance):

- **capability `Ci`** (rightmost/base) → add a constraint `F ~ Ci` (the base gains an ability);
- **transformer `Ti`** → wrap: the annotated type becomes `Ti T[…F…]` (e.g. `Option` ⟹ `OptionT[F, A]`).
  Brace order is **leftmost = outermost**, so the transformer entries' order **is** the nesting;
  capabilities commute (and conventionally sit at the base). An ill-formed order (a base capability
  left of a transformer) is rejected.
- a fully pure-capability annotation `{Sync} T` rewrites to `F[T]`; a mixed one `{Option, Sync} T`
  rewrites to `OptionT[F, T]` with `F ~ Sync`.

**Capability vs transformer is a per-name choice, and one effect may ship both forms.** An effect can
be offered as a capability (rides on the base) *and* as a transformer (an explicit layer) under
*different names* — exactly as cats-effect has both `MonadError` (capability; IO throws/catches) and
`EitherT` (transformer; typed short-circuit). You pick the semantics by which name you write, and
that choice decides whether its brace position is load-bearing:

- `{Option, Error, Sync}` with `Error` the **capability** ⟹ `OptionT[F, T]`, `F ~ Error & Sync`
  (`F := IO` ⟹ `OptionT[IO, T]`). `Error`/`Sync` commute on the base; `Option` is the single layer.
- `{Error, Option, Sync}` with `Error` the **capability** ⟹ **rejected** (a base capability outside a
  transformer; there is no `ErrorT`).
- The transformer form is a different name (`EitherT`): `{EitherT, Option, Sync}` and
  `{Option, EitherT, Sync}` are *both* well-typed but *different* types (`IO[Option[Either[E,_]]]` vs
  `IO[Either[E, Option[_]]]`) — two real layers, order significant.

So "the order is type-checked" means: the braces desugar to a concrete nested type, and the ordinary
checker enforces it (a body producing the wrong nesting is a plain type mismatch). Transformer order
is load-bearing; capability order commutes (same constraint set ⟹ definitionally-equal type).

After Part A, `resolve`/`operator`/`saturate`/`monomorphize` see ordinary HKT-constrained generics
and ordinary transformer types — nothing effect-specific. `def readLine: {Sync} String` becomes
`def readLine[F[_] ~ Sync]: F[String]`; `def getConfig: {Option, Sync} String` becomes
`def getConfig[F[_] ~ Sync]: OptionT[F, String]`, both with `F` inferable.

### Part B — body auto-lift (the effect phase)

A new package `effect` with `EffectDesugaringProcessor`, placed **after `OperatorResolverProcessor`,
before `SaturatedValueProcessor`** (operators must be resolved so application structure is final).
It consumes `OperatorResolvedValue.Key`, produces a new `EffectDesugaredValue.Key`, and
`SaturatedValueProcessor`'s input key is repointed from `OperatorResolvedValue` to
`EffectDesugaredValue` (a one-line change; the expression shape is unchanged — no cases added or
removed, only `flatMap`/`map`/`pure` applications inserted).

It performs, per value body:

- **carrier + capability inference:** unify the carriers of all effectful sub-terms to the one
  ambient `F`; collect the union of capabilities the callees require (read off their desugared
  signatures via `getFact`); check that set ⊆ the function's declared constraint set; error on an
  undeclared effect.
- **bind insertion (the continuation lift):** see algorithm below.
- emit `Monad[F].flatMap`/`map`/`pure` references the same way `matchdesugar` emits
  `PatternMatch.handleCases` (`buildCurriedCall` + a `ValueReference` to the resolved ability
  method).

### The auto-lift algorithm

Given a body whose ambient carrier is `F` (capabilities `C_declared`):

- Compute, bottom-up, whether each sub-expression is **pure `T`** or **effectful `F[T]`** (a
  callee's result is `F[T]` iff its signature carries the carrier; this is signature-driven, not
  value-type-driven).
- For an application `f(a)` where `f`'s parameter is the pure type `A`:
  - if `a` is effectful `F[A]`: introduce a fresh `x : A`, replace `a` with `x`, and record that the
    nearest enclosing **effect boundary** must wrap with `flatMap[F](a, x -> ⟨continuation⟩)`.
  - if `a` is pure: leave it.
- **map vs flatMap:** if, after substitution, the continuation's result is `F[B]`, use `flatMap`
  (no double-wrap); if it is pure `B`, use `map` (result becomes `F[B]`).
- **pure insertion:** if the declared return is `F[T]` but the computed body is pure `T`, wrap with
  `pure[F]`.
- **boundaries:** binds float up to the nearest enclosing **lambda body, `match` case, or function
  body** — never out of a lambda (its return type becomes `F[…]` instead).
- **evaluation order:** multiple effectful arguments bind **left to right**
  (`f(a(), b())` ⟹ `a() >>= x -> b() >>= y -> f(x, y)`).
- **cross-layer `lift` (transformers):** when sub-terms sit at different transformer depths — a base
  `F[A]` used where `OptionT[F, _]` is the ambient monad — insert `MonadTrans.lift` to raise the
  shallower term to the deeper layer. The target monad is the **join of the sub-terms' stacks**: the
  deepest consistent nesting. Inconsistent nestings (`OptionT[StateT[F]]` vs `StateT[OptionT[F]]`)
  have no join ⟹ the **effect-compatibility error**. Nesting is read off the types, never guessed.
- `flatMap`/`map`/`pure` are `Monad`/`Applicative` ability methods of the *current layer* (`OptionT`'s
  bind when in `OptionT`, `F`'s when at the base); `lift` is `MonadTrans.lift`. All are resolved later
  by monomorphize and erased.

### Concrete carrier & discharge

The runtime executes a concrete `data IO[A](block: …)`. So:

- **`main : IO[Unit]`** — concrete, ordinary type syntax. It is the seed; calling `{Sync}` logic
  pins `F := IO`, `Sync[IO]` resolves, and monomorphization specializes the reachable program at
  `IO`. Business logic stays carrier-polymorphic (`{Sync, Dep[Db], …}`) and only the entry point
  commits — so logic remains reusable and testable under another carrier.
- **`Sync` vs `IO`.** `Sync` is the *capability* (ability) the concrete `IO` implements; `IO` is the
  *data type*. Polymorphic code uses `{Sync}`; committed code uses `IO[T]`. There is no `{IO}`.

**`Dep[X]` is one ability parameterized by the dependency type and the carrier**
(`ability Dep[X, F[_]] { def get: F[X] }`). `{Dep[Database]}` and `{Dep[PubSub]}` are distinct
instances keyed by `X`; `get[X]` dispatches by the type asked for (usually inferred). **Coherence
forbids two `Dep`s of the same type** (overlapping instances), so there is never a `get` collision —
one dependency per type, multiple distinct-typed Deps fine.

**Discharge model.** Discharge splits cleanly along the two tiers:

- **Transformer effects discharge cleanly** because they are *concrete* types: `runOptionT :
  OptionT[F, A] -> F[Option[A]]`, `runStateT : (StateT[S, F, A], S) -> F[(A, S)]`, `runReaderT :
  (ReaderT[Env, F, A], Env) -> F[A]`. Each peels one concrete outer layer and leaves the inner
  carrier `F` (still capability-constrained). No capability-set subtraction, no row variable — the
  concrete transformer *is* the structure being removed. The **order of discharge calls** at the edge
  is what fixes the interaction of order-sensitive effects (does an error roll back state?), and it
  is explicit and code-visible. This is why the earlier worry — "you can't subtract a capability" —
  does not bite: structural effects are transformers, not pure capabilities.
- **The base capability `Sync` is discharged by `main : IO[Unit]`**: pinning `F := IO` is the
  realization, not a `run*` call. **`Dep`/`Has` capabilities** discharge by *providing the value*
  (`runReaderT env` over a single `ReaderT[Env, F]`, then `get[X]` projects `Env`), which monomorphize
  collapses to a direct reference for a compile-time singleton.

**Recommended base shape.** A small base — `F ~ Sync` (→ `IO` at `main`) — with a single
`ReaderT[Env]` for all dependencies (`Dep`/`Has` sugar) and concrete transformers (`OptionT`,
`EitherT`, `StateT`, `WriterT`) introduced only where a structural effect is actually used. This is
the pattern real apps converge on and covers the `Dep[Database], Dep[PubSub], Logging, IO` example
directly. Each discharge function has one coherent implementation, so the **IO/effect interaction is
always defined** — by that implementation and by the explicit order of `run*` calls at the edge.
Deep, deliberately order-sensitive towers are *possible* (the machinery is general), just not the
expected idiom.

---

## Deliverable chunks

Each milestone is independently buildable and verifiable (`./mill __.test`). M0–M2 are foundation
and library with **no auto-lift**; M3 is the headline; M4–M5 are composition and polish.

### M0 — HKT carrier foundation (gating prerequisite, no effect syntax)

Close the three HKT gaps so a *hand-written* constrained higher-kinded carrier works.

- Wire `[F[_] ~ Cap]` constraints through resolve → operator → monomorphize (the constraint's
  constrained param is higher-kinded). Most plumbing exists; this connects it.
- Encode a carrier meta's kind as its **expected type** (`Function[Type, Type]`) so `?F` cannot
  unify with a proper type. No new kind-metadata field; cornerstone-clean.
- **Acceptance:** `def runTwice[F[_] ~ Monad](fa: F[String]): F[String] = flatMap(fa, _ -> fa)`
  (with a hand-written `Monad` ability + a `Box`-like instance) type-checks, monomorphizes at a
  concrete `F`, and runs. Tests in `monomorphize` + `ability` suites, mirroring the existing
  `Container[F[_]]` tests.

### M1 — `{...}` signature sugar (Part A)

- Extend `Expression.typeParser` to accept a leading `{ Cap (, Cap)* }` before a type atom.
- Desugar at core level into: one shared inferable carrier `F[_]`, `F ~ Cap` constraints, annotated
  types rewritten to `F[A]`. Multiple `{...}` in one signature share the same `F`.
- **Acceptance:** `def f(x: {Sync} String): {Sync} Unit` and the hand-written
  `def f[F[_] ~ Sync](x: F[String]): F[Unit]` produce **identical** resolved signatures (golden
  test on `OperatorResolvedValue`). No body lifting yet — bodies must already be in monadic form.

### M2 — Library spine + discharge (no auto-lift)

- Define `ability Monad[F[_]] { def flatMap[A,B](fa: F[A], f: A -> F[B]): F[B]; def pure[A](a: A):
  F[A] }` and `Applicative`/`map` (stdlib, abstract).
- Define one capability, `ability Sync[F[_]] { … }` (or fold `IO` primitives into it), abstract in
  stdlib.
- Concrete JVM carrier: `data IO[A](…)` already exists (`jvm/.../IO.els`); add `implement
  Monad[IO]`, `implement Sync[IO]`, and `printlnInternal`-style leaf natives in the `jvm` layer.
- Discharge: backend pins `main`'s carrier to JVM `IO` and runs `block`.
- **Acceptance:** a hand-monadic `def main: {Sync} Unit = flatMap(readLine, s -> println(s))`
  compiles to a jar and runs end-to-end. Proves library + discharge without the desugarer.

### M3 — Body auto-lift (Part B, the headline)

- New `effect` package + `EffectDesugaringProcessor` (after operator, before saturate); repoint
  saturate's input to `EffectDesugaredValue`.
- Implement carrier/capability inference, the bind-insertion algorithm (flatMap/map/pure, boundaries,
  left-to-right order), and the capability ⊆ declared check.
- **Acceptance:** `def main: {Sync} Unit = println(readLine())` compiles and runs (prints the line).
  `def foo: Unit = println(readLine())` is rejected with "uses {Sync} but is declared pure" (and is
  *also* caught by monomorphize as a backstop). Tests: a new `effect` suite over desugaring shapes +
  end-to-end example `examples/src/Effects.els`.

### M4 — Capability composition + propagation + `Dep`

- Multiple capabilities in one signature (`{Dep[Database], Logging, Sync} A`); carrier unification
  across callees; capability propagation (calling a `{Logging}` function makes the caller need
  `{Logging}` unless discharged).
- Add the `Reader[Env]`/`Dep[X, F[_]]` capability (`get[X]` dispatched by type; coherence rejects a
  duplicate same-type `Dep`) and a `Logging` capability, with JVM instances.
- **Acceptance:** a small multi-effect program (`{Dep[Database], Logging, Sync}`) compiles, runs,
  and rejects undeclared-effect cases with precise errors. Tests for realistic stacks, including
  two distinct-typed `Dep`s resolving `get` correctly and a same-type-`Dep` overlap being rejected.

### M4.5 — Transformer effects + cross-layer `lift`

- Add the first transformer effect, `OptionT[F, A]`, with `Monad`/`MonadTrans` instances; extend the
  auto-lift to insert `MonadTrans.lift` across layers and to compute the **stack join** (with the
  effect-compatibility error on inconsistent nestings).
- **Acceptance:** `getConfig : {Option, Sync} String` used inside a `{Sync}` body lifts correctly,
  the enclosing function is forced to `{Option, Sync}` (or rejected if it declares only `{Sync}`),
  and `getConfig.getOrElse(default)` discharges `Option` back to `{Sync}`. Add `EitherT`/`StateT` as
  follow-ups.

### M5 — Discharge, testability, Dep optimization

- `runReaderT`/`runOptionT`/`runStateT` discharge functions (concrete, cleanly typed);
  `main : IO[Unit] = runReaderT(myEnv, businessLogic)` for the `Dep`/`Has`-over-`ReaderT[Env]` base.
- Demonstrate **static testability:** run the same `businessLogic` under a test `Env` with a fake
  `Database`, asserting on output — no production `IO`.
- **Dep erasure:** confirm monomorphization collapses a compile-time-singleton dependency to a
  direct reference (the "Deps optimized away" goal), and document the pattern.
- **Acceptance:** a test that runs the same logic under prod vs test `Env`; a codegen check that a
  singleton dependency leaves no allocation; an order-sensitive pair (`runStateT`/`runEitherT` in
  both orders) shown to produce the two expected, defined results.

### Cross-cutting (fold into the milestones above)

- **Errors:** undeclared-effect, carrier-mismatch, missing-instance messages should point at the
  source span (use the `matchdesugar` `compilerError`/`compilerAbort` pattern).
- **IDE:** surface the inferred capability set at hover (extend `TypeHintIndex`) so the
  *implicit-inside-the-body* binds remain legible — this is how the "no `!` marker" decision buys
  back visibility. (See `docs/ide-type-hints.md`.)
- **`match`/effects interaction:** an effectful scrutinee binds before the `match`
  (`readLine() >>= s -> match s { … }`); decide whether `effect` runs before or after
  `matchdesugar` (recommended: after `operator`, with `match` already lowered to `handleCases`, so
  the scrutinee is an ordinary argument the auto-lift handles uniformly).

## Open design decisions

- **Which form each stdlib effect ships in (capability, transformer, or both).** `Sync`, `Dep`/`Has`,
  `Logging` are capabilities; `Option`/`OptionT`, `State`/`StateT`, `Writer`/`WriterT` are
  transformers. `Error` is the interesting one — ship `MonadError` (capability, IO exceptions) *and*
  `EitherT` (transformer, typed short-circuit) as distinct names, and decide which the stdlib
  defaults/encourages. Document the well-formedness rule (a capability may not appear outside a
  transformer; capabilities cluster at the base) and that capability order commutes while transformer
  order is load-bearing.
- **Effects in data/HOF positions.** `F[A]` is an ordinary type, so a field `run: {Sync} Unit`
  (= `F[Unit]`) is representable — but auto-lift across data construction needs explicit rules
  (constructing `Handler(readLine())` should *store* the action, not bind it). Decide: a data field
  typed `F[A]` is a **storage** position (no auto-bind); only *value-argument positions expecting a
  pure type* auto-bind. Make this rule explicit and tested.
- **Part A placement.** Core-level desugar (recommended) vs a dedicated pre-resolve step. Core keeps
  it with the other sugar; confirm `CoreProcessor` can introduce an inferable generic cleanly.
- **`pure` ambiguity.** A pure body under an effectful return needs `pure[F]`; ensure this never
  fires spuriously when `F` is still a meta (defer like other constraint-covered references).
- **Coherence.** Each concrete carrier must have unique, non-overlapping capability instances; this
  rides on the existing ability overlap checks but should be tested for HKT instances. Note this is
  what gives "one `Dep` per type" for free (`Dep[Database]` twice = overlapping instances → rejected).
- **Discharge typing (resolved by the two-tier split).** "Remove a *capability* from the set" has no
  clean type — so structural effects are **transformers**, not capabilities, and their discharge is
  cleanly typed (`runOptionT : OptionT[F,A] -> F[Option[A]]`: peel the concrete outer, keep inner
  `F`). The only capabilities (`Sync`, `Dep`/`Has`) are never "subtracted": `Sync` is realized by
  `main : IO[Unit]`, `Dep` by providing the env. No capability-row subtraction needed.
- **Stack-join correctness (transformers).** The cross-layer auto-`lift` must compute the join of
  sub-term stacks correctly and reject inconsistent nestings as the effect-compatibility error;
  get this wrong and you either lift to the wrong layer or accept ill-ordered code. Read nesting off
  the types; never infer order.

## Risks

- **HKT-constraint plumbing (M0)** is the one place existing pieces have not been combined; if kind
  encoding via expected-type proves leaky, fall back to a minimal kind check localized to the
  carrier-meta allocation (do **not** reintroduce kind-directed typing broadly — cornerstone).
- **Carrier inference scope.** Keep it to *checking + propagation* with carriers pinned from `main`
  flowing down (first-order substitution); avoid higher-order *inference* of `F` (the unifier
  postpones non-pattern HKT unification — `MonomorphicTypeCheckTest.scala:448`), which is not needed
  when `F` is pinned at the edge.
- **Backstop discipline.** The effect phase must produce well-typed output; rely on monomorphize as
  the final arbiter, but give good errors *first* so users don't see raw `F[Unit]` vs `Unit`
  mismatches.

## Effects and termination are one mechanism

Because the base defaults to `Identity` (pure), a **no-effect function is a plain value the
compile-time NbE evaluator can fully evaluate**, while any effect makes it stuck (a neutral) — which
is exactly the behavior `project_recursion_as_effect` describes. So a **`Recursive` effect** is just
another effect: `{Recursive} A` marks a possibly-non-terminating computation, and a plain `A` (no
`Recursive`) is terminating *by typing*. The effect system and the termination model are therefore
**the same machinery**, not two — a strong consistency check on this design, and the reason `Recursive`
appears in the effect list above alongside `Option`/`State`.

## References

- Cornerstones: "Types Are Values (λ\*)", "Platform-Independence via Layers", "Use-Site
  Verification" in `.claude/CLAUDE.md`.
- Related: `docs/ide-type-hints.md` (hover should show the effect list);
  `project_recursion_as_effect` memory (recursion as an effect; effectful code gets stuck/neutral in
  the pure NbE evaluator — the same mechanism that keeps the compile-time evaluator from running
  `IO`, and the basis for the "effects = termination" unification above).
- Template phase: `matchdesugar` (`lang/src/com/vanillasource/eliot/eliotc/matchdesugar/`).
- Param/constraint introduction: `saturate`
  (`lang/src/com/vanillasource/eliot/eliotc/saturate/processor/SaturatedValueProcessor.scala`).
- HKT evidence: `MonomorphicTypeCheckTest.scala:436-498`,
  `AbilityImplementationCheckProcessorTest.scala:378-451`, `Unifier.scala:159-198`.
