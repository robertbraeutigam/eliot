# Effects (`{E} A`)

## Goal

Give Eliot a first-class effect system so real programs can be written: I/O, dependency injection,
logging, error, state — composed in **direct style** (no `do`/for-comprehensions, no explicit
`flatMap`, no manual `lift`), declared visibly at signatures, and **erased to flat code at runtime**.

**One developer-facing concept: an "effect."** A type may carry a curly-brace list of effects
*before* the result type — `def readLine: {Console} String`. There is no second concept: no "monad,"
"transformer," or "ability" appears in a signature — only effects. Inside a body you write in direct
style and the compiler inserts the monadic plumbing:

```eliot
def greet: {Console} Unit = println(readLine())
```

`readLine` is `{Console} String`; its result flows directly into `println`, which expects a plain
`String`; the compiler binds it and lifts the continuation automatically. The only thing checked is
that the **effects a body uses are a subset of those declared** in its signature (and nothing is
hidden).

## The core idea (read this first)

The whole design turns on one separation:

> **An effect is a property of the *computation*, not a wrapper around the *result*.**

A function that reads an optional config does **not** "return an `Option`." It returns a `String`,
and *performing* it might short-circuit. The optionality is not in the result type — it is an effect
(`Abort`), recorded in the function's brace-set, and the `Option` data structure is **born only when
you discharge that effect** (`runAbort`). Concretely:

```eliot
def getConfig : {Abort} String          -- result is String; performing it may abort
...
config = runAbort(getConfig())           -- config : Option[String]  <- the Option appears HERE
```

This single move dissolves every tangle that ordered monad-transformer stacks create (carrier
nesting in signatures, `lift` insertion, the `{Option}` ⟹ `OptionT[F]` ambiguity, discharge
gating). Effects are an **unordered set** beside a **plain result type**; *structure and ordering
live at the discharge site*, written once, where they are actually decided. This is the
algebraic-effect / mtl reading, made static and zero-cost by Eliot's monomorphization, and with
**no transformer ever appearing in a business signature** — transformers exist only inside the
library's discharge functions and carrier instances.

## Design decisions (settled)

1. **Effects are an UNORDERED SET of capabilities, written beside a PLAIN result type.**
   `{E1, E2, …} A` means "produces an `A`, using effects `E1, E2, …`." The braces are a *set* — order
   is irrelevant (`{Abort, Sync}` ≡ `{Sync, Abort}`), there is no nesting, and the result type is the
   ordinary value you get (`A`, never `F[A]`, never `OptionT[F, A]`). Effects **annotate the
   function**, not the value. **No effects ⟹ a plain value `A`** the compile-time NbE evaluator can
   fully evaluate.
   - *Why a set, not an ordered stack:* ordering needs structural nesting, and nesting forces type
     constructors (transformers) into signatures — the source of every prior ambiguity. We refuse
     that. Order is real but it belongs at the discharge edge (Decision 7), not smeared across every
     signature.

2. **Effects are abilities; their operations are ability methods.** An effect is declared as an
   `ability` whose methods carry the effect in their own row; calling a method *performs* the effect:
   ```eliot
   ability Abort     { def abort[A]          : {Abort} A }
   ability State[S]   { def get : {State[S]} S, def put(s: S) : {State[S]} Unit }
   ability Log        { def log(m: String)   : {Log} Unit }
   ```
   Performing an effect yields the **plain** value (`get()` is an `S`, not `F[S]`); the effect floats
   up into the *caller's* declared set. The developer never says "ability" — that word is for the
   library author defining an effect; the user only ever lists effects.

3. **Effects desugar in a phase *before* monomorphization.** The phase rewrites direct-style bodies
   into ordinary `Monad[F].flatMap`/`map`/`pure` calls on the one ambient carrier `F`; the existing
   checker then type-checks ordinary types. Running before is *required*: pinning `F` to a concrete
   monad and resolving `Monad[F].flatMap` to a concrete instance **is monomorphization** (the same
   "resolve an ability constraint at the concrete use site, flowing from `main`" already done for
   `[A ~ Show]`). The effect phase produces the abstract, constraint-carrying form; monomorphize
   consumes it and **backstops** it (a hidden effect surfaces as a plain `F[Unit]` vs `Unit`
   mismatch).

4. **Zero runtime cost by monomorphization.** Tagless/mtl is "elegant but slow" elsewhere because the
   abstract `F` and its dictionaries don't erase. Eliot monomorphizes from `main` unconditionally, so
   `F := <concrete carrier>` specializes every generic function, `Monad[F].flatMap` resolves to a
   concrete call, and the whole tower collapses to flat code. The abstraction is free by construction.

5. **Deterministic — static discharge, no *dynamic* handlers.** Meaning is fixed at the *operation*
   (`abort` short-circuits, full stop) and resolved statically by ability resolution; there is no
   ambient handler that can reinterpret an effect from outside (contrast Koka/Unison/Kyo). What you
   *choose* at the edge is the **observation** (`runAbort` ⟹ `Option`, `runAbortOr(d)` ⟹ a default)
   and the **carrier** (swap `F := TestSync` to record instead of perform). Discharge functions are
   ordinary, statically-resolved functions — not a `with`-handler construct. This is *stronger*
   determinism than algebraic effects, and it is exactly Eliot's stance.

6. **Declared at signatures, inferred inside — over a set, so trivially.** A function declares its
   effect set; the body's effects are inferred as the **union of its callees' declared sets**
   (carriers unified to the one `F`) and checked **⊆ the declared set**. Undeclared effect ⟹ error;
   unused declared effects are allowed (they just don't fire). No nesting, no lifting, no join — set
   union and subset. This reuses the "sound, not modular" cornerstone: a generic `{Console}` body is
   verified at each concrete `F` that manifests, not proven for all `F`.

7. **Structure and ordering are born at discharge; the *order of discharge calls* fixes interaction.**
   A discharge function is an ordinary function that **removes one effect from the set and reflects it
   into the result type**:
   ```eliot
   def runAbort[A](p: {Abort} A)             : Option[A]
   def runState[S,A](p: {State[S]} A, s: S)  : (A, S)
   def runThrow[E,A](p: {Throw[E]} A)        : Either[E, A]
   ```
   This is the *only* place a data structure (`Option`, the tuple, `Either`) appears. The **order you
   apply discharges** builds the concrete carrier stack and thereby fixes order-sensitive interaction
   — and **both orders are well-typed and produce the two expected, different results**, visible in
   the result type:
   - `runAbort(runState(p, s0))` ⟹ `Option[(A, S)]` — abort discards the state.
   - `runState(runAbort(p), s0)` ⟹ `(Option[A], S)` — state survives the abort.

   There is **no "discharge only the top of the stack" rule** — any effect in the set may be
   discharged; the order chosen *is* the answer to "does an error roll back state?", written once, at
   the boundary where it is decided. (How this types without a transformer in the signature:
   Decision below + "Part C".)

8. **`main` discharges everything to a runnable carrier.** The runtime executes a concrete
   `data IO[A]`, so `main`'s type is the concrete `IO[Unit]` (ordinary type syntax) — the **seed**
   that pins `F := IO` and lets monomorphization specialize the reachable program. Business logic
   stays carrier-polymorphic (`{Sync, Dep[Db], …}`); only the entry point commits, so logic remains
   reusable and statically testable under another carrier. You never write `{IO}` or `{Sync}`; `Sync` is
   the internal base capability the concrete `IO` implements, and the effects you *do* name are the
   fine-grained ones (`Console`, `Clock`, `File`, …) keyed on `Sync` (Decision 10).

9. **The I/O boundary is a `private` leaf native wrapped in Eliot — the carrier never appears in a
   business signature, and the impurity is unreachable.** An effect operation that touches the world
   bottoms out in a **leaf native** — bytecode with a deliberately pure-looking type, e.g.
   `private def printlnInternal(s: String): Unit`. This is the single irreducible impure spot (the
   analogue of a Haskell FFI import). The platform layer *wraps* every such leaf in ordinary Eliot via
   the carrier's reification member `sync`/`delay`:
   ```eliot
   ability Sync[F[_]]    { def sync[A](thunk: Function[Unit, A]): {Sync} A }   -- internal reification tier
   ability Console[F[_]] { def println(s: String): {Console} Unit }            -- a public fine effect (Decision 10)
   -- jvm layer:
   implement Sync[IO]                { def sync(thunk) = IO(thunk) }            -- the only mention of IO
   implement[F[_] ~ Sync] Console[F] { def println(s) = sync(_ -> printlnInternal(s)) }
   private def printlnInternal(s: String): Unit                                -- the only impure, opaque spot
   -- users write:  def greet : {Console} Unit = println("hi")
   ```
   So the whole `IO(_ -> …)` structure stays Eliot-visible to monomorphize/the optimizer; only the
   zero-structure leaf call is opaque (and, being body-less, it is a stuck neutral at compile time —
   never run by the NbE evaluator). **Safety = the leaf is `private`**, which Eliot already enforces
   (`ValueResolver` refuses a private name referenced from another module). App code can never *name*
   the leaf, so it can never do untracked I/O — it reaches I/O only through a `{Console}`-typed
   operation like `println`, which records the effect honestly. `sync`/`delay` staying a public ability
   method is harmless: calling it forces the caller into `{Sync}` (it *is* `{Sync} A`), exactly as calling
   `println` forces `{Console}`, and the only impure thing worth passing it is the `private` leaf; on pure code it is just a
   redundant `pure`. No access control beyond `private` is needed. This is **Haskell's discipline**
   (you cannot fabricate a primitive effect from pure code) realized with **mtl/cats-effect's
   polymorphic-carrier machinery** (the carrier needs `delay` to lift a leaf native into `F`) — possible
   precisely because Eliot is pure, so there is no impure host expression a user would ever need, or be
   able, to capture. *Fail-safe enforcement* (the compiler cannot *detect* a native's impurity — it lives
   in the bytecode): the backend's impure-native registration **asserts the resolved def is
   `Visibility.Private`** and hard-errors otherwise, so a platform author cannot silently reopen the hole;
   pure natives (arithmetic, type-level) stay public and are simply not in that registry.

10. **I/O effects are fine-grained and keyed on `Sync`, not on the concrete carrier.** Following
    cats-effect, world-touching capability is split into small *public* effects — `Console`, `Clock`,
    `File`, `Random`, … — each an ordinary ability whose operations are its methods, abstract in stdlib.
    Users name these (`def greet : {Console} Unit`) and **never `{Sync}`**: `Sync` is the internal
    reification tier (Decision 9), used only inside the platform-layer instances. Each fine effect's
    instance is **generic over the carrier, constrained by `Sync`** — never specialised to `IO`:
    ```eliot
    implement[F[_] ~ Sync] Console[F] { def println(s) = sync(_ -> printlnInternal(s)) }   -- jvm layer
    ```
    The *only* place the concrete `IO` is named is the **base `Sync[IO]` instance**; `Console`/`Clock`/
    `File` mention only `Sync`. (The instance is "platform-specific" merely in *which leaf natives it
    calls* — hence which layer it physically lives in — not in its carrier.) Consequence: a fine effect is
    available for **any carrier with `IO` at its base** — `IO`, `OptionT[IO]`, `StateT[OptionT[IO]]`, … —
    because the base-effect lifting instances (`Sync[OptionT[G]]` from `Sync[G]`, one per transformer; the
    `MonadIO`-style lift) propagate `Sync` up the whole stack and every `Sync`-keyed fine effect rides it.
    So lifting is written **once per transformer for the base `Sync`/`Monad`** (n instances) and *all* fine
    effects work through the entire stack for free — the n×m (effects × transformers) matrix collapses to
    n. `IO` sits only at the innermost base, pinned by `main` (Decision 8); every layer carries `Sync`.
    **Testing is the mirror image:** a pure recording carrier supplies its own *direct*
    `implement Console[TestF]` (not via `Sync`), so `{Console}` logic runs with no `IO` at all — coherent
    because it is a different carrier type (no overlap). The one net-new mechanism this needs is the
    **constrained HKT instance** `implement[F[_] ~ Sync] Console[F]` (M0 established the same constraint on
    a function param; on an instance param it should get its own spike).

## How it compares (for the reader)

| System | Effects in a signature | Result type | Who fixes order/structure | Bind written as | Runtime cost |
|---|---|---|---|---|---|
| Haskell `do` + transformers | ordered tower in the type | `OptionT[…] A` | the written type | `x <- e`, manual `lift` | high |
| tagless-final / mtl-classes | unordered constraints | `m A` | `m` chosen at edge | `do`, lift hidden | medium |
| algebraic effects + handlers (Koka, Unison, Kyo) | unordered row | plain `A` | **handler in scope (dynamic)** | direct / `do` | low–med |
| **Eliot (this plan)** | **unordered set, beside a plain result** | **plain `A`** | **discharge-call order (static)** | **inferred (`f e`)** | **erased to flat code** |

Eliot ≈ algebraic-effect ergonomics (flat set, plain result, direct style) + mtl's static
realization (transformers, but only inside discharge functions and instances) + monomorphized
erasure. Bind is fully implicit *inside* a body; effects are explicit in signatures, so a function is
always visibly effectful even though individual bind points are not marked.

### Lineage: Kyo's surface, mtl's machinery, erased harder than both

To place this precisely against the two systems it most resembles:

- **Surface ≈ Kyo.** `{Abort, Sync} A` is essentially Kyo's `A < (Abort & IO)` — an unordered effect
  *set* beside a *plain* result, direct style. Strictly lighter than mtl's real signatures
  `(MonadState s m, MonadError e m) => m a` (result `m a`, effects as constraints).
- **…but the desugaring *is* mtl.** `{Abort, Sync} A` expands to `[F[_] ~ Abort ~ Sync] F[A]` — exactly
  the mtl form. The sugar is the only thing between the Kyo surface and the mtl internal form.
- **Realization = mtl, not Kyo.** Discharge uses real monad transformers + the classic n² lifting
  instances (`State[OptionT[G]]` from `State[G]`). Kyo deliberately has *no* transformers and *no*
  lifting (a free/continuation encoding). The lifting cost here is the mtl tell. (Open lever: a
  freer-style single carrier could replace the transformer matrix — see Open design decisions.)
- **Determinism = mtl, not algebraic-effects.** Meaning is fixed at the operation and resolved
  statically; no handler reinterprets an effect (Decision 5). You choose *observation* (`runAbort` vs
  `runAbortOr`) and *carrier*, both at compile time. Kyo, being algebraic-effect, permits
  reinterpretation; we do not.
- **Erasure = beyond both.** Whole-program monomorphization collapses the tower to flat code; real mtl
  pays dictionary passing, Kyo pays its encoding overhead, Eliot pays nothing at runtime.

In one line: **Kyo's signature, mtl's machinery, mtl's determinism, monomorphized to nothing.**

## Effect semantics: where order lives (commuting vs non-commuting)

An effect's position in the stack is **not a property of `F`** — `F ~ Abort & State & Sync` is a flat
set. Position is created at **discharge**, by one mechanical rule:

> **The first effect discharged becomes the outermost layer.** The runtime base (`Sync`/`IO`) is
> structurally innermost — never `run*`-discharged, provided by `main`.

Worked example on `p : {Abort, State, Sync} A`:

- `runAbort(p) : {State, Sync} Option[A]` → then `runState` → `(Option[A], S)` — **state survives
  abort** (`OptionT[StateT] = S -> (Option[A], S)`).
- `runState(p, s0) : {Abort, Sync} (A, S)` → then `runAbort` → `Option[(A, S)]` — **abort discards
  state** (`StateT[OptionT] = S -> Option[(A, S)]`).

**Legal discharge orders = available lifting instances.** `runAbort` type-checks only if `OptionT[G]`
satisfies the residual effects (`State[OptionT[G]]`, `Sync[OptionT[G]]` exist). An effect that must be
inner lacks the instance to lift through an outer layer, so discharging it early is a plain
**missing-instance error** — discharge gating with no special machinery (the row-model analogue of the
old ordered plan's "gating falls out of typing").

**Commuting vs non-commuting** (the load-bearing distinction):

- **Commuting** (`Dep`/`Reader`, `Writer`-ish): every discharge order yields the same result, so the
  effect's position is irrelevant and `F ~ Dep[X]` is *semantically complete*. Call `get[X]()` anywhere
  — unambiguous.
- **Non-commuting** (`Abort`, `State`, `Throw`): orders disagree, so `F ~ Abort & State` is
  *deliberately polymorphic over the interaction*. The body works under both; discharge order decides.
  This is the accepted trade for flat-set signatures (the mtl trade-off).

The *operation* is always locally well-defined (`abort()` = "stop here", like `throw` — whether a
`finally` runs is a surrounding-handler question, not a throw-site one); only the *interaction* with
other non-commuting effects is deferred. **No soundness gap:** for the standard transformers every
order has valid lifting instances, so a well-typed body always has at least one valid discharge — you
choose among well-defined alternatives, never an undefined one.

**Committing to an interaction = discharge locally.** If a function's contract depends on "abort rolls
back this state," it runs `runState` *inside* itself — fixing the order there and dropping `State` from
its signature:

```eliot
def attempt : {Abort, Sync} Result =
  (value, _) = runState(riskyStep(), initial)   -- State fixed HERE, not in the signature
  ...                                            -- abort downstream cannot see that state
```

Local discharge is how a discharge-order question becomes part of a function's contract; the signature
then truthfully shows `{Abort, Sync}` with no `State` leaking.

## Current machinery (what already exists)

The system is built on more existing infrastructure than expected.

- **Curly braces already tokenize** as standalone reserved symbols (`token/TokenParser.scala:54`,
  `ast/fact/Primitives.scala:93`). No tokenizer change needed.
- **The type-position parser** is `Expression.typeParser` (`ast/fact/Expression.scala:131`), a single
  restricted hook used by every type annotation (`FunctionDefinition.scala:106`,
  `ArgumentDefinition.scala`, `GenericParameter.scala`). One place to add a leading `{...}`.
- **Higher-kinded function params work, with tests.** `[F[_]]` parses and desugars to
  `F: Function[Type, Type]` (`ast/fact/GenericParameter.scala:79-99`).
  `def f[F[_]](x: F[BigInteger]): F[BigInteger]` type-checks and injectivity-infers `?F := Box` from
  `Box[BigInteger]` (`monomorphize/processor/MonomorphicTypeCheckTest.scala:436-498`).
- **Higher-kinded abilities work, with tests.** `ability Container[F[_]] { def wrap(s: String):
  F[String] }` + `implement Container[Box]`, with return-type-driven dispatch
  (`ability/.../AbilityImplementationCheckProcessorTest.scala:378-451`).
- **Variable application + injectivity unification** are first-class: `applyValue` appends to a spine
  for `VNeutral`/`VTopDef`/`VMeta` (`monomorphize/eval/Evaluator.scala:55-77`,
  `domain/SemValue.scala`); the unifier decomposes `?F[A] ~ Box[A]` to `?F := Box`
  (`monomorphize/unify/Unifier.scala:159-198`).
- **Ability constraints** (`[A ~ Show]`) parse into `AbilityConstraint`
  (`ast/fact/GenericParameter.scala:30,42`) and flow `NamedValue → ResolvedValue →
  OperatorResolvedValue` as `paramConstraints`. The monomorphize `TypeStackLoop` already resolves
  constraint-covered ability references at use sites.
- **`matchdesugar` is the exact template** for the new phase: it rewrites a surface construct into
  ability-method calls, consuming `ResolvedValue.Key` and producing `MatchDesugaredValue.Key`
  (`matchdesugar/processor/MatchDesugaringProcessor.scala`), building curried ability calls with
  `MatchDesugarUtils.buildCurriedCall`/`findAbilityMethodImpl`.
- **`saturate` is the template for introducing a generic param + constraint** as a fact-level
  transform: `SaturatedValueProcessor` mints fresh binders and prepends them to the type stack
  (`saturate/processor/SaturatedValueProcessor.scala:70-145`), carrying `paramConstraints`,
  `inferableArity`, `calculatedReturn` on `OperatorResolvedValue` (`operator/fact/...:10-34`).
- **Pipeline order** (`plugin/LangPlugin.scala:62-99`):
  `… ValueResolver → MatchDesugaringProcessor → OperatorResolverProcessor → SaturatedValueProcessor
  → Ability* → SystemNatives* → MonomorphicTypeCheckProcessor → UsedNames → Uncurry`.

### The net-new pieces

1. **HKT *ability constraint* on a function param** — `[F[_] ~ Sync]`. The pieces (HKT params, HKT
   abilities, constraint plumbing) each exist but had never been wired together. **Done in M0.**
2. **Kind awareness for a carrier meta** — `?F` must unify only with something of kind `Type → Type`.
   Closed in M0 via a localized `CheckState.carrierKinds` check (post-drain), *not* a VMeta field and
   *not* `RoleHint` (cornerstone-clean). See `project_effects_m0`.
3. **Auto-carrier saturation** — inferring/inserting the single `F` and its constraints from `{...}`.
4. **The effect-desugar phase** — capability inference + direct-style bind insertion (no lift).
5. **The library** — `Monad`/`Applicative` abilities; the base capability `Sync` and concrete `IO`;
   further capabilities (`Dep`/`Has`, `Log`, `Abort`, `Throw[E]`, `State[S]`, …); and a **discharge
   function per effect** that reflects it into a result structure. The transformers/`MonadTrans`
   machinery lives **inside the discharge functions and carrier instances only** — never in a user
   signature.

## Architecture

Three parts, all strictly before `monomorphize`. (Discharge functions are ordinary library code, not
a compiler phase.)

### Part A — signature sugar (frontend)

`{E1, E2} A` is pure, type-information-free sugar with **one uniform rule** — no per-effect
classification, no wrapping:

> Introduce **one** shared inferable higher-kinded carrier `F[_]` per signature (marked `auto`,
> counts toward `inferableArity`); for each brace entry `Ei` add a constraint `F ~ Ei`; rewrite each
> `{…} A` occurrence to `F[A]`.

So `def readLine: {Console} String` becomes `def readLine[F[_] ~ Console]: F[String]`; `def transfer(amt:
Money): {State[Account], Abort, Log} Receipt` becomes `def transfer[F[_] ~ State[Account] ~ Abort ~
Log](amt: F[Money]?): F[Receipt]` (carrier shared across all `{…}` in the signature). Done at the
core level (`core/processor/CoreProcessor.scala`, where `data` and `[F[_]]` arity already desugar).
After Part A, everything downstream sees ordinary HKT-constrained generics — nothing effect-specific.

### Part B — body auto-lift (the effect phase)

A new package `effect` with `EffectDesugaringProcessor`, placed **after `OperatorResolverProcessor`,
before `SaturatedValueProcessor`** (operators resolved so application structure is final). It consumes
`OperatorResolvedValue.Key`, produces `EffectDesugaredValue.Key`, and `SaturatedValueProcessor`'s
input key is repointed to it (a one-line change; the expression shape is unchanged — only
`flatMap`/`map`/`pure` applications inserted). Per value body it:

- **infers the carrier + effect set:** unify the carriers of all effectful sub-terms to the one
  ambient `F`; collect the union of effects callees require (read off their desugared signatures via
  `getFact`); check that union ⊆ the declared set; error on an undeclared effect.
- **inserts binds:** the algorithm below — `flatMap`/`map`/`pure` only. **No `lift`**: every
  effectful term lives in the *same* carrier `F`, so there are no layers to cross inside a body.
- emits `Monad[F].flatMap`/`map`/`pure` references the way `matchdesugar` emits
  `PatternMatch.handleCases` (`buildCurriedCall` + a `ValueReference` to the resolved ability method).

### Part C — discharge functions (ordinary library code; no compiler support)

A discharge function removes one effect and reflects it into a result structure. It is typed **at a
concrete transformer**, and *calling it refines the polymorphic carrier* — the transformer never
appears in a business signature:

```eliot
def runAbort[G[_], A](p: OptionT[G, A]) : G[Option[A]] = ...   -- surface type: ({Abort} A) -> Option[A]
```

When the user writes `runAbort(p)` with `p : {Abort, Sync} A` (= `F[A]`, `F ~ Abort & Sync`),
unification solves `F := OptionT[G]` with `G ~ Sync`, and the result is `G[Option[A]]` =
`{Sync} Option[A]`. The remaining effects stay polymorphic in `G`. Applying a second discharge to `G`
refines it further — and **the order of these refinements builds the concrete stack**
(`OptionT[StateT[H]]` vs `StateT[OptionT[H]]`), which is how discharge-call order fixes interaction
(Decision 7). The "you can't subtract a capability" worry never bites: discharge does not subtract a
capability from a row; it **instantiates the carrier to a transformer and peels that concrete layer**.

The cost this concentrates here (and *only* here): the carrier instances need the lifting machinery
(e.g. `State[OptionT[G]]` derived from `State[G]`) — the classic mtl per-effect-pair instances, or a
freer encoding to avoid them. **Paid once, by the effect-library author, never by the developer, and
erased by monomorphization.**

### The auto-lift algorithm (Part B detail)

Given a body whose ambient carrier is `F` (declared effects `E_declared`):

- Compute, bottom-up, whether each sub-expression is **pure `T`** or **effectful `F[T]`** (a callee's
  result is `F[T]` iff its signature carries the carrier — signature-driven, not value-type-driven).
- For an application `f(a)` where `f`'s parameter is the pure type `A`:
  - if `a` is effectful `F[A]`: introduce a fresh `x : A`, replace `a` with `x`, and record that the
    nearest enclosing **effect boundary** wraps with `flatMap[F](a, x -> ⟨continuation⟩)`.
  - if `a` is pure: leave it.
- **map vs flatMap:** if, after substitution, the continuation's result is `F[B]`, use `flatMap`; if
  it is pure `B`, use `map` (result becomes `F[B]`).
- **pure insertion:** if the declared return is `F[T]` but the computed body is pure `T`, wrap with
  `pure[F]`.
- **boundaries:** binds float up to the nearest enclosing **lambda body, `match` case, or function
  body** — never out of a lambda (its return type becomes `F[…]` instead).
- **evaluation order:** multiple effectful arguments bind **left to right**
  (`f(a(), b())` ⟹ `a() >>= x -> b() >>= y -> f(x, y)`).
- **no cross-layer lift, no stack join:** all sub-terms share `F`, so there is never a layer to
  raise. (This whole class of complexity is gone precisely because signatures carry no transformer
  nesting.)

`flatMap`/`map`/`pure` are `Monad`/`Applicative` ability methods of the one carrier `F`; monomorphize
resolves and erases them.

## Deliverable chunks

Each milestone is independently buildable and verifiable (`./mill __.test`). M0 is foundation; M1–M2
are sugar + library; M3 is the headline; M4–M5 are composition, discharge, and polish.

### M0 — HKT carrier foundation ✅ DONE

`[F[_] ~ Cap]` constraint-on-a-param works end-to-end; carrier-meta kind enforced via a localized
post-drain `CheckState.carrierKinds` check (cornerstone-clean, no `RoleHint`, no VMeta field). See
`project_effects_m0`. Acceptance met: a hand-written `def runTwice[F[_] ~ Monad](fa: F[String]):
F[String] = flatMap(fa, _ -> fa)` type-checks, monomorphizes at a concrete `F`, and runs.

### M1 — `{...}` signature sugar (Part A) ✅ DONE

- Extend `Expression.typeParser` to accept a leading `{ Eff (, Eff)* }` before a type atom.
- Desugar at core level by the **one uniform rule**: one shared inferable carrier `F[_]`, `F ~ Eff`
  constraints, each `{…} A` rewritten to `F[A]`. No per-effect classification, no wrapping.
- **Acceptance:** `def f(x: {Sync} String): {Sync} Unit` and the hand-written `def f[F[_] ~
  Sync](x: F[String]): F[Unit]` produce **identical** resolved signatures (golden test on
  `OperatorResolvedValue`); `{Sync, Abort}` and `{Abort, Sync}` produce identical signatures (the set
  property). No body lifting yet — bodies must already be in monadic form.

Implemented (2026-06-20): new `Expression.EffectfulType` AST node parsed by `effectfulTypeParser` (each brace
entry parsed as a `GenericParameter.AbilityConstraint`, i.e. the same shape as a `~` constraint); the new
`core.processor.EffectSugarDesugarer` runs per function inside `CoreProcessor` (before `transformFunction`) and
applies the uniform rule — collecting every `{…}` in the signature, deduplicating into one effect set, prepending a
single inferable `F[_]` carrier with `Eff[…, F]` constraints (carrier appended as each effect's final type arg, so
`Sync`→`Sync[F]`, `State[Account]`→`State[Account, F]`), and rewriting every `{…} A` to `F[A]` across signature and
body. No `EffectfulType` survives into conversion (`CoreExpressionConverter` hard-errors defensively if one does).
Verified by the golden equivalence + set-property tests at the core and operator levels, and an end-to-end ability
test where `{Monad} String` reproduces the M0 `[F[_] ~ Monad]` acceptance program. Next: M2 (library spine + run).

### M2 — Library spine + `Sync` + `Console` + run (no auto-lift) ✅ DONE

Implemented (2026-06-20). Stdlib abstract abilities `Monad[F[_]]` (`flatMap`/`pure`), `Applicative[F[_]]` (`map`),
`Sync[F[_]]` (`sync`), `Console[F[_]]` (`println`/`readLine`); `Console` added to `defaultSystemModules` (the one
user-facing effect resolves with no import — `println` moved out of `String`); the old top-level `println` removed. JVM
layer: `IO.els` gains `implement Monad[IO]`/`Applicative[IO]`/`Sync[IO]` (all fully Eliot-visible); a self-complete
`Console.els` re-declares the `Console` ability (the layer system *mixes* files — a carrier-generic instance must be
colocated with its ability, so the ability is duplicated and the merge verifies the signatures agree) and provides the
**constrained HKT instance** `implement[F[_] ~ Sync] Console[F]` plus the `private` leaf natives `printlnInternal` /
`readLineInternal`. Backend safety check `NativeImplementation.visibilityViolation` hard-errors if an `impure` native is
not `private`. Acceptance met end-to-end: `def echo : {Console} Unit = flatMap(readLine, s -> println(s))` +
`def main : IO[Unit] = echo` compiles to a jar and echoes stdin; `def main : IO[Unit] = println("Hello World!")` still
runs (Console→Sync→IO at `F := IO`); referencing `printlnInternal` cross-module is refused ("Name is private.").

Two general compiler bugs the effect carriers exposed (both fixed in `monomorphize/check/TypeStackLoop`):
- **Carrier collapse:** `applyTypeArgs` wrapped *every* ground type arg as an inert `VConst`; a higher-kinded carrier
  arg (`[F[_]] := IO`) must be an *applicable* `VTopDef` or `F[A]` silently reduces to `A` (the `applyValue` fallback
  returns its argument). Fix: route type-level structures through `Evaluator.groundToSem` (value structures stay `VConst`
  for reification).
- **Uncovered ability-method query:** an uncovered ability method (e.g. `flatMap` of `Monad[F[_]]` called where `Monad`
  is not a constraint) queried `Monad[F, A, B]` (carrier + method params) and never matched the single-param impl. Fix:
  slice the ref's type args to the ability marker's binder count (`abilityArity`).

Deviation from the literal acceptance line: `main` commits to the concrete `IO[Unit]` (Decision 8) rather than
`{Console} Unit`. `{Console}` *directly* on `main` does not yet work — the generated entry wrapper (`block(main)`) does
not pin `main`'s inferable carrier to `IO`, so the Console methods stay abstract; carrier-polymorphic `{Console}` on
*business* functions works (pinned at the `IO[Unit]` call site). Separately, the LSP type-checks the abstract-only
workspace, so `println`-using code now reports "does not implement Console" there (no platform impl) — both are tracked
as IDE/entry-point follow-ups, not M2 blockers.

Original M2 plan (for reference):

- `ability Monad[F[_]] { def flatMap[A,B](fa: F[A], f: A -> F[B]): F[B]; def pure[A](a: A): F[A] }` and
  `Applicative`/`map` (stdlib, abstract).
- **`Sync` — the internal reification tier** (Decision 9), abstract in stdlib:
  `ability Sync[F[_]] { def sync[A](thunk: Function[Unit, A]): {Sync} A }`. `sync`/`delay` embeds a
  deferred leaf native into `F` (it cannot be `pure`, which would force the native at compile time). Not
  a user-facing effect — users name fine effects, never `{Sync}`.
- **`Console` — the first public fine effect** (Decision 10), abstract in stdlib:
  `ability Console[F[_]] { def println(s: String): {Console} Unit; def readLine: {Console} String }`.
- Concrete JVM layer: `data IO[A](…)` already exists (`jvm/.../IO.els`); add `implement Monad[IO]`, the
  **base** `implement Sync[IO] { def sync(t) = IO(t) }` (the only place `IO` is named), the **generic,
  `Sync`-keyed** `implement[F[_] ~ Sync] Console[F] { def println(s) = sync(_ -> printlnInternal(s));
  def readLine = sync(_ -> readLineInternal()) }`, and the `private` leaf natives. Per Decision 10 the
  fine-effect instance is carrier-generic (works for any `F` with `Sync`), not pinned to `IO`; per
  Decision 9 each leaf is `private` and wrapped in Eliot, so the whole `IO(_ -> …)` tree stays
  Eliot-visible and nothing about carriers leaks into native code. Backend pins `main`'s carrier to JVM
  `IO` and runs `block`.
- **Safety check:** the backend's impure-native registration asserts each registered FQN's resolved def
  is `Visibility.Private`, hard-erroring otherwise (a forgotten `private` is caught at build, never a
  silent pure-typed-impure hole). Pure natives stay public, outside that registry.
- **Acceptance:** a hand-monadic `def main: {Console} Unit = flatMap(readLine, s -> println(s))` compiles
  to a jar and runs end-to-end — proves library + the `Console`→`Sync`→`IO` layering, and that the
  **constrained HKT instance** `implement[F[_] ~ Sync] Console[F]` resolves at `F := IO` (the net-new
  case vs. the existing unconstrained `implement Container[Box]`). Plus: a user module referencing
  `printlnInternal` directly fails to resolve (`private` boundary), and a backend test that registering a
  non-`private` impure native hard-errors.

### M3 — Body auto-lift (Part B, the headline) ✅ DONE

Implemented (2026-06-20). New `effect` package: `EffectDesugaredValue` (wraps `OperatorResolvedValue`, body-only
rewrite) + `EffectDesugaringProcessor` placed after `OperatorResolverProcessor`, before `SaturatedValueProcessor`
(whose primary input key is repointed to `EffectDesugaredValue`; its cross-value signature reads stay on
`OperatorResolvedValue`, unchanged by the body-only rewrite). The bind-insertion algorithm is a single recursive
bottom-up pass that returns, per sub-expression, `(rewritten, effectful?)`; an effectful argument flowing into a
*concrete (non-`Function`) value position* is bound with `Monad.flatMap` (or `Applicative.map` when the continuation
is pure), left to right; a pure body under a carrier-binder return is `Monad.pure`-wrapped. `flatMap`/`map`/`pure`
are inserted by fully-qualified `eliot.lang.Monad`/`eliot.lang.Applicative` name (no user import). **No lift** (one
carrier). The rewrite is **idempotent** — `flatMap(readLine, s -> println(s))` passes through untouched (a carrier-typed
`F[A]` parameter, like `flatMap`'s first arg, is a *storage* position, never bound).

"Effectful" is decided structurally (this runs before monomorphize, which stays the sole arbiter/backstop): a callee
result is effectful iff, fully applied, it is headed by one of the callee's own higher-kinded binders (`readLine : F[String]`);
a parameter reference is effectful iff its type is headed by the *current value's* ability-constrained higher-kinded
carrier (`fa : F[String]`, so `println(fa)` lifts). The value's ambient carrier is an **ability-constrained** HKT binder
(the M1 `{E...}` carrier or a hand-written `[F[_] ~ Monad]`) — a bare HKT generic (`C[_, _]`) is *not* a carrier, so
`f[A, B, C[_, _]](c) = id(c)` is never spuriously `pure`-wrapped. The value-param count is the body's leading-lambda
count (NOT the type's arrow arity), so a function-*valued* def (`def f : Function[A, B] = g`) is not mis-read as a
1-parameter `String`-returning function.

Fail-safe: a value with a nullary (pure) return whose body performs an effect is rejected here ("performs an effect but
is declared pure"); a no-carrier value whose body is *not* effectful, or one with no binding to insert, passes through.

**Acceptance met:** `def main : IO[Unit] = println(readLine)` compiles and echoes stdin (auto-lifted to
`flatMap(readLine, x -> println(x))`, carrier pinned `F := IO` by the return); the carrier-polymorphic
`def echo : {Console} Unit = println(readLine)` + `def main : IO[Unit] = echo` does too. `def helper : String =
println(readLine)` (reachable) is rejected with the fail-safe error. Tests: `effect.processor.EffectDesugaringProcessorTest`
(structural: bind shape, idempotency, pure-body no-op, fail-safe) + `jvm.ExamplesIntegrationTest` end-to-end +
`examples/src/Effects.els`. Full suite green.

Deviations from the literal acceptance line: (1) `readLine()` with parens does not parse (`()` is not empty-application
syntax) — direct style uses `readLine` (no parens); (2) `def main : {Console} Unit = ...` still does not *run* (the
generated entry wrapper does not pin `main`'s inferable carrier to `IO`, the M2 Decision-8 entry-point issue), so the
runnable headline pins the carrier via `IO[Unit]` — either on `main` directly or at the `main = echo` call site; (3) the
"declared pure" error fires only for *reachable* values (demand-driven compilation = use-site verification), so an
*unused* `def foo : Unit = println(readLine)` is simply never checked.

### M4 — Multi-effect composition + propagation + `Dep` ✅ DONE

Implemented (2026-06-20). Multiple effects in one signature, carrier-unified across callees; effect propagation as a
set-subset check; the `Log` and `Dep[X, F[_]]` effects with JVM instances. Two general compiler bugs that `Dep`
exposed were fixed along the way.

- **`Log` — a second public fine effect**, a clean mirror of `Console`: stdlib abstract `ability Log[F[_]] { def
  log(s: String): F[Unit] }` (ambient); jvm `Log.els` re-declares it + the constrained HKT instance `implement[F[_] ~
  Sync] Log[F] { def log(s) = sync(_ -> logInternal(s)) }` + `private def logInternal`; backend native writes a
  `[LOG] `-tagged line to stdout (impure, asserted `private`). `Dep` was also made ambient.
- **`Dep[X, F[_]]` — the dependency (reader) effect.** Abstract stdlib `ability Dep[X, F[_]] { def get: F[X] }`
  (ambient). The application supplies a per-dependency-type instance (a compile-time singleton, `implement
  Dep[Database, IO] { def get = pure(theDatabase) }`, colocated with the user's type); monomorphization collapses it
  to a direct reference. `get` is dispatched by the dependency type at the use site, and same-type-`Dep` overlap is
  rejected by the ordinary ability-overlap check.
- **Multi-effect composition + carrier unification** already fell out of M1+M3: a `{Dep[Database], Log, Console}` body
  threads every effect through the one carrier `F`, the auto-lift inserting the binds; `{Sync, Abort}` ≡ `{Abort,
  Sync}` set semantics hold. Type-arg'd effects (`Dep[Database]`) desugar to `Dep[Database, F]` (M1's carrier-append).
- **Effect propagation = a definition-local subset check** (Decision 6), added to `EffectDesugaringProcessor`: a
  carrier-polymorphic body's *used* effects must be ⊆ its *declared* set. Used effects come from its effectful callees
  (an ability method → its owning ability via the FQN's `Qualifier.Ability`; an ordinary `{E...}` function → the
  effects on its carrier binder), excluding the machinery `Monad`/`Applicative`/`Sync`. At *ability granularity*
  (declaring `{Dep[Database]}` covers any `Dep` use; a `Dep[Logger]`-vs-`Dep[Database]` mismatch is left to the use
  site, per use-site verification). A concrete-carrier value (`main : IO[Unit]`) has no declared set and is exempt.
  Undeclared effect ⟹ hard error at the definition. This is a structural well-formedness check on the effect
  annotation, not an instantiation-dependent typing obligation.

**Two general compiler bugs `Dep` exposed, both fixed** (both pre-existing, both *silent miscompiles* — neither
effects-specific; they bite any multi-instance ability whose methods share an erased descriptor):

1. **Backend method-name collision (codegen).** The JVM method name was `vfqn.name.name + mangleSuffix(typeArgs)`,
   dropping the impl-distinguishing qualifier. Two *concrete* impl methods of one ability that also share an erased
   descriptor — `Dep[Database, IO].get` and `Dep[Logger, IO].get`, both `() -> IO` — collided into one method
   (`ClassFormatError`, or one call binding to the wrong impl). (`Show[Hello]`/`Show[World]` escaped only because their
   value-arg descriptors differ.) Fix: `CommonPatterns.mangledMethodName` folds an `AbilityImplementation` index into
   the name (`get$Dep$impl$0`), applied uniformly at the definition, the dedup key, and the call site.
2. **Monomorphize ability mis-dispatch (NbE checker).** Two distinct `Dep`s' `get` in one body both resolved to the
   *first* impl (the Logger method was never even generated). Root cause: the covered-path resolution
   `findConstraintTypeArgs(paramConstraints, abilityName)` matched a constraint by *ability name only* and returned the
   first, so with two `Dep` constraints every `get` ref used the first. Fix (`TypeStackLoop.tryResolveOne`): prefer the
   *reference's own* fully-ground type args (they pin the exact impl), falling back to the constraint only while the
   ref's args are still unsolved metas. The drain loop solves those metas before resolution, so both explicit
   (`get[Database]`) and return-dispatched (`url(get)`) forms now dispatch correctly.

- **Acceptance met:** `examples/src/EffectsMulti.els` — a `{Dep[Database], Log, Console}` program — compiles and runs
  end to end (`[LOG] jdbc://app-db` then the echoed line); the undeclared-effect (`{Console}` calling a `{Log}`
  function) is rejected with a precise propagation error; two distinct-typed `Dep`s in one body resolve `get` to their
  *correct distinct* values (`the-db` / `the-logger`); a same-type `Dep` overlap is rejected. Tests:
  `effect.processor.EffectDesugaringProcessorTest` (subset check) + `jvm.ExamplesIntegrationTest` (M4 section). Full
  suite green.
- **Known pre-existing limitation (NOT exercised by `Dep` or effects, out of M4 scope):** a *generic-data*
  constructor inside a *generic* ability impl (`implement Provide[Aaa] { def give: Box[Aaa] = Box(Aaa(..)) }`) still
  miscompiles (a `NoSuchMethodError` on the `Box` constructor) — a separate generic-data codegen issue. `Dep`'s
  instances construct *concrete* types (`pure(Database(..))`), so they are unaffected.

### M5 — Structural-effect discharge, ordering, testability, Dep erasure

- Add effects whose discharge reflects a **data structure**: `Abort` (⟹ `Option`), `Throw[E]` (⟹
  `Either[E, _]`), `State[S]` (⟹ `(_, S)`), each with its discharge function (Part C), implemented
  over a concrete transformer (`OptionT`/`EitherT`/`StateT`) with the needed `Monad` + lifting
  instances **inside the library**. The same per-transformer base lifting (`Sync`/`Monad`: e.g.
  `Sync[OptionT[G]]` from `Sync[G]`, the `MonadIO`-style lift) is what makes the fine I/O effects of
  Decision 10 work through *any* IO-stack — a `Sync`-keyed `Console`/`Clock`/`File` rides that lift, so
  lifting is written once per transformer for the base (n instances), never per fine effect (the n×m
  matrix collapses to n).
- Demonstrate **ordering at the edge:** `runState(runAbort(p), s0) : (Option[A], S)` vs
  `runAbort(runState(p, s0)) : Option[(A, S)]` — both type-check and give the two expected, different
  results (state-survives-abort vs abort-discards-state).
- Demonstrate **static testability:** run the same business logic under a test carrier / test `Env`
  (fake `Database`), asserting on output — no production `IO`. The test carrier supplies its own *direct*
  `implement Console[TestF]` (not via `Sync`), so `{Console}` logic runs with no `IO` — coherent because
  it is a different carrier type (Decision 10).
- **Dep erasure:** confirm monomorphization collapses a compile-time-singleton dependency to a direct
  reference; document the pattern.
- **Acceptance:** the order-sensitive pair above; a prod-vs-test run of one logic; a codegen check that
  a singleton dependency leaves no allocation; and a `{Console, Abort}` program type-checks — the fine
  effect resolving through the `OptionT[IO]` stack via the base `Sync` lift, confirming the
  constrained-HKT-instance + lifting path of Decision 10.

### Cross-cutting (fold into the milestones)

- **Errors:** undeclared-effect, carrier-mismatch, missing-instance, and **failed-discharge-refinement**
  (`F := OptionT[G]` unsolvable) messages point at the source span (use the `matchdesugar`
  `compilerError`/`compilerAbort` pattern).
- **IDE:** surface the inferred effect set at hover (extend `TypeHintIndex`) so the implicit binds stay
  legible — this is how "no `!` marker" buys back visibility. (See `docs/ide-type-hints.md`.)
- **`match`/effects:** an effectful scrutinee binds before the `match`
  (`readLine() >>= s -> match s { … }`); run `effect` after `operator`, with `match` already lowered to
  `handleCases`, so the scrutinee is an ordinary argument the auto-lift handles uniformly.

## Open design decisions

- **Discharge typing precision.** Part C types each discharge at a concrete transformer and relies on
  the unifier solving `F := OptionT[G]` at the call. Confirm the existing injectivity unification
  (`?F[A] ~ Box[A] ⟹ ?F := Box`, `Unifier.scala:159-198`) extends to `?F := OptionT[G]` with a fresh
  `G` carrying the residual constraints, and that a residual constraint not satisfied by the chosen
  transformer (`State[OptionT[G]]` requires a lifting instance) produces a clear missing-instance error
  rather than a stuck meta.
- **One effect, two observations.** `Abort` discharges to `Option` via `runAbort`, to a default via
  `runAbortOr(d)`. These are different discharge *functions* over the same effect — fine, and the
  intended way to vary observation without dynamic handlers. Decide which observations the stdlib ships
  per effect.
- **Effects in data/HOF positions.** `F[A]` is an ordinary type, so a field `run: {Console} Unit`
  (= `F[Unit]`) is representable, but constructing `Handler(readLine())` should **store** the action,
  not bind it. Rule: a data field typed `F[A]` is a **storage** position (no auto-bind); only
  *value-argument positions expecting a pure type* auto-bind. Make explicit and tested.
- **`pure` ambiguity.** A pure body under an effectful return needs `pure[F]`; ensure it never fires
  spuriously while `F` is still a meta (defer like other constraint-covered references).
- **Coherence.** Each concrete carrier needs unique, non-overlapping effect instances; rides on the
  existing ability overlap checks but should be tested for HKT instances. This is what gives "one `Dep`
  per type" for free (`Dep[Database]` twice = overlapping instances → rejected). The specific net-new
  case to spike is the **constrained HKT instance** (`implement[F[_] ~ Sync] Console[F]`, Decision 10):
  M0 proved `[F[_] ~ Cap]` as a constraint on a *function* param; on an *instance* param it is unverified
  (existing HKT-instance evidence, `implement Container[Box]`, is unconstrained).
- **Part A placement.** Core-level desugar (recommended) vs a dedicated pre-resolve step. Core keeps it
  with the other sugar; confirm `CoreProcessor` can introduce an inferable generic cleanly.
- **Library lifting strategy.** The one real authoring cost (Part C instances). Decide between explicit
  mtl-style per-pair lifting instances vs a single freer/`Free`-style carrier that interprets a set of
  operations (avoids n² but needs its own fusion story under monomorphization). Start with explicit
  transformers for a small fixed effect set; revisit if the matrix grows. Note the matrix is already
  smaller than it looks: fine I/O effects are keyed on `Sync` (Decision 10), so they ride the base
  `Sync`/`Monad` lift and need no per-effect lifting instances — only the base capabilities are lifted
  per transformer (n, not effects × transformers).

## Effects and termination are one mechanism

Because a no-effect function is a plain value the compile-time NbE evaluator fully evaluates, while any
effect makes it stuck (a neutral), a **`Recursive` effect** is just another effect in the set:
`{Recursive} A` marks a possibly-non-terminating computation, and a plain `A` (no `Recursive`) is
terminating *by typing*. The effect system and the termination model are therefore the **same
machinery** — a strong consistency check on this design. See `project_recursion_as_effect`.

## References

- Cornerstones: "Types Are Values (λ\*)", "Platform-Independence via Layers", "Use-Site Verification"
  in `.claude/CLAUDE.md`.
- Related: `docs/ide-type-hints.md` (hover should show the inferred effect set);
  `project_recursion_as_effect` memory (effectful code gets stuck/neutral in the pure NbE evaluator —
  the same mechanism that keeps the compile-time evaluator from running `IO`, and the basis for the
  effects = termination unification).
- Template phase: `matchdesugar` (`lang/src/com/vanillasource/eliot/eliotc/matchdesugar/`).
- Param/constraint introduction: `saturate`
  (`lang/src/com/vanillasource/eliot/eliotc/saturate/processor/SaturatedValueProcessor.scala`).
- HKT evidence: `MonomorphicTypeCheckTest.scala:436-498`,
  `AbilityImplementationCheckProcessorTest.scala:378-451`, `Unifier.scala:159-198`.
