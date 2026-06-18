# Implicit / Inferred Generic Parameters (`auto`)

Status: **W0–W4 implemented; W5–W6 remain.** Input generalization (W1), data-field generalization (W2),
calculated returns (W3), and the limit diagnostics (W4) are built and tested; what is left is propagation /
display polish (W5, including the two W4-deferred *completeness* items — the `Combine`-join postpone and transitive
"viral" bounds) and IDE explorability (W6, including the symbolic `ElaboratedSignature` fallback). This document
records the theory, the surface design, the architectural change it requires, a staged implementation that is
closed and testable at each step, and — equally important — exactly where the system stops working and how it must
say so out loud.

## Goal

Let a user write **bare `Int`** (and, in time, any bounded / indexed constructor) almost everywhere, with the
bounds threaded automatically:

- in **parameter** and **data-field** positions, the omitted bounds become **compile-time generic inputs**
  (universally quantified, monomorphized per call) — so `def isEven(x: Int): Bool` works for every width;
- in **return** positions, the omitted bounds are **calculated** — inferred from the body and propagated to
  callers — so `def double(x: Int): Int = x + x` publishes the doubled range without anyone writing
  `Int[add(MIN,MIN), add(MAX,MAX)]`.

Crucially this buys **precise elision, not inexactness**: every type stays exactly as precise as today; the user
just stops spelling the parts the compiler can recover. Load-bearing parameters that the compiler must *not*
silently invent (`IO`'s `A`, a container's element type) stay mandatory unless their author opts them in.

The same machinery is the intended substrate for future *calculated* indices: algorithm cost (real-time
guarantees), resource/memory usage, and similar — any attribute that composes forward through primitives the way
a numeric bound composes through `+`.

## Theory

### What we are adding is an implicit-domain dependent Π

"Parameter = generic input, return = calculated" is one object in type-theory terms — a dependent function type
whose domain binders are *implicit*:

```
{MIN MAX} → Int[MIN,MAX] → Int[add MIN MIN, add MAX MAX]
   └ implicit Π-binders ┘          └ codomain is a computation over the binders ┘
```

The binders in parameter position are **rigid/universal** (the body cannot inspect them; it must work for all —
that is the "generic input"). The return is the **codomain of the Π**, an expression *mentioning* those binders.
Eliot already has this object: it is `VPi` (`monomorphize/domain/SemValue.scala`). Two things are new: (1)
*marking* the domain binders implicit, and (2) recovering the codomain expression *from the body* instead of
making the author write it.

### Generalize vs. calculate, and why the split is principled

- **Parameter/field (contravariant) positions generalize.** They are the *sources* of polymorphism, so an
  omitted bound mints a fresh universally-quantified binder. Each call site infers it from its argument and
  monomorphizes (`MonomorphicValue.Key`'s `specifiedTypeArguments`, `fact/MonomorphicValue.scala`).
- **Return (covariant) positions calculate.** They are *consumers* of the binders; an omitted bound is *solved*
  from the body, generally to a symbolic expression over the input binders. Output positions never introduce
  quantified binders — a producer cannot be polymorphic in its own output bound (that contravariance is exactly
  why a body-less abstract declaration cannot have a calculated return; see Limits).

This is the standard "generalize at binding sites, solve at use sites," variance-aligned. The unifier already
carries the matching instinct: combinable metas are tainted when they flow into a contravariant `VPi`-domain
position (`monomorphize/unify/Unifier.scala`).

### Why "infer the calculated return" is rare — and why Eliot can do it anyway

Recovering a *closed type-level function* for a result from a body is **anti-unification** (the inverse of
unification): it has **no principal solution**. A body mapping `Int[3,3] ↦ Int[6,6]` fits `λx. x+x`,
`λx. Int[6,6]`, `λx. Int[x*2]`, … infinitely many. You cannot recover the *intensional* computation from
*extensional* behaviour. That is why Haskell (type families) and Agda/Idris make you **write** `Vec (Add m n) a`
/ `Vec (m + n) a` and only **check** it; they infer type *variables*, never type *functions*.

Eliot does not invert — it **calculates forward**, and primarily on *concrete* values. Type-checking here *is*
monomorphization, driven from `main` over concrete types (`used` collects from `main`; `MonomorphicValue.
Key(vfqn, specifiedTypeArguments)` keys each body-check by concrete type arguments). So a producer's body is only
ever checked at the **concrete instantiations actually used**: when `double` is used at `[0,255]`, the checker
checks `x + x` with `x : Int[0,255]` and the return `Int[0,510]` falls straight out — a *byproduct* of
monomorphizing the callee, already sitting in `MonomorphicValue(double,[0,255]).signature`. The symbolic
`add(MIN,MIN)` need never be formed. It composes the same way: `quad[0,255]` checking `double(double(x))` grounds
the inner call at `[0,255]` (→ `Int[0,510]`) and the outer at `[0,510]` (→ `Int[0,1020]`), because the outer
producer is itself grounded, and everything bottoms out at `main`. **No symbolic index, no inversion.**

When a producer's return is needed *independently of any concrete use* — checking a never-called producer, or
showing a principal return type in tooling — there is no concrete driver, so Eliot falls back to **symbolic
forward composition**: check the body with the input binders as **neutral** variables and let NbE propagate, e.g.
`x + x ⤳ Int[add MIN MIN, add MAX MAX]`, a neutral `SemValue` quoted into the signature. This is still forward,
still principal by construction (every primitive carries its own result computation), still never inversion — it
is only the fallback for when concreteness has no driver.

Either way the calculability check is **decidable**: evaluate the result (concretely, or forward on neutrals) and
try to quote it. If the strict quoter (`monomorphize/eval/Quoter.scala`, which already fails on
`VNeutral`/`VMeta`/`VLam`/`VNative`/unforced `VTopDef`) succeeds, the return is calculated; if it gets stuck, that
is precisely a Limit (below) and must be reported, never papered over.

### The architectural consequence

The caller must read the callee's **body-checked** return, not its **source** type stack. Today the checker
re-reads source: at a `ValueReference` it fetches the callee's `SaturatedValue` (switched from `OperatorResolvedValue`
in W1) and evaluates its `typeStack.value.signature` (`monomorphize/check/Checker.scala`, the `ValueReference` case).
A bare-`Int` *return* there still evaluates to an under-applied `VTopDef(IntFQN, None, SNil)` — W1 saturates only
*parameter* positions, so returns are left untouched — and the body's `add(…)` relationship is nowhere in it, because
it was produced while checking the *callee's body* and is not written back.

The fix follows directly from the concrete model above: that body-checked return **already exists** as
`MonomorphicValue(callee, concreteArgs).signature`, produced when the callee is monomorphized. So the caller, when
checking `double(b)`, infers the callee's concrete type args from its arguments (using the body-free saturated
*domain* — see Architecture), then reads the return off the callee's `MonomorphicValue`. That is the entire
"back-edge", and it **reuses an existing fact** rather than introducing a new symbolic one. Only the
use-independent case (no concrete driver) needs a symbolic elaborated signature, as a fallback. Every language
that infers result types makes the *body-derived* type the interface (HM's principal types; Agda's elaborated
signatures normalized at use sites); Eliot's source re-read is the anomaly.

### Cornerstone fit

- `auto` is a **visibility tag** on a parameter — *sanctioned sugar*, the same flavour as the
  `Qualifier.Type`/`Default` namespaces and `[]` vs `()`. It controls *elaboration* (may this argument be
  omitted?), not type equality, and applies uniformly to type-level and value-level binders. It introduces **no**
  type/value stratification.
- The number of holes to fill for a bare `Int` is the count of **leading `auto`-marked binders in `Int`'s
  declaration**, read structurally from the declaration the user wrote — **never** `RoleHint.TypeConstructor.
  typeParamCount` (`core/fact/RoleHint.scala`, which stays write-only). Arity-to-saturate is an *elaboration*
  read of user-written markers, never a *typing* input.
- One evaluator only; `Coerce` (bound-of-record widening) and `Combine` (branch joins) are reused unchanged.

## The marker

A keyword on a generic-parameter binder declares it **omittable**: at any use of the enclosing name that
parameter may be left out and the compiler supplies it. The surface keyword is **`auto`** — chosen for Eliot's
embedded/microcontroller audience (many from C/C++, where `auto` already reads as "the compiler fills this in")
and because the marker's actual contract is *omittable*, not *inferred*: it is mechanism-neutral and so covers
both the parameter (generalize) and return (calculate) halves honestly. The internal/spec vocabulary stays
"inferred input / calculated return"; the keyword need not equal the mechanism name. (Rejected: `implicit` —
collides with abilities; the `~MIN` sigil — illegible to exactly this audience and easy to miss.)

```eliot
type Int[auto MIN: BigInteger, auto MAX: BigInteger]   -- bounds omittable
type IO[A]                                             -- A mandatory: bare `IO` is an error
type Byte = Int[-128, 127]                             -- explicit supply still allowed everywhere
```

Rules:

- **Omittable ≠ forbidden-to-write.** `Int[0,255]` keeps working; bare `Int` triggers saturation. (Idris's
  `{n = 5}` analogue.)
- **Per-author opt-in.** Only the type's author marks its parameters; this is what keeps `IO`'s `A` mandatory
  while `Int`'s bounds vanish — the distinction the uniform "saturate any under-applied constructor" rule cannot
  make on its own.
- **Position, not declaration, picks generalize-vs-calculate.** The marker says "omittable"; *where* the
  omission occurs decides whether the hole becomes a fresh input binder or a calculated output.

## Architecture: one new fact, plus a fallback

1. **`SaturatedValue`** (body-free rewrite; **W1 — built**). For each value, rewrite its source type stack so every
   *input-position* bare inferable reference becomes explicit (`Int` → `Int[$lo,$hi]`) with the fresh binders
   added to the enclosing definition's generic prefix; every *return-position* bare inferable reference is rewritten to
   the kind-correct `Type` placeholder and the value flagged `calculatedReturn` (§2; **W3 — built**), to be *calculated*
   at the use site. Monomorphize and the `ValueReference` read consume `SaturatedValue` instead of `OperatorResolvedValue`. (We cannot
   rewrite `OperatorResolvedValue` itself — it is produced upstream by the `operator` phase, so feeding monomorphize's
   results back would be a fact cycle.) `SaturatedValue` carries the callee's **domain** body-free, which is what lets
   a caller infer the callee's concrete type args without its body.

2. **Calculated returns reuse `MonomorphicValue`** (no new fact, primary path; **W3 — built**). A `calculatedReturn`
   value is filled by the callee's existing per-instantiation body-check: the caller infers concrete type args from
   `SaturatedValue`'s domain and reads `MonomorphicValue(callee, args).signature`'s deep return. On the callee side the
   `Type` placeholder is swapped for a fresh metavariable the body solves (`Checker.installReturnMeta`); on the caller
   side the `ValueReference`-application codomain (the `VType` placeholder) triggers `Checker.resolveCalculatedReturn`,
   redirected from "evaluate the source return level" to "read the callee's monomorphized return". This is the entire
   hot path; it rides the monomorphization the compiler performs anyway.

3. **`ElaboratedSignature`** (fallback, symbolic, use-independent only; **not yet built — deferred**). When a producer's
   return is needed with no concrete driver — a never-called producer, or tooling that wants a principal signature —
   check the body with the input binders as neutral variables, forward-evaluate, and quote the symbolic result into the
   return level. Kept strictly off the driven-from-`main` path; the concrete reuse in (2) handles all reachable code, so
   this fallback is unnecessary for compilation and is left for the tooling work (W6).

Input/data saturation is body-free and lives at the core boundary; return calculation needs the body and lives in
monomorphize — and, on the primary path, *is already computed there* by ordinary monomorphization.

### Fact-flow discipline: forward, don't project

Keep the fact flow **as lean as possible**. When a later stage needs a piece of information, **forward it onto the
follow-up fact that already flows to that stage** — add the field to the fact the consumer already reads — rather than
minting a separate projection fact (a processor that only copies one field) or making the consumer reach back across
phases to an earlier fact. A new fact earns its place only when it carries *new* information (a genuine
derivation/merge, like `SaturatedValue`'s rewrite or `MonomorphicValue`'s body-check), never when it merely re-exposes
a field that an existing fact in the chain could have carried.

This was learned the hard way in W0 (see its status): a dedicated `InferableArity` fact + `InferableArityProcessor`
was built, then removed — it only copied `NamedValue.inferableArity` out of the per-FQN `UnifiedModuleValue` that
already carries it. The count now lives solely as that field and is forwarded along the chain to wherever a consumer
needs it.

## Work items

Each stage is independently mergeable and testable; later stages depend only on earlier ones.

### W0 — Marker surface + plumbing (no behaviour change)

- **What:** parse `auto` on generic-parameter binders; carry an `inferable: Boolean` on the binder through
  `ast` → `resolve` → `core` (`ArgumentDefinition` / generic-parameter structures); expose, per value FQN, its
  *leading inferable-binder count* as a derivable fact for later stages.
- **Where:** `ast/fact/Expression.scala` and the generic-parameter parser; `resolve` + `core` parameter
  structures; a small `InferableArity` (or a field on an existing core fact).
- **How:** purely additive. Bare under-applied constructors still error exactly as today (no saturation yet).
- **Test:** `auto` parses and round-trips to core; an `InferableArity(IntFQN) == 2`-style fact is produced;
  existing suite unchanged; bare `Int` in a signature still produces today's "type mismatch".
- **Status:** DONE. `auto` is a **soft keyword** (a plain lowercase identifier, like `opaque`/`left`; unambiguous
  because generic-parameter names are always upper-case) — no tokenizer reservation. Threaded as `inferable: Boolean`
  on `ast.GenericParameter` and `ast.ArgumentDefinition` (a `type X[..]`/`data` type-constructor's params become value
  *args*, so the marker rides both); the leading-inferable count is computed in `CoreProcessor` (leading run over
  `genericParameters ++ args`) and stored as the field `NamedValue.inferableArity`. That field **is** the per-FQN
  derivable fact — `NamedValue` rides through `ModuleValue`/`UnifiedModuleValue` (both keyed by `ValueFQN`).
  - *What happened (kept as a lesson — see "Fact-flow discipline" above).* A dedicated `InferableArity` fact +
    `InferableArityProcessor` were first built and then **removed**: the processor only copied the count out of the
    `UnifiedModuleValue` that already carries it — a projection fact earning nothing.
  - *Consumption rule going forward.* Don't reach back to `UnifiedModuleValue` from a later phase. **Forward** the
    count along the existing chain: when W1's consumer exists, add the field it needs to the follow-up fact it already
    reads (`ResolvedValue` currently drops it; `SaturatedValue` is the natural carrier into monomorphize), keeping the
    flow lean and linear.
  - Both base `stdlib` and `jvm` `Int.els` declare `Int[auto MIN, auto MAX]`. Tests: `ASTParserTest` (marker
    round-trip) + `UnifiedModuleValueProcessorTest` (per-FQN counts, incl. leading-run-only). `signatureEquality`
    deliberately ignores `inferable`, so the marker never affects layer unification.

### W1 — Input-position generalization for functions (the cheap, no-back-edge win)

- **What:** in `SaturatedValue`, rewrite every *parameter*-position bare inferable reference to an explicit
  application over fresh binders, prepended to the function's own generic prefix. Each occurrence is **independent**
  (matching how `+` is hand-written: left/right ranges differ).
- **Where:** new `SaturatedValueProcessor` (body-free); `monomorphize` entry + the `Checker` `ValueReference` read
  switch from `OperatorResolvedValue.Key` to `SaturatedValue.Key`.
- **How:** because callers read the *rewritten* signature, the synthesized binders are ordinary generic params;
  the existing "too few explicit type args → infer the rest" machinery
  (`Checker.peelLams` / `instantiatePolymorphic` / `TypeStackLoop.instantiateRemaining`) solves them from the
  argument with no new inference. No body *rewriting* needed — the type stack is the single source of truth for
  parameter types (body lambdas are emitted unannotated; see Status), so saturating the signature suffices; no
  elaborated-signature fact needed.
- **Test (end-to-end):** Int *consumers* fully work — `def isEven(x: Int): Bool`, `def store(x: Int): IO[Unit]`;
  a caller `isEven(b)` with `b : Int[0,255]` checks and monomorphizes; bare `IO` (unmarked) still errors;
  `Int[0,255]` explicit still works; two bare `Int` params get independent ranges.
- **Status:** DONE. New `saturate` package: `fact/SaturatedValue` (wraps an `OperatorResolvedValue` carrying the
  rewritten type stack) + `processor/SaturatedValueProcessor` (consumes `OperatorResolvedValue`, emits one
  `SaturatedValue` per value — a no-op pass-through when nothing saturates). Registered in `LangPlugin` right after
  `OperatorResolverProcessor`. `MonomorphicTypeCheckProcessor`'s input key and the `Checker` `ValueReference` read
  (now ~636) switched from `OperatorResolvedValue.Key` to `SaturatedValue.Key`; the ability-impl reads
  (`coercionPayload`/`combinePair`) stay on `OperatorResolvedValue` (their signatures are explicit, so saturation is a
  no-op there).
  - *Forwarded, not projected* (per "Fact-flow discipline"): `inferableArity` now rides `ResolvedValue` →
    `MatchDesugaredValue` → `OperatorResolvedValue` (each had the field + threading added), so the processor reads a
    *referenced* constructor's leading-`auto` count straight off its `OperatorResolvedValue` — no reach-back to
    `UnifiedModuleValue`, no new projection fact.
  - *What the rewrite does.* Walk the signature: preserve existing leading generic `FunctionLiteral`s, then walk the
    curried `Function[dom, cod]` chain. Every domain is a contravariant **parameter position** (the param/return split
    is pure variance — the syntactic arg-count is irrelevant, so no arg-count needs threading); the final non-`Function`
    head is the **return** and is left for W3. In each `dom`, a bare under-applied `auto` reference (`Int`, args < its
    `inferableArity`) has its omitted leading params filled with fresh binders (`Int` → `Int[$Int$0, $Int$1]`), which
    are prepended to the generic prefix; the kind level is synthesized/extended to match. Recurses into ordinary applied
    constructors (`List[Int]`) but **not** into `Function` arrows (cost-in-arrow, Limit 6). Non-`auto` references
    (`inferableArity == 0`, e.g. `IO`) are left bare, so the ordinary check still rejects them (the use-site guardrail).
  - *`saturate` rewrites only the type stack — never the runtime body.* The body's value-parameter lambdas were a
    snag: their parameter-type annotations carried the *un*-saturated type (a bare `Int` vs the saturated
    `Int[$lo,$hi]` domain) → "Type mismatch." The wrong fix (and an explicit non-goal) is to walk/count the body's
    leading lambdas and strip them — that re-introduces a "classify body structure" assumption the uniform model
    forbids. The right fix is upstream and structure-free: `CoreExpressionConverter.buildCurriedBody` now emits the
    value lambdas **unannotated** (`parameterType = None`). The *type stack* is the single source of truth for parameter
    types — the body is always *checked* against it, and the `check(FunctionLiteral, VPi)` case already takes an
    unannotated param's type from the `VPi` domain — so the annotation was pure redundancy. `saturate` therefore copies
    `runtime` through verbatim.
  - *Gotcha that bit, kept as a lesson.* An abstract type-constructor's signature **is its kind-chain**
    `Function[BigInteger, …, Type]` (a `FunctionApplication` chain), *not* a `FunctionLiteral` chain like a
    `def`-with-generics — so the binder-kind extraction must read curried *domains* there, not `FunctionLiteral`
    paramTypes, or the fresh binders wrongly get kind `Type` instead of `BigInteger`.
  - Tests: `MonomorphicTypeCheckTest` "implicit generic inputs (W1)" (caller inference, direct callee monomorphization
    at bound args, two independent ranges, fully-applied `IO[Unit]` return, the bare-`IO` guardrail, explicit unchanged)
    + the `examples/src/ImplicitIntParam.els` end-to-end (`def describe(x: Int): String = intToString(x)`, prints `42`).

### W2 — Data / type-definition field positions (correlated params)

- **What:** extend `SaturatedValue` to `data` field types and `type` bodies. A bare inferable field
  (`data Counter(n: Int)`) adds fresh binders to the **data type's** generic list, *shared* across the
  constructor result, accessors, and match impls (`Counter(Int[lo,hi]) : Counter[lo,hi]`).
- **Where:** run the data saturation at/around `core/processor/DataDefinitionDesugarer.scala` so the synthesized
  binders are in `definition.genericParameters` *before* desugaring — the desugarer already threads
  `genericParameters` through the type constructor (`:48,:52`), the value constructor's result type (`:67-71`),
  accessors (`:115-121`), and the auto-generated `PatternMatch`/`TypeMatch` impls (`:140-172,:222-261`). This is
  the key reuse: the "generic data type" target shape already works (the jvm-Int "mixed-width `data`
  end-to-end" test), so this is "auto-fill `genericParameters`," not new threading.
- **How:** the correlation (field bound = type param, shared everywhere) is exactly what prepending to
  `genericParameters` gives; unlike W1's independent-per-occurrence rule, the field type and the constructor
  result must reference the *same* fresh binders.
- **Test:** `data Counter(n: Int)` desugars to a generic `Counter[lo,hi]`; construct/access/match round-trip with
  the field's bounds; a mixed-field `data Pair(a: Int, b: Int)` yields four independent binders; explicit
  `data SmallInt(v: Int[0,255])` is unchanged.
- **Status:** DONE (single-constructor records; multi-constructor unions deferred). **Implemented in the `saturate`
  phase, not the desugarer** — the planned "augment `definition.genericParameters` before desugaring" is *infeasible*:
  the field's leading-`auto` arity needs name resolution (`Int` → FQN → `inferableArity`), and resolution depends on
  `ModuleNames` ← `CoreAST` of every file, so the (pre-resolution) `DataDefinitionDesugarer` cannot obtain arity without
  a hard fact cycle. So the saturation runs post-resolution and transforms the already-split, already-resolved family
  coordinately (the "re-derives desugarer structural knowledge" trade-off, accepted over restructuring desugaring to be
  arity-aware).
  - *Mechanism (`saturate/processor/SaturatedValueProcessor.scala`, `dataSaturate`).* Each value is classified into a
    data role and rewritten with a **shared binder plan** (`TypePlan`) computed once from the value constructor's
    bare-`auto` fields:
    - **type constructor** (identified by `Type` qualifier + a value constructor of the same name exists — *not* by
      `RoleHint.TypeConstructor`, whose count stays cornerstone-write-only): its kind chain grows `… → Type` ⇒
      `BigInteger → … → Type` and `inferableArity += total`, so bare references to it elsewhere saturate via W1.
    - **value constructor** (`RoleHint.ValueConstructor`, record = ctor name equals type name): each bare field gains
      its binder slice and the result type is applied to *all* the binders (`Counter` ⇒ `Counter[lo,hi]`).
    - **accessor** (new `RoleHint.FieldAccessor(dataType, fieldIndex)`, since field order is not recoverable from the
      abstract, name-less value-constructor signature): `obj` takes `T[all binders]`, the return is the field type at
      its slice.
    - **match impls** (`PatternMatch`/`TypeMatch` by qualifier; data type via `ImplementationMarkerUtils`): a uniform
      walk applies `T ⇒ T[all]` and slices each successive embedded field type in `handleCases`'s Church selector; the
      shared binders are prepended in front of the member's `R`. The `typeMatch` matcher additionally has its `matchCase`
      handler *rebuilt* from the grown generic kinds (`rebuildTypeMatchHandler`): the desugarer built it from the
      desugar-time generics only (`Function[Unit, R]` for a record that had none), so it is replaced with a curried
      `Function[explicit-kinds.., BigInteger, …, R]` over the explicit-generic kinds (recovered from the matcher's own
      leading binders) followed by the W2 binder kinds — letting a type-level `case T[g.., lo, hi]` bind the synthesized
      bounds (the native already applies the handler across the matched value's whole spine, so only the handler's *type*
      needed growing). The abstract `Fields` associated type needs nothing: the match desugarer resolves a surface type
      match straight to this concrete matcher, never through `Fields[R]`.
  - *Fact-flow (forward, don't project).* `RoleHint` now rides `ResolvedValue` → `MatchDesugaredValue` →
    `OperatorResolvedValue` (mirroring W1's `inferableArity` forwarding) so the saturate phase can read it without
    reaching back to `UnifiedModuleValue`. `recordPlanFor` guards its value-constructor lookup with `UnifiedModuleNames`
    (requesting a missing `Default` value aborts with "Could not find", which would otherwise fire for every abstract
    `Type`).
  - *Reuse.* No new threading in the desugarer — the grown-arity generic-data shape is exactly what the existing
    desugarer already produces for explicit generics, and the `handleCases` native is unaffected (it reads `fieldCount`
    from `RoleHint`, independent of generic arity). Tests: `MonomorphicTypeCheckTest` "implicit generic data fields
    (W2)" (construct / reject / four-binder `Pair` / explicit unchanged / access / match) + `ExamplesIntegrationTest`
    end-to-end (`data Counter(n: Int)`, construct+access prints `42`, explicit match prints `7`) +
    `examples/src/ImplicitIntField.els`.
  - *W2 follow-ups:*
    1. **Multi-constructor unions with bare `auto` fields** (`data Maybe = Nothing | Just(value: Int)`) — **still
       deferred (open design point); fail-safe today.** A constructor is only treated as a record when its local name
       equals the data type's, so union constructors fall through to W1: the field generalizes per-occurrence but the
       *type* does not grow (`Just(5) : Maybe`, bounds not threaded onto `Maybe`). This is *exactly* equivalent to
       writing the bound explicitly — `Just(value: Int)` and `Just(value: Int[0, 255])` produce identical behaviour
       (construct type-checks at the bare union type; supplying `Maybe[0, 255]` hard-errors loudly; field extraction is
       gated by the same `Combine`/`Coerce` as any union) — so the auto deferral introduces no new partiality, it just
       does not *gain* bound-tracking through the union. Verified by two guardrail tests in `MonomorphicTypeCheckTest`.
       Growing the union type to `Maybe[lo, hi]` (correlated, per-constructor binders on a shared type constructor, the
       other constructors' slots left free in each constructor's result) is the open design point: the free-binder
       ambiguity (`Nothing : Maybe[lo, hi]` leaves both slots unconstrained — a *producer* polymorphic in its own output
       bound, which §Theory's variance rule forbids) ties it to W3's calculated-return machinery. **Still deferred after
       W3.** W3 (now built) handles the *function-producer* output-bound case by calculating from the body and reading
       the callee's `MonomorphicValue` — but a *union constructor* has no single body to calculate from (each
       constructor leaves the others' slots free), so it remains the same free-binder problem, not a mechanical
       completion. Revisit alongside W4/W5 once the calculated-return limits are wired.
    2. **`typeMatch` over an auto-bounded record — DONE.** The `TypeMatch` matcher's `matchCase` handler is now rebuilt
       (`SaturatedValueProcessor.rebuildTypeMatchHandler`) from the grown generic kinds — `Function[Unit, R]` (or the
       explicit-generic prefix) becomes `Function[explicit-kinds.., BigInteger, …, R]` — so a type-level
       `case Counter[lo, hi] -> …` binds both synthesized bounds. The abstract `Fields` associated type needed nothing
       (it is bypassed: surface type matches resolve straight to the concrete matcher). Tests: `MonomorphicTypeCheckTest`
       "type-level match over an auto-bounded record" (type-check) + `ExamplesIntegrationTest` "type-level match over an
       auto-bounded record (W2 follow-up)" (end-to-end, prints `counter`).

### W3 — Calculated return positions (the back-edge), concrete-first

- **What:** make a `calculated` return resolve to the callee's body-checked return at the call's concrete type
  args, **reusing `MonomorphicValue`** — not a new symbolic fact on the hot path. When checking `double(b)`: infer
  `double`'s concrete type args from `b` against `SaturatedValue`'s body-free domain, then read
  `MonomorphicValue(double, args).signature`'s return.
- **Where:** `Checker.scala`, the `ValueReference` case (the read that W1 already pointed at `SaturatedValue`) — when
  the `ValueReference`'s return level is calculated, redirect from "evaluate the source return level" (which yields an
  under-applied `Int`) to "read the callee's monomorphized return". The callee body-check that produces it already
  runs at concrete args because monomorphization is driven from `main`; `Checker.renormalize` (the codomain re-fire in
  `applyInferred`) already grounds `add(0,0) ⤳ 0` inside it.
- **How:** the caller triggers/depends on `MonomorphicValue(callee, args)` (a normal cross-value fact edge; the
  DAG is acyclic for non-recursive producers — recursion is a Limit, W4). No symbolic quoting on this path.
- **Fallback (use-independent):** if a producer must be given a return with no concrete driver (never called, or
  tooling display), fall back to the symbolic `ElaboratedSignature` (Architecture §3): forward-evaluate on neutral
  binders and quote. Same convergence/limit criterion. Keep strictly off the driven-from-`main` path.
- **Convergence check:** `Quoter.quote` on the (concrete or neutral) result; success ⇒ calculated return; failure
  ⇒ a Limit (W4), reported, never defaulted to `Type`.
- **Test (end-to-end):** `def double(x: Int): Int = x + x` — a caller with `Int[0,255]` observes `Int[0,510]`;
  chained producers (`double(double(b))`) ground through both instantiations; a producer returning a saturated
  `data` (`def mk(n: Int): Counter`); the *bound-of-record* case still distinguishes bare (tightest calculated)
  from explicit `: Int[0,1000]` (widened via `Coerce`); a never-called producer still gets a checkable return via
  the symbolic fallback.
- **Relation to W2 (accessors).** W2 already publishes a record accessor's return *explicitly* — `n(obj: Counter): Int`
  becomes `n[lo,hi](obj: Counter[lo,hi]): Int[lo,hi]` by slicing the field's binders, so access does not wait on W3.
  W3 generalizes this to arbitrary producers and removes the need to *spell* a bare reference's return bounds: it is
  why W2's record-producer tests must annotate the return (`def field(c: Counter[7,7]): Int[7,7]`) rather than write
  bare `: Int`, and W3 is what lets such a return be left bare and calculated from the body.
- **Status:** DONE (concrete-first path; symbolic `ElaboratedSignature` fallback deferred). The back-edge **reuses
  `MonomorphicValue`** with no new fact, exactly as planned; the only addition is a boolean `calculatedReturn` on
  `OperatorResolvedValue` (set by `saturate`, forwarded into monomorphize).
  - *Detection + kind-correct placeholder (`SaturatedValueProcessor`).* `saturate` detects a calculated return —
    `detectCalculatedReturn`: the signature's final non-`Function` head (`returnExpr`) is a bare omittable reference
    applied to fewer than its omittable arity. The arity oracle `inferableInfo` was widened to include **W2-grown**
    record arity (`recordGrowth`/`recordPlanFor`), so a bare `Counter` return (raw type-ctor arity 0, fields add 2) is
    recognised; `fieldContribution` keeps using the *raw* arity (`rawInferableInfo`) so bounds still don't propagate
    transitively through nested records (W5) and the growth lookup stays non-recursive. The gotcha that drove the
    design: a bare under-applied return is **kind-ill-formed** (`Function[Int[lo,hi], Int]` — the codomain `Int` has
    kind `BigInteger → … → Type`, not `Type`), so `TypeStackLoop.walkTypeStack`'s kind-check rejects it. Fix:
    `saturate` rewrites the bare return to the kind-correct `Type` **placeholder** (`replaceReturn`); the real
    (computed) return never lives in the source type stack — the point of "calculated".
  - *Callee side (`TypeStackLoop` + `Checker.installReturnMeta`).* When `calculatedReturn` and a body is present, the
    `Type`-placeholder codomain of the (instantiated) signature is swapped for a **fresh metavariable**
    (`substituteReturn` descends the value-parameter `VPi` arrows); checking the body against that signature solves the
    meta — by ordinary unification — to the body's inferred type (`x + x` at `Int[0,255]` ⟹ `$ret := Int[0,510]`). The
    solved-meta signature is what gets quoted, so `MonomorphicValue(double,[0,255]).signature` is
    `Function[Int[0,255], Int[0,510]]`. A still-unsolved return meta after drain is a hard error
    (`failOnUndeterminedCalculatedReturn`) — Limit 2, never a silent `Type`.
  - *Caller side (`Checker.applyInferred` ⟶ `resolveCalculatedReturn`).* When an application's codomain forces to the
    `VType` placeholder and the callee's `SaturatedValue.calculatedReturn` is set, read the callee's body-checked
    return off `MonomorphicValue(callee, groundArgs).signature.deepReturnType` (`readMonomorphicReturn`), where
    `groundArgs` are the (now-solved) instantiation-meta type arguments quoted to ground. Returns a concrete `SemValue`
    — so chaining works inside-out (`double(double(b))`: inner resolves to `Int[0,510]` and flows into the outer's
    domain), and bound-of-record widening still rides the ordinary check-mode `Coerce` (bare = tightest, explicit =
    wider published contract). Reuse-not-projection: the callee FQN + type args come straight off the instantiated
    target value reference; the existing `MonomorphicValue` fact is the back-edge.
  - *Deferred (all fail-safe — a hard error or an ordinary mismatch at the use site, never a silently-wrong type;
    catalogued for W4 / W5 / W6):*
    1. **Calculated-return value not at a direct application site** — *RESOLVED in W4 for the complete-value case.* A
       calculated-return value referenced as a complete value (no parameters left — a **no-parameter producer**
       `def y: Int = x`) is now resolved at the `ValueReference` read (`Checker.resolveCompleteCalculatedReturn`, sharing
       the Limit-1 recursion guard). A producer **passed higher-order** (`map(double, xs)`) still keeps the placeholder
       buried in its codomain — the higher-order limit (Limit 6, out of scope).
    2. **Calculated-return argument whose bounds aren't ground at the call site** (`double(pick(...))` —
       calculated-return-over-`Combine`): *DIAGNOSED in W4* — `readMonomorphicReturn` now reports a specific, actionable
       error at the call instead of leaking the `Type` placeholder into a confusing `Coerce` mismatch. **Making it
       *compile*** (postponing the calc-return resolution into the drain-and-resolve loop so a `Combine`-joined argument
       grounds first, as ability resolution is) remains a *completeness* improvement carried to **W5** — the deferred
       upper-bound path leaves the callee's instantiation metas unsolved, so the postpone requires reordering against the
       combinable-meta machinery (real regression risk against the passing `Combine` suite).
    3. **Symbolic `ElaboratedSignature` fallback** (Architecture §3) for *use-independent* producers — a never-called
       producer, or tooling that wants a principal signature with no concrete driver. Not built: unreachable producers
       are never monomorphized from `main`, so they are never checked on the compile path; this is purely the IDE/tooling
       concern of **W6** (show a producer's principal calculated return by symbolic forward-evaluation on neutral binders).
    4. **Bare calculated return on a body-less abstract declaration** (Limit 5) — *RESOLVED in W4.*
       `TypeStackLoop.failOnAbstractCalculatedReturn` detects `calculatedReturn && checkingRuntime.isEmpty` and reports at
       the definition ("Abstract declaration `f` must state its return type explicitly…"), also covering the `opaque`
       sub-case (latent silent-`Type` gap, not reachable in the current stdlib but now guarded).
    5. **Transitive / "viral" bounds through nested records** (W5): `inferableInfo`'s W2-growth read is deliberately
       *non-transitive* (`fieldContribution` uses the raw arity), so a `data Outer(inner: Counter)` does not grow `Outer`
       by `Counter`'s bounds. Same fail-safe deferral as W2 follow-up 1.
  - Tests: `MonomorphicTypeCheckTest` "calculated return positions (W3)" (caller observation, direct callee
    monomorphization, reject-too-narrow, chained `double(double(b))`, `Coerce` widening, explicit-return unchanged,
    bare `data` return) + `ExamplesIntegrationTest` two end-to-end cases (`def double(x: Int): Int = x + x` prints
    `42`; `def mk(v: Int): Counter` prints `42`) + `examples/src/ImplicitIntReturn.els`. `intImports` in
    `MonomorphicTypeCheckTest` was corrected to declare `type Int[auto MIN, auto MAX]` (it had lost the markers,
    diverging from the real stdlib).

### W4 — Limits: positive detection and explicit diagnostics

See the dedicated section below for the catalogue. This stage wires each limit to a specific, actionable error
and proves (by test) that none of them silently degrades to a wrong or `Type` result.

- **Status:** DONE (the diagnostic + soundness mandate; two *completeness* items explicitly carried to W5/W6). Every
  W4-listed limit now either works or hard-errors with a specific, actionable message at the use site; none silently
  degrades to a wrong or `Type` result. Tests live in `MonomorphicTypeCheckTest` ("calculated return limits (W4)").
  - **Limit 1 (recursion / value-dependent bound) — DONE.** A recursive calculated-return producer used to drive the
    back-edge into a fact-cache **dead-lock** (the callee re-requests its own in-progress `MonomorphicValue` `Deferred`
    and waits on itself) — verified empirically (the compiler hung). Fixed by tracking the **active fact-request chain**
    (the in-progress ancestors of the fact being generated): `FactGenerator` owns an `IOLocal[List[CompilerFactKey[?]]]`
    that each fact's started fiber prepends its key to (child fibers inherit a copy at fork), exposed via
    `CompilationProcess.activeFactKeys` → `CompilerIO.activeFactKeys`. In the back-edge (`Checker.readMonomorphicReturn`),
    before requesting `MonomorphicValue(callee, args)`, check whether the callee's FQN is already on that chain: a
    non-recursive program has an **acyclic producer call graph**, so a repeated FQN is exactly the recursion signal —
    and it catches the *value-dependent growth* flavour (`f(x + x)`, keys never repeat exactly) at the **first** FQN
    repeat, no arbitrary depth cap. Reports "Cannot calculate the return type of recursive value `f`." Covers self,
    mutual, and growth (three tests).
  - **Limit 2 (stuck/under-constrained result) — DONE (message refinement).** `failOnUndeterminedCalculatedReturn` now
    names the producer in all cases and, when the return forces to a stuck `VNeutral`, names the stuck variable head;
    other non-quotable forms stay with the strict post-drain quoter. Detection is unchanged (still hard-errors, no
    silent `Type`). The unconstrained-meta / neutral cases are **not reachable on the concrete monomorphization path** —
    they need the symbolic forward-evaluation fallback (Architecture §3, deferred to W6) — so this is a defensive
    refinement with no minimal trigger to unit-test.
  - **Limit 3 (branch join, args unground) — DONE (diagnostic); completeness deferred to W5.** A calculated return over
    a `Combine`-joined argument (`double(pick(a, b))`) cannot ground the callee's type args at the call — `pick`'s
    combinable result is resolved only at drain — so the eager resolution used to leak the `Type` placeholder into a
    confusing `Coerce(Type, Int)` mismatch. `readMonomorphicReturn` now reports a specific, actionable error at the call
    ("argument bounds are not determined at this call site … annotate the argument, or give the value an explicit return
    type"). The program was rejected either way; this only fixes the message. **Making such a call *compile*** — the
    planned postpone of the calc-return resolution into the drain loop so the join grounds first — is a *completeness*
    improvement carried to **W5**: it requires reordering against the combinable-meta / instantiation-meta machinery
    (the deferred upper-bound path leaves the callee's instantiation metas unsolved), which is real regression risk
    against the passing `Combine` suite and out of proportion for a limit that is already fail-safe.
  - **Limit 5 (calculated return in a body-less / opaque value) — DONE.** `TypeStackLoop.failOnAbstractCalculatedReturn`
    detects `calculatedReturn && checkingRuntime.isEmpty` at the start of processing and reports at the definition,
    distinguishing the truly abstract case (`runtime.isEmpty` — a platform-layer signature awaiting an implementation)
    from the `opaque` case (body hidden from the checker — a latent silent-`Type` gap, not reachable in the current
    stdlib but now guarded). Explicit abstract signatures (`calculatedReturn = false`) are unaffected.
  - **Non-applied calculated-return read (deferred W3 item 1) — DONE.** A calculated-return value referenced *as a
    complete value* (no parameters left to apply — a no-argument producer used by name, `def y: Int = x`) is now
    resolved at the `ValueReference` read (`Checker.resolveCompleteCalculatedReturn`, the read-site twin of the
    `applyInferred` back-edge, sharing the Limit-1 recursion guard) instead of leaking the `Type` placeholder into a
    mismatch. A calculated-return *function* passed unapplied still keeps the placeholder buried in its codomain — the
    higher-order limit (Limit 6, out of scope).
  - **Limit 4 (omission of a non-inferable parameter) — already built in W1.** Bare `IO` (unmarked) errors at the use
    site, body-free; covered by the W1 bare-`IO` guardrail test.

### W5 — Propagation, ergonomics, display

- **What:** confirm composition (a saturated `data` used bare in a function re-saturates transitively — "viral
  bounds"); give synthesized binders readable, stable names for diagnostics and IDE hints
  (`docs/ide-type-hints.md`); document the explicit-supply / partial-application interactions.
- **Test:** transitive propagation across two `data` layers and a function; rendered signatures show calculated
  returns; hover shows synthesized binders.
- **Status:** TODO.

### W6 — Explorability: examples, generators, and probing (IDE)

Dependent signatures (`Int[add(MIN,MIN), add(MAX,MAX)]`) are precise but illegible. This item makes them legible
and checkable **by example**, exploiting that whole-program monomorphization already computes worked examples for
free. Everything here reduces to one primitive — *request more `MonomorphicValue` facts and observe* — which the
lazy, unordered, cached fact graph already supports (`docs/ide-type-hints.md`, finding 2). Gated behind W3
(calculated returns) and the IDE plan's Layers A (partial facts / `recover`) and C (`TypeHintIndex`).

- **Real-usage examples ("all available examples").** Aggregate the existing `MonomorphicValue(fqn, *)` facts and
  show the in→out set at a definition (`double` used as `Int[0,255]→Int[0,510]`, `Int[0,510]→Int[0,1020]`). Free;
  the only change is letting `TypeHintIndex` hold a *set* of hints per definition range. An unused producer has no
  such facts — which is exactly when generators take over.

- **Generators (only for type-position types).** A generator for a type `T` is a finite enumeration of
  representative *bound instantiations*. Primitive for `Int`: a **boundary-focused** canonical set (`Int[0,0]`,
  `Int[0,255]`, `Int[-128,127]`, `Int[0,65535]`, `Int[0,2^31-1]`, `Int[0,2^63-1]`, `Int[0,2^64]`) hitting each
  `Jvm*` tier and the cross-tier edges. Structurally derived for `data`: `Counter[lo,hi]`'s generator is
  `{ Counter[i] | i ∈ Int-generator }` — composed from its components, like deriving `Arbitrary` but at the bound
  level. The set of types needing a generator is derivable: the type-position constructors of every
  `SaturatedValue` signature (a `data` used only as a runtime value needs none). Examples are produced by
  *speculatively requesting* `MonomorphicValue(fqn, generatedArgs)`.

- **Probing ("error on a bad parameter combination").** Each generated (or real) instantiation either monomorphizes
  to an example or *registers an error* — a counterexample. Surface those at the definition ("`double` does not
  type-check for `Int[0,2^63]`: the intermediate overflows `JvmLong` with no wider `Coerce`"). This is
  **lightweight totality testing** of the over-claim that bare-input generalization is total over all bounds: a
  body using a bound-restricted operation (a narrow-only `Coerce`, a fixed-width intermediate, a missing `Combine`
  join) is only *partial*, and probing finds counterexamples that would otherwise surface as a confusing error at
  a future caller. It is the proactive, example-driven twin of the **Limits** section — the same failures, found
  early by sampling rather than late at a use. Honest scope: probing is a counterexample *finder*, not a totality
  *proof* (sampling is incomplete; the sound version is bound-constraint inference, future work — see the CLAUDE.md
  "Use-Site Verification" cornerstone). It must be **sandboxed**: small samples, a per-probe step/time budget, and
  "probe didn't finish" (e.g. hit the recursion limit) reported as *unknown*, never a false-positive error.

- **Presentation (creative affordances).** A differential inlay on a bare return (`: Int ⟨[0,255]→[0,510],
  [0,65535]→[0,131070]⟩` — the relationship, not the formula); "view-through" an instantiation (pin a bound, drive
  one speculative monomorphization, feed its per-node types into `TypeHintIndex` so every body node shows its type
  at that width); a totality CodeLens (`7/7 sampled bounds ✓` / `⚠ partial above 2^62` — an empirically-derived
  precondition); a representation-transition view marking where the output crosses a `Jvm*` tier (silent widening
  before it costs memory on a small target).

- **Where:** extends `lang/.../ide` — `TypeHintIndex` → set-per-range; a `GeneratorProcessor` deriving generators
  from `SaturatedValue` signatures; a probing driver issuing budgeted speculative `MonomorphicValue` requests.
  Reuses IDE Layers A/C/D wholesale; the only genuinely new code is generator derivation and probing/aggregation.
- **Test:** a `double` definition with two real call sites shows both; an unused producer shows generated examples;
  a deliberately bound-partial helper yields a counterexample at a representation boundary; a recursive producer's
  probe reports *unknown*, not an error.
- **Status:** TODO.

## Limits, and how the system says so out loud

The governing rule (cornerstone "no silent `Type` fallback"; the project "gaps must be fail-safe" guidance): a
return that cannot be calculated, or an omission that cannot be resolved, is a **hard, specific error at the
definition**, never a widening, a `Type`, or a deferred surprise at a caller. Each limit has a *positive*
detector.

1. **Recursion / value-dependent bounds.** A recursive producer's return bound may grow per call
   (`pow2(n): Int` wants `Int[1, 2^n]`, dependent on the *value* `n`, which monomorphization-by-type never
   grounds). Concrete monomorphization of such a callee does not stabilise (or cannot resolve a type-level bound
   from a runtime value); the symbolic fallback cannot reach a closed result without already knowing it.
   - *Detect:* on the primary path, a non-stabilising `MonomorphicValue` request chain (the callee's calculated
     return keeps re-requesting the callee at an ever-changing arg); on the symbolic fallback, a cycle in the
     `ElaboratedSignature` graph.
   - *Report:* "Cannot calculate the return type of recursive value `f`: its result depends on itself (or on a
     runtime value). Write an explicit return type." (Longer-term, tie the stated result to the termination /
     recursion-as-effect model — the closed-form recurrence is exactly what must be declared and proved, as in
     Agda's sized types.)

2. **Stuck / non-quotable result.** The body's type forward-evaluates to a form the strict quoter rejects
   (`VNeutral` on an unresolved head, an unsolved `VMeta`, a surviving `VLam`/`VNative`/unforced `VTopDef`).
   - *Detect:* `Quoter.quote` failure on the forward-evaluated return (the quote failure *is* the signal — no
     separate analysis).
   - *Report:* name the stuck head — "Cannot calculate the return type of `f`: the result depends on `<name>`,
     which is not determined by the inputs. Write an explicit return type." Never fall back to `Type`.

3. **Branch join with no `Combine`.** Two `match`/`fold` arms produce different inferable bounds; the result is
   their least upper bound, resolved by the existing combinable-meta + `Combine` machinery
   (`WellKnownTypes.combinedFQN`, `Unifier`).
   - *Detect:* `Combine` resolution fails (no instance joins the two arm types).
   - *Report:* "Cannot calculate the return type of `f`: branches yield `X` and `Y` with no `Combine` to join
     them. Write an explicit return type, or provide a `Combine` instance." (When `Combine` *does* resolve, this
     is not a limit — branching producers just work.)

4. **Omission of a non-inferable parameter.** A bare reference whose declaration's omitted parameters are **not**
   `auto`-marked (`IO`, a container element type).
   - *Detect:* in `SaturatedValue`, an omitted parameter without the marker.
   - *Report:* "`IO` requires its parameter `A`; it is not `auto`. Supply it explicitly." This is the IO
     guardrail, enforced at the use site, body-free.

5. **Calculated return in a body-less declaration.** An abstract layer declaration (`def readByte(): Int`, no
   body) cannot calculate (nothing to forward-evaluate) and must not generalize (output positions never
   quantify).
   - *Detect:* a `calculated` placeholder on a value whose `runtime` is `None`.
   - *Report:* "Abstract declaration `readByte` must state its return bounds explicitly; there is no body to
     calculate them from." (Inputs of abstract declarations may still generalize via W1.)

6. **Cost-in-arrow / higher-order indices (out of scope, named).** A calculated index that must ride *inside* a
   function-typed parameter ("the cost of applying `f`") needs graded/coeffect machinery (QTT / Idris 2,
   Granule), which this plan does not build.
   - *Detect:* a calculated return whose value depends on an abstracted function's result index.
   - *Report:* a clear "unsupported: calculated indices through higher-order arguments are not yet modelled"
     rather than a wrong answer — leaving the door open for the future cost/resource work.

## Interactions

- **Bound-of-record / `Coerce`.** Bare return = the *tightest calculated* type; an explicit return annotation =
  a *stable published contract*, with the body's tighter type widened to it via the existing check-mode `Coerce`
  (`Checker.unifyOrCoerce`). Both remain available; choosing bare vs. explicit is choosing how much the body
  leaks into the interface. Recommended style mirrors the "annotate boundaries" consensus: bare returns for
  internal helpers, explicit returns for public API.
- **Monomorphization.** Unchanged: synthesized input binders are ordinary generic params, so each call site keys
  a distinct `MonomorphicValue` specialization; the wide/viral parameter lists exist only in the generic form
  and monomorphize away.
- **IDE hints / explorability.** Dependent signatures are precise but illegible, so the IDE makes them legible
  *by example* — real-usage examples (free from `MonomorphicValue`), generated examples, and probing for bad bound
  combinations. See **W6** and `docs/ide-type-hints.md`. This is also the practical answer to the
  "Use-Site Verification" trade-off (CLAUDE.md): generators + probing substitute automated coverage for the
  modular totality proof the type no longer provides.

## Sequencing

```
W0  marker plumbing
 └─ W1  input generalization (functions)         ── ships alone (all Int consumers)
     ├─ W2  data/type field generalization        ── ships alone (generic data)
     └─ W3  calculated returns (back-edge)         ── reuse MonomorphicValue; symbolic fallback
         └─ W4  limits + diagnostics               ── hardens W1–W3; gate before broad use
             ├─ W5  propagation / display polish
             └─ W6  explorability / IDE            ── needs W3 + ide-type-hints Layer A/C
```

W1 and W2 are body-free and independently useful (every Int-*consuming* function and every Int-holding `data`).
W3 is the architectural step (callers read the callee's monomorphized return instead of re-reading source) and
unlocks Int *producers* — primarily by reusing `MonomorphicValue`, with the symbolic `ElaboratedSignature` only as
a use-independent fallback. W4 must land before W3 is exposed widely, because the limits are where correctness
lives: the feature's value is precise elision, and the only way that stays sound is to fail loudly at every
boundary it cannot cross.

## Explicitly out of scope

- **Graded / coeffect effects** (cost-in-arrow, usage semirings). The future cost/resource indices ride on this
  mechanism but the higher-order machinery is a separate project (QTT / Idris 2, Granule).
- **Refinement-type or range-analysis foundations.** Considered and rejected: refinement types move bounds out of
  the type system into a separate (often SMT) inference — a *second mechanism*, contradicting the cornerstone;
  range-analysis demotes bounds from a typed guarantee to a best-effort optimization, abandoning the
  microcontroller resource bet. This plan keeps bounds as ordinary, typed, `auto`-marked generic parameters.
