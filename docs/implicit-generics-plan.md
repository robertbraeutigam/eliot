# Implicit / Inferred Generic Parameters (`infer`)

Status: **Proposed.** Nothing implemented yet. This document records the theory, the surface design, the
architectural change it requires, a staged implementation that is closed and testable at each step, and — equally
important — exactly where the system stops working and how it must say so out loud.

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
re-reads source: at a `ValueReference` it fetches `OperatorResolvedValue.Key(vfqn)` and evaluates
`orv.typeStack.value.signature` (`monomorphize/check/Checker.scala:632-649`). A bare-`Int` return there evaluates
to an under-applied `VTopDef(IntFQN, None, SNil)` — the body's `add(…)` relationship is nowhere in it, because it
was produced while checking the *callee's body* and is not written back.

The fix follows directly from the concrete model above: that body-checked return **already exists** as
`MonomorphicValue(callee, concreteArgs).signature`, produced when the callee is monomorphized. So the caller, when
checking `double(b)`, infers the callee's concrete type args from its arguments (using the body-free saturated
*domain* — see Architecture), then reads the return off the callee's `MonomorphicValue`. That is the entire
"back-edge", and it **reuses an existing fact** rather than introducing a new symbolic one. Only the
use-independent case (no concrete driver) needs a symbolic elaborated signature, as a fallback. Every language
that infers result types makes the *body-derived* type the interface (HM's principal types; Agda's elaborated
signatures normalized at use sites); Eliot's source re-read is the anomaly.

### Cornerstone fit

- `infer` is a **visibility tag** on a parameter — *sanctioned sugar*, the same flavour as the
  `Qualifier.Type`/`Default` namespaces and `[]` vs `()`. It controls *elaboration* (may this argument be
  omitted?), not type equality, and applies uniformly to type-level and value-level binders. It introduces **no**
  type/value stratification.
- The number of holes to fill for a bare `Int` is the count of **leading `infer`-marked binders in `Int`'s
  declaration**, read structurally from the declaration the user wrote — **never** `RoleHint.TypeConstructor.
  typeParamCount` (`core/fact/RoleHint.scala`, which stays write-only). Arity-to-saturate is an *elaboration*
  read of user-written markers, never a *typing* input.
- One evaluator only; `Coerce` (bound-of-record widening) and `Combine` (branch joins) are reused unchanged.

## The marker

A keyword on a generic-parameter binder declares it **omittable**: at any use of the enclosing name that
parameter may be left out and the compiler supplies it. Working spelling — `infer` (bikeshed; alternatives
`implicit`, `auto`, or a sigil like `~MIN`):

```eliot
type Int[infer MIN: BigInteger, infer MAX: BigInteger]   -- bounds omittable
type IO[A]                                               -- A mandatory: bare `IO` is an error
type Byte = Int[-128, 127]                               -- explicit supply still allowed everywhere
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

1. **`SaturatedValue`** (core-phase rewrite, body-free). For each value, rewrite its source type stack so every
   *input-position* bare inferable reference becomes explicit (`Int` → `Int[$lo,$hi]`) with the fresh binders
   added to the enclosing definition's generic prefix; every *return-position* bare inferable reference is left as
   a `calculated` marker. Monomorphize and the `ValueReference` read consume `SaturatedValue` instead of
   `OperatorResolvedValue`. (We cannot rewrite `OperatorResolvedValue` itself — it is produced upstream by the
   `operator` phase, so feeding monomorphize's results back would be a fact cycle.) `SaturatedValue` carries the
   callee's **domain** body-free, which is what lets a caller infer the callee's concrete type args without its
   body.

2. **Calculated returns reuse `MonomorphicValue`** (no new fact, primary path). A `calculated` return is filled by
   the callee's existing per-instantiation body-check: the caller infers concrete type args from `SaturatedValue`'s
   domain and reads `MonomorphicValue(callee, args).signature`'s return. The `ValueReference` read
   (`Checker.scala:632-649`) is redirected from "evaluate the source return level" to "read the callee's
   monomorphized return" when the return is marked `calculated`. This is the entire hot path; it rides the
   monomorphization the compiler performs anyway.

3. **`ElaboratedSignature`** (fallback, symbolic, use-independent only). When a producer's return is needed with no
   concrete driver — a never-called producer, or tooling that wants a principal signature — check the body with the
   input binders as neutral variables, forward-evaluate, and quote the symbolic result into the return level. Kept
   strictly off the driven-from-`main` path; the concrete reuse in (2) handles all reachable code.

Input/data saturation is body-free and lives at the core boundary; return calculation needs the body and lives in
monomorphize — and, on the primary path, *is already computed there* by ordinary monomorphization.

## Work items

Each stage is independently mergeable and testable; later stages depend only on earlier ones.

### W0 — Marker surface + plumbing (no behaviour change)

- **What:** parse `infer` on generic-parameter binders; carry an `inferable: Boolean` on the binder through
  `ast` → `resolve` → `core` (`ArgumentDefinition` / generic-parameter structures); expose, per value FQN, its
  *leading inferable-binder count* as a derivable fact for later stages.
- **Where:** `ast/fact/Expression.scala` and the generic-parameter parser; `resolve` + `core` parameter
  structures; a small `InferableArity` (or a field on an existing core fact).
- **How:** purely additive. Bare under-applied constructors still error exactly as today (no saturation yet).
- **Test:** `infer` parses and round-trips to core; an `InferableArity(IntFQN) == 2`-style fact is produced;
  existing suite unchanged; bare `Int` in a signature still produces today's "type mismatch".
- **Status:** TODO.

### W1 — Input-position generalization for functions (the cheap, no-back-edge win)

- **What:** in `SaturatedValue`, rewrite every *parameter*-position bare inferable reference to an explicit
  application over fresh binders, prepended to the function's own generic prefix. Each occurrence is **independent**
  (matching how `+` is hand-written: left/right ranges differ).
- **Where:** new core-phase `SaturatedValueProcessor`; `monomorphize` entry + `Checker.scala:632-649`
  `ValueReference` read switch from `OperatorResolvedValue.Key` to `SaturatedValue.Key`.
- **How:** because callers read the *rewritten* signature, the synthesized binders are ordinary generic params;
  the existing "too few explicit type args → infer the rest" machinery
  (`Checker.peelLams` :573-595 / `instantiatePolymorphic` :722-734 / `TypeStackLoop.instantiateRemaining`) solves
  them from the argument with no new inference. No body needed; no elaborated-signature fact needed.
- **Test (end-to-end):** Int *consumers* fully work — `def isEven(x: Int): Bool`, `def store(x: Int): IO[Unit]`;
  a caller `isEven(b)` with `b : Int[0,255]` checks and monomorphizes; bare `IO` (unmarked) still errors;
  `Int[0,255]` explicit still works; two bare `Int` params get independent ranges.
- **Status:** TODO.

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
- **Status:** TODO.

### W3 — Calculated return positions (the back-edge), concrete-first

- **What:** make a `calculated` return resolve to the callee's body-checked return at the call's concrete type
  args, **reusing `MonomorphicValue`** — not a new symbolic fact on the hot path. When checking `double(b)`: infer
  `double`'s concrete type args from `b` against `SaturatedValue`'s body-free domain, then read
  `MonomorphicValue(double, args).signature`'s return.
- **Where:** `Checker.scala:632-649` — when the `ValueReference`'s return level is marked `calculated`, redirect
  from "evaluate the source return level" (which yields an under-applied `Int`) to "read the callee's monomorphized
  return". The callee body-check that produces it already runs at concrete args because monomorphization is driven
  from `main`; `renormalize` (`Checker.scala:709-713`) already grounds `add(0,0) ⤳ 0` inside it.
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
- **Status:** TODO.

### W4 — Limits: positive detection and explicit diagnostics

See the dedicated section below for the catalogue. This stage wires each limit to a specific, actionable error
and proves (by test) that none of them silently degrades to a wrong or `Type` result.

- **Status:** TODO.

### W5 — Propagation, ergonomics, display

- **What:** confirm composition (a saturated `data` used bare in a function re-saturates transitively — "viral
  bounds"); give synthesized binders readable, stable names for diagnostics and IDE hints
  (`docs/ide-type-hints.md`); document the explicit-supply / partial-application interactions.
- **Test:** transitive propagation across two `data` layers and a function; rendered signatures show calculated
  returns; hover shows synthesized binders.
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
   `infer`-marked (`IO`, a container element type).
   - *Detect:* in `SaturatedValue`, an omitted parameter without the marker.
   - *Report:* "`IO` requires its parameter `A`; it is not `infer`. Supply it explicitly." This is the IO
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
- **IDE hints.** `docs/ide-type-hints.md` is the natural surface for showing the synthesized binders and the
  calculated return a user did not write.

## Sequencing

```
W0  marker plumbing
 └─ W1  input generalization (functions)         ── ships value alone (all Int consumers)
     ├─ W2  data/type field generalization        ── ships value alone (generic data)
     └─ W3  calculated returns (back-edge)         ── needs ElaboratedSignature fact
         └─ W4  limits + diagnostics               ── hardens W1–W3; gate before broad use
             └─ W5  propagation / display polish
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
  microcontroller resource bet. This plan keeps bounds as ordinary, typed, `infer`-marked generic parameters.
