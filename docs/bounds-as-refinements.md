# Bounds as Refinements: Moving Meta-Information Out of the Type System

**Status: DESIGN — open, no decision.** Written 2026-07-10, following the Interval/Arithmetic
associated-types work (commit 3ad7ba38) and the design discussion it triggered. Extended the same
day with the channel's semantics (§4: attachment modes, named meta slots referenced by
projection, `where` preconditions, and the `Meta`/`Tracked`/`Represent` ability protocol).

## 1. The problem

`Int[MIN, MAX]` was meant to localize its complexity: only the definitions of `+`/`-`/`*` were
supposed to know that bounds exist and how they combine. That did not hold. The bounds spilled
into everything that touches integers generically:

- `Arithmetic` was forced into a **heterogeneous two-parameter shape with three associated result
  types** (`AddResult`/`SubResult`/`MulResult`) — because `Int[0,100]` and `Int[0,50]` are
  *different types*, an operation over them cannot be `A -> A -> A`; each result type must be a
  type-level *function* of both operand types.
- `Interval[S, E]` had to **restate every bound formula at the type level** — its instance carries
  `type AddResult = Interval[AddResult[S1, S2], AddResult[E1, E2]]` beside a body that computes
  exactly the same thing (`Interval(start(a) + start(b), end(a) + end(b))`), a quadruple-nested
  `Combined` chain for multiplication, four `Arithmetic` constraints, and explicit
  `MulResult[...]` annotations on the corner binders to stop unsolved associated-type
  metavariables from aliasing.
- The checker grew a whole constraint discipline to relate declared formulas to computed reality:
  the seven-part fix chain of 3ad7ba38, plus a set of known residual holes (§5).

The warning sign is structural, not incidental: every new bound-generic client will pay the same
tax, and the future roadmap (sized arrays, WCET, resource bounds) would replay it once per
measure.

## 2. Diagnosis: the bounds were never type parameters

`Int[0,1]` and `Int[1,2]` are *not different types* in any sense the rest of the language cares
about. They have the same operations, the same abilities, the same meaning; only their *values'
possible ranges* differ. The bounds are **meta-information about a value and its context,
traveling along with it** — not information about the type. The proof is that we keep building
machinery whose only job is to make distinct-bounded `Int`s behave as one type again:

- **`Coerce`** exists so that `Int[0,1]` flowing where `Int[0,5]` is expected is *not* a type
  error — i.e. so that unification's verdict ("different types") can be overridden.
- **`Combine`** exists so that two branches "of different types" can meet at one result — but
  real types don't join; *lattice facts* join. `Combine` is a lattice join in ability costume.
- **`monomorphize/refine/RefinementSolver.scala`** — 574 lines, and we literally named it
  "refine". Its own header says why it was factored out of the unifier: directional widening,
  joins, and deferred upper bounds "are not equality-shaped". The refinement lattice already
  exists as a separate system; it is merely forced to communicate through type-parameter syntax,
  which is what drags the unifier, the ability matcher, the saturator, the resolver and the
  monomorphizer into bound-awareness.
- **`opaque`** exists for the *dual* problem: the moment the platform gives `Int` a body (the
  representation policy), the checker would unfold it and collapse every range to its layout
  (`Int[0,255]` = `Int[0,100]` = `JvmByte`) — destroying the bounds-as-type identity from the
  other side. So a keyword plus a checker-vs-lowering dual-binding split had to be built to hide
  the body during checking and re-reveal it afterward. It is used by exactly one declaration in
  all Eliot source: jvm's `Int` (`jvm/eliot/eliot/lang/Int.els:27`).

Six patches — `Coerce`, `Combine`, `RefinementSolver`, the associated-type machinery, per-bound
instantiation identity, `opaque` — one disease. Half exist to make distinct-bounded `Int`s act as
one type; the other half to keep them distinct despite sharing one body. Both directions are the
cost of the same decision.

## 3. Three models

**Model A — status quo.** Bounds are type parameters; result bounds are *declared* as associated
type functions on ability instances; the checker verifies declared-vs-computed per instantiation.

**Model B — compute-not-declare.** Bounds stay type parameters, but the associated result types
are dropped: ability methods get calculated (`auto`) returns, computed by the NbE checker from
the resolved instance's body at each concrete instantiation. Formulas survive in exactly one
place — the primitive leaves' dependent signatures (`def add(a: Int[L1,H1], b: Int[L2,H2]):
Int[L1 + L2, H1 + H2]`). Discussed at length before this document; deletes the associated-type
lane but keeps `Coerce`, `Combine`, `RefinementSolver`, and per-bound instantiations intact.

**Model C — the refinement channel (this proposal).** Bounds leave the type system entirely.
`Int` becomes a plain type (not a type constructor). The compiler tracks value ranges in a
separate channel: an **abstract interpretation whose domains and transfer functions are
user-defined, written in Eliot**, evaluated by the one NbE evaluator as a rider on the
monomorphize walk it already performs. Because the language is whole-program, monomorphized,
total and recursion-free, the flow graph from `main` is a DAG and the analysis is **exact
evaluation**, not conservative approximation — the unique lever no mainstream refinement system
has (Liquid Haskell needs an SMT solver; Ada, whose constrained integer subtypes are the closest
precedent and target the same embedded domain, needs runtime checks).

### The visibility rule ("ignored everywhere else", made concrete)

Refinements are **invisible to**:

- unification — definitional equality never consults them (the unifier gets *purer*, restoring
  the "unify is pure definitional equality" guardrail with zero exceptions);
- ability instance identity and matching — `implement Numeric[Int]`, one parameter, done;
- name resolution;
- monomorphic instantiation identity — `Int` is one type, so call sites with ranges `[0,10000]`
  and `[0,8100]` share an instantiation. Specialization-by-range becomes a **backend optimization
  knob** (like inlining) instead of a semantic obligation; the instantiation-explosion problem
  stops existing rather than being mitigated.

Refinements are **visible to**:

- contract checks — a range annotation (`Int{0..255}` on a parameter, return, or `data` field —
  strawman syntax, see below) is an *assertion*: `computed ⊆ declared`, verified at each manifest
  use site;
- the transfer rules themselves;
- representation selection and codegen;
- `where` preconditions on defs over slot projections (§4.3); (future) `implement` guards over
  refinements, under the discipline below.

**Held invariant:** refinement information flows *into* checks and codegen, never back into type
formation. A guard reading a refinement may reject the program or pick a machine representation;
it must never select a different result *type*. This is the line that keeps the two channels from
re-entangling.

### Cornerstone fit

This does not violate "types are values / one evaluator" — it corrects a subtler mistake. The
refinement values *are values*: the range of an `Int` is an `Interval[BigInteger]` — the very
`data` we just built (single-parameter, once Interval itself collapses under C), in the compiler
pool where it already lives. Transfer rules are
ordinary Eliot functions (`Interval`'s `add`/`subtract`/`multiply` bodies — already written).
The one evaluator evaluates them; the compiler-platform overlay machinery hosts them. Nothing
gets a second evaluator or a bespoke Scala semantics. The correction: **refinements are values,
but they are not type *arguments***. The just-completed Interval work is not wasted — it stops
play-acting as "the bound of an Int via type application" and becomes, literally, the domain
value the tracker carries.

### Surface syntax: the channel must be syntactically distinct

An earlier draft of this document claimed the surface could stay `Int[0,255]`, with the bracket
payload silently reinterpreted as a contract. That claim does not survive scrutiny, for three
reasons:

- **Interpretation would be name-dependent.** `[...]` means type application. `List[Int]` must
  remain a real application; `Int[0,255]` would mean "contract in the channel", distinguishable
  only by knowing that `Int` is a tracked type. The parser can be taught that; users cannot —
  identical syntax teaches identical semantics, and the whole point of C is that these are
  different things. The confusion would resurface forever ("why can't I
  `implement Foo[Int[0,255]]`?", "why does `f[T]` instantiate `T = Int`, dropping my bounds?").
- **Positional brackets don't scale to more domains.** Once length and cost are domains too,
  `Array[T, 4]` would re-conflate a real type parameter (the element type — different code) with
  meta-information (the length — same code, different extent).
- **The language already sets the standard.** Effects are Eliot's existing second channel —
  meta-information about usage, not a type parameter — and they have their own syntax
  (`{Console} A` prefix rows, `-E` negatives), their own fact threading, and a lattice check
  (`used ⊆ declared`). Refinements are the same species of thing (`computed ⊆ declared`) and
  deserve the same treatment: an honest syntactic channel, not borrowed type-application clothes.

**How others separate the channels.** Liquid Haskell: brace-and-binder refinements
(`{v:Int | 0 <= v}`) in an annotation layer; type application and instance resolution see only
base types. F*/Dafny: `x: int | p` binder-predicate forms. Ada — the embedded-world precedent —
uses entirely different mechanisms for generics (`generic type T`) and constraints
(`subtype Byte is Integer range 0 .. 255`), with dedicated syntax per constraint kind. Whiley:
`type nat is (int x) where x >= 0`. The inverse cautionary tale is **Rust const generics** —
indices as ordinary generic parameters, precisely today's Eliot model — which produced the
years-long `generic_const_exprs` swamp and APIs infected with bound plumbing: the same wall the
associated-type chain hit here. TypeScript's flow narrowing marks the other extreme — a purely
inferred channel with no syntax at all — unsuitable here only because boundary contracts
(natives, fields, public APIs) are load-bearing.

**The differentiation rule.** A *real* generic parameter changes what the code is — `List[Int]`
vs `List[String]` need different element handling; it participates in identity, dispatch and
equality. *Meta-information* constrains which values flow through the *same* code —
`[0,255]`-ranged and `[0,1000]`-ranged integers run identical operations (representation is
codegen's business). Litmus: if two instantiations would never need different code — only
different checks or layouts — it is channel information, not a parameter. Sized arrays split
exactly along this line: element type = parameter (brackets), length = channel.

**Strawman syntax** (to be validated against the grammar):

- **Contract annotation**: a suffix brace on a type, dual to the effect row's prefix brace —
  `def f(x: Int{0..255}): {Abort} Int{0..510}`. Slots are *named*
  (`Int{range: 0..255}`, `Array[T]{size: 4}`); the bare form (`Int{0..255}`) is sugar for a
  type's sole slot. The brace content is (sugar for) an expression evaluating to the domain
  value: `Int{range: Interval(0, 255)}` is the explicit form, `lo..hi` the range-domain literal
  sugar.
- **Refined aliases**: `type Byte = Int{0..255}` — an alias for the *pair* (type, contract).
- **Domain declaration**: the brace channel on the type declaration itself, as named slots —
  `type Int {range: Interval[BigInteger]}` — each desugaring to a `Tracked` instance (§4.4).
  Signatures never bind meta variables; they reference parameter metas by *projection*:
  `def add(a: Int, b: Int): Int {range: a.range + b.range}` (§4.2). The lattice operations live
  in a `Meta[D]` instance (§4.4).
- **Preconditions**: the existing `where` clause, extended to defs, over slot projections —
  `def pop[T](ls: List[T]): T where ls.size.start > 0` (§4.3).
- **Contract points**: annotations in signatures and on `data` fields; native signatures declare
  their outputs' refinements (`readByte : Int{0..255}`).
- **Instance heads are refinement-blind**: `implement Foo[Byte]` is rejected (or lint-collapses
  to `Foo[Int]`). Bare `Int` stays bare: as a parameter its refinement is a per-use fact from
  flow (⊤ until known), as a return it is computed.

Grammar caveats to check: suffix braces vs block/effect-row braces; whether `..` is free.
Fallback punctuations: `@`-attachment (`Int @ 0..255`) or a keyword form (`Int is 0..255`).
Migration is a mechanical rewrite of a few dozen sites; the language is young enough that honest
syntax is cheaper than a compatibility shim that forever teaches the wrong model.

What disappears regardless of the chosen form: every construct that *binds bound variables* —
`implement ... Arithmetic[Int[L1,H1], Int[L2,H2]]` patterns,
`intToString[Min: BigInteger, Max: BigInteger](value: Int[Min, Max])` (becomes
`intToString(value: Int)`), the four-way `Interval` constraints, the formulas.

**Fail-safe (per the gaps-must-be-fail-safe rule):** a value with no computable refinement — a
native without a transfer rule, an unhandled construct — is ⊤ (unknown), and ⊤ **loudly fails**
any contract or guard that demands a bound, mirroring the stuck-native stall. ⊤ is also always
*soundly representable* (bignum), so unannotated code compiles, just fat — never wrong.

## 4. The channel's semantics: attachment, binders, and the ability protocol

### 4.1 Two attachment modes: value-facts and computation-facts

Meta-information attaches in two different ways, and Eliot already has one channel of each kind:

- **Value-attached facts** describe what a value *is* — ranges, sizes/lengths, units, taint.
  They propagate along **dataflow**: through bindings, into constructors, out of accessors, with
  the value wherever it goes. This is the channel this document designs; it rides the
  NbE/monomorphize walk.
- **Computation-attached facts** describe what evaluating something *does* — effects, `Inf`, and
  later WCET/cost/energy. They propagate along **control flow**: combined at sequencing, combined
  at branches, checked at declaration boundaries. **The effect system is this channel, already
  built**: union at sequencing and branches, `used ⊆ declared` at the boundary, subtraction at
  discharge. WCET is the same shape with a different algebra — *sum* at sequencing, *max* at
  branches — so the honest way to build cost tracking later is to generalize the effect
  accounting walk (`EffectUsageCollector` + the discharge-summary DAG walk) from the union
  lattice to arbitrary monoids, not to force cost into the value channel.

The two modes reconcile at the carrier boundary, and Eliot's effect design already made the move:
arrows are not colored; effects ride the **carrier** — a computation reified as a value carries
its computation-facts as facts on that value (`{Console} Unit`). Likewise a function value's
meta-information naturally describes its application behavior (a transfer function is exactly
this). "Attaches to the call-chain" is thus the special case of "attaches to a value" where the
value is a function or carrier — the same unification-by-reification the λ* cornerstone applies
to types.

Two consequences worth recording:

- The prefix/suffix brace duality of §3 is semantically real, not aesthetic: prefix brace =
  computation channel (`{Console} Int` — what evaluating does), suffix brace = value channel
  (`Int{0..255}` — what the value is). `def f(x: Int{0..255}): {Abort} Int{0..510}` shows one
  type carrying both, each in its own position.
- Value-facts attach to **program-point knowledge**, not to runtime objects: the same value can
  be known as `[0,1000]` before a check and `[0,9]` inside the `if (x < 10)` branch. That is
  what makes path narrowing possible at all, and why the channel erases perfectly — it was never
  a property of the object, only of the analysis. `data` fields are where a fact is deliberately
  made persistent (the annotation re-establishes knowledge at every read site), which is why
  fields are contract points.

(A third value-attached domain has been running implicitly all along: **binding time** — the
static/residual distinction the reduce-and-reify work watches via `VNeutral`-ness is a textbook
abstract domain.)

### 4.2 Declaring meta-information; referencing it by projection

The declaration form puts the brace channel on the type declaration itself, as **named slots**:

```
type Int     {range: Interval[BigInteger]}
type List[T] {size:  Interval[BigInteger]}
```

A brace in *declaration* position states the named meta slots a type carries and each slot's
domain type (the single-parameter Interval of the post-C world); a brace in *use* position
states or checks a **value of a slot** (`Int{range: 0..255}`, with `Int{0..255}` as sugar for a
type's sole slot). Multi-slot types are first-class
(`type Array[T] {size: Interval[BigInteger], align: Alignment}`). Since domain values exist only
at compile time, declaration-position brace content resolves against the **compiler pool** — a
new but natural use of the two-pool machinery.

Signatures never *bind* meta variables; they **reference parameter metas by projection**:

```
def add(a: Int, b: Int): Int {range: a.range + b.range}
```

The parameter list is completely undecorated — even the natives that define transfers have plain
runtime signatures plus one return annotation. `a.range + b.range` is plain `+` on two
`Interval` values — the `Numeric[Interval[BigInteger]]` instance *is* the transfer function,
evaluated by the one NbE evaluator. This is the dependent-refinement-signature style of Liquid
Haskell / F* (`val add: a:int -> b:int -> r:int{r = a + b}` — refinements reference *parameter
names*), with the twist that fits Eliot: those systems predicate over the runtime value (hence
they need SMT); here the projection denotes the **domain value** directly and computes with it
(hence plain evaluation). The scoping comes free in a deep way: "parameters in scope in the
return position" *is* Π-codomain dependency, and `VPi` is already the language's one primitive
Π-former — the channel rides existing scoping machinery rather than adding a namespace.

The discipline — how this stays consistent with generics and ordinary parameters:

- **Two binder namespaces, plus projections.** `[T]` binds a type parameter (upper-case;
  participates in identity and dispatch); `(a: …)` binds a value parameter. There is no third,
  meta-binder namespace: meta expressions reference the metas of already-named things
  (parameters, `val`s) through slot projections — `a.range`, `ls.size`. Braces mark the
  *channel*; they do not bind.
- **Slot accessors are channel-scoped, not runtime functions.** `range(a)` cannot exist as an
  ordinary `Int -> Interval[BigInteger]`: a runtime Int carries no interval (perfect erasure),
  and if `a.range` were allowed in a value position, *program behavior would depend on inference
  precision* — a smarter compiler release would change what a program computes. This extends the
  held invariant of §3 one notch: channel facts flow into checks and layout, never into type
  formation, and never into **values** either. `a.range` is legal in meta positions (return
  braces, `where` clauses, contract expressions) and a hard error in runtime bodies; name
  collisions resolve by context, since slot names are not a namespace in runtime expressions.
- **Parameter-position braces are contracts**: `Int{range: 0..255}` on a parameter checks the
  incoming meta (⊑) — never a binding.
- **Return-position braces are expressions**: on a body-less native they are **defining** —
  axiomatic, exactly as trusted as the native's type signature is today; on a bodied def they are
  a **checked contract** (computed ⊑ stated), which yields relational contracts on ordinary
  functions for free.
- **No infection**: annotations appear only at contract points (primitive leaves, deliberate
  contracts). A wrapper calling `add` states nothing; the channel computes through it. This is
  the difference from today's `[L1, H1, L2, H2]` world, where every wrapper re-bound and
  re-stated.
- **One meta level**: domain expressions evaluate channel-free — `+` on Intervals is not itself
  tracked, and domain types are untracked. No meta-of-meta tower, and no bootstrapping cycle
  (checking `Interval`'s own code never demands Interval-as-domain; the compiler pool's
  single-owner DAG ordering does the rest).

Projections extend to the computation channel (§4.1): a carrier-typed parameter's grade is
projected the same way (`step.cycles`), so a cost-transfer combinator needs no binders either —

```
def fold[T, A](ls: List[T], init: A, step: F[A]): {cycles: ls.size.end * step.cycles + 7} F[A]
```

— both channels composing in one signature (size × step-cost), everything referenced through
names that already exist. Nested projections (`p.first.range` — a runtime field path, then a
meta slot) are syntactically well-formed and deliberately meaningless until the containers
question (§7) is answered; when it is, the surface is already waiting.

The layer split survives cleanly: the *base* states the instance method signatures **with**
transfer braces — platform-independent semantics; today's `AddResult = Int[L1+L2, H1+H2]`
formula, relocated into the honest channel — and platforms supply the native bodies.

Unstated meta is **demand-driven**, refining §3's fail-safe: ⊤ is always *sound* ("I know
nothing" is true, just imprecise), so unstated, undemanded flow does not error. Errors fire where
a demand meets ⊤ or fails: a violated contract, a `where` clause needing a bound, or the
representation policy — which is where strictness scales with the target automatically. On the
JVM, ⊤ lays out as bignum (sloppy code compiles, just fat); on an ATtiny the policy has no ⊤
layout, so the same code errors at the exact value that could not be bounded, with the ⊤
provenance in the message.

### 4.3 Preconditions: `where` on defs

The existing `where` machinery (compile-time predicates on `implement` blocks) extends to defs,
with slot projections available:

```
def pop[T](ls: List[T]): T where ls.size.start > 0
```

Because the predicate ranges over the domain value — the *knowledge* — this reads "the list is
**provably** non-empty". Its semantics has three parts, all inherited from existing stances:

- **At the call site it is a demand**: the caller's computed meta must satisfy the predicate; a
  caller with ⊤ or `[0, n]` errors loudly at that call, with provenance.
- **Inside the body it is an assumption**: the body checks under the hypothesis, satisfying inner
  demands.
- **At definition time it defers**: with the parameter's meta neutral the predicate is
  undecidable, so it rides to each manifest use — use-site verification, exactly as ability
  guards already behave ("exactly one survivor at the use site").

Relational preconditions across parameters fall out (`where as.size == bs.size` for a strict
`zip`), and libraries get an honest static/dynamic pair: `pop` with a `where` (caller must
prove) beside `pop` returning `{Abort} T` (caller handles failure). Note the two dots do
different work: `ls.size` projects the slot (channel-scoped, §4.2), while `.start` is Interval's
ordinary accessor — past the projection, predicates are arbitrary Eliot over the domain value,
not a fixed vocabulary.

### 4.4 The ability protocol: `Meta`, `Tracked`, `Represent`

Everything the channel needs from user space is expressed as abilities — the language's one
mechanism for "this type must provide these functions" — so enforcement is ordinary instance
resolution and coherence, and the **only new compiler machinery is the rider walk itself** (plus
the brace grammar and the demand points). Three abilities, all in `eliot.compiler`, instances in
the owning layer's `eliot-compiler/` overlay (checking-only instances — the `Effect`/`Throw`
compiler-pool precedent):

```
ability Meta[D] {
   def contains(outer: D, inner: D): Bool   -- ⊑: every contract check (was Coerce's guard)
   def join(a: D, b: D): D                  -- branch merges (was Combine)
   def top: D                               -- the "know nothing" fail-safe
}
```

plus later `meet` (path narrowing) and possibly a rendering method for error messages. A missing
instance is the ordinary "No ability implementation found" error. One instance per **domain**,
not per type-pattern: `Meta[Interval[BigInteger]]` serves Int's value range *and* List's size —
today's per-pattern `Combine`/`Coerce` instances collapse into it.

Each declared slot desugars to an instance (precedent: `data` desugars to constructor values via
`DataDefinitionDesugarer`):

```
type Int {range: Interval[BigInteger]}

-- desugars to (one instance per slot; the slot name enters the instance's identity) --

implement Tracked[Int, Interval[BigInteger]] {
   def fromValue(value: Int): Interval[BigInteger] = ...   -- α: n ↦ Interval(n, n)
}
```

`Tracked[T, D ~ Meta[D]]`'s one core method `fromValue` is **α, the abstraction function** of
abstract interpretation: literal seeding is its special case (`5` ⤳ `Interval(5, 5)`), and it
doubles as the **CTFE bridge** — whenever NbE fully reduces an expression to a concrete value
(constantly, in a total whole-program-evaluated language), the channel re-seeds exact meta via α,
so compile-time-known values never suffer ⊤. Each slot carries its own α: for
`type List[T] {size: Interval[BigInteger]}` the instance is pattern-generic
(`implement[T] Tracked[List[T], …]`) and α is "length as a singleton", an ordinary fold.

Coherence does most of the association's enforcement, and named slots give it the right unit:
identity and uniqueness are per **(type, slot)** — how the slot name enters the generated
instance's identity (most likely a synthesized marker, like the qualifier system) is an
implementation detail. Two layers declaring the *same* slot with the same domain merge as split
halves of one instance (the `(ability, pattern)` identity that just shipped for the split
`Arithmetic[Int]`); the same slot with *different* domains is a conflict — and since such
instances are disjoint patterns the stock overlap check will not catch, per-(type, slot)
uniqueness needs an exactly-one-survivor search keyed on type+slot (the shape guard resolution
already uses at use sites) or a trivial whole-program scan. Multiple *slots* per type are simply
multiple declarations in the braces — no longer a future door but the first-class form
(`Array[T]{size: 4}` contracts against the `size` slot).

Representation policy joins the same pattern: what the `opaque` body did becomes a platform-side
instance —

```
implement Represent[Int] {                                  -- jvm layer's compiler overlay
   def layout(d: Interval[BigInteger]): JvmLayout = ...     -- the old fitsByte/fitsShort/… fold
}
```

— consulted by codegen. "Representation derived in Eliot, not Scala" is preserved; the `opaque`
track stays dead.

De-risking precedent: "the checker resolves and evaluates ability instances mid-check" is already
running in production — `RefinementSolver.unifyOrCoerce` resolves the `Coerce` instance by name
and evaluates its `coerce` body through the NbE evaluator today (`resolveAbility` is a
constructor parameter of the solver). This design re-points that pattern from the ability being
deleted to the two being introduced.

What the compiler cannot check — the lattice laws (`join` associative/commutative/idempotent,
`contains` reflexive/transitive, `top` absorbing) and, more importantly, **soundness of
transfers** (the stated interval of `add` must contain every possible sum) — are the instance
author's obligations, verified by tests, at the same trust level as a native's signature.
Defining a domain is compiler-extension-grade work; the `eliot.compiler` import signposts it.

**The successor map** — every piece of today's apparatus has a named, ordinary successor:

| Today | Under C |
|---|---|
| `Coerce`'s `where`-guard containment check | `Meta.contains` |
| `Combine` / `Combined` | `Meta.join` |
| Associated-type result formulas (`AddResult = …`) | transfer braces on signatures (`Int {range: a.range + b.range}`) |
| The `opaque type Int[…] = fold(fits…)` body | `Represent.layout` |
| Compile-time `Interval[BigInteger, BigInteger]` as "the bound of an Int" | the domain value itself (`Interval[BigInteger]`) |
| Bound-generic binders (`[L1: BigInteger, …]`) | slot projections (`a.range`) in meta positions only |

## 5. The deletion inventory

What each model removes from today's implementation. This is the heart of the comparison.

### 5.1 Checker-side machinery

| Machinery (today) | Where | Fate under B | Fate under C |
|---|---|---|---|
| Associated-type application/reduction: `reduceAssocApplications` + its four hooks (check-mode forced-expected, `inferValueReference` result, `injectForImpl`, `TypeStackLoop` pre-quote), `assocReductionCache`, the pure `assocSubstitution` wired into `PostDrainQuoter.quoteSem`, the `inferSpine` assoc-headed special case | `monomorphize/check/AbilityResolver.scala` (442 lines, the bulk), `CheckState`, `PostDrainQuoter`, `Checker` | **deleted** | **deleted** |
| `MetaRole.AbstractAssoc` + per-reference assoc-meta identity | `monomorphize/domain/MetaRole.scala` | deleted | deleted |
| Unifier's applied-assoc non-injectivity postponement | `monomorphize/unify/Unifier.scala` | deleted | deleted |
| Quoter loud-fail on applied abstract-assoc | `monomorphize/eval/Quoter.scala` | deleted | deleted |
| Bare-vs-applied assoc return distinction in `detectCalculatedReturn` | `saturate/SaturatedValueProcessor.scala` | simplified | deleted |
| `searchAbilities` explicit-type-arg attachment + the `applied` flag threaded through application targets | `resolve/processor/ValueResolver.scala` | kept | deleted |
| Own-assoc-member-as-opaque-Constructor conformance handling | `ability/util/AbilityMatcher.scala` | kept | deleted |
| **`RefinementSolver`**: `unifyOrCoerce` (check-mode Coerce insertion), `resolveCombines` (covariant-meta candidate join), `resolveUpperBounds`, `reconcileRefinements` (conversion-payload splicing) | `monomorphize/refine/RefinementSolver.scala` (574 lines) | **kept** | **transformed**: its lattice *algorithms* (containment, join) become the channel's core over domain values; its *type-system entanglement* — meta roles, candidate interception inside `Unifier.unify`, coerce-expression splicing into read-back — is deleted. Unification never calls it. |
| Combinable/candidate/taint meta-role data + the unification-time interception that accumulates candidates instead of failing | `Unifier` + `MetaRole` map, `CheckState.pendingUpperBounds` | kept | deleted (branch types must be *equal*; ranges join in the channel) |
| Per-bound-pair monomorphic instantiation identity for `Int` | monomorphize | kept | deleted (one `Int`; backend may re-specialize as an optimization) |
| The **`opaque` qualifier** and its whole track: the keyword (`token/TokenParser`), the flag threaded through the entire fact chain (`ast/TypeAliasDefinition` → `core/NamedValue`, incl. its role in `signatureEquality` → `resolve/ResolvedValue` → `operator/OperatorResolvedValue.checkingRuntime` → `saturate/BinderRoles` → `used/CodegenProjection`), the checker-vs-lowering **dual-binding split** (`NativeBinding` keeps opaque bodies stuck so distinct ranges stay distinct; `fact/TransparentBinding` re-reveals them), and the Phase-3 unfolding pass (`monomorphize/lowering/RepresentationLowering.scala`) | ~20 files across token/ast/core/resolve/operator/saturate/monomorphize/used; **one use site in all Eliot source** (jvm `Int`) | kept — B still checks bounds as types, so the representation body must stay hidden from the checker | **deleted** — under C, `Int` is a plain body-less type; representation comes from the refinement interval via the `Represent` instance (§4.4), so there is no type body to hide: keyword, flag threading, dual bindings, and opaque unfolding all go. Codegen still maps ground values to layouts, but it reads the channel, not a hidden type body. |

Note on the associated-types *feature* itself: under C, no stdlib client remains (`Arithmetic`
loses its members, `Combine` retires — see below). The language feature could be kept as general
type-level programming or removed outright; either way, the entire hard part — the seven-fix
chain of 3ad7ba38 — existed to serve bound arithmetic, and that pressure is gone.

### 5.2 Compiler-facing Eliot (lang/stdlib layers)

| Machinery (today) | Where | Fate under B | Fate under C |
|---|---|---|---|
| `ability Coerce[From, To]` + check-mode auto-insertion | `lang/eliot/eliot/compiler/Coerce.els` | kept | **retired from the bounds role** (widening = refinement weakening, a `Meta.contains` check in the channel (§4.4) — no expression is spliced, no conversion body runs at the type level). Whether `Coerce` survives as a general cross-type conversion point is an independent, low-stakes decision; bounds were its only client. |
| `ability Combine[A, B] { type Combined }` | `lang/eliot/eliot/compiler/Combine.els` | kept | **fully retired** — the join survives as `Meta.join`, one instance per domain (§4.4). Its per-type instances go: `Combine[Int, Int]` (stdlib `Int.els:67`), `Combine[BigInteger, BigInteger]` (added only to feed Interval's `MulResult` formula). |
| `Arithmetic[A, B]` heterogeneous two-param shape + 3 assoc members | `stdlib/eliot/eliot/lang/Arithmetic.els` | members deleted, shape kept | **both deleted**: operands of one range-tracked type are the *same type*, so the ability collapses to single-parameter `Numeric[T]` with plain `T -> T -> T` methods — the shape Robert originally wanted to write. (No mixed-type instance exists today, so nothing is lost.) |
| `implement Arithmetic[Int[L1,H1], Int[L2,H2]]` with formula lines | `stdlib/eliot/eliot/lang/Int.els:41-49` | formulas move onto method signatures | replaced by `implement Numeric[Int]` — the formulas become the transfer braces on the method signatures (`def add(a: Int, b: Int): Int {range: a.range + b.range}`, §4.2), whose bound arithmetic is the existing compile-time `Interval` code |
| Interval's type-level half: four-way `Arithmetic` constraint, 3 formula lines incl. the quadruple `Combined` chain | `stdlib/eliot/eliot/lang/Interval.els:37-46` | deleted | deleted; instance becomes `implement[T ~ Numeric[T]] Numeric[Interval[T]]` with bodies only |
| Corner-binder `MulResult[...]` annotations (the val-aliasing workaround) | jvm + compiler-overlay `Interval.els` | deleted | deleted |
| Bound-generic signatures on plain functions (`intToString[Min, Max]`) | stdlib | kept | deleted (`intToString(value: Int)`) |

### 5.3 JVM platform layer (`jvm/eliot/eliot/lang/Int.els`, 205 lines today)

| Machinery (today) | Fate under B | Fate under C |
|---|---|---|
| 27 width-specific bound-generic native leaves (`nativeAddByteToByte[M1,X1,M2,X2]…` × add/subtract/multiply × 9 width pairs), each with a dependent result signature | kept | **collapsed**: width selection moves to codegen, which reads the operands' and result's refinement intervals and picks the instruction; the Eliot surface needs ~3 leaves (`add`/`subtract`/`multiply` on `Int`) or none beyond the ability methods |
| `ability IntArith` + 5 guarded instances (the width-dispatch family, ~55 lines) + the inner result-width `fold`s | kept | **deleted** — this was codegen policy expressed as ability dispatch because widths lived in types |
| `nativeWiden` + the guarded `implement Coerce[Int[Smin,Smax], Int[Tmin,Tmax]] where …` instance | kept | **deleted** — widening is a representation change decided by codegen from refinements; no user-space conversion function exists |
| `opaque type Int[auto MIN, auto MAX] = fold(fitsByte…)` representation-policy body | kept | the *policy* survives but changes home and input: the platform's `Represent[Int]` instance (§4.4), consulted by codegen (keeping "representation derived in Eliot, not Scala"); the `opaque`-marked type body goes, and with it the entire compiler-side `opaque` track (see the §5.1 row) |
| `implement Compare[Int[L, H]]` bound-generic header | kept | `implement Compare[Int]` |

### 5.4 Known holes and gotchas that die with the machinery

- **Unannotated `val`s alias assoc metas** (must annotate `val c11: MulResult[S1,S2]`) — the
  construct no longer exists.
- **Caller-side applied-assoc returns loud-fail** (no reduce hook at the codomain application) —
  the construct no longer exists.
- **Silently-dropped postponed unification constraints** (TODO.md, recorded 2026-07-09): the
  fail-safe fix is still owed while any postponement exists, but its main producer — applied
  associated-type constraints — disappears, shrinking the postponed lane to near-empty.
- **`searchImplementationScope` over-matching** (impl methods can't restate result formulas on
  their own signatures): moot under C; a *prerequisite fix* under B.
- **Layer-merge lexical-equality burden** shrinks: the elaborate dependent signatures that had to
  be spelled character-exact across layers (formulas, 27 leaf signatures) mostly vanish.

### 5.5 What stays untouched, and what B does *not* buy

Untouched under C: the one NbE evaluator and the whole monomorphize walk (the channel is a rider
on it); the ability-guards feature (`where` keeps its non-bounds clients, e.g. the `Throw` lift's
`E1 != E2`); the compiler-platform overlay machinery (it becomes the domain host); `Interval` as
an ordinary runtime type *and* as the Int domain value.

The B column above is the strongest argument in this document: **B deletes only §5.1's
associated-type rows and the stdlib formulas.** `Coerce`, `Combine`, `RefinementSolver`'s
entanglement, the 27 leaves, `IntArith`, `nativeWiden`, per-bound instantiation identity, and the
entire `opaque` track all survive B untouched, and B additionally makes `auto` returns infectious
through user signatures.
C deletes all of it and gives user code plain, stable signatures (`def area(w: Int, h: Int):
Int`). B is not a step toward C — its new machinery (calculated ability-method returns) would be
discarded by C. **If C is credible, B should not be built.**

## 6. The six scenarios

**S1 — an application function.** `def area(w: Int{0..100}, h: Int{0..100}): Int` (strawman
contract syntax, §3). A: return must be spelled `MulResult[...]` or a widened bound checked via
Coerce. B: `auto` or a widened contract. C: parameters carry contract refinements; the body's
range `[0,10000]` is computed in the channel; the plain return type `Int` is complete and
stable; an optional `: Int{0..20000}` return is a checked, Coerce-free assertion.

**S2 — Interval.** A: today's three-file split with restated formulas (§1). B: bodies only, but
still bound-generic headers and per-bound instantiations. C:
`implement[T ~ Numeric[T]] Numeric[Interval[T]]` — one constraint, three one-line bodies, and the
compile-time copy doubles as the Int domain implementation.

**S3 — width dispatch.** A/B: the `IntArith` guarded family + 27 leaves at the ability level.
C: codegen reads each site's refinement interval and picks the layout/instruction; the policy
stays in Eliot as the platform's `Represent[Int]` instance (§4.4). Guards over refinements at the
*ability* level are not needed for this — which conveniently defers the instance-guard
discipline question (§7).

**S4 — a `data` field.** `data Pixel(v: Int{0..255})`: the annotation is a contract — checked at
every construction site, trusted at reads, and it fixes the field's representation. An
*unannotated* field (`data P(v: Int)`) is ⊤: sound (bignum layout) but fat, and loud the moment a
guard or contract demands better. Fields are boundaries; this matches the "annotate boundaries"
idiom rather than whole-program-flow field typing (deferred as an optimization).

**S5 — the fold accumulator.** The honest scenario: no model wins today. Summing an unknown
number of `[0,255]` values has no finite range; A/B cannot type a pinned accumulator (the bound
grows per step and Coerce correctly refuses to narrow), and C's exact analysis hits the same
truth (the fixpoint diverges → ⊤ → a demanding contract fails loudly; an unannotated accumulator
is soundly a bignum). All three force what is mathematically forced: a bignum accumulator, or
*sized* structures so the sum is `n * 255`. The difference is what "sized" costs: under A/B,
sizes mean `Array[T, N]` with type-level nat arithmetic — the entire `Int[MIN,MAX]` pain
replayed; under C, length is *one more domain* (`append` transfer = `+1`, `zip` = `min`), and
size × element-range composes into the accumulator bound inside the same channel. C turns the
scenario from "unsolvable without a second infection" into "solved by the mechanism's next
client."

**S6 — the future roadmap (sizes, WCET, resource/energy bounds).** Every planned static
guarantee is "meta-information traveling along". A/B replay the infection once per measure; C
adds a domain and transfer rules per measure, uniformly. C also unlocks what type parameters
structurally cannot ever do: **path sensitivity** — `if (x < 10)` narrowing `x`'s refinement in a
branch (a type argument cannot change mid-flow), which is the eventual mechanism for proving
array indices in range after an explicit check.

## 7. What C must prove (open questions)

1. **Containers.** Where does `List[Int]`'s element range live? Starter position: refinements
   attach to bindings and values; `data` fields carry refinements only when annotated (else ⊤).
   Refinements-on-type-occurrences (full Liquid-style decorated types) is the richer, more
   entangled alternative — explicitly deferred.
2. **Guards over refinements on `implement`.** Def-level `where` preconditions are designed
   (§4.3); the open half is *instance selection* depending on the channel
   (`implement … where <refinement predicate>`). That is coherent only if all guarded siblings
   agree on their type signatures (selection then changes *which body runs*, never a type) — the
   held invariant of §3. S3 shows the one former client (width dispatch) doesn't need it; defer
   until a client does.
3. **Transfer through higher-order code.** Mostly falls out of the NbE walk (closures are
   applied, not abstracted over), but the story at genuinely opaque points (effectful natives'
   callbacks) needs writing down: their signatures' declared refinements are the boundary.
4. **The `auto MIN/MAX` parameter behavior.** Today a bare-`Int` parameter *generalizes* over
   bounds. Under C a bare `Int` parameter is just `Int`, its meta a per-use fact from flow —
   demand-driven per §4.2 (⊤ is sound; only demands error). What must still be specified is the
   propagation policy into a *shared* type instantiation: exact per-call-context analysis vs
   `Meta.join`-ing contexts at the boundary — both sound, a precision/cost dial, with boundary
   annotations as user-chosen join points.
5. **Migration scale.** A re-architecture of `monomorphize`'s bound handling plus a rewrite of
   `Int.els`/`Arithmetic.els`/`Interval.els` in stdlib and jvm. Pre-1.0 this is as cheap as it
   will ever be; the deletion inventory (§5) is large enough that the end state is *less* code
   than today.

## 8. Recommendation and migration plan

Adopt C as the working direction, pending the open questions above; do **not** build B in the
meantime (§5.5). The change is large, so the plan below is a **strangler-fig migration**: build
the channel *beside* the type-parameter system, initially **seeded from the type parameters**
(so it can be verified against the existing behavior while types still carry the truth), port
consumers one at a time under that verification, concentrate the unavoidable semantic switch in
one bounded flag-day step, and only then delete. Every step lands compilable with green tests.

**Why a flag day is unavoidable but small.** Three consumers of bounds *cannot* be ported early:
check-mode widening (`Coerce` insertion), branch joins (`Combine`), and arithmetic result types
(the assoc formulas) — as long as `Int[0,1]` and `Int[0,5]` are distinct types, the type-level
obligations they discharge keep existing. But they don't need porting: at the moment `Int` loses
its parameters, the mismatches that trigger them **stop arising** — they die by equality, not by
rewrite. Everything else (representation, codegen widths, the channel itself, all new syntax)
*can* be built and verified beforehand, which is what shrinks the flag day to a bounded,
mostly-mechanical commit.

Two safety mechanisms run through the plan:

- **Shadow verification** (steps 2–5): while types still carry bounds, the channel must compute
  the *identical* interval at every Int-typed node, asserted as a hard error in the test
  configuration. The whole existing integration suite becomes an agreement harness (and a free
  performance benchmark for the rider walk).
- **A toy tracked type** (step 4): the full protocol — slot declaration, `Tracked`/α, contracts,
  transfer braces, projections — is exercised end-to-end on a test-only type *before* Int
  migrates onto it, so Int's migration is mechanics, not discovery.

### The steps

**Step 0 (parallel, recommended) — DONE (2026-07-10): close the postponed-constraint hole.** Convert
leftover postponed unification constraints to hard errors (TODO.md item, with its triage pass). Not a
dependency, but the migration wants loud failures, not silent drops, while the checker is under
surgery. *Landed:* `Unifier.flushPostponed()` is the final step of `TypeStackLoop.runPostDrainPipeline`
— it re-`drain()`s (triaging away constraints the meta-defaulting just made verifiable), then turns
each survivor into a hard "Type mismatch." with its recorded context. The triage keeps two shapes
benign because a more precise fail-safe already owns them: an applied abstract associated type
(`MetaRole.AbstractAssoc` head — the assoc reducer's obligation, its loud-fail / the strict quoter
covers a real leak) and a `$bad-apply` head (a phantom meta defaulted to `VType`/`VConst` then applied
— already-diagnosed or vacuous). The class the flush is the sole backstop for is a postponed
application whose meta *solved to a concrete head* that then mismatches. See `PostponedFlushTest`.

**Step 1: collapse the jvm width machinery into width-reading intrinsics. — DONE (2026-07-10).**
Replace the 27 width-pair leaves + the `IntArith` 5-instance guard family + the inner result-width
`fold`s with three width-agnostic intrinsics (`add`/`subtract`/`multiply` on `Int`) whose emission reads the
operands'/result's *lowered representations* — exactly what `intLessThanOrEqual` already does
(one leaf, all widths), and on the JVM all 27 leaves already share one emission pattern (unbox,
apply, rebox), so the codegen barely changes. No type-system change; result types still come
from the assoc formulas. Lands green against the unchanged integration suite, deletes ~150 lines
of `jvm/Int.els` early, and establishes the "codegen reads widths, not names" pattern the
channel needs later. *Landed:* `jvm/eliot/eliot/lang/Int.els` now has three body-less leaves
`nativeAdd`/`nativeSubtract`/`nativeMultiply` (dependent result signatures unchanged — `add`/`subtract`/
`multiplyMin`/`multiplyMax` of the operand bounds), the `Arithmetic[Int, Int]` method bodies are single
calls to them (no `nativeWiden` widen/narrow), and the whole `IntArith` ability + its 5 guarded instances
are gone (205 → 108 lines). `Intrinsics.addLeaves`/`subtractLeaves`/`multiplyLeaves` (9 FQNs each) collapse
to `nativeAddFQN`/`nativeSubtractFQN`/`nativeMultiplyFQN`; `ExpressionCodeGenerator`'s arithmetic-leaf arm
is unchanged in behavior (it already read the reps) — only `longOpcode`/`bigIntegerOp` now match the three
FQNs by `===`. `nativeWiden` + the guarded `Coerce[Int, Int]` stay (deleted at Step 7). Operands may now
reach a leaf at *different* representations (the old code pre-widened to a common one); the emission already
unboxes each operand from its own rep, so this is correct and more precise. Full `__.test` green; the
`ArithmeticAbility`/`Ranges` examples build and run end-to-end.

**Step 2: the channel in shadow mode.** Introduce the rider: a per-binding/per-node meta value
carried through the monomorphize walk, with (a) `Meta` in `eliot.compiler` +
`implement Meta[Interval[BigInteger, BigInteger]]` in the stdlib overlay (today's two-parameter
compile-time Interval — its collapse to one parameter comes in step 8, not here); (b) seeding
**from the type arguments** (a value of type `Int[a,b]` seeds `[a,b]`; a literal seeds its
singleton); (c) transfer at arithmetic = the channel's own Interval computation, joins at
branch merges via `Meta.join` — checker-invoked instance resolution, the existing
`RefinementSolver.unifyOrCoerce` pattern re-pointed; (d) the per-node meta table exported as a
fact codegen can read (the channel→backend plumbing, exercised in step 3; new facts must
serialize for the incremental cache). Shadow assertion: channel interval == type-derived
interval everywhere, hard error under test. Green: no user-visible change plus the agreement
suite. Split into 2a (straight-line propagation) and 2b (joins) if needed.

**Step 2a — DONE (2026-07-10).** Straight-line transfers, shadow assertion, and the exported
fact, built as a **post-pass** (not the in-checker rider the prose above imagines) — refinements
are strictly downstream of type formation (§3's held invariant), so the channel can run entirely
over the checker's output with zero risk to the checker's ~15 invariants; a `where`-guarded
*instance* selection over a refinement stays downstream too (it may change which body runs, never a
result type — the invariant outlaws the type-changing half), so it never forces the rider unless
refinements become type identity (§7.1, deferred). Landed:
`monomorphize/channel/RefinementChannelProcessor` (a `TransformationProcessor[MonomorphicValue.Key,
RefinementTable.Key]`) walks each ground body; at the platform arithmetic **leaves**
(`eliot.lang.Int::nativeAdd`/`nativeSubtract`/`nativeMultiply` — where operands/result are concrete)
it recomputes the result interval by resolving the compiler-pool `Arithmetic[Interval[BigInteger,
BigInteger], …]` instance (`AbilityImplementation.Key(add/subtract/multiply, [Interval, Interval],
Platform.Compiler)`) and evaluating its reduced body on the two interval *values* through the one
NbE evaluator (`ReducedBindingClosure.reduceInstance` + `Evaluator.applyValue`/`force` +
`Quoter.quote` — the `RefinementSolver.combinePair` pattern re-pointed at the `Interval` transfer),
then asserts it equals the interval the `Int` associated-type formulas produced. A divergence is a
hard `compilerError` at the node (fail-safe verified by a forced-mismatch probe). The `RefinementTable`
fact (per-node `[min,max]` by source position, first-order/serializable; `FactCache.CACHE_VERSION`
bumped 4→5) is demanded once per generated value from `MonomorphicUncurryingProcessor`, so the check
runs on the critical path and gates the build. The whole existing integration suite is now the
agreement harness (871/871 green); the genuine cross-check is **multiplication** (Interval's
corner `min`/`max` vs `Int`'s `multiplyMin`/`multiplyMax` — e.g. `(0,100)×(0,50)=(0,5000)`), add/subtract
agree by construction but are live regression guards. Seeding is "from the type" (transitional, per
(b)); the transfer/join machinery is the durable part. **Deferred to 2b:** the `Meta` ability +
`implement Meta[Interval[…]]` and branch-merge joins via `Meta.join` (2a needs neither — transfers use
the existing `Arithmetic[Interval,Interval]`); the `Combine`-at-covariant-meta join is already
materialised as `nativeWiden` coercions post-monomorphization, so only `match`/`if` joins remain to
recompute there.

**Step 3: representation from the channel.** Add `Represent` + the jvm instance (the
`fitsByte/…` fold logic as an Eliot body over the interval). `RepresentationLowering` computes
the layout **both** ways — opaque-body unfold and `Represent.layout(channel interval)` — asserts
they agree, then the backend consumes the channel-derived one; the opaque path is demoted to a
shadow check. Green: layouts provably identical on the whole suite. After this step, the only
load-bearing consumers of bounds-in-types are the three that die at the flag day.

**Step 4: slots, contracts, projections — proven on a toy type.** The syntax and its semantics,
landed per construct so nothing is ever parse-but-ignored (per the fail-safe rule):
4a — declaration-position slots (`type X {name: D}`) with the `Tracked` desugar, the `Meta[D]`
demand, and per-(type, slot) uniqueness; declare Int's `range` slot *alongside* its still-present
type parameters (transitional; seeding stays "from type args"). 4b — use-position contracts on
parameters, returns, `val`s, and `data` fields (`Int{range: 0..255}`, sole-slot sugar), checked
against the channel via `Meta.contains`, ⊤ failing any demand; field contracts check at
construction and seed at reads. 4c — return-brace transfer expressions with projections
(`{range: a.range + b.range}`): defining on body-less natives, checked on bodied defs. Each
sub-step is additive and green; the toy tracked type gets end-to-end tests (α, contracts,
transfers, joins) without touching Int. Grammar risk (suffix brace vs block/effect-row braces)
is confronted in 4a on the unambiguous declaration position first.

**Step 5: `Numeric` groundwork; the domain code comes off `Arithmetic`.** Land single-parameter
`Numeric[T]` + `implement Numeric[BigInteger]` (plain `T -> T -> T` signatures), and rewrite the
*domain-side* Interval arithmetic — the code the channel has been running since step 2 — onto
`Numeric[BigInteger]`/the BigInteger natives directly, off `Arithmetic`'s assoc machinery. The
`+`/`-`/`*` *operators* cannot switch yet (pre-flag-day, `Int[0,100] + Int[0,50]` still needs the
heterogeneous typing), and the runtime `Interval[S, E]` instance stays on `Arithmetic` untouched.
Green: channel agreement suite unchanged; `Arithmetic` now has no compile-time-critical client.

**Step 6 — the flag day: `Int` loses its type parameters.** The one bounded atom; everything it
needs was pre-landed. Contents: (a) stdlib `Int.els` → plain `type Int {range: …}`; aliases
become refined aliases (`type Byte = Int{byteMin..byteMax}`); the `Arithmetic[Int,Int]` instance
becomes `implement Numeric[Int]` whose natives carry transfer braces (step 4c syntax; step 1
intrinsics as bodies); the `Combine[Int,Int]` instance and `Arithmetic.els` are deleted;
`intToString(value: Int)`; `Compare[Int]` header simplified. (b) The operators switch to
`[T ~ Numeric[T]]`. (c) The checker types literals as plain `Int` and the channel's seed source
switches from type-args to self (α for literals, transfer braces at natives, contracts at
boundaries); the shadow assertion is retired. (d) Test expectations update: out-of-range errors
become contract violations at demand sites; where tests expect narrow layouts inside generic
containers, `data` field contracts are added (unannotated fields are now soundly-⊤/bignum — the
accepted S4 trade). What needs *no* code change: `Coerce` insertion, `Combine` accumulation, and
per-bound instantiation simply stop being triggered — their deletion is step 7, separately
green.

**Step 7: deletions, each its own green commit** (all paths dead since step 6): 7a `Coerce`'s
bounds role — the jvm guarded instance, `nativeWiden`, the `unifyOrCoerce` insertion/splicing
path. 7b `Combine` + the combinable/candidate/taint `MetaRole` data, `resolveCombines`,
`pendingUpperBounds`, the unification-time interception. 7c the associated-types lane
(`reduceAssocApplications` + hooks, `assocReductionCache`/`assocSubstitution`,
`MetaRole.AbstractAssoc`, the unifier postponement arm, the saturate/resolve/`AbilityMatcher`
arms) — no client remains, per the §5.1 note. 7d the `opaque` track (keyword, fact-chain flag,
`NativeBinding`/`TransparentBinding` dual split, the unfold path — delete the step-3 shadow
check first). 7e vestiges: `CodegenProjection`'s width-collapse, per-bound identity leftovers,
`BinderRoles` simplification.

**Step 8: cleanups and follow-ons.** Collapse `Interval[S, E]` → `Interval[T]` with
`implement[T ~ Numeric[T]] Numeric[Interval[T]]`, bodies only (deleting its formula half and the
corner annotations — the S2 payoff). Add `where` preconditions on defs (§4.3) — additive, kept
off the critical path deliberately. Enrich LSP hover from the per-node meta fact (step 2d)
so `Int` hovers show the computed range. Then the **second domain** — `List`/`Array` `size` —
which is the real test of "user-defined tracking" and the prerequisite for flow grades
(TODO.md).

### Risks to watch

- **Channel performance**: Meta/transfer evaluation runs Eliot via NbE per node; step 2's shadow
  suite measures it for free. Mitigations if needed: cache instance resolutions, intern common
  intervals.
- **Grammar**: the suffix-brace/`..` questions (§3) are confronted at step 4a on the easiest
  position first; fallback punctuation exists.
- **Test churn concentration**: by design, expectation updates land almost entirely in step 6 —
  if that commit's test diff grows beyond expectations, that is the signal to stop and re-derive,
  not push through.
- **Specialization regression**: post-flag-day, unannotated generic containers of `Int` widen to
  bignum on the JVM; intended (S4), but MCU-facing code and size-sensitive tests want field
  contracts — write them as part of step 6, not later.
