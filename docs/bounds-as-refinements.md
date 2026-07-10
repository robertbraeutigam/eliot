# Bounds as Refinements: Moving Meta-Information Out of the Type System

**Status: DESIGN — direction adopted (C); Steps 0–3 + 5 landed, Step 4 nearly done (4c transfer half landed;
only the Step-6-gated join generalization remains); Step 6 adopted "Staged R2" and its first sub-step (6-i:
representation sourced from the per-node channel table, shadow-verified) has landed — see §8 Step 6.** Steps 0–3 done; Step 4's `^Meta` **desugar machinery**
landed (4a meta structure + 4b-i transfer-companion desugar + Int's slot), **Step 5** landed (single-parameter
`Numeric[T]` + `implement Numeric[BigInteger]`), and **Step 4c's transfer half** landed (2026-07-10): the
channel now evaluates the leaf's `^Meta` transfer companion (`rangeAdd^Meta`/…) instead of Scala-resolving the
domain instance. The transfer is spelled as **plain** `intervalAdd`/… functions bottoming at `Numeric[BigInteger]`
natives — *not* a `Numeric[Interval]` ability instance, because a transitively-reached Eliot-body instance never
dispatches under the channel's NbE (see §8 Step 4c). All committed & green (1271/0). The one remaining Step-4
piece — retiring the 2b `handleCases` join recognition via general body-meta-translation — needs flow-derived
metas and is therefore **Step-6-gated**. Written 2026-07-10, following the
Interval/Arithmetic associated-types work (commit 3ad7ba38) and the design discussion it triggered.
Extended the same day with the channel's semantics (§4), and again 2026-07-10 with the decisive
simplification that reframes §3–§4: meta-information is carried by a **`^Meta` companion** in a new
`Qualifier.Meta` namespace — a *fourth* output of `DataDefinitionDesugarer` beside the type and
value constructors — so slots, transfers, and seeding reduce to ordinary constructor calls and
companion functions the one NbE evaluator already runs, not new machinery. Two consequences of that
turn: the `Tracked` ability **dissolves** into `^Meta` (the protocol is now just `Meta` + the landed
`Represent`), and value-position contract annotations + refined aliases are **deferred** (Option B) —
the only brace positions are the type *declaration* (the meta constructor) and the *return* type
(the transfer), and every per-value demand is a `where` clause.

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

- contract checks — a return-position transfer (`def add(…): Int {a + b}`, §4.2) on a *bodied* def
  is an *assertion*: `computed ⊆ declared`, verified at each manifest use site;
- the transfer rules themselves (the `^Meta` companions);
- representation selection and codegen;
- `where` demands on defs over slot projections (§4.3) — the sole per-value precondition surface
  (Option B); (future) `implement` guards over refinements, under the discipline below.

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

**The syntax, as adopted** (reusing the constructor/companion machinery rather than a parallel
grammar — §4.2/§4.4). A new `Qualifier.Meta` namespace joins `Type`/`Default`, and `{…}` is a
**third constructor bracket** dual to `[…]` (type) and `(…)` (value). Only two positions carry a
brace:

- **Domain declaration** — the brace on a type *declaration*, stating its named meta **slots**:
  `type Int {range: Interval[BigInteger]}`. This desugars to a **meta constructor** `Int^Meta` (a
  fourth output of `DataDefinitionDesugarer`, beside the type and value constructors), plus a slot
  *accessor* per slot. A `data` type's meta constructor is *auto-derived from its fields*; only a
  field-less native type (`Int`) needs the explicit declaration. Multiple slots are just multiple
  entries (`type Array[T] {size: Interval[BigInteger], align: Alignment}`).
- **Transfer (return position)** — the brace on a *return* type, referencing parameters by name:
  `def add(a: Int, b: Int): Int {a + b}`. This desugars to the function's **`^Meta` companion**
  `add^Meta(a: Interval, b: Interval): Interval = a + b` — the transfer the channel evaluates.
  Arguments pass **positionally** (`Int {sizeExpr, alignExpr}` for a two-slot domain); there is no
  named-argument form, because Eliot has none. The bare `Int{0..255}` on a native return is
  sole-slot sugar, and its content is an ordinary expression: `0..255` is **`infix ..` on
  `Interval`** (a library operator — the tokenizer already lexes `..` as one operator token,
  distinct from the subject-last `.`), not dedicated range grammar.

Everything else uses no brace:

- **Demands are `where` clauses** — a precondition on a def over slot projections, on the existing
  ability-guard `where` machinery: `def pop[T](ls: List[T]): T where ls.size.start > 0` (§4.3).
  Parameter types stay **bare**; there is no per-parameter contract brace. `where` is the single
  place a per-value demand lives, so it is never spliced or threaded per-parameter.
- **Native output refinements** are the return-brace of the native (`def readByte: Int{0..255}`),
  i.e. `readByte^Meta = 0..255` — axiomatic, at the native's trust level.
- **Instance heads are refinement-blind**: `implement Foo[Int]`, never `Foo[<refined Int>]`. Bare
  `Int` stays bare: as a parameter its meta is a per-use fact from flow (⊤ until demanded), as a
  return it is computed by `^Meta`.

**Deferred (Option B, 2026-07-10).** Contract annotations in *value* positions (`Int{0..255}` on a
parameter, `val`, or `data` field) and **refined aliases** (`type Byte = Int{0..255}`) are set
aside. Both reintroduce a refinement that must ride a type *occurrence* through resolution and, in
contravariant (parameter) position, either desugar to a spliced `where` — the plumbing this design
wants to avoid — or reappear as a type wrapper (`RefinedType`, the suffix dual of the effect row);
neither is needed for the Int migration. Under B, per-value demands are `where` clauses and a
persistent field bound is a `where` on the constructor (§6 S4); an unannotated field is ⊤/bignum.
The `RefinedType`-wrapper route is the sketched path if value-position contracts are wanted later,
kept off the critical migration.

Grammar: the two live brace positions are unambiguous — a *leading* brace is the effect row
(`{Console} A`, prefix), a brace *adjacent to a type atom* is the meta constructor (`Int{…}`,
suffix), exactly as `f(x)` vs `f (x)` already disambiguate by adjacency, and the meta bracket lives
in the type-atom parser only (a `{` in a value position stays a block). Migration is a mechanical
rewrite of a few dozen sites; the language is young enough that honest syntax is cheaper than a
compatibility shim that forever teaches the wrong model.

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
  (`Int{a + b}` — what the value is). Under Option B the value-channel suffix brace lives only in
  the two producing positions (a type *declaration*'s slots, a *return*'s transfer);
  `def add(a: Int, b: Int): {Abort} Int{a + b}` shows one return type carrying both channels.
- Value-facts attach to **program-point knowledge**, not to runtime objects: the same value can
  be known as `[0,1000]` before a check and `[0,9]` inside the `if (x < 10)` branch. That is
  what makes path narrowing possible at all, and why the channel erases perfectly — it was never
  a property of the object, only of the analysis. A `data` field carries its meta through the
  type's auto-derived meta constructor (§4.2); a *persistent* bound — one demanded of every value
  a field ever holds — is a `where` on the constructor (§6 S4), not a per-field brace.

(A third value-attached domain has been running implicitly all along: **binding time** — the
static/residual distinction the reduce-and-reify work watches via `VNeutral`-ness is a textbook
abstract domain.)

### 4.2 Declaring meta-information, and the `^Meta` companion

The declaration form puts the brace on the type declaration as **named slots**, desugaring to a
**meta constructor** in the new `Qualifier.Meta` namespace — the fourth output of
`DataDefinitionDesugarer`, beside the type constructor (`^Type`), the value constructor
(`^Default`), and the field accessors:

```
type Int     {range: Interval[BigInteger]}
type List[T] {size:  Interval[BigInteger]}

-- Int {range: …} desugars to (one meta constructor; the slot name becomes an accessor) --
Int^Meta(range: Interval[BigInteger])
```

A `data` type's meta constructor is **auto-derived from its fields** — `data Pixel(v: Int)` yields
`Pixel^Meta(v: Interval[BigInteger])`, an untracked field (`String`) contributing a `Unit` slot —
so an explicit `{…}` declaration is only needed for a field-less native type. Multi-slot types are
first-class (`type Array[T] {size: Interval[BigInteger], align: Alignment}`); arguments are
positional (no named-argument form — Eliot has none). Since domain values exist only at compile
time, declaration-position brace content resolves against the **compiler pool** — a natural use of
the two-pool machinery.

The transfer is the same idea one level up. Every named value already desugars to a value function
(`^Default`) with a stacked type companion; the channel adds a **third companion, `^Meta`**, and a
return brace is simply its body:

```
def add(a: Int, b: Int): Int {a + b}

-- desugars to the ordinary add^Default, plus --
add^Meta(a: Interval[BigInteger], b: Interval[BigInteger]): Interval[BigInteger] = a + b
```

The parameter list is undecorated; the sole annotation is the one return brace, and its parameter
references are the projections (`^Meta`'s parameters are the *same* parameters, retyped to their
domains). `a + b` is plain `+` on two `Interval` values — the `Numeric[Interval[BigInteger]]`
instance *is* the transfer, evaluated by the one NbE evaluator. This is the
dependent-refinement-signature style of Liquid Haskell / F* (`val add: a:int -> b:int -> r:int{r =
a + b}`), with the twist that fits Eliot: those systems predicate over the runtime value (hence
SMT); here the projection denotes the **domain value** directly and computes with it (plain
evaluation). The scoping comes free: "parameters in scope in the return position" *is* Π-codomain
dependency, and `VPi` is already the one primitive Π-former.

**`^Meta` is always produced** (Unit for untracked types), so a function *always* has a transfer to
evaluate — `Meta[Unit]` is the trivial instance (`top = unit`, `join = unit`, `contains _ _ =
true`) that makes untracked flow total and never-erroring. Which of three cases a function is in is
decided solely by whether a brace and a body are present:

1. **Native leaf + brace** → `^Meta` is axiomatic (as trusted as the native's type signature). The
   channel *evaluates* it. This is what replaces the hand-written Scala transfer recognition of
   Steps 2–3 (§8 Step 4).
2. **Bodied def + brace** → `^Meta` is a *declared* transfer; the body-propagated meta is checked
   ⊑ it, and callers use `^Meta` — the deliberate abstraction boundary.
3. **Bodied def, no brace** → `^Meta` is *synthesized by meta-translating the body*: each call
   `f(x)` becomes `f^Meta(x^Meta)`, each literal its α (below), each `match` a `Meta.join`. This is
   the "a wrapper states nothing; the channel computes through it" case, now uniform — the
   difference from today's `[L1, H1, L2, H2]` world where every wrapper re-bound and re-stated.

Case 3's translation is trivial for first-order code (all of Int and the toy type) and deepens only
at genuinely higher-order points, where `f^Meta` must receive the `^Meta`s of its *function-typed*
arguments (the `fold`/cost example below) — the boundary §7 defers.

The discipline — how this stays consistent with generics and values:

- **Two binder namespaces, plus projections.** `[T]` binds a type parameter (upper-case;
  participates in identity and dispatch); `(a: …)` binds a value parameter. There is no third,
  meta-binder namespace: `^Meta` references the metas of already-named things through slot
  projections — `a.range`, `ls.size`. Braces mark the *channel*; they do not bind.
- **Slot accessors are channel-scoped.** `range` lives in `Qualifier.Meta`, so it is simply not in
  scope in a runtime (`Default`) position — the namespace separation *is* the "hard error in
  runtime bodies" the design wants; a slot accessor can never leak into a value, so program
  behavior never depends on inference precision (extending §3's held invariant one notch: channel
  facts flow into checks and layout, never into type formation, and never into **values**). For a
  single-slot type the projection is elidable: inside `Int^Meta` the sole domain value is the
  parameter itself (`a`, not `a.range`).
- **Parameter demands are `where`, not braces.** A per-value precondition is a `where` clause
  (§4.3), never a brace on the parameter (Option B). Parameter types stay bare; the demand lives
  in one place instead of spliced per-parameter.
- **Return-position braces**: on a body-less native, **defining** (axiomatic); on a bodied def, a
  **checked contract** (computed ⊑ stated) — relational contracts on ordinary functions for free.
- **One meta level**: domain expressions evaluate channel-free — `+` on Intervals is not itself
  tracked, and domain types are untracked. No meta-of-meta tower, and no bootstrapping cycle
  (checking `Interval`'s own code never demands Interval-as-domain; the compiler pool's
  single-owner DAG ordering does the rest).

**α — literal seeding — is the value constructor's own `^Meta`.** For a constructed type it is
`cons^Meta`/`nil^Meta` ("length as a singleton", an ordinary fold); for a native primitive it is
the channel's literal base case (`5 ⤳ Interval(5, 5)`). It doubles as the **CTFE bridge** —
whenever NbE fully reduces an expression to a concrete value (constantly, in a whole-program-
evaluated language) the channel re-seeds exact meta via α, so compile-time-known values never
suffer ⊤. This is exactly what the standalone `Tracked` ability was; it dissolves into `^Meta`
(§4.4).

Projections extend to the computation channel (§4.1): a carrier-typed parameter's grade is
projected the same way (`step.cycles`), so a cost-transfer combinator needs no binders either —

```
def fold[T, A](ls: List[T], init: A, step: F[A]): {cycles: ls.size.end * step.cycles + 7} F[A]
```

— both channels composing in one signature (size × step-cost), everything referenced through
names that already exist; this is precisely the higher-order `^Meta`-of-function-args boundary
(`step.cycles`) that case 3 defers. Nested projections (`p.first.range`) are syntactically
well-formed and deliberately meaningless until the containers question (§7) is answered.

The layer split survives cleanly: the *base* states the instance method signatures **with**
transfer braces (their `^Meta` bodies) — platform-independent semantics; today's
`AddResult = Int[L1+L2, H1+H2]` formula, relocated into the honest channel — and platforms supply
the native `^Default` bodies.

Unstated meta is **demand-driven**, refining §3's fail-safe: ⊤ is always *sound* ("I know
nothing" is true, just imprecise), so unstated, undemanded flow does not error. Errors fire where
a demand meets ⊤ or fails: a `where` clause needing a bound, or the representation policy — which
is where strictness scales with the target automatically. On the JVM, ⊤ lays out as bignum (sloppy
code compiles, just fat); on an ATtiny the policy has no ⊤ layout, so the same code errors at the
exact value that could not be bounded, with the ⊤ provenance in the message.

### 4.3 Preconditions: `where` on defs — the demand surface

The existing `where` machinery (compile-time predicates on `implement` blocks) extends to defs,
with slot projections available. Under Option B (§3) this is the **sole** way a per-value demand on
a parameter is stated — there is no parameter contract brace — so a demand is written once, over
the parameters it relates, and never spliced or threaded per-parameter:

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

### 4.4 The ability protocol: `Meta` and `Represent`

Everything the channel needs from user space is **two** abilities — so enforcement is ordinary
instance resolution and coherence, and the **only new compiler machinery is the `^Meta` desugar
plus the rider walk**. Both live in `eliot.compiler`, instances in the owning layer's
`eliot-compiler/` overlay (checking-only — the `Effect`/`Throw` compiler-pool precedent):

```
ability Meta[D] {
   def contains(outer: D, inner: D): Bool   -- ⊑: every demand/contract check (was Coerce's guard)
   def join(a: D, b: D): D                  -- branch merges (was Combine)
   def top: D                               -- the "know nothing" fail-safe
}
```

plus later `meet` (path narrowing) and a rendering method for errors. A missing instance is the
ordinary "No ability implementation found" error. **One instance per domain**, not per type-pattern:
`Meta[Interval[BigInteger]]` serves Int's value range *and* List's size — the per-pattern
`Combine`/`Coerce` instances collapse into it — and `Meta[Unit]` is the trivial instance covering
every untracked type.

The old `Tracked` ability is **gone**: its `fromValue` (α) is the value constructor's `^Meta`
(§4.2), and slot structure is the meta constructor the desugarer already emits. So the desugar, not
an ability, carries the association (precedent: `data` desugars to constructor values via
`DataDefinitionDesugarer`):

```
type Int {range: Interval[BigInteger]}

-- DataDefinitionDesugarer emits, beside ^Type / ^Default / accessors --
Int^Meta(range: Interval[BigInteger])          -- the meta constructor; `range` becomes an accessor
```

Coherence gives the association the right unit: the meta constructor is **one per type** (like the
value constructor), merged across layers by the existing constructor merge — two layers declaring
the *same* slot with the same domain are split halves of one (the `(ability, pattern)` identity
that shipped for the split `Arithmetic[Int]`); the *same* slot with a *different* domain is a
conflict a whole-program scan catches. Multiple *slots* per type are simply multiple entries in the
braces — the first-class form, not a future door.

Representation policy is the second ability — unchanged since Step 3 (landed): what the `opaque`
body did, as a platform-side instance keyed on the **domain** (not per tracked type — deferred to
Step 8):

```
implement Represent[Interval[BigInteger]] {              -- jvm layer's compiler overlay
   def layout(range: Interval[BigInteger]): Type = ...   -- the old fitsByte/fitsShort/… fold over the interval
}
```

— consulted by codegen (`layout` returns a `Type`, a representation-type value in λ*). "Representation
derived in Eliot, not Scala" is preserved; the `opaque` track stays dead.

De-risking precedent: "the checker resolves and evaluates ability instances mid-check" is already in
production — `RefinementSolver.unifyOrCoerce` resolves `Coerce` by name and evaluates its body
through NbE today, and Steps 2–3 already resolve+evaluate the compiler-pool `Interval` arithmetic,
`Meta.join`, and `Represent.layout` instances the same way. This design re-points that pattern from
the ability being deleted to the two being kept.

What the compiler cannot check — the lattice laws (`join` associative/commutative/idempotent,
`contains` reflexive/transitive, `top` absorbing) and, more importantly, **soundness of transfers**
(the stated interval of `add` must contain every possible sum) — are the instance author's
obligations, verified by tests, at a native's trust level. Defining a domain is
compiler-extension-grade work; the `eliot.compiler` import signposts it.

**The successor map** — every piece of today's apparatus has a named, ordinary successor:

| Today | Under C |
|---|---|
| `Coerce`'s `where`-guard containment check | `Meta.contains` |
| `Combine` / `Combined` | `Meta.join` |
| Associated-type result formulas (`AddResult = …`) | the function's `^Meta` companion (the return brace, `Int {a + b}`) |
| `Tracked.fromValue` (α, literal seeding) | the value constructor's `^Meta` + the literal base case |
| The `opaque type Int[…] = fold(fits…)` body | `Represent.layout` (Step 3, landed) |
| Compile-time `Interval[BigInteger, BigInteger]` as "the bound of an Int" | the domain value itself (`Interval[BigInteger]`) |
| Bound-generic binders (`[L1: BigInteger, …]`) | `^Meta` parameters (the same params, retyped to their domains) |

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
| `implement Arithmetic[Int[L1,H1], Int[L2,H2]]` with formula lines | `stdlib/eliot/eliot/lang/Int.els:41-49` | formulas move onto method signatures | replaced by `implement Numeric[Int]` — the formulas become the return-brace `^Meta` companions of the method signatures (`def add(a: Int, b: Int): Int {a + b}` ⤳ `add^Meta`, §4.2), whose bound arithmetic is the existing compile-time `Interval` code |
| Interval's type-level half: four-way `Arithmetic` constraint, 3 formula lines incl. the quadruple `Combined` chain | `stdlib/eliot/eliot/lang/Interval.els:37-46` | deleted | deleted; instance becomes `implement[T ~ Numeric[T]] Numeric[Interval[T]]` with bodies only |
| Corner-binder `MulResult[...]` annotations (the val-aliasing workaround) | jvm + compiler-overlay `Interval.els` | deleted | deleted |
| Bound-generic signatures on plain functions (`intToString[Min, Max]`) | stdlib | kept | deleted (`intToString(value: Int)`) |

### 5.3 JVM platform layer (`jvm/eliot/eliot/lang/Int.els`, 205 lines today)

| Machinery (today) | Fate under B | Fate under C |
|---|---|---|
| 27 width-specific bound-generic native leaves (`nativeAddByteToByte[M1,X1,M2,X2]…` × add/subtract/multiply × 9 width pairs), each with a dependent result signature | kept | **collapsed**: width selection moves to codegen, which reads the operands' and result's refinement intervals and picks the instruction; the Eliot surface needs ~3 leaves (`add`/`subtract`/`multiply` on `Int`) or none beyond the ability methods |
| `ability IntArith` + 5 guarded instances (the width-dispatch family, ~55 lines) + the inner result-width `fold`s | kept | **deleted** — this was codegen policy expressed as ability dispatch because widths lived in types |
| `nativeWiden` + the guarded `implement Coerce[Int[Smin,Smax], Int[Tmin,Tmax]] where …` instance | kept | **deleted** — widening is a representation change decided by codegen from refinements; no user-space conversion function exists |
| `opaque type Int[auto MIN, auto MAX] = fold(fitsByte…)` representation-policy body | kept | the *policy* survives but changes home and input: the platform's `Represent[Interval[BigInteger]]` instance (§4.4, domain-keyed; landed Step 3), consulted by codegen (keeping "representation derived in Eliot, not Scala"); the `opaque`-marked type body goes, and with it the entire compiler-side `opaque` track (see the §5.1 row) |
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

**S1 — an application function.** `def area(w: Int, h: Int): Int` (Option B: bare parameters, §3).
A: return must be spelled `MulResult[...]` or a widened bound checked via Coerce. B: `auto` or a
widened contract. C: parameter types are plain `Int`; any per-value precondition is a `where`
clause (`where w.range.end <= 100`); the body's range `[0,10000]` is computed in the channel from
the callers' actual metas; the plain return type `Int` is complete and stable; an optional
`: Int{0..20000}` return brace is a checked, Coerce-free assertion (the def's `^Meta`).

**S2 — Interval.** A: today's three-file split with restated formulas (§1). B: bodies only, but
still bound-generic headers and per-bound instantiations. C:
`implement[T ~ Numeric[T]] Numeric[Interval[T]]` — one constraint, three one-line bodies, and the
compile-time copy doubles as the Int domain implementation.

**S3 — width dispatch.** A/B: the `IntArith` guarded family + 27 leaves at the ability level.
C: codegen reads each site's refinement interval and picks the layout/instruction; the policy
stays in Eliot as the platform's `Represent[Interval[BigInteger]]` instance (§4.4, landed Step 3).
Guards over refinements at the *ability* level are not needed for this — which conveniently defers
the instance-guard discipline question (§7).

**S4 — a `data` field.** `data Pixel(v: Int)`: the field's meta rides the auto-derived
`Pixel^Meta(v: Interval[BigInteger])` — per construction site, whatever value flows in (§4.2), so
layout follows the actual metas under whole-program monomorphization. A *persistent* bound demanded
of every value the field ever holds is a `where` on the constructor (`data Pixel(v: Int) where v
fits 0..255`, Option B), which turns an ⊤/over-wide value into a loud error rather than a silent
bignum. An unannotated field is ⊤: sound (bignum layout) but fat. Fields are boundaries; this
matches the "annotate boundaries" idiom rather than whole-program-flow field typing (deferred).

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

1. **Containers.** Where does `List[Int]`'s element range live? Under the meta-constructor model a
   `data` type's meta rides its auto-derived `^Meta` fields (§4.2), so the element range is the
   `List^Meta`'s slot — but propagating it precisely through a heterogeneous container is the open
   part. Refinements-on-type-*occurrences* (full Liquid-style decorated types — the deferred
   `RefinedType`-wrapper / value-position-contract route, §3) is the richer, more entangled
   alternative — explicitly deferred with Option B.
2. **Guards over refinements on `implement`.** Def-level `where` demands are designed (§4.3); the
   open half is *instance selection* depending on the channel
   (`implement … where <refinement predicate>`). That is coherent only if all guarded siblings
   agree on their type signatures (selection then changes *which body runs*, never a type) — the
   held invariant of §3. S3 shows the one former client (width dispatch) doesn't need it; defer
   until a client does.
3. **Transfer through higher-order code — the `^Meta`-of-function-args boundary.** First-order
   `^Meta` synthesis (§4.2 case 3) is trivial; the deep part is that a higher-order function's
   `^Meta` must receive the `^Meta`s of its *function-typed* arguments (`fold`'s `step.cycles`), so
   `^Meta` companions themselves become higher-order. Mostly this falls out of the NbE walk
   (closures are applied, not abstracted over), but the story at genuinely opaque points (effectful
   natives' callbacks) needs writing down: their signatures' declared return braces are the
   boundary. Not needed for the Int migration.
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
- **A toy tracked type** (step 4): the full protocol — slot declaration + meta constructor, α (the
  value constructor's `^Meta`), transfers (the return-brace `^Meta`), joins — is exercised
  end-to-end on a test-only type *before* Int migrates onto it, so Int's migration is mechanics,
  not discovery.

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

**Step 2b — DONE (2026-07-10).** Branch-merge joins via `Meta.join`, same post-pass. Landed the
durable protocol piece: **`ability Meta[D] { def join(a: D, b: D): D }`** in
`lang/eliot/eliot/compiler/Meta.els` (borrowed into the compiler pool, beside `Coerce`/`Combine`) +
**`implement[T ~ Compare[T]] Meta[Interval[T, T]]`** in the stdlib compiler overlay
(`stdlib/eliot-compiler/.../Interval.els`) — generic over the endpoint type (min/max via the
endpoints' own `Compare`, so it needs no `BigInteger` import, which had caused an import-shadow
error), the value-level twin of `Combine[Int, Int]`. Only `join` exists (its one 2b consumer);
`contains`/`top` land with theirs. The channel now recognises the pattern-match eliminator
(`WellKnownTypes.isPatternMatchHandleCases`) and, for a `match` whose arms carry different `Int`
ranges, recomputes the merge: `match` desugars to `handleCases(scrutinee, cases)` where `cases` is
the **Scott encoding** `Lam($selector -> $selector(arm1)(arm2)…)`; the channel parses that structure
(peel the selector lambda, flatten the `$selector` application, peel each arm lambda) and reads
**every** arm's *pre-coercion* interval — off the `nativeWiden` operand where an arm was widened to
the merged range, off its own type where it already is that range — then folds them through
`Meta.join` (resolved on `Interval` exactly as the transfer resolves, `applyIntervalInstance` shared)
and asserts the result equals the merge's type (the type-level `Combine` join = the `handleCases`
result type). Reading *all* arms (not just widened ones) is load-bearing: a widest un-widened arm
dropped would false-mismatch. Any non-matching `cases` shape or non-Int arm is silently skipped
(fail-safe). Verified end-to-end on a divergent-range `match` (`First -> 5` ⊔ `Second -> 15` =
`[5,15]`, agreeing) and by a forced-mismatch probe; covered in CI by an `ExamplesIntegrationTest4`
case + the `MatchRanges` example. 871/871 green.

**Key distinction the channel encodes (why *only* `match`/`if` merges are join-checked):** a
`match`/`if` result is *runtime-chosen*, so the channel *must* join the arms (it cannot know which
runs) and its join equals `Combine`'s — a valid agreement. A `pick[A](a: A, b: A): A` "join" is a
different animal: at runtime the value is exactly `a`, so the endgame channel (tracking real flow) is
*tighter* than the type's `Combine` join there; that case legitimately diverges and is deleted with
`Combine` at Step 6, so it is **not** a join-check target (it manifests as `nativeWiden(pick(a,b))`,
which the channel leaves alone). This is the sharpest way to see that the channel is not "verifying
the type system" but a genuinely independent, more precise analysis.

**Step 3: representation from the channel. — DONE (2026-07-10).** Add `Represent` + the jvm instance (the
`fitsByte/…` fold logic as an Eliot body over the interval). `RepresentationLowering` computes
the layout **both** ways — opaque-body unfold and `Represent.layout(channel interval)` — asserts
they agree, then the backend consumes the channel-derived one; the opaque path is demoted to a
shadow check. Green: layouts provably identical on the whole suite. After this step, the only
load-bearing consumers of bounds-in-types are the three that die at the flag day. *Landed:*
`ability Represent[D] { def layout(range: D): Type }` in `lang/eliot/eliot/compiler/Represent.els`
(**domain-keyed like `Meta[D]`**, not the design's per-tracked-type `Represent[Int]` — deferred to
Step 8's second-domain work, since one interval-tracked type exists; `layout` returns `Type` because a
layout *is* a representation-type value in λ*). The jvm instance
`implement Represent[Interval[BigInteger, BigInteger]]` ships in a **new jvm compiler overlay**
`jvm/eliot-compiler/eliot/compiler/Represent.els` (jvm's first `eliot-compiler/` root, colocated with the
ability — the ability is re-declared per the per-file-resolution `Console` precedent), its `layout` body the
verbatim `opaque type Int` fold reading `start`/`end` of the interval **value** instead of the `MIN`/`MAX`
type parameters. `monomorphize/channel/RefinementRepresentation.channelLayout` resolves the compiler-pool
`Represent[Interval]` instance and evaluates `layout(Interval(min,max))` through the one NbE evaluator
(`ReducedBindingClosure.reduceInstance` + `applyValue`/`force`/`Quoter` — the `RefinementChannelProcessor`
transfer/join pattern re-pointed at `Represent.layout`); `RepresentationLowering.representInt` runs it
alongside the `opaque` unfold and hard-errors on divergence (fail-safe verified by a forced-mismatch probe:
the whole jvm suite went 193/0 → 162/31 and the CLI emits "Refinement channel disagrees with the type at an
integer representation: channel chose …JvmBigInteger… but the opaque body chose …JvmByte…"). When no platform
`Represent` instance is on the path — a bare representation-bearing `Int` stub in a lang unit test with no
overlay — `channelLayout` returns `None` and lowering falls back to the `opaque` unfold (reduced coverage,
never a false accept). `FactCache.CACHE_VERSION` 5→6 (uncurry gained the `Represent` dependency). **Gotcha
hit:** `Int`/`BigInteger` are auto-imported `eliot.lang` prelude modules, so `import eliot.lang.Int`/`BigInteger`
in the overlay caused a *silent* double-import shadow (only surfaced because the overlay is demanded via the
swallowing channel, not the base compile) that aborted the file's `ModuleValue` merge — the fix is to import
only `Bool` and `Interval`. 1261/1261 green.

**Step 4: the `^Meta` companion, replacing the shadow-mode Scala recognition.** Steps 2–3 compute
transfers, joins, and layout by **recognizing native FQNs in Scala** inside
`RefinementChannelProcessor`/`RefinementRepresentation`. Step 4 replaces that scaffolding with the
uniform `^Meta` mechanism (§4.2/§4.4); net-subtractive in Scala. Landed per construct so nothing is
ever parse-but-ignored (per the fail-safe rule).

**Design as built — the meta value is a `data` STRUCTURE named `T$Meta`, not the bare domain
(2026-07-10).** `type Int {range: Interval[BigInteger, BigInteger]}` desugars *exactly as*
`data Int$Meta(range: Interval[BigInteger, BigInteger])` — type ctor, value ctor, slot accessor, and
a `PatternMatch` impl the accessor reduces through — by reusing `DataDefinitionDesugarer` on a
synthetic `DataDefinition`. The `$Meta` name suffix (`$` is not an identifier character, so it can
never collide with a user type) keeps the meta type distinct from `Int^Type` **without** a separate
`MetaType` namespace — which was considered but rejected because `CoreExpressionConverter` qualifies
a bare uppercase name to `Type` with no way to say `MetaType`, so the meta structure's self-references
would mis-resolve. This is what makes the transfer companion's parameter type a pure name transform
(`T → T$Meta`) with **no cross-definition lookup**. Meta structures are compiler-pool-only (dead in
the runtime pool, never code-generated); only the `^Meta` *transfer companions* live in
`Qualifier.Meta`. (A rename to a real `MetaType` namespace is mechanical if ever wanted.)

- 4a — **`Qualifier.Meta` + the meta structure. DONE** (781d226c + reworked 39a980f4). `Qualifier.Meta`
  in both `module.fact` and `resolve.fact` Qualifiers (4 exhaustive matches updated: 2 `Show`,
  `ValueResolver.convertQualifier`, `DocText.kindLabel`); the `type X {slots}` grammar (parsed after
  the generic params, before the `= body`) → `FunctionDefinition.metaSlots`; `MetaConstructorDesugarer`
  synthesizes the `X$Meta` `DataDefinition` and reuses `DataDefinitionDesugarer` (so multi-slot types
  are multi-field structures for free). `Meta[Unit]`/always-produce and the `data`-field auto-derive
  are deferred (only field-less native `Int` needs the explicit declaration so far).
- 4b-i — **return-brace → `^Meta` transfer companion desugar. DONE** (805521e3). The return brace
  `: T {expr, …}` → `FunctionDefinition.returnMeta`; `MetaTransferDesugarer` emits
  `f^Meta(a: T$Meta, …): R$Meta = R$Meta(<brace exprs>)` in `Qualifier.Meta`, parameter/return types
  via the `T → T$Meta` name transform. Inert (no real def carried a brace yet).
- 4b-ii-a — **Int's `range` slot. DONE** (f4b62dc7). `type Int[auto MIN, auto MAX] {range:
  Interval[BigInteger, BigInteger]}` in stdlib `Int.els` (+ `import eliot.lang.Interval`) →
  `Int$Meta` generated in the real stdlib+jvm compile; verified unperturbed (jvm 193/0). Inert and
  transitional (Int keeps its `[MIN, MAX]` parameters, the shadow-mode source of truth until Step 6).

- 4c — **channel evaluates the `^Meta` transfer companion (transfer half). — DONE (2026-07-10).** The
  channel now recomputes each arithmetic transfer by evaluating an Eliot `^Meta` companion instead of
  Scala-resolving the domain instance directly. Base-layer vessels `def rangeAdd(a: Int, b: Int): Int
  {intervalAdd(range(a), range(b))}` / `rangeSubtract` / `rangeMultiply` in stdlib `Int.els` (co-located
  with the `range` slot, so `Int$Meta` + the accessor resolve per-file) desugar (`MetaTransferDesugarer`)
  to `rangeAdd^Meta(a: Int$Meta, b: Int$Meta): Int$Meta = Int$Meta(intervalAdd(range(a), range(b)))`;
  `RefinementChannelProcessor.runTransfer` maps each leaf `nativeAdd → rangeAdd^Meta`,
  `ReducedBindingClosure.reduceInstance`s the companion at empty type args, applies the two operand ranges
  wrapped as `Int$Meta(Interval(lo, hi))`, and reads the result `Int$Meta`'s `range` slot back. Verified
  load-bearing by forced-mismatch (break `intervalAdd` → CLI `ArithmeticAbility` build hard-errors "channel
  computed [0, 50] but the type is [0, 150]"). Full suite 1271/0.

  **The blocker that Step 5 was supposed to fix was only half the story.** The original blocker was the
  assoc `+` (`Arithmetic[Interval]::add`, `AddResult`), which Step 5 removed by giving the domain a
  non-assoc `Numeric[Interval]` instance. But routing the vessel through *any* Eliot-body ability **instance**
  — assoc or not — still fails: a transitively-reached ability method evaluates to a `VStuckNative` in a
  type/`SemValue` position that the checker's ability-ref collector (`collectAbilityRefs`, expression-only)
  and the only type-position rewriter (`reduceAssoc`, associated-types-only) both ignore, so it never
  dispatches to the instance body and `Quoter.quote` fails ("Cannot quote stuck native application
  `Numeric::add^Numeric`"). Instance dispatch under the channel only ever works via the **explicit**
  `AbilityImplementation.Key` resolution the channel does for the join (and Step 5's transfer did) —
  *never* implicitly through a companion body. The resolution: spell the transfer as a **plain function**,
  not an ability instance. `implement Numeric[Interval[T, T]]` in the compiler overlay is replaced by plain
  `intervalAdd`/`intervalSubtract`/`intervalMultiply[T ~ Numeric (& Compare)]` functions whose bodies bottom
  directly at the endpoints' `Numeric[BigInteger]` **natives** (which *do* re-fire transitively) + `Compare`;
  the vessel calls those. Step 5's `Numeric[T]`/`Numeric[BigInteger]` groundwork is fully load-bearing (the
  endpoints); only the `Numeric[Interval]` *instance* it added is superseded by the plain functions. `opaque`
  is **not** touched — it was never the blocker (Robert flagged it as removable; it turned out to be the
  Eliot-instance-dispatch limitation, not `opaque`). `FactCache.CACHE_VERSION` 7→8.

  **The join half stays via `Meta[Interval]::join` (not yet body-meta-translation).** The 2b `handleCases`
  recognition + explicit `Meta.join` resolution is kept: it already routes the merge through the Eliot
  `Meta.join` instance (the ^Meta protocol's join), via the *explicit* `AbilityImplementation.Key` path that
  works. Fully *retiring* the `handleCases` recognition needs general body-meta-translation (every node's
  meta from α/`^Meta`/`Meta.join`), which requires **flow-derived** operand metas — but in shadow mode the
  channel reads operand metas from *types*, so body-meta-translation is a **Step-6-era** generalization (once
  metas come from flow, not types). Recorded here rather than forced now. First-order only; the higher-order
  `^Meta`-of-function-args boundary (§7 Q3) stays deferred.

**Checkpoint (2026-07-10):** 4a + 4b-i + 4b-ii-a + **Step 5 + Step 4c (transfer half)** are committed and
green (full suite 1271/0). The transfer (2a) shadow check now fires via the `^Meta` companions; the join
(2b) still via the explicit `Meta[Interval]::join` path — **no coverage regression** (both proven by
forced-mismatch). The `^Meta` desugar machinery is complete and exercised end-to-end on real `Int`
arithmetic. What remains of Step 4 is only the join-half generalization, which is Step-6-gated (flow metas).
`where`-on-defs and any value-position contract remain out of Step 4 (deferred — Option B / §8 Step 8).

**Step 5 (sequenced BEFORE Step 4c — the transfer companion's evaluation depends on this
non-assoc arithmetic; see Step 4): `Numeric` groundwork; the domain code comes off `Arithmetic`. — DONE
(2026-07-10).** Land single-parameter
`Numeric[T]` + `implement Numeric[BigInteger]` (plain `T -> T -> T` signatures), and rewrite the
*domain-side* Interval arithmetic — the code the channel has been running since step 2 — onto
`Numeric[BigInteger]`/the BigInteger natives directly, off `Arithmetic`'s assoc machinery. The
`+`/`-`/`*` *operators* cannot switch yet (pre-flag-day, `Int[0,100] + Int[0,50]` still needs the
heterogeneous typing), and the runtime `Interval[S, E]` instance stays on `Arithmetic` untouched.
*Landed:* `stdlib/eliot/eliot/lang/Numeric.els` declares `ability Numeric[A]` (`add`/`subtract`/`multiply :
A -> A -> A`, **no operators** — reached only by name or inside an instance body) + body-less
`implement Numeric[BigInteger]`, mirroring `Arithmetic[BigInteger]` (runtime track, borrowed to the compiler
pool; native-bound compile-time). `StdlibNativesProcessor` binds `Numeric::add`/`subtract`/`multiply` both
ways (ability-method FQN + impl-method dispatch), alongside — not replacing — the `Arithmetic` twins. The
compiler overlay `stdlib/eliot-compiler/eliot/lang/Interval.els` gains
`implement[T ~ Numeric[T] & Compare[T]] Numeric[Interval[T, T]]` — bodies only, endpoint arithmetic via
`Numeric`, corner min/max via `Compare`, **no `AddResult`/`SubResult`/`MulResult` formulas and no
corner-binder `MulResult[...]` annotations** (the S2 payoff, arriving early because both endpoints share one
type `T` at the domain). The overlay's old `Arithmetic[Interval, Interval]` copy is **removed** (its
`add`/`subtract`/`multiply` names collided with `Numeric`'s in the one file — "Name defined in multiple
abilities" — and the channel no longer resolves it; the runtime `Arithmetic[Interval]` split across base + jvm
is untouched). `RefinementChannelProcessor.runTransfer` re-points from `Arithmetic::add`/… on `[Interval,
Interval]` to `Numeric::add`/… on the single `[Interval]` type arg (`numericAbilityMethod`). Calling the bare
ability method (`add(start(a), start(b))`) inside the same ability's instance body resolves to the abstract
`Numeric::add` (dispatched per-type at monomorphization) and passes the recursion check — the resolver's
`searchImplementationScope` over-match is gated to type-context, not bodies (precedent:
`Effect[ThrowCarrier]::pure`). `FactCache.CACHE_VERSION` 6→7. **Load-bearing PROVEN by forced-mismatch**
(break the `Numeric[Interval]` `add` high endpoint → the CLI build of `ArithmeticAbility` hard-errors
"Refinement channel disagrees … channel computed [0, 50] but the type is [0, 150]"), so the channel genuinely
evaluates the `Numeric` instance rather than silently skipping. Green: full suite 1271/0; the arithmetic
examples (`ArithmeticAbility`/`Intervals`/`Ranges`/`MatchRanges`) build + run end-to-end.
The refinement channel is now fully off `Arithmetic`; the only remaining compile-time `Arithmetic` clients
are `Int`'s type-parameter bound formulas (`L1 + L2`, `multiplyMin`/`multiplyMax`), which die at the Step 6
flag day.

**Step 6 — the flag day: `Int` loses its type parameters.** The one bounded atom; everything it
needs was pre-landed. Contents: (a) stdlib `Int.els` → plain `type Int {range: …}`; aliases stay
plain (`type Byte = Int` — refined aliases are deferred, Option B, so `Byte`'s byte bound comes
from a `where`/representation demand, not the alias); the `Arithmetic[Int,Int]` instance becomes
`implement Numeric[Int]` whose natives carry return-brace `^Meta` companions (step 4b; step 1
intrinsics as `^Default` bodies); the `Combine[Int,Int]` instance and `Arithmetic.els` are deleted;
`intToString(value: Int)`; `Compare[Int]` header simplified. (b) The operators switch to
`[T ~ Numeric[T]]`. (c) The checker types literals as plain `Int` and the channel's seed source
switches from type-args to self (α for literals, `^Meta` transfers at natives, `where`/
representation demands at boundaries); the shadow assertion is retired. (d) Test expectations
update: out-of-range errors become `where`-demand or representation-policy violations at their use
sites; where tests expect narrow layouts inside generic containers, a constructor `where` is added
(unannotated fields are now soundly-⊤/bignum — the accepted S4 trade). What needs *no* code change:
`Coerce` insertion, `Combine` accumulation, and per-bound instantiation simply stop being triggered
— their deletion is step 7, separately green.

**Step 6 is staged ("Staged R2", decided 2026-07-10).** The flag day is *not* one big-bang: a
mapping pass established that the checker's equality core (Coerce/Combine/assoc-reduction) degrades to
clean no-ops when `Int == Int` (no rewrites, no zero-arg crashes) and that literal typing is one
stdlib alias line (`IntegerLiteralType[V] = Int[V, V]` → `= Int`), but that the entire risk
concentrates in the **representation-layout pipeline**: `RepresentationLowering.representInt`,
`RefinementRepresentation.channelLayout`/`intIntervalOf`, and the jvm `opaque type Int =
fold(fitsByte[MIN,MAX]…)` body all read `Int`'s *two type arguments* for the interval, and none of
them survive `Int` going nullary. So representation must be re-pointed at the **per-node channel
table** *in lockstep* with dropping the params. To de-risk that (the "one place it's hard"), Step 6 is
split so the representation-plumbing lands first, shadow-verified, while `Int` still carries its
bounds; only then does the atomic flip follow.

- **6-i — representation sourced from the per-node channel table. — DONE (2026-07-10).** The `Int`'s
  machine layout now comes from the channel's per-node interval (`RefinementTable`, keyed by source
  position), not from the `Int[min, max]` type — the type is kept only as the **shadow cross-check**.
  `RepresentationLowering.representationOf`/`representInt` gained a `nodeInterval: Option[(BigInt,
  BigInt)]`, preferred over the type interval (`RefinementRepresentation.channelLayout(gv,
  nodeInterval)` + a new interval-keyed `channelLayoutForInterval`); `RefinementRepresentation`'s
  `representInt` asserts the table-derived layout equals the `opaque`-unfold (type-derived) one, so a
  divergence is still a hard error. `MonomorphicUncurryingProcessor` — which already *demanded* the
  table (formerly discarded) and already holds each body node's `Sourced` position one hop up — builds
  a position→interval map and threads it into `lowerUncurried`. **Load-bearing proven by
  forced-mismatch** (perturb the table interval → the representation assertion fires: "channel chose
  JvmLong but the opaque body chose JvmByte"), so the layout genuinely reads the table rather than
  silently falling back to the type. **Key gotcha surfaced and handled** (exactly the position-based
  node-identity risk this staging exists to flush out): the checker splices a `nativeWiden(x)`
  coercion at the *same* source position as its operand `x`, so the channel records two distinct
  intervals at one position (the operand's own vs. the widened range); an *ambiguous* position is
  dropped from the map and lowering falls back to the type. These `nativeWiden` wrappers exist only in
  shadow mode (they die at the flag day with `Coerce`), so post-flag-day no genuine node is ambiguous
  by position — the fallback is a shadow-mode-only concession, correct for the end state. Byte-for-byte
  identical output on the whole suite (green); the three `RefinementReconciliationIntegrationTest`
  `pick`/join cases were the ones that first exposed the collision. No cache-version bump (no fact
  structure changed; shadow-mode behavior is identical to the type-driven path).
- **6-ii — the atomic flip (NEXT).** `Int` nullary; `IntegerLiteralType[V] = Int`; delete
  `Arithmetic.els` + `Combine[Int]`; operators → `Numeric`; `implement Numeric[Int]`; jvm `Int.els`
  loses the `opaque`-body `MIN/MAX` reads, `nativeWiden`/`Coerce[Int]`, and the dependent result
  signatures; the channel's seed source switches from type-args to flow (α for literals, `^Meta`
  transfers, joins, cross-value propagation; ⊤ at parameter/return boundaries → bignum on JVM); the
  shadow assertion retires; large test-expectation churn (~318 `Int[` across 19 files; ~15 out-of-range
  tests change meaning — bound *enforcement* has no JVM replacement until `where`-on-defs, Step 8).

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
