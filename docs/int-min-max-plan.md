# Plan: Implementing `Int[MIN, MAX]` (dependent integer type)

Status: Phase 2 (explicit `integerLiteral[n]`), **Phase 3 (check-mode `Coerce` insertion),
Phase 4 (combining a covariant multi-candidate metavariable via the `Combine` ability), Phase 5
(runtime arithmetic — `+`/`-`/`*` with dependent bounds, type-level AND on the JVM), and Phase 6
(the global literal flip — value-position `n` ⟹ `integerLiteral[n] : Int[n,n]`) are DONE.** Real
programs now compute from **bare literals with no imports**: `println(intToString(2 + 3 * 4))` prints
`14`, and `def widened: Int[0, 1000] = 7` widens the bare `7` through the `Coerce` instance. See
"Phase 6 — As built" below. Implicit `Int` range widening works through a user-space `Coerce` instance,
resolved by name and evaluated by the one NbE evaluator (`Int[3,3] → Int[0,10]` accepted,
`Int[5,5] → Int[0,3]` rejected); and a covariant metavariable with more than one candidate (a `match`
result, or `f[A](a:A,b:A):A`) resolves to the `Combine` join of its candidates, with per-meta polarity
tracking ("taint on contravariant use") keeping the join sound. Dependent-bounds `+` type-checks
(`Int[3,3] + Int[4,4] : Int[7,7]`) via the `add` `BigInteger` native, and `renormalize` re-fires natives
stuck on now-solved metavariables. The remaining frontier is **Phase 7** (consolidated tests / examples /
docs polish) and the deferred width-selection / termination work noted under "Risks". This document is the
durable cross-session reference.

## Goal & chosen model

Make `Int[MIN, MAX]` a real, usable dependent integer type:

- Integer literals carry their value as a singleton type `Int[V, V]`. This is reached in
  two steps so the tree never breaks: first the value is mintable **explicitly** as
  `integerLiteral[n] : Int[V, V]` (the `Runtime.els` decls `IntegerLiteralType[V] = Int[V, V]`
  and `def integerLiteral[V: BigInteger]: Int[V, V]` already exist), used in tests while bare
  literals stay `BigInteger`; only at the end does a **frontend desugar** wrap every
  *value-position* literal `n` into `integerLiteral[n]`, making `Int[V,V]` the default. Monomorphize
  is **not** involved either way (see "Literals as `Int[V,V]`" below).
- Range compatibility (using `Int[0,5]` where `Int[0,10]` is expected) is decided
  by a user-space **`Coerce` ability inserted in the checker's check mode** — *not*
  by the unifier. `unify` is pure definitional equality.
- `+` / `-` / `*` (and comparisons) become natives with type-level bounds
  propagation **and** JVM runtime bytecode, so real programs compute.

### Key insight: literal kind is decided by *syntactic position*, not expected type

A literal is a `BigInteger` (a type-level *bound*) or an `Int[V,V]` (a runtime *value*)
purely by **where it sits** — the frontend already distinguishes the two:

- A literal in **type/bound position** (inside `[...]`, e.g. the bounds of `Int[5, 5]`)
  stays a bare `BigInteger`.
- A literal in **value position** (`def x: Byte = 5`, a function argument, …) is wrapped
  into `integerLiteral[5] : Int[5, 5]`.

So `BigInteger` is the type of *bounds*; `Int[MIN, MAX]` is the type of *runtime integers*.
The discriminator is the existing `typeContext` flag in core conversion — no checker
involvement, no expected-type peeking.

**Tradeoff (DECIDED — confirm if it bites):** because the rule is positional, a
value-position literal is *always* `Int[V,V]`, even where `BigInteger` is expected. So
`def one: BigInteger = 1` no longer type-checks (it becomes `integerLiteral[1] : Int[1,1]`).
The principled stance: **`BigInteger` values arise only in type/bound position or from
`BigInteger`-typed params/natives — you cannot mint one from a value-position literal.** If a
concrete need for value-position `BigInteger` appears, add a `Coerce[Int[V,V], BigInteger]`
(semantically sound — the singleton's value *is* the bound) rather than reintroducing
expected-type-directed typing.

## De-risking: explicit constructor first, global flip last

The global literal flip (value-position `n` ⟹ `integerLiteral[n]`) is the single **most
dependent** step, not the least: the instant bare literals become `Int[V,V]`, ordinary code
like `def x: Int[0,10] = 5` needs `Int[5,5] → Int[0,10]` widening — i.e. the check-mode
`Coerce` insertion of Phase 3. Doing the flip first therefore breaks the whole tree (every
literal retyped, no widening, no arithmetic) while delivering *no* working `Int`.

So the order is inverted: build and test all the `Int` machinery (Coerce widening, branch
`Combine`, arithmetic) against the **explicit** `integerLiteral[n]` constructor — which already
exists and needs no global change — while bare literals keep meaning `BigInteger` and the
codebase stays green. Each phase is validated in isolation (e.g. `f(integerLiteral[3])` against
`def f(x: Int[0,10])` exercises `Coerce` end to end). Only once that all works does Phase 6
perform the one-line `CoreExpressionConverter` desugar that makes `Int[V,V]` the default literal
type, together with its `BigInteger`-value-position fallout audit.

## What exists today (the foundation)

- **`unify` is pure definitional equality.** No `refinements` map, no assignability
  arm. Directional widening is the check-mode `Coerce` seam below.
- **One NbE evaluator** (`monomorphize/eval/Evaluator.scala`). It reduces `match` on
  a concrete scrutinee directly, so pure type-level code runs inside the checker's own
  evaluator; there is no separate compile-time interpreter. Stuck terms stay neutral.
- **`Bool`** — opaque `type Bool` in `lang` with compile-time natives `true`/`false`/`&&`
  (`SystemNativesProcessor`) and `lessThanOrEqual` (on `BigInteger`). Each reduces only
  when its arguments are concrete, otherwise stays stuck so unification still solves
  metavariables. (Opaque keeps the representation platform-dependent; the JVM backend
  maps it to a platform boolean.)
- **`Coerce` / `Option` declared and wired.** `ability Coerce[From, To] { def coerce(value: From): Option[To] }`
  (`Coerce.els`) and the abstract `Option.els` (now with body-less `some`/`none` constructors) are in
  place; `WellKnownTypes` exposes `coerceFQN`/`someFQN`/`noneFQN`/`optionFQN`. The check-mode *insertion*
  is **DONE** (Phase 3) — `Checker.unifyOrCoerce`/`tryCoerce`/`coerceWith`.
- **`Bool` eliminator.** `Bool.fold(condition, whenTrue, whenFalse)` (`Bool.els` + `boolFoldFQN` native in
  `SystemNativesProcessor`) — the only way to branch on opaque `Bool` (no `if`/`then`/`else`); used by the
  `Coerce` instance body.
- **`Int.els`** declares the abstract `type Int[MIN, MAX]`, the width aliases (`Byte`/`Short`/`Long`/…),
  `+` with dependent bounds, the internal `nativeWiden`, and the parametric range-widening
  `Coerce[Int[Smin,Smax], Int[Tmin,Tmax]]` instance (Phase 3). (`add` is declared abstract; its native +
  runtime are Phase 5.)

## Check-mode `Coerce` insertion (the assignability seam)

### Design

`unify` stays pure definitional equality. Directional widening (`Int[0,5]` used where
`Int[0,10]` is expected) is an ordinary user-defined coercion the checker inserts in
**check mode** — never in `unify`, never in inference (subtyping interacts badly with
metavariable unification; confining it to check mode keeps inference symmetric):

```
ability Coerce[from, to] {        -- declared in lang (Coerce.els); checker-invoked by name
   def coerce(value: from): Option[to]
}
```

The single parametric Int instance lives in stdlib (`Int.els`); the compiler hardcodes
only the *name* `Coerce` (by-name protocol recognition, the codebase's existing pattern).
The four bounds bind in the **instance head** by resolution-time first-order unification
on the head spine — there is no in-body `case Int[..]` match, so no `typeMatch` on an
abstract `type` is needed (verified against `ability/util/AbilityMatcher.scala`,
`tracePatternMetas`; cf. working `implement Show[Pair[String,String]]` /
`implement[A] Show[Box[A]]`):

```
implement [smin, smax, tmin, tmax] Coerce[Int[smin,smax], Int[tmin,tmax]] {
   def coerce(value) =
      if le(tmin, smin) && le(smax, tmax)   -- existence: depends ONLY on the bounds
        then Some(nativeWiden(value))        -- payload: the (possibly runtime) conversion
        else None
}
```

**NbE separates the two halves automatically.** For `x : Int[0,5]` checked against `Int[0,10]`:

- the `if` condition `le(0,0) && le(5,10)` has all bounds **known**, so NbE forces it to
  `true` — the `Some`/`None` choice is decided entirely at compile time;
- the payload `nativeWiden(x)` is **stuck on `x`** (runtime value unknown), so NbE leaves it
  as a residual — a well-typed runtime expression;
- result: `coerce(x)` ⤳ `Some(nativeWiden(x))`. The checker discriminates this through Option's
  abstract `fold`, reaches the some-branch, and **splices `nativeWiden(x)` into the call site** as
  a bare `Int[0,10]`. No `Option` survives at runtime. For `Int[0,5] → Int[0,3]` it forces to
  `None` → none-branch → compile-time type error. When representations already match
  (`Int[0,5] → Int[0,6]`, both byte) the payload is identity — zero runtime cost, same mechanism.
  Narrowing (`Int[0,10] → Int[0,5]`) is *not* this mechanism: it is an ordinary explicit stdlib
  function returning `Option`, with no checker support.

**Discriminate via `fold`, not a constructor match.** `Option` is an opaque platform `type`, not a
`data` declaration — no `Some`/`None` constructors for the compile-time evaluator to pattern-match.
The checker applies Option's abstract eliminator with two checker-internal sentinels — roughly
`fold(coerce(term), onNone = ⟨reject⟩, onSome = λp. ⟨accept p⟩)` — and evaluates *that*. The
reductions `fold(Some p, n, s) ⤳ s p` and `fold(None, n, s) ⤳ n` are compiler-as-platform natives
(`NativeFunction`); the backend supplies a different native for the real layout. The instance body's
`if … then Some … else None` is likewise `Bool`/`Option` abstract operations (natives), not surface
constructors.

### Where it lives — leaf check mode

```
check(term, expected):
  actual = infer(term)
  if unify(actual, expected): done                  -- pure definitional equality, unchanged
  else resolve Coerce[actual, expected]:
    evaluate fold(coerce(term), onNone, onSome) via NbE:   -- discriminate via abstract fold
      accept(payload) => replace term with payload   -- payload may carry runtime nativeWiden
      reject          => report mismatch (contract violated)
      stuck           => report mismatch / postpone  -- abstract bounds: cannot prove widening
```

Resolution is **two-level, so no conditional-instance machinery is required** (Eliot has none): the
ability resolves the coercion *family* by constructor (one parametric `Coerce[Int[..],Int[..]]`
instance), and the returned `Option` decides whether *these specific bounds* coerce. **Abstract
bounds → `stuck`:** inside a generic function where `a,b` are unknown, `le(...)` does not reduce, so
`coerce(x)` is neither `Some` nor `None`; the checker falls back to requiring definitional equality
(or postpones until monomorphization makes the bounds concrete). Implicit widening only happens where
the compiler can actually see it is safe.

### Scope

- **Leaf positions only** — argument / let-binding / return (`Int[0,5]` passed where `Int[0,10]` is
  expected). Coercing *inside* a constructor (`List[Int[0,5]] → List[Int[0,10]]`) needs variance
  reasoning + a structural rewrite — deferred; leave a boundary comment so it is a known edge.
- **Termination guard deferred** — evaluating `coerce` runs type-level code, which Girard's paradox
  lets diverge. The stuck-residual/fuel guard lands with the termination model
  (`project_recursion_as_effect`) — see "Risks".

### As built (Phase 3 — DONE)

The implementation kept the design's spirit (user-space `Coerce`, resolved by name, run on the one NbE
evaluator) but **simplified two planned work items** — both faithful to the cornerstone:

- **Discrimination is direct `VTopDef`-head inspection, not an in-language `fold`.** The checker forces
  `coerce(value)` and matches the head FQN: `WellKnownTypes.someFQN` ⟹ accept, anything else (`none` or
  stuck) ⟹ no coercion. So **no `Option` `fold`/`some`/`none` native reductions were needed** — `some`/
  `none` are ordinary body-less `def`s that evaluate to stuck `VTopDef`s (the same applied-constructor
  shape the unifier and `MatchNativesProcessor` already dispatch on). Recognizing `some`/`none` by
  well-known FQN is the sanctioned by-name-protocol pattern.
- **No new `Quoter` direction.** The widen is the identity on the Long-only backend (see "Representation
  & promotion"), so an accepted coercion just re-types the term: `expr.copy(expressionType = expected)`.
  `nativeWiden` exists only so the instance *body* type-checks (`some(nativeWiden value) : Option[To]`);
  it is never emitted to the runtime tree today. A real read-back lands with range-based width selection.
- **A `Bool` eliminator native was added** (`Bool.fold(condition, whenTrue, whenFalse)`, `boolFoldFQN` in
  `SystemNativesProcessor`) — the language has no `if`/`then`/`else` and `Bool` is opaque, so the instance
  body needs it to select `some`/`none` on the bounds check.

Files: `Int.els` (parametric instance + `nativeWiden`); `Option.els` (`some`/`none`); `Bool.els` +
`SystemNativesProcessor` (`fold`); `WellKnownTypes` (`some/none/option/boolFold` FQNs);
`Checker.unifyOrCoerce`/`tryCoerce`/`coerceWith`/`peelSigMetas`.

**Type-param binding subtlety (load-bearing).** The impl `coerce` body references the bound parameters
`Smin…Tmax` as *free names* (only value params are runtime lambdas — `CoreExpressionConverter.buildCurriedBody`),
and the pure NbE evaluator has **no metastore**. So `coerceWith` peels the impl *signature*'s type-param
`VLam`s into fresh metas, unifies the value-level function type against `VPi(actual, _ => Option[expected])`
and drains to solve them, then rebuilds the body env binding each name to the **forced (concrete)** meta value
— binding the metas directly would make the `lessThanOrEqual` native go stuck on unforced `VMeta`s at eval time.

**Grammar gotchas surfaced (and fixed) here:** generic/ability type parameters must be **upper-case**
identifiers (`acceptIfAll(isUpperCase, …)` in `GenericParameter`), and `def` names must be **non-upper-case**
— so the abstract `Option` constructors are `some`/`none` (not `Some`/`None`), and the existing
`Coerce.els`/`Int.els` declarations (`from,to`; `m1,M1,…`; `smin,…`) had to be upper-cased; they had never
actually been parsed before this phase.

Acceptance (met): implicit widening flows through the user-defined `Coerce` instance inserted in check mode;
`Int[3,3] → Int[0,10]` succeeds while `Int[5,5] → Int[0,3]` is a compile error; definitional equality is
untouched.

## Phase 4 — As built (DONE)

`Combine` is a **type-only** by-name ability — `ability Combine[A, B] { type Combined }` (`Combine.els`) — with
one parametric instance, `Combine[Int[Amin,Amax], Int[Bmin,Bmax]] { type Combined = Int[min(Amin,Bmin),
max(Amax,Bmax)] }` (`Int.els`), backed by `min`/`max` compile-time natives on `BigInteger`
(`SystemNativesProcessor`, same concreteness discipline as `lessThanOrEqual`). The implementation kept the plan's
spirit (accumulate candidates while `unify` stays pure equality; resolve at drain via the one NbE evaluator) but
**simplified / pinned down** several points:

- **Candidate accumulation is intercepted in `unify` *before forcing*, not in `solveMeta`'s already-solved arm.**
  `unify` forces both sides first, so a *solved* meta is replaced by its solution before the meta cases ever run —
  the planned "already-solved arm adds rather than `unify`s" would have been dead code. Instead `unify` matches the
  raw right side: a concrete type unified against an *already-solved combinable* meta (contributions always arrive
  as `check(arg, ?A)`, so the meta is the expected/right side) is recorded as a candidate and the unification
  succeeds; the first candidate is recorded when the meta is solved in `solveMeta`'s empty-spine `None` branch. The
  result-against-expected direction `?A ~ E` is left to the existing check-mode `Coerce` path (Phase 3),
  preserving its behaviour exactly.
- **Polarity = taint on contravariant use** (see the DECIDED note below) — `peelLams` registers each instantiation
  meta `combinable`; the `VPi`-vs-`VPi` case taints any meta in a domain. `metasOf` does **not** follow a meta's
  solution, so a meta that an earlier argument already pinned is still tainted by a later function-typed argument
  (`useTwice` is rejected in any argument order).
- **Resolution lives in the checker's drain-resolve loop**, not a separate pass: `Checker.resolveCombines` (folded
  into `TypeStackLoop`'s `step`) reads the unifier's accumulated `candidates`, and for each still-`combinable` meta
  with ≥2 distinct candidates folds them pairwise through `combinePair` (resolve `Combine` by name → evaluate the
  instance's `Combined`, binding its type params to the ability-resolution-returned type args), solves the meta to
  the join, and verifies each contributor `Coerce`s into it (`coercionExists`, reusing the Phase-3 `coercionHolds`
  core). When the join doesn't exist — no instance, or the meta was tainted — it falls back to `strictReunify`
  (one `addMismatch` per extra candidate, *not* `unify`, to avoid one error per `Int` bound).
- **`combinePair` only probes pairs that share a head constructor.** Probing `resolveAbility` for a pair with no
  implementation makes the ability machinery emit a "does not implement" error as a *fact-generation side effect*
  the caller cannot suppress, so cross-constructor pairs (`String`/`Int`) skip straight to the strict mismatch.
  The only instance today is the head-homogeneous `Combine[Int,Int]`; cross-constructor `Combine` is therefore not
  reached (a deliberate, documented limitation).
- **No `handleCases` recognition** — the trigger is purely generic meta-solving, so `match` branches and
  `f[A](a:A,b:A):A` are the same code path, exactly as planned.
- **Result-against-declared-type is checked against the join, not the first candidate** (closing what was briefly a
  soundness gap). The natural place to check "the result fits the declared type `E`" runs during the body walk, when
  the result meta is only solved to its *first* candidate — so checking there would accept a narrower `E` that the
  eventual join overflows (`def x: Int[3,5] = pick(int[3], int[7])`, join `Int[3,7]` ⊄ `Int[3,5]`). Instead, when a
  term's inferred type is a bare combinable meta **that has accumulated argument contributions** and the expected type
  is **concrete**, the check is *deferred* (`CheckState.pendingUpperBounds`) and discharged by
  `Checker.resolveUpperBounds` after drain, so the *join* is what must `Coerce` into `E`. Two guards keep this from
  over-firing: a fresh result meta with **no** contributions (e.g. an ability method's `B` in `convert(x): B`) must
  unify eagerly so value/ability resolution that depends on its solution proceeds; and an `expected` that is itself a
  (flowing) meta is an intermediate contribution (e.g. `id(i)` as an argument) that must unify now to propagate
  through the chain.

Acceptance (met): `pick[A](a:A,b:A):A` over two `Int` ranges joins to their covering range (and further widens to
a broader expected range); identical-range candidates need no join; a join that overflows a *narrower* declared
result type is rejected (checked against the join, post-drain); a contravariant (function-typed) parameter taints
the meta and rejects the join; and a pair with no `Combine` instance (`String`/`Int`) falls back to the ordinary
single "Type mismatch." No known soundness gaps remain in Phase 4.

## How integers flow today (research findings)

- Tokenizer: `Token.IntegerLiteral(content: String)`.
- AST/core: `IntegerLiteral(Sourced[String])` (string passed through).
- resolve: `ValueResolver` converts string -> `BigInt`
  (`Expression.IntegerLiteral(Sourced[BigInt])`); preserved through
  OperatorResolved / Monomorphic / Uncurried.
- monomorphize: `Checker` hardcodes the literal's type to
  `VTopDef(WellKnownTypes.bigIntFQN, None, SNil)` (i.e. `BigInteger`).
  `Evaluator` produces `VConst(Direct(value, bigIntGroundType))`.
- JVM: `ExpressionCodeGenerator` emits `Long.valueOf(toLong)`; `NativeType`
  maps `BigInteger` -> `java.lang.Long`. No arithmetic exists yet; only `inc`
  on `BigInteger` (a compile-time native).

## Native mechanisms (two of them)

1. **Compile-time / type-level** via `NativeBinding` facts
   (`StdlibNativesProcessor`, `SystemNativesProcessor`,
   `DataTypeNativesProcessor`, `UserValueNativesProcessor`,
   `MatchNativesProcessor`). A body-less `def` becomes a `VTopDef(fqn, None, SNil)`
   (stuck) unless a processor binds a `VNative` reducer. Examples: `inc` fires
   `VConst(Direct(n)) -> Direct(n+1)`; `MatchNativesProcessor` bakes
   `handleCases`/`typeMatch` so the NbE evaluator reduces `match` on a concrete
   scrutinee. The NbE evaluator/unifier are **pure**; all `NativeBinding`s must be
   pre-fetched before evaluation.
2. **JVM runtime** via `NativeImplementation.implementations: Map[ValueFQN, ...]`,
   consumed in `JvmClassGenerator.createModuleMethod`. Emits ASM bytecode.

## Phases

The prerequisite evaluation machinery is **done** (single NbE evaluator, opaque `Bool`,
`Coerce`/`Option` declared, pure `unify`). The phases below are the `Int`-specific work on top.

- **Phase 2 — Explicit `integerLiteral[n]` constructor (de-risk; no global flip).**
  - The base `Runtime.els` already declares `def integerLiteral[V: BigInteger]: Int[V, V]`
    (abstract) and `IntegerLiteralType[V] = Int[V, V]`. Goal of this phase: make `integerLiteral[n]`
    usable *explicitly* in value position and type-check to `Int[n,n]` from the signature (value
    stays a runtime residual), **without** touching `CoreExpressionConverter` — bare literals keep
    meaning `BigInteger`, so the tree stays green and nothing is retyped yet.
  - This is the safe test harness for Phases 3–5: widening/combine/arithmetic are all exercised with
    explicit `integerLiteral[n]` literals. No `BigInteger`-value-position fallout until Phase 6.
  - `integerLiteral` is a **platform-layered def** (layers cornerstone, like `println`/`IO`): the
    abstract decl suffices for type-checking now; the jvm-layer body that emits the right-sized
    integer lands with the arithmetic in Phase 5 (a trivial body earlier is fine). A compile-time
    `NativeBinding` is only needed once constant folding of runtime ints is wanted (Phase 5+), and
    even then it is *data* for the existing evaluator, not a code change.
- **Phase 3 — `Coerce[Int,Int]` instance + check-mode insertion (range subsumption). (DONE.)**
  Implemented; see "Check-mode `Coerce` insertion" below for the as-built design (which simplified two
  of the planned work items). The parametric instance lives in `Int.els`; the leaf check-mode hook is
  `Checker.unifyOrCoerce`/`tryCoerce`/`coerceWith`. Tests: `MonomorphicTypeCheckTest`'s
  "check-mode Coerce insertion" cases (widen accepted, non-containing rejected, definitional equality
  untouched) + the "Coerce[Int, Int] instance" resolution test.
- **Phase 4 — combining a covariant multi-candidate metavariable (the `Combine` ability). (DONE.)**
  See "Phase 4 — As built" below for the as-built design (which simplified a couple of planned work
  items). Files: `Combine.els` (type-only ability); `Int.els` (parametric `Combine[Int,Int]` instance);
  `BigInteger.els` + `SystemNativesProcessor` (`min`/`max` natives); `WellKnownTypes`
  (`combinedFQN`/`minFQN`/`maxFQN`); `Unifier` (candidate accumulation + `combinable`/taint);
  `Checker.resolveCombines`/`combinePair`/`coercionExists`/`resolveUpperBounds` +
  `CheckState.combineResolved`/`pendingUpperBounds`; `TypeStackLoop` (combine step folded into the drain-resolve
  loop, upper-bound discharge after it). Tests: `MonomorphicTypeCheckTest`'s "Combine[Int, Int] instance" cases.
  The plan-of-record below is retained for context.

  The phenomenon is **not** match-specific. Whenever *more than one type is unified against the same
  metavariable*, today the first solution wins and any unequal later candidate fails (the already-solved
  arm of `Unifier.solveMeta`, `Unifier.scala:88-92`: `unify(solved, rhs)`). A `match` hits this because
  it desugars to one `PatternMatch.handleCases[R](value, cases): R` call
  (`DataMatchDesugarer.buildHandleCasesCall`) so every branch body solves the one `R`; but
  `f[A](a: A, b: A): A` with `a: Int[0,5]`, `b: Int[3,10]` hits the *identical* seam. Phase 4 makes such
  a metavariable resolve to the **combination of its candidates** instead of the first — which is the
  standard "solve a type variable to the join of its lower bounds." There is **no `handleCases`
  recognition**; the trigger is generic meta-solving.
  - **The ability (`Combine`).** A by-name ability with a structured instance head, *type-only* (no
    runtime method — unlike `Coerce`): it answers "what single type covers both?" and nothing else.
    ```
    ability Combine[A, B] { type Combined }
    implement [aMin,aMax,bMin,bMax]
       Combine[Int[aMin,aMax], Int[bMin,bMax]] {
       type Combined = Int[min(aMin,bMin), max(aMax,bMax)]
    }
    ```
    The structured instance head avoids any in-body `case Int[..]` match (same discipline as `Coerce`).
    `min`/`max` are ordinary Eliot (now runnable on the one evaluator). Resolution mirrors `tryCoerce`:
    quote the two candidate types to ground, `resolveAbility(combineFQN, Seq(t1, t2))`, evaluate the
    `Combined` associated type. Fold pairwise across all candidates → the meta's solution `R`.
  - **`Combine` and `Coerce` are layered, not redundant.** `Combine` (type-level) *picks* the meta
    solution `R`; the Phase-3 `Coerce` (which carries the `nativeWiden`) then *materializes* each
    contributor at `R`. For `Int` this composes for free: `R` contains every input range by
    construction, so each per-contributor `Coerce[Int[..], R]` already exists. A `Combine` author isn't
    forced to make every input coercible to `Combined`; if one isn't, the widen just emits the normal
    mismatch — no new error machinery. User-facing error: `Cannot combine types: Int and String.`
  - **Soundness gate — only *covariant* metas may combine (the new prerequisite).** Joining candidates
    is sound only where the metavariable sits in **output (covariant)** position. Counterexample in an
    input position: `useTwice[A](f: A -> Unit, x: A, y: A)` with `f: Int[0,5] -> Unit`, `x: Int[0,5]`,
    `y: Int[0,10]`. Combining `A` up to `Int[0,10]` lets `f` (accepts only `0..5`) be called with a
    value statically allowed to be `10` — unsound; a contravariant meta needs the *meet*, or a rejection,
    never the join. `handleCases`'s `R` is safe **because** `R` is purely covariant (it is the output;
    nothing consumes an `R`). Pure definitional equality is what lets the checker ignore variance today;
    `Combine` brings the obligation in, so Phase 4 must add **per-meta polarity tracking** — the one
    piece the current design lacks. Near term the only covariant-only metas are exactly the match result
    and result-position type params, so combining can be restricted to those; contravariant metas keep
    today's strict-equality behaviour (defer meet/reject).
  - **Where it binds — accumulate in unify, resolve at drain (keeps `unify` pure).** `unify` stays pure
    definitional equality (`Unifier.scala:22` — "no assignability or directional widening here"), so
    `Combine` must **not** fire inside `solveMeta`'s comparison. Instead: a meta accumulates the types
    unified against it as candidate constraints (the already-solved arm *adds* rather than `unify`s);
    at `Unifier.drain()` (check-mode), a covariant meta is solved to `Combine(candidates…)`, then the
    existing per-position `Coerce` widens each contributor to `R`. Concrete-vs-concrete unification stays
    strict equality; the lattice solve sits in the same check-mode layer as `Coerce`. This **subsumes**
    the earlier `handleCases`-shape wiring — both the `match` and the `f[A](a,b)` cases fall out of the
    one meta rule. (When an *expected* type `E` is present — declared return, function argument — `E` is
    just another candidate unified into the meta, so checking position needs no special path.)
  - **Validate alongside:** the value-in-type `Eq` caveat (see the open sub-decision below) is
    load-bearing here — `Int[V,V]` embeds a value in a type, and `Combine` compares/combines those
    embedded bound values, so `GroundValue` read-back must be sound for this case.
- **Phase 5 — Runtime arithmetic. (DONE.)** `+`/`-`/`*` on `Int` with dependent bounds
  (`Int[a,b] + Int[c,d] : Int[a+c, b+d]`). Two execution targets: (a) a
  **compile-time `NativeBinding`** (for type-level bound computation), and (b) the
  **JVM** intrinsic (`LADD`/`LSUB`/`LMUL`, unbox Long -> op -> rebox) for runtime. Runtime
  stays `Long`. See "Phase 5 — As built" above for the as-built design (compile-time + JVM halves,
  the `renormalize` native-re-firing fix, and the two latent parser/lexer bugs it surfaced).

  **Compile-time half — As built (DONE).** `add(a,b): BigInteger` is declared in
  `lang/BigInteger.els` next to `min`/`max`/`lessThanOrEqual` and backed by the same
  compile-time native discipline (`SystemNativesProcessor.bigIntBinaryNative(addFQN)(_+_)`,
  `WellKnownTypes.addFQN`); it reduces only when both bounds are concrete, else stays stuck.
  `Int.els`'s `+` return type `Int[add(LMin,RMin), add(LMax,RMax)]` therefore computes the
  summed range at the type level. `Int[3,3] + Int[4,4]` type-checks to `Int[7,7]`, widens to a
  broader expected range (via the Phase-3 `Coerce`), and is rejected against a narrower one.
  Tests: `MonomorphicTypeCheckTest`'s "dependent-bounds +" cases (`addPrelude`/`addImports`).

  **Load-bearing fix — `renormalize` re-fires stuck natives.** A native applied to a
  *still-unsolved* metavariable goes stuck as a body-less `VTopDef(fqn, None, spine)` whose
  `fire` dropped the `VNative` reducer; ordinary `force` never re-fires it (it only unfolds
  *cached* bodies). This bit `+`: the codomain `add(LMin,RMin)` is built during application
  inference while the bound metas are unsolved, so each `add` stuck — and stayed stuck even
  after the metas solved (the error showed `Int(add(3,4), add(3,4))`). Fix: `Evaluator.renormalize`
  (+ `Checker.renormalize`) deeply forces and, for each body-less `VTopDef` whose FQN resolves
  (via the binding cache) to a `VNative`, re-applies the native to the renormalised spine —
  `add(3,4) ⤳ 7`. It is called on the codomain in `Checker.applyInferred`, **guarded to skip a
  bare `VMeta` result** so it never collapses a covariant combinable-meta to its provisional first
  candidate (which would break the Phase-4 `Combine` deferral / `resolveUpperBounds`). This is the
  same "bind to *forced* concrete values, not raw metas" discipline already used by the Coerce/Combine
  resolvers (plan "As built" notes), generalised into the evaluator.

  **JVM runtime half — As built (DONE).** `+`/`-`/`*` (compile-time bounds: `add`/`subtract` and
  `multiplyMin`/`multiplyMax` — the latter two are the min/max of the four corner products, correct
  across zero) plus `integerLiteral` and `intToString` are **inline JVM intrinsics**, recognised by
  well-known FQN in `jvm`'s `Intrinsics` object, emitted at the call site by
  `ExpressionCodeGenerator.generateIntrinsic`, and excluded from `JvmClassGenerator.createModuleMethod`'s
  body-less "Function not implemented." abort. `integerLiteral[V]` pushes the boxed-`Long` constant `V`
  (read from its type arg) — it *cannot* be a single `NativeImplementation` method (one method can't bake a
  per-`V` constant); `+`/`-`/`*` push both operands, unbox to `long`, apply `LADD`/`LSUB`/`LMUL`, rebox;
  `intToString` calls `Long.toString`. `Int` is mapped to `java.lang.Long` in `NativeType.types`. Chosen
  inline over the `NativeImplementation`-method route to avoid coupling to call-site name mangling.
  `nativeWiden` stays identity (no width selection yet). `Int → String` was added to stdlib (the chosen
  observability path) as abstract `def intToString` + the JVM intrinsic. Files: `Int.els` (`-`/`*`/
  `intToString` + precedence), `BigInteger.els` (`subtract`/`multiplyMin`/`multiplyMax`),
  `SystemNativesProcessor` + `WellKnownTypes` (the bound natives), `jvm` `Intrinsics` /
  `ExpressionCodeGenerator` / `JvmClassGenerator` / `NativeType`. Tests: `MonomorphicTypeCheckTest`
  ("dependent-bounds +/-/*", "operator precedence") + `ExamplesIntegrationTest` (runtime `7`/`6`/`14`/`-7`)
  + `examples/src/Arithmetic.els`.

  **Two latent bugs surfaced and fixed here (both pre-existing, exposed by the new operators):**
  - **Type-alias bodies were parsed with the greedy full expression parser.** `TypeAliasDefinition` used
    `component[Expression]` for the `= …` body, which consumes the *following* top-level definition's leading
    `infix`/`left`/… identifiers (not keywords) as an application chain — silently dropping that definition's
    fixity. Harmless until an `infix` def directly followed a `type` alias (it does now: the width aliases
    precede `+`, and the `def add` that used to sit between them moved to `BigInteger.els`). Fixed to
    `Expression.typeParser` (a single type atom) — the correct, consistent choice for a type position.
  - **No negative integer literals.** `-128` lexed as the `-` operator + `128`, so the signed width aliases
    (`type Long = Int[-9223…, 9223…]`) failed once the `-` operator existed and they were actually checked.
    Added a `negativeIntegerLiteral` token (atomic `-` *glued* to digits, before the symbol parser) so binary
    `a - b` still needs spacing. Negative *results* always worked at runtime (`Long`); this is only about
    *writing* a negative bound literal.

  **Not auto-imported (deferred to Phase 6).** `Int`/`Runtime` are intentionally **not** in
  `defaultSystemModules` — adding them there breaks every `ProcessorTest` that doesn't provide a matching
  stub. Code using arithmetic imports `eliot.lang.Int` / `eliot.lang.Runtime` explicitly for now; Phase 6
  (the literal desugar that mints `integerLiteral[n]` everywhere) is where they become ambient, together
  with the test-harness stubs. (`stdlib/Runtime.els` now imports `Int` for its own `Int[V,V]` body.)
- **Phase 6 — Flip literals to `Int[V,V]` (the global switchover; monomorphize untouched). (DONE.)**
  See "Phase 6 — As built" below. The one-line desugar in `CoreExpressionConverter` plus making
  `Int`/`Runtime` ambient (`defaultSystemModules`) is all the production change; the rest was test/stdlib
  fallout. Plan-of-record retained:
  - `core/processor/CoreExpressionConverter.scala`: in the `IntegerLiteral` case (currently ignores
    `typeContext`), when `typeContext == false` rewrite `n` into a
    `NamedValueReference("integerLiteral", typeArgs = Seq(IntegerLiteral(n)))` — i.e.
    `integerLiteral[n]`. When `typeContext == true`, leave the bare literal. The inner `[n]` lands in
    `typeArgs` (type context), so it stays `BigInteger`. (The match-desugarers already synthesise
    FQN-keyed calls this way — `DataMatchDesugarer.buildHandleCasesCall`, `TypeMatchDesugarer`.)
  - **Why monomorphize needs no change:** after the wrap, the only bare `IntegerLiteral`s that reach
    monomorphize are type/bound-position ones, so the existing `Checker.scala` "bare literal ⟹
    `BigInteger`" rule becomes correct *by construction*. Value-position literals are ordinary
    `integerLiteral[n]` applications, type-checked/evaluated by the generic machinery.
  - Audit tests/stdlib for the value-position `BigInteger` pattern (`def … : BigInteger = <lit>`) —
    see the tradeoff under "Key insight". This fallout is now contained to the final phase, with the
    `Coerce` safety net already in place.

  **Phase 6 — As built (DONE).** The production change is exactly two edits, both faithful to the
  cornerstone (the desugar is a pure frontend rewrite; monomorphize and the evaluator are untouched):
  - **The desugar** in `CoreExpressionConverter.convertExpression`'s `IntegerLiteral` case: value
    position (`typeContext == false`) ⟹ `NamedValueReference(integerLiteral, typeArgs = [IntegerLiteral(n)])`;
    type/bound position (`typeContext == true`) keeps the bare `IntegerLiteral`. The discriminator is the
    existing `typeContext` flag — no checker / expected-type involvement.
  - **`Int` and `Runtime` are now ambient** — added to `ModuleName.defaultSystemModules`, so every module
    auto-imports them and `integerLiteral`/`Int` resolve for any value-position literal. Consequently code
    must **not** import them explicitly (that double-imports and shadows): the explicit `import eliot.lang.Int`
    was removed from `stdlib/Runtime.els`, `examples/Arithmetic.els`, and the JVM integration tests, which all
    now use bare literals (`2 + 3 * 4`).
  - **The documented tradeoff bit exactly once, as predicted:** value-position `BigInteger` literals no
    longer type-check (`def x: BigInteger = 5` ⟹ `Int[5,5] ⊄ BigInteger`). Fallout was purely in tests; the
    handful of fixtures that minted a `BigInteger` from a literal were retargeted to `Int[n,n]` (literal/app
    tests) or to `String`-carried value-equality (the "calculate concrete value" tests), which exercise the
    same mechanism without depending on `Int`.
  - **Test-harness stubs.** `ProcessorTest` gained minimal ambient `Int`/`Runtime` stubs
    (`intStubContent`/`runtimeStubContent`) in its base `systemImports`, plus `systemModulesWithoutInt` for
    fixtures that use `Int` as a *local* declaration name (the ability tests' `data Int`) and write no
    literal — they opt out of the ambient `Int` to avoid the shadow. The `MonomorphicTypeCheckTest`
    `Coerce`/`Combine`/arithmetic fixtures moved their formerly-local `type Int` + instances + operators into
    the *ambient* `Int`/`Runtime` import stubs (`intImports`), mirroring reality (the snippets now just use
    the ambient `Int`).
  - Files: `core/processor/CoreExpressionConverter.scala` (desugar); `module/fact/ModuleName.scala`
    (`Int`/`Runtime` ambient); `stdlib/Runtime.els`, `examples/Arithmetic.els`,
    `jvm/.../ExamplesIntegrationTest.scala` (drop explicit imports / bare literals). Tests: `ProcessorTest`,
    `MonomorphicTypeCheck(Processor)Test`, `MatchNativesProcessorTest`, `AbilityImplementationCheckProcessorTest`,
    `CoreProcessorTest`, `ValueResolverTest`, `OperatorResolverProcessorTest`, `MatchDesugaringProcessorTest`
    (+ its new `IntValue` matcher), and `ExamplesIntegrationTest` (runtime `7`/`6`/`14`/`-7` from bare literals).
- **Phase 7 — Tests / examples / docs.** Literal typing; range accept/reject; arithmetic bounds; a
  JVM run test; an example `.els`; fold notes back here.

## Decisions & open sub-decisions

- DECIDED: assignability is a user-defined **`Coerce[from, to]`** ability inserted in check mode,
  returning `Option`, discriminated via `fold`. `unify` is pure definitional equality.
- DECIDED: the `Coerce[Int,Int]` instance binds bounds in the **two-parameter instance head**
  (`implement [smin,smax,tmin,tmax] Coerce[Int[smin,smax], Int[tmin,tmax]]`), so no in-body
  `case Int[..]` match and no `typeMatch` on an abstract `type` is required.
- DECIDED: mechanism = the single NbE evaluator running pure Eliot; no separate compile-time interpreter.
- DECIDED: literal typing is a **frontend desugar** (value-position `n` → `integerLiteral[n]`),
  *not* expected-type-directed typing in the Checker; `integerLiteral` is a platform-layered def.
  Monomorphize is untouched. Literal kind is positional (type vs value context), so value-position
  `BigInteger` literals are unsupported (see the tradeoff under "Key insight").
- DECIDED (ordering): the global desugar is the **last** step (Phase 6), not the first. It is the
  most dependent change (a flipped literal needs Phase 3 widening to be usable), so `Int` is built
  and tested against the explicit `integerLiteral[n]` constructor first — see "De-risking."
- DECIDED: `Bool` is an opaque `type Bool` in `lang` with compile-time natives (not a `data` type) —
  its representation is platform-dependent.
- DECIDED (Phase 4): multi-candidate metavariable resolution is a *type-only* by-name **`Combine[A, B]
  { type Combined }`** ability (instance: `Combined = Int[min,max]`), layered over `Coerce` — `Combine`
  picks the meta solution, `Coerce` widens each contributor to it. The trigger is **generic
  meta-solving**, not `handleCases` recognition: `match` branches and `f[A](a: A, b: A): A` are the same
  phenomenon. Implementation = accumulate candidates in `unify`, resolve to their `Combine` at
  `drain()`, keeping `unify` pure equality.
- DECIDED (Phase 4): **per-meta polarity tracking = "taint on contravariant use."** Every implicit
  type-parameter instantiation meta (allocated in `Checker.peelLams`) is registered `combinable`
  (covariant by default); a meta is *un*-registered (tainted) the moment it appears in a `VPi` *domain*
  during unification (`Unifier`'s `VPi`-vs-`VPi` case, via a non-forcing `metasOf` so a meta already
  solved to its first candidate is still tainted). Only still-`combinable` metas are joined; tainted ones
  fall back to strict equality. This catches the `useTwice[A](f: A -> Unit, x: A, y: A)` counterexample
  in any argument order without a full variance analysis. Chosen over full variance analysis (more code,
  trickier over closures) and over "combine all, defer soundness" (knowingly unsound).
- OPEN (validate at Phase 2): **value-constructor read-back.** NbE's `Quoter` quotes
  a value constructor with `valueType = GroundValue.Type` rather than its real data
  type. Harmless while read-back stays internally consistent (a single evaluator), but
  `GroundValue`'s `Eq` is universal (it compares `valueType`), so it is load-bearing
  for a *value embedded in a type* — exactly the `Int[V,V]` singleton case this plan
  introduces. Decide where the data type is threaded (e.g. carry it on the
  constructor's `NativeBinding`) and validate against the literal-typing work.
- NOTE (not a blocker for `Int`): NbE does **not** reduce a `match` whose scrutinee is
  a *symbolic* (unsolved-metavariable) value — such a native stays a neutral and never
  "re-fires" when the meta is later solved. The `Coerce` bounds are concrete at the
  point of insertion, so this does not affect the `Int` work; it is a known limitation
  only for open-term type-level match, deferred until a concrete use appears.
- Out of scope for now: overflow/width enforcement; everything maps to `Long`.

## Representation & promotion (value-level coercion) — DESIGN NOTE

Concern (raised by the author): `Int[MIN,MAX]` is meant to drive the *optimal
backend representation* (`Int[0,10]` -> Byte, `Int[0,1000]` -> Word, ...). If a
branch `Combine`s `Int[0,10]` (Byte) and `Int[0,1000]` (Word) into `Int[0,1000]`
(Word), the Byte arm's runtime value must actually become a Word, or the type
would mislead the backend. So we need a *value-level promotion*. Does that
replace the type-level `Combine`?

**Resolution: no — they are complementary, at different levels.**

- **Promotion is semantically the identity.** Widening `Int[0,10] -> Int[0,1000]`
  preserves the integer value (10 stays 10); only the machine encoding changes
  (zero/sign-extend). So the *type-level* reasoning is sound and complete on its own;
  promotion is purely a backend realization.
- The type-level `Combine` answers "what type covers both branches?" — load-bearing for
  type-checking everything downstream. It stays.
- Promotion answers "how do I physically materialize this value at the target
  type's representation?" — only matters at a representation boundary.

**It is not specific to branches.** A promotion is needed at *every* assignability
boundary where representations differ (a `Coerce` widen succeeding): branch/match arms
vs. their combined result, arg vs. param, body vs. declared return. In the `Coerce`
design the promotion *is* the `nativeWiden` payload spliced at the call site — when
widths exist it becomes a real conversion, today it is identity.

**Invariant that keeps the backend honest:** *after coercion insertion, every value's
physical representation equals `repr(its static type)`.* The backend may assume this;
the inserted widens establish it; nothing changes a type without a matching widen.

**For now:** the backend maps everything to `Long`, so `repr` is constant and
every `nativeWiden` is a **no-op**. Range-based width selection (Byte/Word/...) is a
separate future feature, and promotion is meaningless without it — build them
together, later.

## Risks

- Literal-retyping fallout in existing tests/stdlib — now deferred to the global flip (Phase 6),
  with the `Coerce` widening net already in place.
- Check-mode insertion machinery (Phase 3): the `Option` `fold` natives and the
  value→runtime read-back `Quoter` direction are the genuinely new parts.
- **Termination (becomes load-bearing at Phase 3).** Running user code at compile
  time can loop, and type-level recursion only enters the checker once the
  `Coerce[Int,Int]` instance runs during checking. No guard exists yet (no
  normal program reduces to a non-terminating term in the hot path today). When one
  is needed, build the minimal *on-architecture* version: a step-limit kept **local
  to the `eval` package** that, on exhaustion, returns a **stuck residual** (caught
  inside the evaluator entry, converted to a neutral) rather than throwing — it flows
  through the existing `Quoter`/`PostDrainQuoter` "Cannot resolve type." path, needing
  no `Unifier`/`Checker` fuel field and no `CompilerIO` exception-catch. Do **not**
  build the throw-based ambient counter (threaded through `eval`/`applyValue`/`force`
  + `Unifier` + `Checker` + `Quoter`): it is throwaway under the planned
  recursion-as-effect model, where unbounded recursion simply *gets stuck* (a neutral,
  like any unavailable effect) and bounded recursion carries its **fuel as an
  in-language value** with a type-level bound, terminating by construction. So
  termination is ultimately a typing property + in-language fuel; the `eval`-local
  step-limit is only an interim guard, and "ran out → stuck → cannot resolve" mirrors
  "effect unavailable → stuck," making it a stepping stone rather than a mechanism to
  rip out later.
