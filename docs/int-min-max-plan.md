# Plan: Implementing `Int[MIN, MAX]` (dependent integer type)

Status: design settled; foundational machinery in place. The frontier is Phase 3 (the
check-mode `Coerce` insertion). Literal typing as `Int[V,V]` is built and tested via an
**explicit** `integerLiteral[n]` constructor (Phase 2) and only flipped to the global
default literal type at the **end** (Phase 6), once the supporting machinery works — see
"De-risking: explicit constructor first, global flip last." This document is the durable
cross-session reference.

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
join, arithmetic) against the **explicit** `integerLiteral[n]` constructor — which already
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
- **`Coerce` / `Option` declared.** `ability Coerce[from, to] { def coerce(value: from): Option[to] }`
  (`Coerce.els`) and the abstract `Option.els` are in place; `WellKnownTypes.coerceFQN`
  exists. The check-mode *insertion* does not yet (Phase 3).
- **`Int.els`** declares the abstract `type Int[MIN, MAX]`, the width aliases
  (`Byte`/`Short`/`Long`/…), and `+` with dependent bounds
  (`Int[m1,M1] + Int[m2,M2] : Int[add(m1,m2), add(M1,M2)]`). No `Coerce[Int,Int]` instance yet.

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

### Work items

- `Int.els`: add the parametric `Coerce[Int,Int]` instance (head-bound bounds, as above) + the
  native widen.
- `check/TypeStackLoop.scala`: add the leaf check-mode coercion hook above. It discriminates the
  `Option` result through the abstract `fold` eliminator, **not** a constructor match.
- Evaluator: give `Option`'s `fold` (and the `Some`/`None` constructors) compiler-side native
  reductions (`fold(Some p,…) ⤳ s p`, `fold(None,…) ⤳ n`) so the discrimination forces at compile
  time. `Option` stays an opaque platform `type`; reuse the existing `NativeFunction`
  compiler-as-platform path rather than introducing a `data Option`.
- Value→runtime-expression read-back — a **new** `Quoter` direction. Today's `Quoter` *rejects*
  neutrals; the residual `nativeWiden · x` neutral must read back to a
  `MonomorphicExpression.FunctionApplication`, treating the stuck native/var as the *desired* output,
  not an error. Not a reuse of `Quoter.quote` as-is.
- Coercion-insertion test: `Int[0,5]` into `Int[0,10]` succeeds and inserts the widen; into `Int[0,3]`
  is a type error.

Acceptance: implicit widening flows through a user-defined stdlib `Coerce` instance inserted in check
mode; `Int[0,5] → Int[0,10]` succeeds while `Int[0,5] → Int[0,3]` is a compile error.

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
  - This is the safe test harness for Phases 3–5: widening/join/arithmetic are all exercised with
    explicit `integerLiteral[n]` literals. No `BigInteger`-value-position fallout until Phase 6.
  - `integerLiteral` is a **platform-layered def** (layers cornerstone, like `println`/`IO`): the
    abstract decl suffices for type-checking now; the jvm-layer body that emits the right-sized
    integer lands with the arithmetic in Phase 5 (a trivial body earlier is fine). A compile-time
    `NativeBinding` is only needed once constant folding of runtime ints is wanted (Phase 5+), and
    even then it is *data* for the existing evaluator, not a code change.
- **Phase 3 — `Coerce[Int,Int]` instance + check-mode insertion (range subsumption). (FRONTIER.)**
  Write the parametric `Coerce` instance in `Int.els` and add the leaf check-mode hook in
  `TypeStackLoop` — see "Check-mode `Coerce` insertion" for the full design and work items. The
  genuinely new machinery is the `Option` `fold`/`Some`/`None` native reductions and the
  value→runtime-expression read-back `Quoter` direction. Verify widening accepted / narrowing
  rejected end to end.
- **Phase 4 — branch/match result synthesis (the join / LUB).** When there is no expected type to
  check against (branches of `if` / `match`), the checker must synthesise one type from several:
  `join(Int[a,b], Int[c,d]) = Int[min(a,c), max(b,d)]`. Decide its ability home — likely another
  by-name ability with a structured instance head (same pattern as `Coerce`), so it also avoids any
  in-body `case Int[..]` match. `min`/`max` are ordinary Eliot (now runnable). Wire into branch/match
  result-type synthesis.
- **Phase 5 — Runtime arithmetic.** `+`/`-`/`*` on `Int` with dependent bounds
  (`Int[a,b] + Int[c,d] : Int[a+c, b+d]`). Two execution targets: (a) a
  **compile-time `NativeBinding`** (for type-level bound computation), and (b) the
  **JVM** `NativeImplementation` (`LADD`/`LSUB`/`LMUL`, unbox Long -> op -> rebox)
  for runtime. Runtime stays `Long`. `Int.els` already declares `+` with the
  dependent-bounds signature; back it with both natives.
- **Phase 6 — Flip literals to `Int[V,V]` (the global switchover; monomorphize untouched).** Now
  that widening/join/arithmetic work and are tested, make `Int[V,V]` the default literal type:
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
- OPEN (Phase 4): the ability home and shape of the branch/match join (`Int[min,max]`).
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
branch joins `Int[0,10]` (Byte) and `Int[0,1000]` (Word) to `Int[0,1000]`
(Word), the Byte arm's runtime value must actually become a Word, or the type
would mislead the backend. So we need a *value-level promotion*. Does that
replace the type-level join?

**Resolution: no — they are complementary, at different levels.**

- **Promotion is semantically the identity.** Widening `Int[0,10] -> Int[0,1000]`
  preserves the integer value (10 stays 10); only the machine encoding changes
  (zero/sign-extend). So the *type-level* reasoning is sound and complete on its own;
  promotion is purely a backend realization.
- The type-level join answers "what type does this branch have?" — load-bearing for
  type-checking everything downstream. It stays.
- Promotion answers "how do I physically materialize this value at the target
  type's representation?" — only matters at a representation boundary.

**It is not specific to branches.** A promotion is needed at *every* assignability
boundary where representations differ (a `Coerce` widen succeeding): branch/match arms
vs. their joined result, arg vs. param, body vs. declared return. In the `Coerce`
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
