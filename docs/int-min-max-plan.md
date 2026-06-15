# Plan: Implementing `Int[MIN, MAX]` (dependent integer type)

Status: design settled; foundational machinery in place; the real `Int` use is
the current frontier. This document is the durable reference for the
`Int[MIN, MAX]` work so it survives across sessions.

> **Superseded mechanism note (2026-06-15).** The assignability mechanism described
> throughout this doc as the **`TypeRefinement` ability + a `refinements` hook in the
> `Unifier`** has been **removed**. Per `docs/cornerstone-fidelity-plan.md` Phase 2,
> `unify` is now pure definitional equality, and directional Int widening moves to a
> user-defined **`Coerce` ability inserted in the checker's check mode** (returning
> `Option`, discriminated via `fold`). So wherever this doc says "run `assignableFrom`
> in `unify`," read "evaluate `Coerce.coerce` in check mode and splice the residual
> widen." The `Int`-specific prerequisites below (TypeMatch-for-abstract-`type` to read
> bounds, literal-typing-as-`Int[V,V]`, the native widen) are unchanged and still the
> real work; only the assignability *seam* changed. The `Coerce`/`Option` declarations
> and `WellKnownTypes.coerceFQN` already exist; the check-mode insertion does not yet.

## Current status (2026-06-11)

- **Design settled.** Literal typing as `Int[V,V]`; the `TypeRefinement` ability
  (`assignableFrom` now, `combine` later); representation/promotion as a deferred,
  identity-for-now coercion seam. See sections below.
- **Mechanism settled and built — *not* a separate backend.** The earlier plan
  routed type-level evaluation through a fact-bounded `interpret` backend
  (`EvaluatedValue`). That backend was **removed**: the NbE evaluator now reduces
  `match` on a concrete scrutinee directly, so pure type-level code runs inside the
  checker's own evaluator. The `TypeRefinement` hook is wired into the `Unifier`.
  This supersedes the old "CRITICAL FINDING / eval backend" framing entirely. (The
  evaluator rework was tracked separately as the now-complete "NbE total evaluation"
  effort — match-as-`VNative` for concrete scrutinees, removal of the `interpret`
  package, and the assignability hook are all done. Its residual deferrals that bear
  on the `Int` work — a termination guard and value-constructor read-back — are
  folded into "Risks" and "Decisions" below.)
- **What exists today (the foundation, from nbe P6):**
  - `Bool` — opaque `type Bool` in `lang` with compile-time natives
    `true`/`false`/`&&` (`SystemNativesProcessor`) and `lessThanOrEqual` (on
    `BigInteger`). Each reduces only when its arguments are concrete, otherwise
    stays stuck so unification still solves metavariables.
  - `ability TypeRefinement[T] { def assignableFrom(target: Type, source: Type): Bool }`
    in `lang` — single op, no default body. A type constructor with **no** impl is
    compared structurally by the unifier (the pre-refinement default).
  - The `Unifier` carries `refinements: Map[ValueFQN, SemValue]` and, at a
    same-head `VTopDef` comparison, runs the impl through the pure evaluator: a
    concrete `Bool` accepts/rejects; anything stuck falls back to `unifySpines`
    (so meta-solving is preserved). `MonomorphicTypeCheckProcessor` builds the map.
  - `Int.els` declares the abstract `type Int[MIN, MAX]`, the width aliases
    (`Byte`/`Short`/`Long`/…), and `+` with dependent bounds
    (`Int[m1,M1] + Int[m2,M2] : Int[add(m1,m2), add(M1,M2)]`). There is **no**
    `implement TypeRefinement[Int]` yet — it is blocked (see Phases).

## Goal & chosen model

Make `Int[MIN, MAX]` a real, usable dependent integer type:

- Integer literals carry their value as a singleton type `Int[V, V]` (wiring up
  the currently-unused `Runtime.els` alias `IntegerLiteralType[V] = Int[V, V]`)
  instead of being hardcoded to `BigInteger`
  (`monomorphize/check/Checker.scala`, the `IntegerLiteral` case).
- Range compatibility is decided by the user-space `TypeRefinement` ability: when
  the checker compares two types built from the same type constructor and that
  constructor implements the ability, it *runs* the ability to decide
  compatibility / combine the types; otherwise it falls back to today's structural
  data equality. **This hook is built (nbe P6); it works for `data` types today.**
- `+` / `-` / `*` (and comparisons) become natives with type-level bounds
  propagation **and** JVM runtime bytecode, so real programs compute.

### Key insight that keeps existing tests working

Literal typing becomes **expected-type-directed**:

- Checked against `BigInteger` (a type-level *bound* position, e.g. `Box[one]`,
  `def one: BigInteger = 1`) -> stays `Direct(v, BigInteger)`. Existing behaviour
  preserved.
- Checked against an `Int[...]` or a metavariable (a runtime *value* position)
  -> becomes `Int[v, v]`, then unified (which triggers the ability's range check
  when bounds are concrete).

So `BigInteger` stays the type of the *bounds*; `Int[MIN, MAX]` is the type of
*runtime integers*. No conflation, minimal breakage.

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

(The former third mechanism — a fact-bounded `interpret` backend / `EvaluatedValue`
— was removed; closed type-level terms now reduce inside the NbE evaluator itself.
`MatchNativesProcessor` bakes the `handleCases`/`typeMatch` reducers that make this
work; the checker pre-fetches all `NativeBinding`s and evaluates purely.)

## Ability design: `TypeRefinement` (name + operations)

The ability captures, for a refinement type whose values denote *sets* of
concrete values (e.g. `Int[a,b]` denotes the integers in `[a,b]`), how the
type checker should compare and combine those types. Deliberately named in
plain, non-mathematical terms (same spirit as calling typeclasses "ability").

**Ability:** `TypeRefinement[T]`. **Operations:**

- `assignableFrom(target: Type, source: Type): Bool` — directional, reads like
  Java's `isAssignableFrom`: true iff a value of `source` may be used where
  `target` is expected. Used for **assignability / subsumption** at every
  `check(actual, expected)`. **Built (nbe P6).** For Int:
  `assignableFrom(Int[c,d], Int[a,b]) = (c <= a) && (b <= d)`
  (target `[c,d]` accepts source `[a,b]` iff `[a,b]` is inside `[c,d]`).
- `combine(a: Type, b: Type): Type` — the join / least upper bound. Used to
  **synthesise** one type from several when there is no expected type to check
  against: branches of `if` / `match`, etc. **Not yet added to the ability.** For
  Int: `combine(Int[a,b], Int[c,d]) = Int[min(a,c), max(b,d)]`.

Notes:

- `assignableFrom` is derivable from `combine` + existing data equality:
  `assignableFrom(target, source) == (combine(target, source) === target)`.
  `combine` is the fundamental op; we still expose `assignableFrom` explicitly
  (the assignability path is hot and reads clearer; possible default impl later).
- A **meet / intersection** (`Int[max(a,c), min(b,d)]`) would be needed only if
  the checker ever has to combine multiple *upper-bound* expectations on one
  unknown; it introduces empty/uninhabited ranges (a type error) and a bottom
  representation, so it is **deferred** until a concrete need appears.
- Law (not necessarily enforced): `combine` is the LUB w.r.t. `assignableFrom`.
- Fallback: a type with no `TypeRefinement` impl uses plain structural data
  equality (current behaviour, decided directly by the unifier).

### Parameterization & dispatch

`Int` is a type *constructor* (`Int[MIN, MAX]`); the ability's methods must
operate on *applied* types (`Int[a,b]`), and `assignableFrom`/`combine` take
**two independent applications** of the same constructor (`Int[c,d]` vs
`Int[a,b]`) — so a fully-applied impl like `implement[a,b] TypeRefinement[Int[a,b]]`
can't work (it would force both arguments to share one pair of bounds).

Resolution — combine two existing, tested features:

1. **Higher-kinded ability dispatched on the bare constructor.** Exactly like the
   tested `ability Container[F[_]]` + `implement Container[Box]`. So
   `TypeRefinement` dispatches on the constructor `Int`.
2. **Methods typed over `Type`, bodies pattern-match the constructor.** The
   `def personName(t: Type) = t match { case Person[name] -> ... }` idiom from
   `TypeValues.els`. Each argument is matched independently, giving independent
   bounds.

Resolved shape:

```
ability TypeRefinement[T] {                          // T = the refined constructor (HKT dispatch key)
   def assignableFrom(target: Type, source: Type): Bool
   def combine(a: Type, b: Type): Type               // not yet added
}

implement TypeRefinement[Int] {                      // dispatch on the Int constructor
   def assignableFrom(target: Type, source: Type): Bool = target match {
      case Int[tmin, tmax] -> source match {
         case Int[smin, smax] -> tmin <= smin && smax <= tmax
      }
   }
   def combine(a: Type, b: Type): Type = a match {
      case Int[amin, amax] -> b match {
         case Int[bmin, bmax] -> Int[min(amin, bmin), max(amax, bmax)]
      }
   }
}
```

The checker invokes these methods *itself* during unification (it already knows
the head constructor), so dispatch is checker-driven via the
`refinements` map keyed on the type-constructor FQN rather than normal
argument-type inference.

### Checker integration — DONE (nbe P6)

The hook runs `assignableFrom` through the **NbE evaluator** (not a separate
backend). At a same-head type comparison (`VTopDef` vs `VTopDef` with the same
FQN) where that constructor has a `TypeRefinement` impl, the `Unifier` evaluates
`force(applyValue(applyValue(impl, expected), actual))`:

- A concrete `Bool` `Direct(true)`/`Direct(false)` accepts/rejects.
- Anything else (stuck on metavariable bounds) falls back to `unifySpines`,
  preserving meta-solving (so unification still solves `MIN := a, MAX := b`).

Direction matters: `assignableFrom(target = expected, source = inferred)` so we
accept *widening*, not narrowing — verify the `unify(l, r)` argument order at the
`check` call sites. `combine` (once added) plugs into branch/match result-type
synthesis the same way.

## Phases

The prerequisite evaluation machinery (nbe P1–P6) is **done**. The phases below
are the `Int`-specific work that sits on top.

- **Phase 0 — Foundations. DONE (built differently than first drafted).** `Bool`
  is an **opaque `type Bool`** in `lang` with compile-time natives
  `true`/`false`/`&&` + `lessThanOrEqual(BigInteger, BigInteger): Bool`, *not* a
  `data Bool = False | True`. (Opaque keeps the representation platform-dependent;
  the JVM backend maps it to a platform boolean.) `TypeRefinement[T]` +
  `assignableFrom` defined and the checker hook wired. `min`/`max` are not yet
  needed (no `combine`).
- **Phase 1 — `TypeMatch` for abstract `type` constructors. (BLOCKER — do first.)**
  Extracting bounds via `case Int[tmin, tmax]` requires a `TypeMatch` ability impl
  for `Int`, but `DataDefinitionDesugarer` generates `TypeMatch`/`handleCases`
  impls only for `data` definitions *with constructors* — an abstract
  `type Int[MIN, MAX]` gets none. Until this exists, `implement TypeRefinement[Int]`
  cannot even be written. Teach the desugarer (or a sibling) to emit a `typeMatch`
  impl for a constructor-less `type C[...]` that destructures its type arguments so
  `case Int[tmin, tmax] -> …` binds `tmin`/`tmax`. (Note already recorded in
  `Int.els`.)
- **Phase 2 — Literals as `Int[V,V]`.** Expected-type-directed literal typing in
  `Checker` (see "Key insight"). Audit monomorphize tests assuming `BigInteger` in
  value position. Needed to actually *produce* `Int` values.
- **Phase 3 — `implement TypeRefinement[Int]` (range subsumption).** With Phase 1
  in place, write the pure-Eliot impl shown above and add it to `Int.els`. The
  checker hook already runs it. Verify widening accepted / narrowing rejected end
  to end.
- **Phase 4 — `combine` + branch/match synthesis.** Add `combine` to the ability,
  implement for `Int` (`min`/`max` as ordinary Eliot, now runnable), and wire it
  into branch/match result-type synthesis in the checker.
- **Phase 5 — Runtime arithmetic.** `+`/`-`/`*` on `Int` with dependent bounds
  (`Int[a,b] + Int[c,d] : Int[a+c, b+d]`). Two execution targets: (a) a
  **compile-time `NativeBinding`** (for type-level bound computation), and (b) the
  **JVM** `NativeImplementation` (`LADD`/`LSUB`/`LMUL`, unbox Long -> op -> rebox)
  for runtime. Runtime stays `Long`. `Int.els` already declares `+` with the
  dependent-bounds signature; back it with both natives.
- **Phase 6 — Tests / examples / docs.** Literal typing; range accept/reject;
  `TypeRefinement` fallback to equality; arithmetic bounds; a JVM run test; an
  example `.els`; fold notes back here.

## Decisions & open sub-decisions

- DECIDED: ability `TypeRefinement[T]`; op `assignableFrom(target, source): Bool`
  built; `combine(a, b): Type` planned. `meet`/intersection deferred.
- DECIDED: mechanism = the NbE evaluator's `TypeRefinement` hook (the `interpret`
  eval backend was removed). `TypeRefinement[Int]` is written in **pure Eliot**.
- DECIDED: `Bool` is an opaque `type Bool` in `lang` with compile-time natives
  (not a `data` type) — its representation is platform-dependent.
- OPEN: how the `TypeMatch`-for-abstract-`type` desugaring binds type arguments
  (Phase 1) — the shape of the synthesised `typeMatch` impl for a constructor-less
  `type`.
- OPEN (validate at Phase 2): **value-constructor read-back.** NbE's `Quoter` quotes
  a value constructor with `valueType = GroundValue.Type` rather than its real data
  type. Harmless while read-back stays internally consistent (a single evaluator), but
  `GroundValue`'s `Eq` is universal (it compares `valueType`), so it is load-bearing
  for a *value embedded in a type* — exactly the `Int[V,V]` singleton case this plan
  introduces. Decide where the data type is threaded (e.g. carry it on the
  constructor's `NativeBinding`) and validate against the literal-typing work.
- NOTE (not a blocker for `Int`): NbE does **not** reduce a `match` whose scrutinee is
  a *symbolic* (unsolved-metavariable) value — such a native stays a neutral and never
  "re-fires" when the meta is later solved. `assignableFrom`'s scrutinees are concrete
  bounds, so this does not affect the `Int` work; it is a known limitation only for
  open-term type-level match, deferred until a concrete use appears.
- Out of scope for now: overflow/width enforcement; everything maps to `Long`.

## Representation & promotion (value-level coercion) — DESIGN NOTE

Concern (raised by the author): `Int[MIN,MAX]` is meant to drive the *optimal
backend representation* (`Int[0,10]` -> Byte, `Int[0,1000]` -> Word, ...). If a
branch `combine`s `Int[0,10]` (Byte) and `Int[0,1000]` (Word) to `Int[0,1000]`
(Word), the Byte arm's runtime value must actually become a Word, or the type
would mislead the backend. So we need a *value-level promotion*. Does that
replace the type-level `combine`?

**Resolution: no — they are complementary, at different levels.**

- **Promotion is semantically the identity.** Widening `Int[0,10] -> Int[0,1000]`
  preserves the integer value (10 stays 10); only the machine encoding changes
  (zero/sign-extend). So the *type-level* reasoning (`combine`/`assignableFrom`)
  is sound and complete on its own; promotion is purely a backend realization.
- `combine` answers "what type does this branch have?" — load-bearing for
  type-checking everything downstream. It stays.
- Promotion answers "how do I physically materialize this value at the target
  type's representation?" — only matters at a representation boundary.

**It is not specific to branches.** A promotion is needed at *every* assignability
boundary where representations differ (`assignableFrom` succeeding by widening):
branch/match arms vs. their `combine`d result, arg vs. param, body vs. declared
return. So it is a **systematic coercion-insertion pass**: walk the typed
program; wherever an expression of static type `S` sits in a context wanting `T`
with `repr(S) != repr(T)`, wrap it in `promote_{S->T}`. Driven by a
`repr(type) -> machine type` function; branches get `T` from `combine`, others
from the expected type.

**Invariant that keeps the backend honest:** *after the coercion pass, every
value's physical representation equals `repr(its static type)`.* The backend may
assume this; the pass establishes it; nothing changes a type without a matching
`promote` inserted.

**For now:** the backend maps everything to `Long`, so `repr` is constant and
every promotion is a **no-op**. Range-based width selection (Byte/Word/...) is a
separate future feature, and promotion is meaningless without it — build them
together, later. The type-level `TypeRefinement` design stands unchanged now; we
only **reserve the seam** (a coercion pass slot between type-checking and
codegen, identity until widths land).

**Open future choice:** where `repr` + `promote` live once widths exist —
(1) a separate backend-facing concern (`repr` fn + `promote` native), keeping
`TypeRefinement` purely type-level (current lean); or (2) extra value-level
methods on `TypeRefinement` so user-defined refinement types also define their
own representation/widening (more unified, heavier). Either way `combine`
survives.

## Risks

- **Phase 1 is the real unknown.** Synthesising a `TypeMatch` impl for an abstract
  `type` constructor is new ground; everything downstream depends on it.
- Literal-retyping fallout in existing tests/stdlib (Phase 2).
- **Termination (becomes load-bearing at Phase 3).** Running user code at compile
  time can loop, and type-level recursion only enters the checker once
  `implement TypeRefinement[Int]` runs during unification. No guard exists yet (no
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
