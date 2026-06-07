# Plan: Implementing `Int[MIN, MAX]` (dependent integer type)

Status: design / in progress. This document is the durable reference for the
`Int[MIN, MAX]` work so it survives across sessions.

## Goal & chosen model

Make `Int[MIN, MAX]` a real, usable dependent integer type:

- Integer literals carry their value as a singleton type `Int[V, V]` (wiring up
  the currently-unused `Runtime.els` alias `IntegerLiteralType[V] = Int[V, V]`)
  instead of being hardcoded to `BigInteger`
  (`monomorphize/check/Checker.scala`, the `IntegerLiteral` case).
- Range compatibility is decided by a **user-space ability** (working name
  `Category`, see "Ability design" below): when the checker compares two types
  built from the same type constructor and that constructor implements the
  ability, it *runs* the ability to decide compatibility / combine the types;
  otherwise it falls back to today's structural data equality.
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
   `DataTypeNativesProcessor`, `UserValueNativesProcessor`). A body-less `def`
   becomes a `VTopDef(fqn, None, SNil)` (stuck) unless a processor binds a
   `VNative` reducer. Example: `inc` fires `VConst(Direct(n)) -> Direct(n+1)`.
   The NbE evaluator/unifier are **pure**; all `NativeBinding`s must be
   pre-fetched before evaluation.
2. **JVM runtime** via `NativeImplementation.implementations: Map[ValueFQN, ...]`,
   consumed in `JvmClassGenerator.createModuleMethod`. Emits ASM bytecode.

Both are keyed by the same `ValueFQN`.

## Ability design: `TypeRefinement` (DECIDED: name + operations)

The ability captures, for a refinement type whose values denote *sets* of
concrete values (e.g. `Int[a,b]` denotes the integers in `[a,b]`), how the
type checker should compare and combine those types. Deliberately named in
plain, non-mathematical terms (same spirit as calling typeclasses "ability").

**Ability:** `TypeRefinement[T]`. **Operations (two):**

- `assignableFrom(target: T, source: T): Bool` -- directional, reads like Java's
  `isAssignableFrom`: true iff a value of `source` may be used where `target` is
  expected. Used for **assignability / subsumption** at every
  `check(actual, expected)`. For Int:
  `assignableFrom(Int[c,d], Int[a,b]) = (c <= a) && (b <= d)`
  (target `[c,d]` accepts source `[a,b]` iff `[a,b]` is inside `[c,d]`).
- `combine(a: T, b: T): T` -- the join / least upper bound. Used to **synthesise**
  one type from several when there is no expected type to check against:
  branches of `if` / `match`, etc. For Int:
  `combine(Int[a,b], Int[c,d]) = Int[min(a,c), max(b,d)]`.

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
  equality (current behaviour).

### OPEN design question: parameterization & dispatch

`Int` is a type *constructor* (`Int[MIN, MAX]`); the ability's methods operate on
*applied* types (`Int[a,b]`, which have kind `Type`, cf. `TypeValues.els` where
`def personName(t: Type)` matches `case Person[name]`). Need to settle how
`TypeRefinement` is parameterized and dispatched: implemented for the
constructor `Int`, with methods receiving the applied `Int[...]` types, and the
checker dispatching on the head constructor it sees during unification. Proposed
shape (to confirm):

```
ability TypeRefinement[T] {
   def assignableFrom(target: T, source: T): Bool
   def combine(a: T, b: T): T
}

implement TypeRefinement[Int] {
   def assignableFrom(target: Int, source: Int): Bool = target match {
      case Int[tmin, tmax] -> source match {
         case Int[smin, smax] -> tmin <= smin && smax <= tmax
      }
   }
   def combine(a: Int, b: Int): Int = a match {
      case Int[amin, amax] -> b match {
         case Int[bmin, bmax] -> Int[min(amin, bmin), max(amax, bmax)]
      }
   }
}
```

### Checker integration (pre-fetch then pure)

The `Checker` already has a `resolveAbility` callback
(`getFact(AbilityImplementation.Key(...))`) and the pure `Unifier` compares
types via `groundEquals` on `Structure(name, args)`. Plan: pre-resolve the
ability impls for the type constructors that appear, inject a
`typeFQN -> {assignableFrom, combine} SemValue` map into the `Unifier` (same
pattern as `NativeBinding` pre-fetching), and at the `VConst` vs `VConst` /
`groundEquals(Structure, Structure)` site:

- if the head has the ability AND both sides have fully-ground bounds, run
  `assignableFrom(target = expected, source = inferred)` and accept on `True` /
  reject on `False`;
- otherwise fall back to structural equality (also the path when bounds are
  metas/generics, so unification still solves `MIN := a, MAX := b`).

Direction matters: target = expected, source = inferred -- verify the
`unify(l, r)` argument order at the `check` call sites so we accept *widening*,
not narrowing. The `combine` op plugs into branch/match result-type synthesis.

## Phases

- **Phase 0 -- Foundations.** `Bool` (`data Bool = False | True`) + two-arg
  compile-time `VNative` reductions on `BigInteger` (`+`, `-`, `*`,
  `lessThanOrEqual`/`<=`, `&&` on Bool, plus `min`/`max` for `combine`) in
  `StdlibNativesProcessor`; body-less decls in `BigInteger.els` / `Bool.els`.
- **Phase 1 -- Literals as `Int[V,V]`.** Expected-type-directed literal typing
  in `Checker`. Audit monomorphize tests assuming `BigInteger` in value position.
- **Phase 2 -- `TypeRefinement` + range-checked compatibility.** Define
  `ability TypeRefinement[T]` and `implement TypeRefinement[Int]` in stdlib
  (pattern-match on `Int[a,b]` like `TypeValues.els`); hook
  `assignableFrom`/`combine` into the checker/unifier as above. Start with a
  short feasibility spike (pure evaluation of the ability inside the Unifier;
  ability impls resolving for a *type constructor*; the parameterization/dispatch
  question above).
- **Phase 3 -- Runtime arithmetic (JVM).** Infix `+`/`-`/`*` on `Int` with
  dependent bounds (`Int[a,b] + Int[c,d] : Int[a+c, b+d]`); `NativeImplementation`
  emitting `LADD`/`LSUB`/`LMUL` (unbox Long -> op -> rebox). Runtime stays `Long`.
- **Phase 4 -- Tests / examples / docs.** Literal typing; range accept/reject;
  ability fallback to equality; arithmetic bounds; a JVM run test; an example
  `.els`; a design note.

## Decisions & open sub-decisions

- DECIDED: ability `TypeRefinement[T]`; ops `assignableFrom(target, source): Bool`
  and `combine(a, b): T`. `meet`/intersection deferred.
- OPEN: parameterization & dispatch of `TypeRefinement` over a type *constructor*
  whose methods receive *applied* types (see Ability design section).
- OPEN: `Bool` as pure stdlib data type (recommended) vs native.
- Out of scope for now: overflow/width enforcement; everything maps to `Long`.

## Risks

- Phase 2 purity: the Unifier is synchronous/pure -> all ability impls must be
  pre-fetched; missing impls would wrongly fall back to equality.
- Literal-retyping fallout in existing tests/stdlib (Phase 1).
