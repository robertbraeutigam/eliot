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

## Ability design (the `Category` / lattice ability) -- UNDER DISCUSSION

The ability captures, for a refinement type whose values denote *sets* of
concrete values (e.g. `Int[a,b]` denotes the integers in `[a,b]`), how the
type checker should compare and combine those types. Required operations:

- `subsetOf(sub: T, sup: T): Bool` -- the partial order (set inclusion). Used
  for **assignability / subsumption**: every `check(actual, expected)`. For Int:
  `subsetOf(Int[a,b], Int[c,d]) = (c <= a) && (b <= d)`.
- `union(a: T, b: T): T` -- the join / least upper bound. Used to **synthesise**
  one type from several when there is no expected type to check against:
  branches of `if` / `match`, etc. For Int:
  `union(Int[a,b], Int[c,d]) = Int[min(a,c), max(b,d)]`.

Notes:

- `subsetOf` is derivable from `union` + existing data equality:
  `subsetOf(a, b) == (union(a, b) === b)`. `union` is the fundamental op; we may
  still expose `subsetOf` explicitly (clarity / cheaper / possible default impl).
- A **meet / intersection** (`Int[max(a,c), min(b,d)]`) would be needed only if
  the checker ever has to combine multiple *upper-bound* expectations on one
  unknown; it introduces empty/uninhabited ranges (a type error) and a bottom
  representation, so it is **deferred** until a concrete need appears.
- Law (not necessarily enforced): `union` is the LUB w.r.t. `subsetOf`.
- Fallback: a type with no such ability uses plain structural data equality
  (current behaviour).

Name is not yet settled (candidates: `Lattice`, `Refinement`, `Subset`/set-themed,
...). `Category` clashes with category theory and is a placeholder.

### Checker integration (pre-fetch then pure)

The `Checker` already has a `resolveAbility` callback
(`getFact(AbilityImplementation.Key(...))`) and the pure `Unifier` compares
types via `groundEquals` on `Structure(name, args)`. Plan: pre-resolve the
ability impls for the type constructors that appear, inject a
`typeFQN -> {subsetOf, union} SemValue` map into the `Unifier` (same pattern as
`NativeBinding` pre-fetching), and at the `VConst` vs `VConst` /
`groundEquals(Structure, Structure)` site:

- if the head has the ability AND both sides have fully-ground bounds, run
  `subsetOf(actual, expected)` and accept on `True` / reject on `False`;
- otherwise fall back to structural equality (also the path when bounds are
  metas/generics, so unification still solves `MIN := a, MAX := b`).

Direction matters: `subsetOf(actual = inferred, sup = expected)` -- verify the
`unify(l, r)` argument order at the `check` call sites so we accept *widening*,
not narrowing. The `union` op plugs into branch/match result-type synthesis.

## Phases

- **Phase 0 -- Foundations.** `Bool` (`data Bool = False | True`) + two-arg
  compile-time `VNative` reductions on `BigInteger` (`+`, `-`, `*`,
  `lessThanOrEqual`, plus `min`/`max` for `union`) in `StdlibNativesProcessor`;
  body-less decls in `BigInteger.els` / `Bool.els`.
- **Phase 1 -- Literals as `Int[V,V]`.** Expected-type-directed literal typing
  in `Checker`. Audit monomorphize tests assuming `BigInteger` in value position.
- **Phase 2 -- The ability + range-checked compatibility.** Define the ability
  and `implement ...[Int]` in stdlib (pattern-match on `Int[a,b]` like
  `TypeValues.els`); hook `subsetOf`/`union` into the checker/unifier as above.
  Start with a short feasibility spike (pure evaluation of the ability inside the
  Unifier; ability impls resolving for a *type constructor*).
- **Phase 3 -- Runtime arithmetic (JVM).** Infix `+`/`-`/`*` on `Int` with
  dependent bounds (`Int[a,b] + Int[c,d] : Int[a+c, b+d]`); `NativeImplementation`
  emitting `LADD`/`LSUB`/`LMUL` (unbox Long -> op -> rebox). Runtime stays `Long`.
- **Phase 4 -- Tests / examples / docs.** Literal typing; range accept/reject;
  ability fallback to equality; arithmetic bounds; a JVM run test; an example
  `.els`; a design note.

## Open sub-decisions

1. Ability name (see above).
2. Exact operation set (`subsetOf` + `union`; `subsetOf` primitive vs derived;
   `meet` deferred?).
3. `Bool` as pure stdlib data type (recommended) vs native.
4. Out of scope for now: overflow/width enforcement; everything maps to `Long`.

## Risks

- Phase 2 purity: the Unifier is synchronous/pure -> all ability impls must be
  pre-fetched; missing impls would wrongly fall back to equality.
- Literal-retyping fallout in existing tests/stdlib (Phase 1).
