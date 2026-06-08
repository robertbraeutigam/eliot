# Plan: Implementing `Int[MIN, MAX]` (dependent integer type)

Status: design settled; implementation under way. This document is the durable
reference for the `Int[MIN, MAX]` work so it survives across sessions. The
companion doc `path-a-compile-time-ability-eval.md` holds the detailed plan +
progress for the evaluation backend this feature now depends on.

## Current status (2026-06-08)

- **Design settled.** Literal typing as `Int[V,V]`; the `TypeRefinement` ability
  (`assignableFrom` + `combine`); representation/promotion as a deferred,
  identity-for-now coercion seam. See sections below.
- **Mechanism settled.** See the CRITICAL FINDING below: `match` does not reduce
  at the NbE type level, so `TypeRefinement` cannot be "run" by the symbolic
  checker directly. **Resolved by building a compiler-internal evaluation
  backend** (Path A) that compiles-and-runs pure type-level code behind a fact
  boundary (`EvaluatedValue`). Detailed plan + phasing: `path-a-...md`.
- **Progress.** P1 of the eval backend is **DONE** (merged to master,
  `5f4e9b3a`): the `EvaluatedValue` fact + `EvaluationProcessor` interpret the
  uncurried IR for the pure subset (`lang/.../interpret/`). Next is P2 (the
  `handleCases`/`typeMatch` backend natives).
- **Heads-up / to reconcile:** master commit `b90dcf8a "Add + to Int"` added a
  `+` declaration to `stdlib/.../Int.els` (author's work). Reconcile this with
  the arithmetic plan (Phase 3 / eval-backend `+` native) when we get there --
  see "Native arithmetic" note under Phases.

## CRITICAL FINDING (drove the mechanism change)

Empirically verified (throwaway probe tests in `MonomorphicTypeCheckTest`, since
reverted): **`match` does NOT reduce at the NbE type level.** Neither a data
match (`PatternMatch.handleCases`) nor a type match (`TypeMatch.typeMatch`)
reduces during type checking. A function whose body is a `match` stays stuck
when evaluated in a type position (e.g. `Box[pick(True)]` does not reduce to
`Box[1]`); the probe failed with `"Ability not found"` + `"Type mismatch"`.

Root cause: the type checker (`TypeStackLoop.drainAndResolveLoop`) only resolves
*which* ability impl a call dispatches to (for codegen + associated types); it
never *executes* the call. `match` desugars to the `PatternMatch`/`TypeMatch`
ability calls, whose per-type impls are **JVM-native** (instanceof dispatch in
bytecode) with no NbE-reducible body, and the pure `Evaluator` cannot resolve or
run abilities. Only true natives with a `VNative` reducer (e.g. `inc`) reduce at
the type level.

**Resolution (DECIDED): a fact-bounded, compiler-internal evaluation backend.**
Rather than special-casing `match` in the checker, evaluate pure type-level code
by **compiling-and-running it** through a dedicated backend, recursively to a
fixpoint, behind the `EvaluatedValue` fact. No special handling in the checker:
by the uncurried-IR stage dispatch is already resolved, so the backend just runs
the resolved `handleCases`/`typeMatch` impls as ordinary backend natives (like
the JVM backend runs `println`). Hybrid with NbE: the backend evaluates *closed*
pure terms; NbE keeps unification + open/generic reasoning. Full rationale,
architecture, and phasing live in `path-a-compile-time-ability-eval.md`.

Consequence for this doc: the type-level design below (literal typing, the
`TypeRefinement` interface, representation/promotion) is unchanged; only the
**checker integration** changes -- `assignableFrom`/`combine` are *run* via an
`EvaluatedValue` request rather than reduced inside the pure Unifier (see the
updated "Checker integration" section).

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
3. **Compile-time execution backend** (NEW, P1 done) via the `EvaluatedValue`
   fact + `EvaluationProcessor` (`lang/.../interpret/`). Interprets the uncurried
   IR for the pure subset; provides its own backend natives (currently `inc`;
   `handleCases`/`typeMatch`/comparisons/arithmetic to come). This is what lets
   the checker *run* pure type-level code. See `path-a-...md`.

All are keyed by `ValueFQN` (#3's fact key also carries the ground arguments).

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

### Parameterization & dispatch (RESOLVED in shape, nuance for the spike)

`Int` is a type *constructor* (`Int[MIN, MAX]`); the ability's methods must
operate on *applied* types (`Int[a,b]`), and `assignableFrom`/`combine` take
**two independent applications** of the same constructor (`Int[c,d]` vs
`Int[a,b]`) -- so a fully-applied impl like `implement[a,b] TypeRefinement[Int[a,b]]`
can't work (it would force both arguments to share one pair of bounds).

Resolution -- combine two existing, tested features:

1. **Higher-kinded ability dispatched on the bare constructor.** Exactly like the
   tested `ability Container[F[_]] { def wrap(s: String): F[String] }` +
   `implement Container[Box]` (AbilityImplementationCheckProcessorTest, "higher-kinded
   abilities"). So `TypeRefinement` dispatches on the constructor `Int`.
2. **Methods typed over `Type`, bodies pattern-match the constructor.** The
   `def personName(t: Type) = t match { case Person[name] -> ... }` idiom from
   `TypeValues.els`. Each argument is matched independently, giving independent
   bounds.

Resolved shape:

```
ability TypeRefinement[T] {                          // T = the refined constructor (HKT dispatch key)
   def assignableFrom(target: Type, source: Type): Bool
   def combine(a: Type, b: Type): Type
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

Residual nuance to settle in the Phase 2 spike: whether `T` needs an explicit
kind annotation to dispatch on a 2-arg constructor (`T[_, _]`?) or whether the
checker keys dispatch purely on the head FQN (`Int`), making `T` effectively a
phantom dispatch key. The checker invokes these methods *itself* during
unification (it already knows the head constructor), so dispatch is
checker-driven via `AbilityImplementation.Key(typeRefinementFQN, Seq(Int))`
rather than normal argument-type inference.

### Checker integration (via the eval backend) -- UPDATED

(Supersedes the earlier "pre-fetch then pure" idea, which assumed the ability
could be reduced inside the pure Unifier. It can't -- see CRITICAL FINDING.)

The integration runs `assignableFrom`/`combine` through the eval backend:

- When the checker compares two same-head types (`Int[..]` vs `Int[..]`) with
  **fully-ground bounds** and the head constructor has a `TypeRefinement` impl,
  it resolves the impl (`AbilityImplementation.Key`) and requests
  `EvaluatedValue.Key(assignableFromImplFQN, typeArgs, [target, source])`. Accept
  on `True`, reject on `False`.
- Otherwise fall back to structural equality (also the path when bounds are
  metas/generics, so unification still solves `MIN := a, MAX := b`).
- This must happen on the IO-capable side (the `Checker`, which can request
  facts), not inside the synchronous pure `Unifier`. Likely seam: the
  same-head/`VTopDef`-vs-`VTopDef` comparison surfaces a "needs refinement check"
  obligation that the Checker discharges via `EvaluatedValue` (exact wiring is
  P7 of the eval-backend plan).

Direction matters: `assignableFrom(target = expected, source = inferred)` so we
accept *widening*, not narrowing -- verify the `unify(l, r)` argument order at
the `check` call sites. `combine` plugs into branch/match result-type synthesis
(also via `EvaluatedValue`).

## Phases

**Prerequisite (separate plan): the eval backend.** `path-a-...md` phases
P1..P4 build + integrate the compile-time evaluation backend. P1 (fact +
interpreter skeleton) is DONE; P2 (`handleCases`/`typeMatch` natives), P3
(termination/cycles), P4 (checker requests `EvaluatedValue` for closed type
terms; the reverted `match` probes become the green target) follow. The
`Int[MIN,MAX]` phases below sit on top of that.

- **Phase 0 -- Foundations.** `Bool` (`data Bool = False | True`) +
  `lessThanOrEqual(BigInteger, BigInteger): Bool` as the one true new primitive.
  With `match` now runnable (eval backend), `&&`/`min`/`max` are **ordinary
  Eliot** (no longer natives). Add `Bool` to `defaultSystemModules`. (= P5 in
  `path-a`.)
- **Phase 1 -- Literals as `Int[V,V]`.** Expected-type-directed literal typing
  in `Checker`. Audit monomorphize tests assuming `BigInteger` in value position.
- **Phase 2 -- `TypeRefinement` + range-checked compatibility.** Define
  `ability TypeRefinement[T]` + `implement TypeRefinement[Int]` in **pure Eliot**
  (the `match`-based bodies shown above); checker integration via the eval
  backend (see updated "Checker integration"). The earlier "spike inside the
  Unifier" is obsolete -- the eval backend is the mechanism.
- **Phase 3 -- Runtime arithmetic.** `+`/`-`/`*` on `Int` with dependent bounds
  (`Int[a,b] + Int[c,d] : Int[a+c, b+d]`). Two execution targets now: (a) the
  **eval backend** native (for type-level bound computation), and (b) the **JVM**
  `NativeImplementation` (`LADD`/`LSUB`/`LMUL`, unbox Long -> op -> rebox) for
  runtime. Runtime stays `Long`.
  - *Reconcile with master's `b90dcf8a "Add + to Int"`* which already declares a
    `+` in `Int.els`. Decide the final signature (dependent bounds) and back it
    with both natives.
- **Phase 4 -- Tests / examples / docs.** Literal typing; range accept/reject;
  `TypeRefinement` fallback to equality; arithmetic bounds; a JVM run test; an
  example `.els`; fold notes back here.

## Decisions & open sub-decisions

- DECIDED: ability `TypeRefinement[T]`; ops `assignableFrom(target, source): Bool`
  and `combine(a, b): T`. `meet`/intersection deferred.
- DECIDED: mechanism = compile-and-run via the `EvaluatedValue` eval backend
  (Path A), not symbolic reduction in the checker. `TypeRefinement[Int]` is
  written in **pure Eliot**.
- DECIDED: `Bool` is a pure stdlib data type (`match` runs now, so no need for a
  native Bool).
- OPEN: parameterization & dispatch of `TypeRefinement` over a type *constructor*
  whose methods receive *applied* types (HKT ability on the bare constructor; see
  Ability design section). Note: by the uncurried-IR stage dispatch is already
  resolved, which simplifies the eval-backend side.
- OPEN: exact `EvaluatedValue.Key` shape for the hook (how to pass the two
  type-as-data arguments) -- pin down in eval-backend P7.
- Out of scope for now: overflow/width enforcement; everything maps to `Long`.

## Representation & promotion (value-level coercion) -- DESIGN NOTE

Concern (raised by the author): `Int[MIN,MAX]` is meant to drive the *optimal
backend representation* (`Int[0,10]` -> Byte, `Int[0,1000]` -> Word, ...). If a
branch `combine`s `Int[0,10]` (Byte) and `Int[0,1000]` (Word) to `Int[0,1000]`
(Word), the Byte arm's runtime value must actually become a Word, or the type
would mislead the backend. So we need a *value-level promotion*. Does that
replace the type-level `combine`?

**Resolution: no -- they are complementary, at different levels.**

- **Promotion is semantically the identity.** Widening `Int[0,10] -> Int[0,1000]`
  preserves the integer value (10 stays 10); only the machine encoding changes
  (zero/sign-extend). So the *type-level* reasoning (`combine`/`assignableFrom`)
  is sound and complete on its own; promotion is purely a backend realization.
- `combine` answers "what type does this branch have?" -- load-bearing for
  type-checking everything downstream. It stays.
- Promotion answers "how do I physically materialize this value at the target
  type's representation?" -- only matters at a representation boundary.

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
separate future feature, and promotion is meaningless without it -- build them
together, later. The type-level `TypeRefinement` design stands unchanged now; we
only **reserve the seam** (a coercion pass slot between type-checking and
codegen, identity until widths land).

**Open future choice:** where `repr` + `promote` live once widths exist --
(1) a separate backend-facing concern (`repr` fn + `promote` native), keyed on
representation, keeping `TypeRefinement` purely type-level (current lean); or
(2) extra value-level methods on `TypeRefinement` so user-defined refinement
types also define their own representation/widening (more unified, heavier).
Either way `combine` survives.

## Risks

- Eval-backend maturity: the `TypeRefinement` hook depends on eval-backend
  P2..P4 (match natives, termination/cycles, checker integration). Tracked in
  `path-a-...md`.
- Checker/eval-backend boundary: detecting "closed enough to run" and discharging
  the refinement check on the IO-capable side without a pure-Unifier escape hatch.
- Literal-retyping fallout in existing tests/stdlib (Phase 1).
- Termination: running user code at compile time can loop (eval-backend P3).
