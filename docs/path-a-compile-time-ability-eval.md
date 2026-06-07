# Path A: compile-time evaluation of `match` / ability code in the type checker

Durable plan for making the NbE type checker **run** (reduce), not just resolve,
ability-dispatched code at compile time. This unblocks pure-Eliot
`TypeRefinement` (see `int-min-max-plan.md`) and, more importantly, is core
infrastructure for Eliot's dependent-types roadmap (any compile-time computation
over data/types).

## Why (recap of the blocker)

Verified empirically: `match` does not reduce at the NbE type level. The checker
resolves *which* impl an ability call dispatches to (for codegen/associated
types) but never *executes* it; the pure `Evaluator` can't resolve abilities, and
the per-type `handleCases`/`typeMatch` impls are JVM-native (no NbE body). So any
function whose body is a `match` stays stuck in a type position.

## Key structural insight

`match` desugars to **two** primitive ability methods; everything else is plain
lambdas/applications the evaluator already reduces (Church encoding).

- **Data match** `b match { case True -> 1  case False -> 2 }` =>
  `handleCases(b)(\ $sel -> $sel(h_True)(h_False))`
  where `Cases[R] = Function[H_0, Function[H_1, ... R]]` and each `h_i` is a
  curried lambda over constructor i's fields. `handleCases[R](value: T, cases: Cases[R]): R`.
- **Type match** `t match { case P[n] -> n  case _ -> 0 }` =>
  `typeMatch(t, \ n -> n, \ _ -> 0)`, chained per constructor with the wildcard
  as the innermost `notMatched`. `typeMatch[R](obj: Type, matched: Fields[R], notMatched: Function[Unit, R]): R`,
  `Fields[R] = Function[g0, Function[g1, ... R]]` over the constructor's generic params.

Both impls are body-less natives today (JVM instanceof dispatch). **If we give
these two a structural NbE reducer, all `match` reduces** — and functions/abilities
built on `match` (`&&`, `min`, `assignableFrom`, `combine`, ...) reduce for free.

### The two reducers (operational semantics)

- `handleCases(value, cases)` where `value = Structure(ctorᵢ, [f0..fk])` from a data
  type with `n` constructors and `ctorᵢ` at index `i`:
  1. build `pickᵢ = \ a0 .. a_{n-1} -> a_i` (nested `VLam`),
  2. `selected = applyValue(cases, pickᵢ)`  (reduces to `h_i`),
  3. result = `[f0..fk].foldLeft(selected)(applyValue)`.
  Needs constructor metadata `(i, n)` for `ctorᵢ`.
- `typeMatch(obj, matched, notMatched)` testing against case constructor `C`,
  `obj = Structure(H, [a0..am])`:
  - if `H == C`: `[a0..am].foldLeft(matched)(applyValue)`,
  - else: `applyValue(notMatched, unit)`.
  Needs the target constructor identity `C`.

## The two real architectural problems

1. **Constructor metadata `(index, siblingCount)` for `handleCases`.** The
   reducer is pure (no IO). Cleanest: thread a second lookup into the `Evaluator`
   alongside `lookupTopDef`, e.g. `lookupCtorMeta: ValueFQN => Option[(Int, Int)]`,
   built by an IO prefetch (parallel to `NativeBinding` prefetch) from the data
   type's constructor list. The `Evaluator` recognises the two well-known
   match-primitive FQNs and reduces them structurally using its lookups. (Doing it
   as a plain `VNative` NativeBinding does NOT work — a `VNative` closure has no
   access to per-constructor metadata.)

2. **Target constructor identity `C` for `typeMatch`.** Two options, decide via a
   spike:
   - (a) **Self-contained:** if the desugared `typeMatch` call carries `C` as a
     type argument (the ability `TypeMatch[T]` is dispatched on `T = C`; check
     whether the ORE `ValueReference.typeArguments` actually carries it), the
     reducer reads it and stays uniform. PREFERRED if available.
   - (b) **Resolution-aware:** otherwise the evaluator must know the per-call
     resolved impl (which encodes `C`). That means threading the resolution
     (`abilityResolutions`, keyed by source-positioned FQN) into evaluation —
     bigger, and must also reach `VTopDef` thunk evaluators built by
     `UserValueNativesProcessor`.

   Note `handleCases` does NOT have this problem: its dispatch is intrinsic to the
   scrutinee's own constructor, so a uniform structural reducer suffices.

### Thunk propagation (why uniform reducers are nice)

`UserValueNativesProcessor` builds each value's body as a `VTopDef` thunk with a
context-free `Evaluator`. If the match reducers live *in the `Evaluator` itself*
(driven by `lookupCtorMeta` + well-known FQNs), every thunk evaluator gets them
automatically — no dispatch table, no ORE rewriting. This is why option 2(a) is
strongly preferred: it keeps everything uniform and avoids making thunks
resolution-aware.

## Phases

- **P-A1 — `handleCases` reducer + ctor-metadata prefetch.** Add
  `lookupCtorMeta` to `Evaluator`; reduce the well-known `handleCases` FQN
  structurally. Prefetch ctor metadata in the checker and in
  `UserValueNativesProcessor` thunks. Re-add the reverted data-match probe; make
  it pass. (Find the fact that lists a data type's constructors in order.)
- **P-A2 — `typeMatch` reducer.** Spike decision 2(a) vs 2(b). Implement the
  reducer; make the reverted type-match probe pass.
- **P-A3 — termination safeguard.** Add a fuel/depth bound to compile-time
  reduction (`force`/`applyValue` recursion) so user code can't hang the
  compiler; surface as a clean "evaluation did not terminate" error.
- **P-A4 — Phase 0 primitives.** `Bool` (`data Bool = False | True`) + a
  `lessThanOrEqual(BigInteger, BigInteger): Bool` native (the one true new
  primitive) in `StdlibNativesProcessor`; `&&`/`min`/`max` as ordinary Eliot
  (now reducible via P-A1). Add `Bool` to `defaultSystemModules`.
- **P-A5 — `TypeRefinement` + Int, literal typing.** `ability TypeRefinement[T]`
  (`assignableFrom`, `combine`) + `implement TypeRefinement[Int]` in pure Eliot;
  literals typed `Int[V,V]` (expected-type-directed) per `int-min-max-plan.md`.
- **P-A6 — checker hook.** In unify, at `VTopDef`-vs-`VTopDef` same-head (and the
  `Structure` ground case), if the head has a `TypeRefinement` impl and both
  sides have concrete bounds, evaluate `assignableFrom(expected, inferred)` and
  accept on `True`; else fall back to structural equality. Prefetch the impls
  into the `Unifier` (pure). `combine` plugs into branch/match join synthesis.
- **P-A7 — tests / examples / docs.** Range accept/reject; arithmetic-free
  range checks; `TypeRefinement` fallback to equality; an example `.els`;
  fold notes back into `int-min-max-plan.md`.

## Open decisions / risks

- **2(a) vs 2(b)** for `typeMatch` constructor identity — spike first; shapes the
  whole evaluator change.
- **Constructor-metadata source fact** — identify which fact lists a data type's
  constructors in declaration order (likely from `DataDefinitionDesugarer`
  output / module facts).
- **Termination** — compile-time eval of user code can loop; needs a real bound
  (P-A3) before this is safe to ship.
- **Scope of generality** — P-A1/P-A2 make *all* `match` reduce (general
  compile-time evaluation), which is the valuable, reusable outcome. The
  `TypeRefinement` hook (P-A6) is then a thin consumer. Resist scope-creeping
  into full general ability-dispatch-in-evaluator unless 2(b) forces it.
