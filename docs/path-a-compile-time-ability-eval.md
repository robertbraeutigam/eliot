# Path A: compile-time evaluation via a fact-bounded pure eval backend

Durable plan for evaluating pure type-level Eliot code at compile time by
**compiling-and-running it through a dedicated, compiler-internal backend**,
rather than teaching the NbE checker to symbolically reduce `match`/abilities.
This unblocks pure-Eliot `TypeRefinement` (see `int-min-max-plan.md`) and is core
infrastructure for Eliot's dependent-types direction.

## Why (blocker recap)

`match` does not reduce at the NbE type level (verified). The checker resolves
*which* impl a call dispatches to but never *runs* it; per-type
`handleCases`/`typeMatch` impls are backend-native with no NbE body. So functions
whose bodies use `match` stay stuck in type positions.

## Chosen approach (decisions made)

1. **A second, compiler-internal Eliot backend that *executes* pure code.** It is
   used to evaluate closed type-level expressions: compile-then-run, recursively,
   to a fixpoint, until only ground values / backend primitives remain.
2. **Realized as its own generator behind a fact boundary** (`EvaluatedValue`
   fact). Initial implementation: a tree-walking interpreter over the existing
   **uncurried monomorphic IR** (same input the JVM backend consumes). The fact
   interface stays intact if we later swap the implementation to direct JVM
   compile-and-run.
3. **Pure-only.** No IO, no platform natives, no JAR machinery -- a *reduced*
   backend. Purity is enforced for free: it simply doesn't provide impure natives,
   so IO at the type level becomes a compile error.
4. **Hybrid with NbE.** The new backend evaluates CLOSED pure type terms; the
   existing monomorphize NbE keeps doing unification and open/generic reasoning.
   They meet at an "is this closed enough to run?" boundary (all args ground, no
   metas/neutrals).

### Why this needs no special `match` handling in the checker

Dispatch is already resolved by the time we reach the uncurried IR. So a `match`
appears there as a call to a *concrete* `handleCases`/`typeMatch` impl whose FQN
encodes the data type / constructor. The eval backend just runs the resolved
impl. The type checker has zero knowledge of `match` -- it only requests an
`EvaluatedValue` fact. "Providing ability implementations normally" = the eval
backend supplies `handleCases`/`typeMatch` (and `inc`, comparisons, ...) as its
own natives, exactly as the JVM backend supplies `println` via
`NativeImplementation`.

## Architecture

- **`EvaluatedValue` fact** (defined in `lang`/`eliotc` so the checker can request
  it): `Key(target, args: Seq[GroundValue]) -> GroundValue`, where `target`
  identifies the monomorphic value to run (its monomorphic/uncurried key) and
  `args` are the ground runtime arguments. This is the stable interface and the
  unit of caching.
- **`EvaluationProcessor`** (new backend module/plugin, parallel to `jvm`;
  decoupled from the checker purely via the fact): given the key, fetch the
  value's `UncurriedMonomorphicValue`, bind params to `args`, interpret the body
  to a `GroundValue`. Nested closed user-space calls are reduced by requesting
  further `EvaluatedValue` facts -- this *is* the "compile-then-run recursively"
  loop, with caching and laziness from the fact system.
- **Eval-backend natives** (the backend's `NativeImplementation` equivalent, as
  Scala fns over `GroundValue`):
  - `handleCases(value, cases)` -- structural: `value = Structure(ctorᵢ, fields)`;
    build the Church selector `pickᵢ = \a0..a_{n-1} -> a_i` for `ctorᵢ`'s
    index/count, apply `cases` to it, then apply the result to `fields`.
  - `typeMatch(obj, matched, notMatched)` -- the running impl's FQN encodes the
    target constructor `C`; if `obj`'s head is `C`, apply `matched` to `obj`'s
    args, else `notMatched(unit)`.
  - `inc`, `lessThanOrEqual`, later `+`/`-`/`*` -- direct `BigInt` ops.
  (`handleCases` needs each constructor's `(index, siblingCount)`; source it from
  the data type's constructor-list fact.)
- **Checker integration (hybrid).** The IO-capable Checker (not the pure
  Evaluator), when it needs the *value* of a closed pure type-level application,
  builds the `EvaluatedValue.Key` and requests the fact, splicing the resulting
  `GroundValue` back in. Start narrow (the `TypeRefinement` hook), generalize to
  arbitrary closed type-level subterms as needed.
- **Termination & cycles.** Fuel/timeout in the interpreter -> clean
  "type-level evaluation did not terminate" error. Cycle guard (like
  `UserValueNativesProcessor.generating`) so a value whose type depends on
  running itself errors instead of looping.

### Layering note (why facts make this work)

Phase order is not fixed -- facts are lazy/on-demand. So monomorphize requesting
`EvaluatedValue`, which requests `UncurriedMonomorphicValue` -> `MonomorphicValue`
for the *referenced* (pure, already-concrete) values, composes fine. Only a true
self-cycle is an error (handled by the guard).

## Phases

- **P1 -- `EvaluatedValue` fact + interpreter skeleton. [DONE]** Implemented in
  `lang/.../interpret/{fact/EvaluatedValue, processor/EvaluationProcessor}`,
  registered in `LangPlugin` (inert until an `EvaluatedValue` fact is requested).
  Interprets literals, parameter refs, monomorphic value refs, application, and
  function literals (closures) over the uncurried IR; nested ground value calls
  recurse via further `EvaluatedValue` facts (the compile-then-run fixpoint);
  `inc` is the first backend native. Value domain has Ground + Closure; the fact
  boundary is ground-only. Tests: `inc(1)->2`, `inc(inc(1))->3`,
  `addOne(inc(inc(1)))->4` (user fn via the fact). NOTE: kept in `lang` for now,
  not a separate module -- the fact boundary already gives the swappability.
- **P2 -- match natives. [DONE]** `handleCases` + `typeMatch` backend natives in
  `EvaluationProcessor`. Both are abstract ability impls (the JVM backend generates
  their bodies), so the eval backend supplies them, dispatched by the impl FQN's
  qualifier (`AbilityImplementation("PatternMatch"/"TypeMatch", _)`) rather than a
  fixed FQN. `handleCases(value, cases)`: `value = Structure(ctorᵢ, fields)`; build a
  `Native` Church selector picking `ctorᵢ`'s declaration-order index, apply `cases`
  to it -> handler, apply handler to `fields` (or a unit placeholder for a field-less
  ctor, mirroring the JVM `null`). `typeMatch(obj, matched, notMatched)`: the impl
  FQN encodes target ctor `C` via `ImplementationMarkerUtils.firstPatternType­Constructor­Name`;
  if `obj`'s head matches `C`, apply `matched` to its fields, else `notMatched(unit)`.
  Constructor metadata (declaration order) is derived from `UnifiedModuleNames` +
  `RoleHint.ValueConstructor` (same as `DataMatchDesugarer.findAllConstructors`); no
  dedicated ctor-list fact exists. A third `EvalValue.Native` variant + a uniform
  `applyValue` were added (uncurrying fully flattens lambdas/applications, so
  application is always saturated). Tests: nullary data-match, field-binding
  data-match, type-match (matched + wildcard fallthrough) -- all through the fact.
- **P3 -- termination + cycles.** Fuel/timeout; self-cycle guard; error surfaces.
- **P4 -- checker integration (hybrid).** Checker detects a closed pure type-level
  application and requests `EvaluatedValue`; splice result. Re-add the reverted
  probe tests (`Box[pick(True)]` etc.) as the integration target -> green.
- **P5 -- Phase 0 primitives.** `Bool` (`data Bool = False | True`) +
  `lessThanOrEqual` native; `&&`/`min`/`max` as ordinary Eliot (now runnable).
  Add `Bool` to `defaultSystemModules`.
- **P6 -- `TypeRefinement` + Int + literal typing.** `ability TypeRefinement[T]`
  (`assignableFrom`, `combine`) + `implement TypeRefinement[Int]` in pure Eliot;
  literals typed `Int[V,V]` (expected-type-directed).
- **P7 -- TypeRefinement hook.** In unify, at same-head `Int[..]` vs `Int[..]`
  with concrete bounds, request `EvaluatedValue` for `assignableFrom(expected,
  inferred)`; accept on `True`, else fall back to structural equality. `combine`
  drives branch/match join synthesis.
- **P8 -- tests / examples / docs.** Range accept/reject; `TypeRefinement`
  fallback to equality; example `.els`; fold notes into `int-min-max-plan.md`.

## Open decisions / risks

- **`EvaluatedValue.Key` shape** -- how to encode "the monomorphic value to run"
  (its monomorphic key incl. type args) plus the ground runtime `args`. Pin down
  in P1.
- **Constructor-metadata fact** -- RESOLVED (P2): there is no dedicated fact;
  declaration order is recovered from `UnifiedModuleNames` (filter Default-qualifier
  upper-case names) cross-checked against each value's `RoleHint.ValueConstructor`
  and sorted by source position -- the same derivation `DataMatchDesugarer` uses.
- **Module placement** -- new backend module (parallel to `jvm`) vs a package in
  `lang`. Fact lives where the checker can see it; processor in the backend.
- **Boundary generality** -- start with the `TypeRefinement` hook (narrow), decide
  later how broadly to route closed type-level reduction through the backend.
- **Termination policy** -- fuel vs wall-clock; error attribution.
- **Value/`Type`-as-data representation** -- types-as-values already run on JVM
  (`TypeValues.els`); confirm the same `GroundValue.Structure` representation
  feeds the interpreter cleanly.

## Superseded

The earlier draft of this doc proposed bespoke NbE `VNative` reducers for
`handleCases`/`typeMatch` inside the checker/evaluator. That is replaced by the
fact-bounded eval backend above (no special `match` handling in the checker).
