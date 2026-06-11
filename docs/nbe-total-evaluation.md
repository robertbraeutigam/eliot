# NbE total evaluation — fold the closed-term executor into the checker's evaluator

Durable plan for making the NbE evaluator reduce **all pure code** to normal form,
so that for a *closed* (symbol-free) term the normal form **is** the final value.
This rolls the capability of the separate `interpret` backend
(`docs/path-a-compile-time-ability-eval.md`) into NbE itself and **removes the
`interpret` package**. The result is one evaluator and one semantics for the whole
compiler, and seamless type-level computation for the dependent-types direction
(`docs/int-min-max-plan.md`).

## Why (the realisation)

The checker's NbE evaluator already reduces primitives (`VNative`) and unfolds
user-function bodies (`UserValueNativesProcessor` produces a lazy `VTopDef`). It only
gets stuck on `match` — and that stuckness is from a **missing reduction rule**, not
from design. `match` desugars to a call to an abstract `handleCases`/`typeMatch`
ability impl, which has **no body in the language** (the backends synthesise it). NbE
knows four moves — beta-reduce a `VLam`, fire a `VNative`, unfold a `VTopDef` *with a
body*, grow a spine — and a body-less `VTopDef` matches none of them, so it sticks.

There are two distinct stucknesses:

- **Stuck on a *symbolic* scrutinee** (`handleCases(x, …)`, `x` an unsolved
  meta/neutral): **load-bearing.** Same as `f(x)`; you cannot reduce a match on an
  unknown, and the normal form must keep it as structure so the unifier can compare.
- **Stuck on a *concrete* scrutinee** (`handleCases(True, …)`): **incidental.** The
  result is determinate; NbE only sticks because it lacks the rule.

For a closed term every scrutinee is eventually concrete, so adding the concrete rule
lets NbE compute final values — which is exactly what `interpret` was built to do
behind a fact boundary. `interpret` was the *conservative* way to get closed-term
match evaluation **without** first touching the NbE/unifier core. Once NbE has the
rule, `interpret` is redundant.

Timing makes this clean: `EvaluatedValue`/`EvaluationProcessor` (path-a P1/P2) are
**not yet consumed** by anything (the checker integration, path-a P4, was never
done). So we can pivot before any dependency forms.

## What changes (essence)

1. **`match` becomes pre-baked `VNative`s.** A processor fetches the constructor
   metadata in `CompilerIO` (as `DataTypeNativesProcessor` already does for
   constructors) and bakes it into pure `VNative` closures for the `handleCases` /
   `typeMatch` impl FQNs. **No effectful evaluator is needed** — the IO happens at
   fact-generation time, the evaluator stays pure.
2. **Primitives need no "unification."** With a single evaluator there is nothing to
   reconcile: `NativeBinding` is already the extensible primitive registry, and the
   duplicate `inc` in `interpret` simply disappears *with* `interpret`.
3. **`interpret` is removed.** Its ORE-vs-uncurried second evaluator, the `EvalValue`
   domain, the `EvaluatedValue` fact, and the structural match natives are all
   subsumed by NbE's existing ORE evaluator plus the new match rules.

The pre-existing bridge utilities (`Evaluator.groundToSem` / `semToGround`) stay —
they are used by ability pattern matching and quoting independently of `interpret`.

## Architecture

### Match as `VNative` (the core mechanism)

A new natives processor (call it `MatchNativesProcessor`) emits `NativeBinding`s for
the abstract match impls, intercepting the FQNs that `UserValueNativesProcessor`
would otherwise bind to a useless `VTopDef(fqn, None, …)`:

- **`handleCases` (one impl FQN per data type).** `VNative` collecting `(value,
  cases)`. The data type's **constructor list in declaration order** is baked in
  (fetched via `UnifiedModuleNames` + `RoleHint.ValueConstructor`, sorted by source
  position — the same derivation `DataMatchDesugarer.findAllConstructors` and the
  current `EvaluationProcessor` use). On a concrete `VConst(Structure(ctor, fields))`:
  compute `ctor`'s index `i` in the baked list, build the Church selector
  `pickᵢ = \a0 … a_{N-1} -> a_i` as nested `VLam`s, `applyValue(cases, pickᵢ)` to get
  the handler, then apply the handler to `fields` (or to a unit placeholder for a
  field-less constructor). All via `SemValue` `applyValue` — pure.
- **`typeMatch` (one impl FQN per constructor).** `VNative` collecting `(obj,
  matched, notMatched)`. The target constructor name is baked in (via
  `ImplementationMarkerUtils.firstPatternTypeConstructorName`). On a concrete
  `VConst(Structure(head, fields))`: if `head` matches `C`, apply `matched` to
  `fields`, else apply `notMatched` to unit.

On a **symbolic** `value`/`obj`, the native stays stuck — see "re-firing" (P4) for
the optional open-term extension.

### Read-back / quoting

`Quoter.quote` (and `Checker.forceAndConst`) already read a forced `SemValue` back to
a `GroundValue`. Two correctness items:

- **Value-constructor `valueType`.** `interpret` grounds a value constructor as
  `Structure(vfqn, args, returnType)` (the value's *data type*); NbE's `semToGround`
  uses `GroundValue.Type`. Because `GroundValue`'s `Eq` is universal (it compares
  `valueType`), a value embedded in a type must quote with its real type, not `Type`.
  With a single evaluator there is no cross-evaluator disagreement, but the read-back
  must still be correct for values-in-types. Decide where the value's type is threaded
  (the constructor's `NativeBinding` can carry it).
- **Closed-term entry point.** "Fully evaluate this closed application to a
  `GroundValue`" becomes "evaluate to a `SemValue`, `force`, `quote`" — no separate
  fact. The checker is already in `CompilerIO`; it pre-fetches bindings and evaluates,
  exactly as it does today, and the new match rules make the result ground.

### Recursion, caching, termination

Nested user calls are already handled by NbE's lazy `VTopDef` bodies, the
`UserValueNativesProcessor.generating` guard, and `force` — no `EvaluatedValue`
recursion needed. The new risk is **non-termination in the hot checking path**:
type-level computation is now Turing-complete and a `match`-driven loop would hang the
checker. `interpret` quarantined this behind an explicit request with its own fuel;
folding it in means fuel/step-limiting must live in the evaluator/`force` itself
(see P3). This is the single most important thing to get right before relying on it.

## Phases

- **P1 — `match` `VNative`s (concrete scrutinee). DONE.** `MatchNativesProcessor`
  (`monomorphize/processor`) emits `handleCases`/`typeMatch` `NativeBinding`s with baked
  constructor metadata; Church selector + dispatch in `SemValue`. Registered in
  `LangPlugin` ahead of `UserValueNativesProcessor` (first-registration-wins via
  `registerFactIfClear` + `Deferred.complete`), so it intercepts the abstract impl FQNs
  before they get a body-less `VTopDef`. `MatchNativesProcessorTest` checks NbE reduces
  `negate(True)`, a field-binding data-match, and type-match dispatch (matched +
  wildcard) to ground values via `Evaluator.semToGround`.
  - **Scope notes for later phases:** at P1 the evaluator ignored
    `ValueReference.typeArgs`, so type-application scrutinees like `Tag["hello"]` reduced
    to a spine-less `VTopDef` and binding a type argument out of a type-match
    (`case Tag[name] -> name`) did not work; **P2 now threads the args into the spine**,
    so that case reduces. Arithmetic natives like `inc` have no NbE `NativeBinding` (they
    were `interpret`-only) — P1/P2 tests avoid them.
- **P2 — closed-term evaluation + quoting. DONE (value-type read-back deferred).**
  The evaluator now threads `ValueReference.typeArgs` into the spine (`eval` folds each
  evaluated type arg via `applyValue`), so a type-application scrutinee like
  `Tag["hello"]` keeps `"hello"` in the constructor spine and a type-match
  `case Tag[name] -> name` binds it — the field-extraction-from-type-args case the P1
  note deferred. The closed-term read-back entry point is `eval → Quoter.quote`
  (`Quoter.quote` forces internally and already handles every fully-reduced shape:
  `VConst`, `VType`, `VPi`, and the body-less constructor `VTopDef(fqn, None, spine)`);
  no separate fact is needed. `MatchNativesProcessorTest` covers both the type-arg
  binding and the `Quoter`-entry-point read-back.
  - **Deferred: value-constructor `valueType` read-back.** Value constructors still
    quote with `valueType = GroundValue.Type` rather than their data type. This is
    load-bearing **only** for values embedded in types compared via `GroundValue`'s
    universal `Eq` — i.e. the dependent direction wired in P6 — and is harmless while
    read-back stays internally consistent (a single evaluator, post-P5). Fixing it
    requires a design choice the plan flags as open (carry the data type on the
    constructor's `NativeBinding` — e.g. an extra `VTopDef` field, vs. building value
    constructors as saturating `VConst(Structure(fqn, fields, dataType))` `VNative`
    chains, which would also change `MatchNativesProcessor`'s `VTopDef`-head dispatch).
    No current term reduces to a case where the wrong `valueType` is observable, so the
    decision is best made and validated against P6's concrete values-in-types use — same
    deferral philosophy as P4.
- **P3 — termination / fuel in NbE. DEFERRED to P6 (decided 2026-06-10).** An ambient
  throw-based step counter (threaded through `eval`/`applyValue`/`force` + `Unifier` +
  `Checker` + `Quoter`, caught at the `CompilerIO` boundary) was scoped and ready to
  build but **not built**, for two reasons:
  - **Not load-bearing yet.** No normal program reduces to a non-terminating term in the
    hot path. The only divergence is a recursive definition the evaluator force-unfolds
    with no base case (a self-referential type alias today; match-driven type-level
    recursion only once **P6** wires type-level evaluation into the checker). The guard
    becomes load-bearing exactly when P6 lands, so it is built then — when we also know
    more about the effect direction below.
  - **Largely orthogonal to the target design.** Eliot will eventually model **recursion
    as an effect** (alongside an `IO`-like `Effect`). Because the pure compile-time
    evaluator has no implementation for effects, effectful code — including *unbounded*
    recursion — trivially **gets stuck** (produces a neutral/residual), the same way `IO`
    and a `match` on a symbolic scrutinee already get stuck; it never loops. *Bounded*
    recursion will carry its **fuel as an in-language value** with a type-level proof of
    the bound, so it terminates *by construction* and the evaluator needs no out-of-band
    counter. In that world a step counter is redundant (at most a deep last-resort
    fallback). So termination is a **typing property + in-language fuel**, not an ambient
    runtime guard — and the throw-based counter would be throwaway.
  - **When P6 needs a guard, build the minimal *on-architecture* version, not the
    throw-based one:** a step-limit kept **local to the `eval` package** that, on
    exhaustion, returns a **stuck residual** (caught inside the evaluator entry, converted
    to a neutral) rather than throwing. That flows through the *existing*
    `Quoter`/`PostDrainQuoter` "Cannot resolve type." path — no `Unifier`/`Checker` fuel
    field, no `CompilerIO` exception-catch combinator. "Ran out → stuck → cannot resolve"
    is the same *shape* as "effect unavailable → stuck," so it is a stepping stone toward
    the effect model rather than a mechanism to rip out later. Default budget and error
    wording are decided at that point.
- **P4 — re-firing neutrals (advanced; *beyond* the closed-term goal; deferred).**
  Make a `VNative` stuck on a neutral/meta **re-fire** when the argument is later
  refined (today `applyValue`'s `VNative`-on-neutral case *drops* the native into an
  opaque neutral). Options: a `NeutralHead` variant carrying the native so `force`
  re-attempts firing once the spine arg forces to concrete, or glued evaluation
  (retain syntax, re-evaluate on meta-solve). Needed only for `match` whose scrutinee
  depends on an unsolved meta — i.e. *symbols involved*. Not required for the immediate
  `TypeRefinement` use (its scrutinees are concrete bounds), so this can wait for a
  concrete use case.
- **P5 — remove `interpret`. DONE (2026-06-10).** Deleted
  `interpret/{fact/EvaluatedValue, processor/EvaluationProcessor}` (the latter held the
  `EvalValue` domain and the structural `handleCases`/`typeMatch` natives) and
  `EvaluationProcessorTest`; unregistered `EvaluationProcessor` from `LangPlugin` (import
  + pipeline entry). Nothing consumed `EvaluatedValue` (path-a P4 was never wired), so
  removal was self-contained. The P1/P2 oracle cases were already re-expressed at the NbE
  level in `MatchNativesProcessorTest` (P1/P2), which is now the sole coverage — the match
  cases (nullary/field-binding `handleCases`, matching/binding/wildcard `typeMatch`,
  `Quoter` read-back) carried over; the `inc`-based arithmetic cases were **dropped** with
  the duplicate `inc` native, which lived only in `EvaluationProcessor` (NbE has no `inc`
  `NativeBinding`). Full `__.test` green.
- **P6 — `TypeRefinement` assignability hook (slice 1). DONE (2026-06-10).** The `Unifier`
  carries a `refinements: Map[ValueFQN, SemValue]` (type-constructor FQN → its custom
  `assignableFrom` impl). In the `VTopDef`-same-FQN case it runs the impl through the pure
  evaluator (`force(applyValue(applyValue(impl, expected), actual))`): a concrete `Bool`
  `Direct(true)`/`Direct(false)` accepts/rejects; anything else (stuck on metavariable bounds)
  falls back to `unifySpines`, preserving meta-solving. `MonomorphicTypeCheckProcessor` builds
  the map by scanning each referenced type constructor's module names for a custom
  `assignableFrom` `TypeRefinement` impl (the resolver's own lookup, via `getFact`
  +`AbilityMatcher.matchImpl` — **non-erroring**, so a constructor with no custom impl is just
  omitted and compares structurally, which *is* the default). `Bool` is an opaque `type Bool`
  (lang) with compile-time natives `true`/`false`/`&&`/`typeEquals`/`lessThanOrEqual` in
  `SystemNativesProcessor`; `TypeRefinement` is a lang ability with a structural-equality
  default. `RefinementUnifyTest` verifies accept/reject/fallback and a realistic
  nested-match + `lessThanOrEqual`/`&&` range refinement (widening accepted, narrowing
  rejected). Whole-repo tests green; the JVM `HelloWorld` example still builds and runs.
  - **Rejected mechanisms (with reasons), for the next reader:** (a) hardcoding `Int` in the
    checker — wrong, the hook is generic over any type with a custom impl; (b) a new
    non-erroring resolver fact — rejected as a parallel generator; (c) auto-generating a
    default `implement TypeRefinement[C]` per type constructor — fails structurally: types
    declared across multiple resource files (`String`/`Unit`/`Function`/`IO` in
    lang+stdlib+jvm) get duplicate markers → overlap errors, `Type` is `VType` (no marker can
    match it), and the default body self-references `TypeRefinement[Type]`. The chosen
    custom-impl-only detection has none of these problems.
  - **Deferred to `int-min-max-plan.md` (the real `Int` use needs more than the hook):**
    `combine` (branch/match result synthesis); literal-typing-as `Int[V,V]` (Phase 1) to
    produce `Int` values; and **`TypeMatch` for abstract `type` constructors** — extracting
    bounds via `case Int[tmin,tmax]` requires a `TypeMatch` impl, which is currently generated
    only for `data` types, so `implement TypeRefinement[Int]` on the abstract `type Int` is not
    yet functional and was left out of `Int.els` (a note marks the intent).
- **P7 — tests / docs / reconcile.** Mark `path-a-compile-time-ability-eval.md`
  superseded (its P2 match semantics/metadata derivations are reused here, retargeted
  to `SemValue`). Fold notes into `int-min-max-plan.md`.

## Open decisions / risks

- **Fuel policy.** Resolved by deferral — see **P3** above. Termination will be a typing
  property (recursion-as-effect) + in-language fuel, not an ambient runtime counter; the
  interim guard (built at P6) is a `eval`-local step-limit that yields a *stuck residual*,
  not a thrown budget. Step-count vs wall-clock / default budget / error wording are
  settled when that interim guard is built.
- **Re-firing design (P4).** Neutral-head-carrying-native vs glued evaluation; how it
  interacts with the unifier's speculative application and `drain()`. Only needed for
  open-term match.
- **Value-as-data `valueType` in quoting.** Ensure values embedded in types quote with
  their data type, not `GroundValue.Type` (universal `Eq` makes this load-bearing).
- **Processor ordering.** `MatchNativesProcessor` must win over
  `UserValueNativesProcessor` for the abstract impl FQNs; confirm the
  prepend/precedence mechanism (mirrors how `StdlibPlugin` prepends its natives).
- **Performance.** Reduction must stay demand-driven; guard against a type comparison
  triggering unbounded computation during unification.
- **Module placement.** `MatchNativesProcessor` belongs in `lang` (alongside the other
  natives processors); constructor metadata derivations already live there.

## Relationship to existing docs

- **Supersedes** the `interpret`-backend approach of
  `path-a-compile-time-ability-eval.md` (its P3–P8). Path-a's premise ("NbE cannot
  reduce `match`") is *narrowed*: NbE cannot reduce `match` on a *symbolic* scrutinee
  (correct, load-bearing), but it *can* on a *concrete* one once given the rule — which
  is all the closed-term goal needs. The match metadata derivations from path-a P2 are
  reused, retargeted from the `EvalValue` domain to `SemValue`.
- **Unblocks** `int-min-max-plan.md` directly: type-level pure evaluation is now a
  property of the checker's own evaluator rather than a separate backend reached via a
  fact.
