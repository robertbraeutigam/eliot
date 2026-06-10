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
  - **Scope notes for later phases:** the evaluator ignores `ValueReference.typeArgs`
    (the `_` in `eval`), so type-application scrutinees like `Tag["hello"]` reduce to a
    spine-less `VTopDef` — type-match *dispatch* (head-name compare) works, but binding a
    type argument out of a type-match (`case Tag[name] -> name`) does not yet thread the
    arg into the spine. That field-extraction-from-type-args case is **P2** (closed-term
    eval + quoting / value-as-data read-back). Arithmetic natives like `inc` have no NbE
    `NativeBinding` (they were `interpret`-only) — P1 tests avoid them.
- **P2 — closed-term evaluation + quoting.** Confirm/extend `Quoter.quote` for fully
  reduced values; fix value-constructor `valueType` read-back. Provide the helper the
  checker uses to reduce a closed type-level subterm to a `GroundValue`.
- **P3 — termination / fuel in NbE.** Fuel or step-budget threaded through
  `eval`/`applyValue`/`force`; clean "type-level evaluation did not terminate" error
  with source attribution. Keep reduction demand-driven (triggered by unify/quote),
  not eager, to bound cost.
- **P4 — re-firing neutrals (advanced; *beyond* the closed-term goal; deferred).**
  Make a `VNative` stuck on a neutral/meta **re-fire** when the argument is later
  refined (today `applyValue`'s `VNative`-on-neutral case *drops* the native into an
  opaque neutral). Options: a `NeutralHead` variant carrying the native so `force`
  re-attempts firing once the spine arg forces to concrete, or glued evaluation
  (retain syntax, re-evaluate on meta-solve). Needed only for `match` whose scrutinee
  depends on an unsolved meta — i.e. *symbols involved*. Not required for the immediate
  `TypeRefinement` use (its scrutinees are concrete bounds), so this can wait for a
  concrete use case.
- **P5 — remove `interpret`.** Delete `interpret/{fact/EvaluatedValue,
  processor/EvaluationProcessor}`, the `EvalValue` domain, the structural
  handleCases/typeMatch natives, and their tests; unregister from `LangPlugin`.
  Migrate the P1/P2 `interpret` tests into NbE-level tests (kept as the oracle until
  this point). Drop the duplicate `inc` (it lived only in `EvaluationProcessor`).
- **P6 — `TypeRefinement` / Int integration.** With NbE reducing `match`, the pure
  Eliot `TypeRefinement` code (`assignableFrom`, `combine`) runs *during checking*
  directly — no fact hop. Wire the unify hook from `int-min-max-plan.md` to evaluate
  through NbE.
- **P7 — tests / docs / reconcile.** Mark `path-a-compile-time-ability-eval.md`
  superseded (its P2 match semantics/metadata derivations are reused here, retargeted
  to `SemValue`). Fold notes into `int-min-max-plan.md`.

## Open decisions / risks

- **Fuel policy.** Step-count vs wall-clock; where to attribute the error; default
  budget. Lands in the hot path, so it must be cheap.
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
