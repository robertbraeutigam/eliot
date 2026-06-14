# Plan: Cornerstone Fidelity (Types-Are-Values / λ\*)

Status: design note + clean-up backlog. No code design is *blocked* on this; it is the
durable record of where the implementation still drifts from the language cornerstone and
what to do about each spot.

## The cornerstone (recap)

Eliot is a non-stratified Pure Type System (λ\*, `Type : Type`): types are ordinary values,
type constructors are ordinary functions, and "type-level" only means "forced by the compiler
before codegen." One evaluator, one value domain (`SemValue`), type checking = Normalisation by
Evaluation. See the "Language Cornerstone" section in `.claude/CLAUDE.md` for the full statement
and the list of *sanctioned* surface distinctions (qualifier namespaces, `[]`/`()` syntax,
`Expression.typeParser`) that are sugar, not violations.

## Current status (2026-06-14)

A code audit against the cornerstone found that **the deepest break — two separate evaluators —
is already resolved.** The earlier `eval`/`interpret` compile-time evaluator (and its
`SystemValueEvaluator`, which represented function types as `VConst(Structure(...))` while
monomorphize used `VPi`) was removed during the "NbE total evaluation" work. Today there is exactly
one evaluator: `monomorphize/eval/Evaluator.scala`, one `SemValue` domain, one binding cache, and a
uniform type-stack walk (`TypeStackLoop`, "no concept of generic parameters"). `VType` is an
ordinary bound value. Unsolved metas surface as explicit errors (`PostDrainQuoter`) with no silent
`Type` fallback.

What remains is therefore modest: (1) ~~**stale documentation/comments** that still describe the
removed second evaluator~~ — **done (Phase 1)**; (2) one **genuine design refinement** — type
equality vs. refinement/assignability is split between structural unification and an FQN-keyed
side-channel; (3) a low-priority **guardrail** keeping kind metadata out of semantic phases.

Explicitly **out of scope** (sanctioned sugar or principled primitives, not violations):
`Qualifier.Type`/`Default`, `[]` vs `()`, `Pattern.isTypePattern`, the restricted `typeParser`,
`VPi` being a primitive Π-former with a dedicated `Quoter` read-back (Π is the one irreducible type
former in a PTS — this is correct, not a special-case to remove), and backend FFI/native type
tables + erasure (the phase boundary doing its job).

---

## Phase 1 — Reconcile stale "second evaluator" framing (docs only, low risk)

These references describe code that no longer exists and actively mislead anyone reasoning about
the cornerstone. Pure documentation; no behaviour change.

- ✅ DONE — `SystemNativesProcessor.scala` comment no longer references the deleted
  `SystemValueEvaluator`; it now states plainly that the Π-former is the single primitive type former
  so every function type is a `VPi` (read back to a `Function` structure only at quote time).
- ✅ DONE — `.claude/CLAUDE.md` pipeline list. The defunct "7. eval" step was removed and the work
  it described folded into the `monomorphize` step's description (definition evaluation now runs in
  the single NbE phase). Confirmed via commit `ea93f48b` "Remove last vestiges of eval", which
  deleted the `eval` package (`DataTypeEvaluator`, `SystemValueEvaluator`, `ExpressionValue`,
  `Value`, …) and moved its responsibilities into `monomorphize`.
- ✅ DONE — `docs/type-value.md` deleted. It was almost entirely stale (the "Stripping Problem"/
  "Proposed Fix" sections described the removed `TypeCheckedValue`, `stripLeadingLambdas`,
  `stripUniversalTypeIntros`, `extractBodyTypeParams`). It is superseded by the "Language Cornerstone"
  section of `.claude/CLAUDE.md` (canonical principle) and this plan (open items: Unifier
  equality/refinement in Phase 2, `RoleHint` guardrail in Phase 3).
- ✅ DONE — the obsolete auto-memory note "Evaluator (eval phase) cannot do type inference; use
  compilerAbort" no longer exists. A sweep of `memory/` found no such claim; the surviving notes
  (`project_eval_nativefunction`, `project_symbolic_removal`, `project_nbe_total_evaluation`)
  correctly reflect the unified NbE checker, which *does* infer via metavariables/unification.

Acceptance: ✅ met. A repo-wide grep for `SystemValueEvaluator`, `eval phase`, `compile-time
evaluator`, `stripLeadingLambdas`, `TypeCheckedValue` returns only intentional historical/guardrail
notes (e.g. the "do not reintroduce a second compile-time evaluator" warning in `.claude/CLAUDE.md`
and `int-min-max-plan.md`), not live architectural claims.

**Phase 1 complete (2026-06-14).**

---

## Phase 2 — One notion of comparison: definitional equality vs. refinement (design refinement)

This is the only genuine code-level deviation, and it is subtle. `Unifier.unify`
(`monomorphize/unify/Unifier.scala`) already *is* evaluator-based definitional equality: it
`Evaluator.force`s both sides (= normalises) and compares, solving metavariables as inference on
top. That part is on-principle — keep it.

The wrinkle is the same-head `VTopDef` case (`Unifier.scala:78-87`): for two type constructors with
the same FQN, assignability is decided either by

1. running a `TypeRefinement.assignableFrom` implementation pulled from an FQN-keyed
   `refinements: Map[ValueFQN, SemValue]` side-map (e.g. `Int[MIN,MAX]` subtyping), or
2. falling back to structural `unifySpines`.

Two things to make uniform / clarify, in increasing ambition:

- **Separate the two concepts explicitly.** `unify` currently conflates *definitional equality*
  (force + compare, principled) with *refinement/subtyping* (run a predicate). Subtyping is
  genuinely a relation richer than equality (`Int[0,5]` ≤ `Int[0,10]` is not definitional equality),
  so it legitimately needs *something* beyond equality — but the code should name that boundary
  rather than burying it in a single `match` arm. Consider splitting "are these definitionally
  equal?" from "is `actual` assignable to `expected`?" so the refinement path is a first-class step,
  not an exception inside equality.
- **Make assignability uniform rather than an FQN side-channel.** Today only constructors present in
  the `refinements` map get predicate-driven assignability; everything else is structural. Evaluate
  whether assignability can be expressed as "every type former optionally *is* an
  `assignableFrom` value" reached through normal evaluation/ability resolution, so the `Unifier`
  doesn't need a bespoke `Map` injected by `MonomorphicTypeCheckProcessor`. The end state: comparison
  is always "force both, then either definitional-compare or run the type's own assignability value,"
  with no per-FQN special table.
- **Guard termination.** Running `assignableFrom` inside unification executes user/type-level code in
  the checker (Girard's paradox means this can diverge). Coordinate with the termination model
  (`project_recursion_as_effect`) — at minimum a stuck-residual/fuel guard so a non-terminating
  refinement predicate fails as a compile error rather than hanging the compiler.

Acceptance: definitional equality and assignability are distinguishable in the code (named methods
or clearly separated arms); refinement is not a special case keyed on a hand-built FQN map; a
divergent refinement predicate is caught rather than hanging. Behaviour for existing `Int[MIN,MAX]`
tests is preserved.

Risk: this touches the live `Int[MIN,MAX]` frontier (`int-min-max-plan.md`). Do Phase 1 first;
treat Phase 2 as a refactor to land *alongside* that work, not a prerequisite for it.

---

## Phase 3 — Keep kind/arity metadata out of semantic phases (guardrail, low priority)

`RoleHint.TypeConstructor(typeParamCount)` (`core/fact/RoleHint.scala`) carries kind-ish data and is
documented as for syntactic/desugaring phases only ("semantic phases ... must not consult the
hint"). This is currently respected, but it is the seed of a re-stratification if a future semantic
phase starts reading `typeParamCount` to distinguish "type parameters" from ordinary parameters —
exactly the distinction the cornerstone denies.

- Audit that no semantic phase (type checking, monomorphization, used, uncurry) reads `RoleHint`
  for anything other than reconstructing the programmer's *written shape* (match desugaring is the
  legitimate consumer).
- Consider a lightweight invariant (comment-enforced today; a test or assertion if cheap) that the
  monomorphize phase never branches on `RoleHint`.

Acceptance: documented invariant holds under audit; no semantic-phase dependence on `typeParamCount`.

---

## Decisions

- **Π is primitive, and that is on-principle.** `VPi` and its dedicated `Quoter` read-back to
  `Structure(functionDataTypeFQN, …)` are *not* a violation to remove: a PTS has one irreducible
  type former (Π), and function types being the sole semantic value with a quote rule reflects that.
  Do not try to "unify" `Function` into an ordinary `data` declaration.
- **`Type : Type` inconsistency is accepted.** We do not add a universe hierarchy to recover logical
  consistency; totality/termination is a separate concern (see `project_recursion_as_effect`).
- **Sanctioned sugar stays.** Qualifier namespaces and `[]`/`()` syntax are the deliberate
  developer-facing gimmick; no work item should try to erase them.
