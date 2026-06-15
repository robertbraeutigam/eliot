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
removed second evaluator~~ — **done (Phase 1)**; (2) one **genuine design refinement** — keep
`Unifier` as pure definitional equality and move widening out to a user-defined `Coerce` ability
that the checker inserts (design decided 2026-06-15, Phase 2 below); (3) a low-priority **guardrail**
keeping kind metadata out of semantic phases.

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

## Phase 2 — `Unifier` is pure definitional equality; widening is a user-defined `Coerce` ability

**Design decided (2026-06-15).** The unifier keeps doing exactly one thing — evaluator-based
*definitional (value) equality*: `Evaluator.force` both sides (= normalise) and compare, solving
metavariables as inference on top (`Unifier.unify`). There is no second, weaker comparison and no
notion of "assignability" inside unification. The directional part — that an `Int[0,5]` may be used
where an `Int[0,10]` is expected — is handled *outside* the unifier, by an ordinary user-defined
coercion that the checker inserts.

### Why not subtyping in the unifier

`Int[0,5]` and `Int[0,10]` are genuinely **different values** (different `MAX`); no normalisation
makes them equal, so this is not definitional equality. And because the bounds drive the chosen
machine representation, `Int[0,5]` (a byte) is *not* bit-compatible with `Int[0,10]` (could be a
word) — so it is not true subtyping either: a real, possibly representation-changing **conversion**
may be required. Baking a `≤` relation into `unify` would therefore (a) lie about bit-compatibility
and (b) re-introduce the directional comparison the cornerstone wants to avoid. The honest primitive
is a conversion the programmer (stdlib) defines, applied at a well-defined site.

### The `Coerce` ability returning `Option`

Replace the predicate-only `TypeRefinement[T]` (`assignableFrom(target, source): Bool`, currently in
`lang`, never implemented — see `Int.els`) with a **`Coerce` ability** whose method *performs* the
conversion and whose `Some`/`None` result *is* the existence proof:

```
ability Coerce[from, to] {        -- declared in lang: well-known, checker-invoked by name
   def coerce(value: from): Option[to]
}
```

The single parametric Int instance lives in **stdlib** (`Int.els`); the compiler hardcodes nothing
but the *name* `Coerce` (by-name protocol recognition, the codebase's existing pattern):

```
implement [smin, smax, tmin, tmax] Coerce[Int[smin,smax], Int[tmin,tmax]] {
   def coerce(value) =
      if le(tmin, smin) && le(smax, tmax)   -- existence: depends ONLY on the bounds
        then Some(nativeWiden(value))        -- payload: the (possibly runtime) conversion
        else None
}
```

**The two halves live in two phases, and NbE separates them automatically.** When the checker
evaluates `coerce(x)` at compile time for `x : Int[0,5]` against `Int[0,10]`:

- the `if` condition `le(0,0) && le(5,10)` has all bounds **known**, so NbE forces it to `true` — the
  `Some`/`None` choice is decided entirely at compile time (your "the compiler can evaluate the
  Option, it is pure code");
- the payload `nativeWiden(x)` is **stuck on `x`** (the runtime value is unknown), so NbE leaves it
  as a residual — a well-typed runtime expression;
- result: `coerce(x)` ⤳ `Some(nativeWiden(x))`. The checker reads the `Some`, **unwraps it, and
  splices `nativeWiden(x)` into the call site** as a bare `Int[0,10]`. There is no `Option` at
  runtime and the user never handles one. For `Int[0,5] → Int[0,3]` it forces to `None` → the
  coercion does not exist → compile-time type error.

This is precisely the cornerstone's phase/erasure split: the type-only part (existence) is forced
away at compile time, the value-dependent part (the byte→word `nativeWiden`) is residualised to
codegen. When representations already match (`Int[0,5] → Int[0,6]`, both byte) the instance returns
`Some(value)` and the payload is identity — zero runtime cost, same mechanism. Narrowing
(`Int[0,10] → Int[0,5]`, value-dependent, genuinely fallible) is *not* this mechanism: it is an
ordinary explicit stdlib function returning `Option`, with no checker support.

It is more faithful than a separate `IsTrue`/evidence type would be: no bespoke witness type, no
canonical-witness synthesis — the ability method's `Some`/`None` is the proof and its payload is the
staged conversion, all through the one evaluator.

### Where the directional logic lives — leaf check-mode, not `unify`

Coercion insertion belongs in the bidirectional checker's **check mode** (`check/TypeStackLoop.scala`),
where there is a known *expected* type and a freshly *inferred* actual — never in `unify` and never
in inference (subtyping interacts badly with metavariable unification; confining it to check mode
keeps inference symmetric):

```
check(term, expected):
  actual = infer(term)
  if unify(actual, expected): done                  -- pure definitional equality, unchanged
  else resolve Coerce[actual, expected]:
    evaluate coerce(term) via NbE:
      Some(payload) => replace term with payload     -- payload may carry runtime nativeWiden
      None          => report mismatch (contract violated)
      stuck         => report mismatch / postpone    -- abstract bounds: cannot prove widening
```

Resolution is **two-level, so no conditional-instance machinery is required** (Eliot has none today —
confirmed: no `given`/`where` constraints anywhere): the ability resolves the coercion *family* by
constructor (one parametric `Coerce[Int[..],Int[..]]` instance), and the returned `Option` decides
whether *these specific bounds* coerce. That is exactly the job constrained instances would have
done, expressed as ordinary value-level code in the instance body.

**Abstract bounds → `stuck`.** Inside a generic function where `a,b` are unknown, `le(...)` does not
reduce, so `coerce(x)` is neither `Some` nor `None`; the checker then falls back to requiring
definitional equality (or postpones until monomorphization makes the bounds concrete). You only get
implicit widening where the compiler can actually see it is safe.

### Scope for now (per 2026-06-15 decision)

- **Leaf positions only.** Coerce at argument / let-binding / return positions (`Int[0,5]` passed
  where `Int[0,10]` is expected). Coercing *inside* a constructor (`List[Int[0,5]] → List[Int[0,10]]`)
  needs variance reasoning + a structural rewrite — deferred; leave a comment marking the boundary so
  it is a known edge, not a silent gap.
- **Termination guard deferred.** Evaluating `coerce` in the checker runs type-level code, which
  Girard's paradox lets diverge. The stuck-residual/fuel guard is *not* part of this phase — it lands
  later with the termination model (`project_recursion_as_effect`), the same guard the
  `Int[MIN,MAX]` work needs.

### Net change list

- Delete `lang/resources/eliot/eliot/lang/TypeRefinement.els`; add the `Coerce` ability there, and a
  `WellKnownTypes` FQN for `Coerce` (replacing `typeRefinementAssignableFromFQN`).
- `stdlib/resources/eliot/eliot/lang/Int.els`: add the parametric `Coerce[Int,Int]` instance + the
  native widen; drop the deferred-`TypeRefinement` comment.
- `Unifier.scala`: remove the `VTopDef` refinement arm (`:74-87`) and the `refinements` field →
  `unify` becomes pure definitional equality.
- `MonomorphicTypeCheckProcessor`: remove `buildRefinements` / `assignableFromImpls` and the
  `refinements =` wiring.
- `check/TypeStackLoop.scala`: add the leaf check-mode coercion hook above.
- Replace `RefinementUnifyTest` with a coercion-insertion test (`Int[0,5]` into `Int[0,10]` succeeds
  and inserts the widen; into `Int[0,3]` is a type error).

Acceptance: `unify` does only definitional equality (no `refinements` map, no assignability arm);
implicit widening flows through a user-defined stdlib `Coerce` instance inserted in check mode;
`Int[0,5] → Int[0,10]` succeeds while `Int[0,5] → Int[0,3]` is a compile error.

Risk: touches the live `Int[MIN,MAX]` frontier (`int-min-max-plan.md`); land alongside that work. The
deferred follow-ups it depends on (matching on the abstract `type Int` constructor to read bounds;
literal-typing-as-`Int[V,V]`; the native widen) are tracked there.

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
