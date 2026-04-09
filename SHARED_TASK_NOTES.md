# NbE Migration - Shared Task Notes

## Migration plan: `docs/nbe-migration-plan.md`

## Current State: Phase 1 COMPLETE

Phase 1 (eval2 in isolation) is implemented and tested:
- `lang/src/.../eval2/fact/Sem.scala` — Semantic domain (Lit, TypeUniv, Struct, Lam, Neut with Param/Ref heads)
- `lang/src/.../eval2/util/MetaState.scala` — EvalIO type alias, inProgress recursion guard
- `lang/src/.../eval2/util/Evaluator2.scala` — ORE → Sem evaluator with beta-reduction
- `lang/src/.../eval2/util/Quoter.scala` — Sem → Value quoter
- Tests: `Evaluator2Test` (18 tests), `QuoterTest` (13 tests) — all pass

## Next: Phase 2 — Metavariables and Unifier

Per the plan (§6, Phase 2):
1. Add `Head.Meta(id: MetaId)` to `Sem.Head`
2. Add `MetaId` opaque type, `MetaInfo`, meta fields to `MetaState`
3. Implement `freshMeta`, `solveMeta`, `force` in MetaState
4. Implement `Unifier.unify` in `eval2/util/Unifier.scala` with pattern unification
5. Unit tests: unify Lit==Lit, Struct==Struct, Lam==Lam, ?m==concrete, pattern unification, occurs check

### Key design decisions from Phase 1
- `Quoter.quote` is currently pure (`Sem => Option[Value]`). Phase 2 must change it to `EvalIO[Option[Value]]` to force metas before quoting.
- `Evaluator2.apply` on non-function/non-neutral currently returns a sentinel neutral. Should become a proper error once the elaborator provides context.
- Bare `Neut(Ref(vfqn), [])` quotes to `dataType(vfqn)`. Ref with non-empty spine returns None — will need handling when type constructors like `Function[A, B]` appear.
- FunctionLiteral with no type annotation aborts. Phase 2 should create a fresh meta instead.

### Pre-existing test failures (not caused by this work)
- 2 in EvaluatorTest (existing eval package)
- Several in monomorphize2 tests (known broken, see memory)
