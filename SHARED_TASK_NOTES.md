# NbE Migration - Shared Task Notes

## Current State

**Phases 1+2 are implemented.** The `eval2` package is complete with:
- `eval2/fact/Sem.scala` - Semantic domain (Lit, TypeUniv, Struct, Lam, Neut) with Meta head
- `eval2/util/MetaState.scala` - Metacontext with EvalIO monad, freshMeta, solveMeta, force
- `eval2/util/Evaluator2.scala` - NbE evaluator (ORE -> Sem), handles top-level references, recursion guard, type args
- `eval2/util/Quoter.scala` - Sem -> Value conversion (for output)
- `eval2/util/Unifier.scala` - Pattern unification (empty-spine metas only for now)

All eval2 tests pass (Evaluator2Test: 18, QuoterTest: 12, UnifierTest: 19). Pre-existing failures in monomorphize2 and old eval tests are unrelated.

## Next: Phase 3 - Elaborator

Create `monomorphize2/typecheck/elab/` with:
1. `ElabExpr.scala` - Internal expression mirror with Sem types instead of Value
2. `Elaborator.scala` - Bidirectional check/infer per rules in nbe-migration-plan.md section 3.3
3. `Zonk.scala` - Walk ElabExpr, force+quote all Sems to produce MonomorphicExpression

Key reference: `docs/nbe-migration-plan.md` sections 3.1-3.5

The elaborator should NOT be wired into the processor yet - create a separate test class that calls Elaborator.elaborate directly.

## Design Notes

- `Unifier.solveFlex` currently only handles empty-spine metas. Higher-order pattern unification (non-empty spine) deferred - returns error. This is fine for Eliot's current HM-like system.
- `Closure` wraps `(Env, OperatorResolvedExpression)`. For meta solutions that need lambda abstraction, a different approach will be needed (see plan section 2.5 risk 7.3).
- The `Evaluator2.buildTypeStructure` method handles values without runtime body by checking if signature is TypeUniv and creating a Struct, otherwise returning Neut(Ref).
- `MetaState.EvalIO[A] = StateT[CompilerIO, MetaState, A]` - all eval2 operations thread through this monad.
