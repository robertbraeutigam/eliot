# NbE Migration - Shared Task Notes

## Current State: Phase 1 Complete

Phase 1 (eval2 in isolation) is implemented and tested. The migration plan is in `docs/nbe-migration-plan.md`.

## What was done

- Created `eval2/fact/` package: `Sem`, `Head`, `Closure`, `Env`, `MetaId`
- Created `eval2/util/` package: `MetaState` (inProgress set only), `Evaluator2`, `Quoter`
- 22 passing Evaluator2 tests, 10 passing Quoter tests

## Next: Phase 2 - Metavariables and Unifier

Per the plan (§6 Phase 2):
1. Extend `MetaState` with meta operations: `freshMeta`, `solveMeta`, `lookupMeta`, `force`
2. Implement `Unifier.unify` with pattern unification for meta-vs-other case
3. Unit tests: `Lit == Lit`, `Struct == Struct`, `Lam == Lam`, `?m == concrete`, pattern unification, occurs check

Key design decisions:
- `MetaState` currently only has `inProgress: Set[ValueFQN]`. Needs `metas: Map[MetaId, MetaInfo]` and `nextId: Int`
- `force(s: Sem)` chases meta solutions - implement in MetaState or as standalone util
- `EvalIO[A] = StateT[CompilerIO, MetaState, A]` is already the right monad stack

## Pre-existing test failures (not ours)

- monomorphize2 tests: 28 failures (pre-existing, see memory note)
- Old EvaluatorTest: 2 failures (pre-existing, commented-out code in Evaluator.scala)

## File locations

- Source: `lang/src/com/vanillasource/eliot/eliotc/eval2/{fact,util}/`
- Tests: `lang/test/src/com/vanillasource/eliot/eliotc/eval2/util/`
- Plan: `docs/nbe-migration-plan.md`
