# monomorphize3 NbE Implementation - Task Notes

## Current State: Step 10 complete (pending SKILL.md)

All 10 steps of the NbE migration plan are implemented. Steps 1-9 build the NbE type checker; Step 10 is the final parity audit and cleanup.

## What's Done

- All monomorphize3 tests pass: 14 ProcessorTest + 61 TypeCheckTest + 1 ignored (nested HK)
- Full parity audit complete: every monomorphize2 test has a corresponding monomorphize3 test
- scalafmt run: no changes needed, code was already formatted
- `./mill __.compile` passes
- `./mill __.test` passes for monomorphize3 (29 pre-existing failures in other packages, unchanged)
- Duplicate import in Monomorphic3TypeCheckTest.scala fixed

## Remaining Item

- **SKILL.md**: The `.claude/skills/eliot-monomorphize3/SKILL.md` file could not be written due to sandbox permissions. The content was prepared and attempted. The next human iteration should create the directory and file manually, or grant write permissions to `.claude/skills/eliot-monomorphize3/`.

## File Layout

```
lang/src/.../monomorphize3/
  fact/GroundValue.scala, Monomorphic3Value.scala, Monomorphic3Expression.scala, NativeBinding.scala
  domain/SemValue.scala, Env.scala, MetaStore.scala
  eval/Evaluator.scala, Quoter.scala
  unify/Unifier.scala
  check/CheckState.scala, Checker.scala, TypeStackLoop.scala
  processor/Monomorphic3Processor.scala, SystemNativesProcessor.scala, DataTypeNativesProcessor.scala, UserValueNativesProcessor.scala

lang/test/.../monomorphize3/processor/
  Monomorphic3ProcessorTest.scala, Monomorphic3TypeCheckTest.scala
```
