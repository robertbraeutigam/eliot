# monomorphize3 NbE Implementation - Task Notes

## Current State: Step 9 fully ported

Steps 1-9 are complete with all tests green. Step 9 adds recursion handling via cycle detection in UserValueNativesProcessor.

## What Works

- `./mill lang.compile` passes
- All 75 monomorphize3 tests pass (14 ProcessorTest + 61 TypeCheckTest + 1 ignored nested HK)
- No regressions to existing tests (~29 pre-existing failures in other packages, unchanged)
- Everything from Steps 1-8 plus:
- Direct recursion: `def f: Function[Int, Int] = f` terminates
- Mutual recursion: `def f = g`, `def g = f` terminates

## Key Design Decisions Made

### All decisions from Steps 1-8 still apply (see git history)

### Mutual recursion uses concurrent generation guard (new in Step 9)
The `UserValueNativesProcessor` uses a `ConcurrentHashMap`-backed set (`generating`) to track which FQNs are currently having their NativeBinding generated. When `collectBindings` encounters a ValueReference whose FQN is in the `generating` set, it skips fetching that binding (returns empty map for it). This prevents fact system deadlocks where `f`'s NativeBinding depends on `g`'s NativeBinding depends on `f`'s NativeBinding.

### VTopDef body bindings are still collected for non-recursive values (Step 9)
Unlike the simpler approach of removing all body binding collection, we keep it intact for non-recursive values. This is needed because type-level computation tests (Step 7) require body bindings — e.g., `type AlwaysString[A, B] = String` needs String's system NativeBinding in the thunk, and `def one: Person = Person(1)` needs Person's data constructor NativeBinding.

### No changes to force/unfoldTopDef were needed (Step 9)
The plan suggested modifying `force(VTopDef)` to only unfold when spine is fully VConst. This wasn't needed because the cycle detection in `collectBindings` means recursive VTopDefs evaluate to VNeutral in their cached bodies (the recursive ref is skipped). The unifier's existing VTopDef FQN equality handles the structural comparison.

## Next Steps (Step 10)

1. Step 10: Final parity audit and cleanup
   - Walk through both original test files (monomorphize2). For each test, verify a corresponding Monomorphic3 test exists and passes
   - Write `.claude/skills/eliot-monomorphize3/SKILL.md` describing the NbE architecture
   - `./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources`
   - `./mill __.compile && ./mill __.test`

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
