# monomorphize3 NbE Implementation - Task Notes

## Current State: Step 7 fully ported

Steps 1-7 are complete with all tests green. Step 7 adds type-level computation.

## What Works

- `./mill lang.compile` passes
- All 68 monomorphize3 tests pass (11 ProcessorTest + 57 TypeCheckTest + 1 ignored nested HK)
- No regressions to existing tests (~29 pre-existing failures in other packages, unchanged)
- Everything from Steps 1-6 plus:
- Type-level computation: non-type value type parameters, concrete literal values, reject differing literals, concrete data values, reject differing data values, type-level function calls

## Key Design Decisions Made

### All decisions from Steps 1-6 still apply (see git history)

### VTopDef uses Option[Lazy[SemValue]] (new in Step 7)
Changed from `Lazy[SemValue]` to `Option[Lazy[SemValue]]` to distinguish between values with bodies (Some) and opaque/abstract values (None). This allows `force` and `unfoldTopDef` to skip opaque values.

### Evaluator.force unfolds VTopDef (new in Step 7)
When `force` encounters `VTopDef(_, Some(cached), spine)`, it evaluates `cached.value`, applies the spine, and recursively forces the result. This enables type-level computation where values like `def one: BigInteger = 1` reduce to `VConst(Direct(1, bigIntType))` during unification.

### Evaluator.applyValue unfolds VTopDef for VNative (new in Step 7)
When a VNative receives a VTopDef argument, `unfoldTopDef` is called first. If the unfolded result is concrete (VConst, VLam, etc.), the native fires on it. If the unfolded result is VNeutral (thunk couldn't fully resolve), the native fires on the original VTopDef. This ensures data type natives like Box correctly receive computed type arguments while not breaking when values can't be fully resolved.

### UserValueNativesProcessor pre-fetches body bindings (new in Step 7)
The VTopDef Lazy thunk previously used `Evaluator(_ => None, Map.empty)`, which couldn't resolve ValueReferences in the body. Now, `generateFromKeyAndFact` pre-fetches NativeBindings for all ValueReferences in the runtime body and passes them to the thunk's evaluator. This enables type-level computation with data constructors (e.g., `def one: Person = Person(1)` correctly evaluates to the Person structure).

### Unifier solveMeta skips self-referential solutions (new in Step 7)
When `solveMeta(id, spine, rhs)` finds that rhs is a VMeta with the same id, it skips the solve. This prevents infinite loops when the same meta appears on both sides of a unification (can happen when VNative sticks on metas during type-level computation).

## Next Steps (Step 8)

1. Step 8: Lambda inference and ability implementation resolution
   - Port tests: lambda type inference (2 cases), resolve ability ref to concrete implementation
   - `check(FunctionLiteral, VPi)` without annotation uses VPi's domain directly
   - `infer(ValueReference)` checks if value is ability method dispatched to concrete implementation
2. Step 9: Recursion via lazy VTopDef
3. Step 10: Final parity audit

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
