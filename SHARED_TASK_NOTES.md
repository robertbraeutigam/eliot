# monomorphize3 NbE Implementation - Task Notes

## Current State: Steps 1-4 partially complete

Steps 1-3 (skeleton, domain, evaluator, unifier) are fully implemented and compile.
Step 4 (checker, TypeStackLoop, processor wiring) is structurally complete but has a key bug.

## What Works

- `./mill lang.compile` passes
- Simple non-generic tests pass: `def f: BigInteger`, `def f: BigInteger = 42`, `def f: String = "hello"`, `def f: BigInteger = constVal`
- No regressions to existing tests (29 pre-existing failures, same count)

## The Key Bug: Function type resolution in fetchEvaluatedSignature

**Symptom**: Any test involving `Function[A, B]` types fails with "Not a function." error.

**Root cause**: `Monomorphic3Processor.fetchEvaluatedSignature(vfqn)` evaluates a value's type stack to get its TYPE (kind). For `Function^Type` (defined as `opaque type Function[A, B]`), the type stack has 2 levels:
- level 0 (signature): `(A: Type) -> (B: Type) -> Type` (a FunctionLiteral ORE)
- level 1 (kind): `Function(Type)(Function(Type)(Type))` (the kind expression)

The function currently evaluates the LAST level (level 0 = signature) which gives `VLam("A", a => VLam("B", b => VType))`. But this is the VALUE of Function, not its TYPE. The checker's `infer(ValueReference)` needs the TYPE.

**The confusion**: `fetchEvaluatedSignature` folds ALL levels and returns the last result. For single-level stacks (like `BigInteger^Type`), this is correct (evaluating `ValueReference(Type^Type)` gives VType). For multi-level stacks, it should return the kind (last-but-one evaluation used as expected type), not the signature evaluation.

**Possible fixes**:
1. Change `fetchEvaluatedSignature` to evaluate the KIND level (the second level from the bottom, or VType for single-level stacks) instead of the signature level. The type/kind of a value IS the evaluation of the levels ABOVE its signature.
2. Or, evaluate the type stack correctly: for N levels, the TYPE of the value is determined by levels[1..N-1], not levels[0].

**How to verify the fix**: The test `"be monomorphized with one parameter"` (`data A\ndata B\ndef f(a: A): B`) in `Monomorphic3TypeCheckTest` should pass - it currently fails with "Not a function." because `Function(A)(B)` can't be evaluated.

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

## Next Steps

1. Fix `fetchEvaluatedSignature` to return the correct TYPE (kind) of values
2. Get all non-generic Step 4 tests passing
3. Move to Step 5 (generics) - see `docs/nbe-migration-plan.md`
