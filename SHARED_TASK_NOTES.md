# monomorphize3 NbE Implementation - Task Notes

## Current State: Steps 1-5 implemented

Steps 1-4 (skeleton, domain, evaluator, unifier, checker, TypeStackLoop) are complete with all tests green.
Step 5 (generics, polytype instantiation, explicit type args) is implemented with core tests passing.

## What Works

- `./mill lang.compile` passes
- All 33 monomorphize3 tests pass (9 ProcessorTest + 24 TypeCheckTest)
- No regressions to existing tests (29 pre-existing failures, same count)
- Non-generic: values, functions, literals, value references, parameter usage, error reporting
- Generic: explicit type args, implicit type arg inference via meta instantiation, multi-parameter generics

## Key Design Decisions Made

### VNative.fire takes SemValue (not GroundValue)
Changed from plan's `GroundValue => SemValue` to `SemValue => SemValue`. This allows Function native to handle VPi and VMeta inputs, which is essential for:
- Evaluating kind expressions like `Function(Type)(Function(Type)(Type))` where inner evaluation produces VPi
- Implicit type arg instantiation where fresh metas flow through the Function native

### VMeta passes through VNative (not stuck)
Only VNeutral blocks VNative application. VMeta is allowed to fire, enabling meta-based polytype instantiation. The Function native correctly propagates metas through VPi construction.

### Evaluator resolves ParameterReference from Env names
Added `Env.lookupByName` as primary resolution for ParameterReference (before nameLevels). This allows VLam closures created during fetchEvaluatedSignature (with empty nameLevels) to correctly resolve their captured parameters.

### TypeStackLoop.applyTypeArgs binds type params in Checker state
When applying explicit type args to a VLam signature, the corresponding type parameter names are also bound in the Checker's state. This ensures the runtime body check resolves type parameters to their monomorphized concrete types.

### Unifier errors carry source positions
Changed from `List[String]` to `List[Sourced[String]]` so errors point to the actual expression, not just the value name.

### VLam in applyInferred: meta-based polytype instantiation
When FunctionApplication encounters VLam as target type, creates fresh meta and instantiates. Recursive applyInferred then handles the resulting VPi. This naturally handles both single and multiple leading type parameters.

## Next Steps (Step 6+)

1. Port remaining Step 5 tests from monomorphize2: multi-parameter unification, more explicit type arg edge cases, error reporting for generic type mismatches
2. Step 6: Higher-kinded types and explicit kind restrictions
3. Step 7: Type-level computation (Box[one] unifying with Box[oneDifferently])
4. Step 8: Lambda inference, ability implementation resolution
5. Step 9: Recursion via lazy VTopDef
6. Step 10: Final parity audit

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
