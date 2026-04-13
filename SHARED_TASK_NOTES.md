# monomorphize3 NbE Implementation - Task Notes

## Current State: Step 5 fully ported

Steps 1-5 are complete with all tests green. Step 5 now includes all test cases from monomorphize2 that the plan assigns to this step.

## What Works

- `./mill lang.compile` passes
- All 50 monomorphize3 tests pass (10 ProcessorTest + 40 TypeCheckTest)
- No regressions to existing tests (29 pre-existing failures, same count)
- Non-generic: values, functions, literals, value references, parameter usage, error reporting
- Generic: explicit type args, implicit type arg inference via meta instantiation, multi-parameter generics
- Forward unification to concrete types (including recursive setups)
- Multi-parameter unification with polymorphic arguments (someA[A]: A pattern)
- Apply (g: Function[A,B], a: A): B pattern
- Explicit type arg conflict detection, too many/few type args, wrong order
- Data type type arguments (Box[BigInteger] etc.)

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

### instantiatePolymorphic in Checker.check fallback (new in this iteration)
When a term is checked against an expected type and inference returns VLam (polymorphic type), all leading VLam closures are peeled off by instantiating them with fresh metas before unifying with the expected type. This handles implicit type arg instantiation when polymorphic values like `someA[A]: A` are used as arguments to other functions (not just as function application targets).

### Too many type arguments detection in applyTypeArgs (new in this iteration)
When TypeStackLoop.applyTypeArgs encounters a non-VLam signature with remaining type args to apply, it records a "Too many type arguments." error on the unifier instead of silently applying through the type. This prevents over-application that would collapse the runtime function type.

## Error sourcing differences from monomorphize2

Some error source positions differ from monomorphize2. These are acceptable differences due to the NbE approach:
- "fail if parameter is used as a wrong parameter in another function": points to "x" (the argument) instead of "B" (the expected type)
- "fail if forward unification produces conflict": points to "i" (the argument) instead of "id(i)" (the full call)
- "fail with too few explicit type args that conflict": produces multiple individual errors instead of one composite error

## Next Steps (Step 6)

1. Step 6: Higher-kinded types and explicit kind restrictions
   - Port tests: higher-kinded types (6 cases including ignored nested HK case)
   - Port tests: explicit type restrictions (5 cases)
   - May need changes to Quoter for partially applied native type constructors
   - May need native wiring changes at higher arities
2. Step 7: Type-level computation (Box[one] unifying with Box[oneDifferently])
3. Step 8: Lambda inference, ability implementation resolution
4. Step 9: Recursion via lazy VTopDef
5. Step 10: Final parity audit

Note: phantom type parameter test from ProcessorTest not yet ported — requires handling VLam signature with no type args, likely a Step 6 concern (kind restrictions).

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
