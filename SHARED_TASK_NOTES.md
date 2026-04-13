# monomorphize3 NbE Implementation - Task Notes

## Current State: Step 8 fully ported

Steps 1-8 are complete with all tests green. Step 8 adds lambda inference and ability implementation resolution.

## What Works

- `./mill lang.compile` passes
- All 71 monomorphize3 tests pass (12 ProcessorTest + 59 TypeCheckTest + 1 ignored nested HK)
- No regressions to existing tests (~29 pre-existing failures in other packages, unchanged)
- Everything from Steps 1-7 plus:
- Lambda type inference: unannotated lambdas infer parameter type from VPi context
- Ability implementation resolution: ability method references resolve to concrete implementations via paramConstraints

## Key Design Decisions Made

### All decisions from Steps 1-7 still apply (see git history)

### Ability resolution uses paramConstraints (new in Step 8)
The Checker receives `paramConstraints` from the OperatorResolvedValue being monomorphized. When `infer(ValueReference)` encounters a ValueFQN with `Ability` qualifier, it:
1. Finds the matching constraint by ability name in `paramConstraints`
2. Evaluates the constraint's typeArgs (which already include the constrained parameter, e.g., `A ~ Show` has typeArgs=[ParameterReference("A")])
3. Converts to eval.fact.Value via `GroundValue.toEvalValue`
4. Looks up `AbilityImplementation` fact via a callback
5. Replaces the reference FQN with the implementation's FQN

### GroundValue.toEvalValue handles ValueFQN specially (new in Step 8)
When converting `GroundValue.Direct(valueFQN, _)` to `Value.Direct`, it uses `Types.fullyQualifiedNameType` instead of the GroundValue's valueType. This ensures the Value matches the eval package's representation for AbilityImplementation fact lookups.

### Constraint typeArgs already include the constrained parameter (new in Step 8)
The AST parser's `GenericParameter.extendWithDefault` adds the generic parameter to the ability constraint when no explicit type args are provided. So `A ~ Show` becomes `ResolvedAbilityConstraint(ShowFQN, [ParameterReference("A")])`, NOT `ResolvedAbilityConstraint(ShowFQN, [])`. Don't append the parameter value again.

### Unannotated lambda check against VPi (already existed, confirmed in Step 8)
`check(FunctionLiteral(_, None, body), VPi(d, c))` uses VPi's domain directly as the parameter type. This was implemented in an earlier step but the tests confirming it were added in Step 8.

## Next Steps (Step 9)

1. Step 9: Recursion via lazy VTopDef
   - Port tests: direct recursion without infinite loop (2 cases), mutual recursion without infinite loop (2 cases)
   - UserValueNativesProcessor represents each value as VTopDef(fqn, lazy thunk, SNil)
   - force(VTopDef) unfolds only when spine is fully VConst
   - Two VTopDefs with same FQN unify by FQN equality + spine unification
2. Step 10: Final parity audit

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
