# monomorphize3 NbE Implementation - Task Notes

## Current State: Step 6 fully ported

Steps 1-6 are complete with all tests green. Step 6 adds higher-kinded types and explicit kind restrictions.

## What Works

- `./mill lang.compile` passes
- All 62 monomorphize3 tests pass (11 ProcessorTest + 51 TypeCheckTest + 1 ignored nested HK)
- No regressions to existing tests (~30 pre-existing failures in other packages, unchanged)
- Everything from Steps 1-5 plus:
- Higher-kinded types: single generic placeholder, lower arities, parameter returning identity, two type args, mismatch detection
- Explicit type restrictions: explicit Type restriction, Function[Type,Type] restriction, two-arg Function restriction, nested HK restriction, HK type invoking its parameter
- Phantom type parameters (VLam with no type args → fresh meta instantiation)

## Key Design Decisions Made

### All decisions from Steps 1-5 still apply (see git history)

### Unifier postpones non-pattern meta spines (new in Step 6)
When `solveMeta` encounters an unsolved meta with a non-empty spine (e.g., `?A[?B]`), it postpones instead of solving directly. This prevents incorrect solutions where the spine is ignored (e.g., solving `?A = BigInteger` when the constraint is actually `?A(?B) = BigInteger`). The drain loop was also fixed to recognize re-postponed constraints as "no progress" to avoid infinite loops.

### TypeStackLoop.instantiateRemaining peels off unapplied VLam (new in Step 6)
After applying explicit type args, any remaining VLam closures (unapplied type parameters) are instantiated with fresh metas. This handles:
- Phantom type parameters: `def f[I: BigInteger]: String` → VLam peeled off, signature = String
- Ensures the signature is always quotable to GroundValue

### Higher-kinded types work through NbE naturally
No domain changes were needed. VPi is fully general. Higher-kinded type constructors (Box[_], Function[_,_]) are VNative chains that fire when applied. When passed as type args, they are bound as SemValues and applied normally. The postponement mechanism in the unifier handles cases where higher-kinded metas can't be directly solved (e.g., `?A[?B] = VPi(BigInteger, _ => String)` is postponed, and the lack of errors means the program type-checks).

## Next Steps (Step 7)

1. Step 7: Type-level computation (Box[one] unifying with Box[oneDifferently])
   - Port tests: type level functions (6 cases)
   - `force` must unfold VTopDef when spine is fully VConst
   - Unifier on VConst does structural GroundValue equality (already implemented)
   - Postponement handles metas not yet solved
2. Step 8: Lambda inference, ability implementation resolution
3. Step 9: Recursion via lazy VTopDef
4. Step 10: Final parity audit

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
