# ExpressionValue Tree Operations Unification Plan

## Context

Multiple processors in the `base` module implement nearly identical tree traversal and manipulation
operations on `ExpressionValue`. The `ExpressionValue` companion object already serves as the
canonical home for tree utilities (`containsVar`, `transform`, `concreteValueOf`, `functionType`,
`FunctionType` extractor). This plan proposes extracting three more duplicated operations into that
companion object.

## Duplications Found

### 1. Strip Leading FunctionLiterals (3 locations)

Three processors strip `FunctionLiteral` wrappers from the front of an `ExpressionValue`:

**`TypeEvaluator.stripTypeParams`** (monomorphize/processor/TypeEvaluator.scala:126):
```scala
@tailrec
def stripTypeParams(expr: ExpressionValue): ExpressionValue =
  expr match {
    case FunctionLiteral(_, _, body) => stripTypeParams(body)
    case other                       => other
  }
```

**`UncurryingProcessor.dropLambdas`** (uncurry/processor/UncurryingProcessor.scala:67):
```scala
@tailrec
private def dropLambdas(signature: ExpressionValue): ExpressionValue =
  signature match {
    case ExpressionValue.FunctionLiteral(_, _, body) => dropLambdas(body)
    case _                                           => signature
  }
```

**`SymbolicTypeCheckProcessor.stripUniversalIntros`** (symbolic/processor/SymbolicTypeCheckProcessor.scala:98):
```scala
@tailrec
private def stripUniversalIntros(expr: ExpressionValue): ExpressionValue =
  expr match {
    case ExpressionValue.FunctionLiteral(_, Value.Type, body) => stripUniversalIntros(body)
    case other                                                => other
  }
```

The first two are **identical**. The third is a semantic refinement that only strips lambdas where
`paramType == Value.Type`.

### 2. Extract Leading FunctionLiteral Parameters (2 locations)

**`TypeEvaluator.extractTypeParams`** (monomorphize/processor/TypeEvaluator.scala:117):
```scala
def extractTypeParams(signature: ExpressionValue): Seq[String] =
  signature match {
    case FunctionLiteral(name, _, body) => name +: extractTypeParams(body)
    case _                              => Seq.empty
  }
```

**`DataTypeEvaluator.extractParameters`** (eval/processor/DataTypeEvaluator.scala:40):
```scala
private def extractParameters(body: ExpressionValue): Seq[(String, Value)] =
  body match {
    case FunctionLiteral(name, paramType, innerBody) =>
      (name, paramType) +: extractParameters(innerBody)
    case _                                           => Seq.empty
  }
```

Both walk leading `FunctionLiterals` collecting data. The first extracts names only, the second
extracts `(name, type)` pairs. If we provide the richer `(String, Value)` version, the name-only
variant is `.map(_._1)`.

### 3. Capture-Avoiding Substitution (1 location, but fundamental)

**`Evaluator.substitute`** (eval/util/Evaluator.scala:128):
```scala
private def substitute(body: ExpressionValue, paramName: String, argValue: ExpressionValue): ExpressionValue =
  body match {
    case ParameterReference(name, _) if name == paramName                 => argValue
    case ParameterReference(_, _)                                         => body
    case FunctionApplication(target, arg)                                 =>
      FunctionApplication(substitute(target, paramName, argValue), substitute(arg, paramName, argValue))
    case FunctionLiteral(name, paramType, innerBody) if name != paramName =>
      FunctionLiteral(name, paramType, substitute(innerBody, paramName, argValue))
    case _                                                                => body
  }
```

Only one call site, but this is a fundamental tree operation (as fundamental as `containsVar` and
`transform` which already live in the companion). Making it public in the companion canonicalizes it
alongside the other tree operations.

## What is NOT Worth Unifying

- **Operations on different expression types** (TypedExpression, MonomorphicExpression, resolved
  Expression, CoreExpression) -- structurally similar but different ADTs; type class machinery
  would add more complexity than it saves.
- **`isKindAnnotation`/`isKindExpression`** in ValueResolver vs TypeStackBuilder -- different
  expression types (CoreExpression vs resolved Expression).
- **`flattenApplication`/`flattenLambda`** in UncurryingProcessor -- only one call site each.
- **`matchTypes`** in MonomorphicTypeCheckProcessor -- only one call site.
- **`processApplicationChain`** in UsedNamesProcessor -- operates on MonomorphicExpression.
- **Effectful `transformM`** -- only one candidate call site (`TypeEvaluator.resolveDataTypeRefs`),
  and that traversal has custom non-uniform logic that wouldn't fit a generic signature.
- **`SymbolicTypeCheckProcessor.stripUniversalIntros`** -- enforces a specific semantic constraint
  (`Value.Type` only), making it a meaningful refinement rather than pure duplication. At 4 lines,
  keeping it private is fine.

## Proposed Changes

### Step 1: Add three methods to `ExpressionValue` companion object

**File:** `base/src/com/vanillasource/eliot/eliotc/eval/fact/ExpressionValue.scala`

Add after the existing `transform` method (around line 31), plus `import scala.annotation.tailrec`:

```scala
/** Strip all leading FunctionLiteral wrappers, returning the innermost body. */
@tailrec
def stripLeadingLambdas(expr: ExpressionValue): ExpressionValue =
  expr match {
    case FunctionLiteral(_, _, body) => stripLeadingLambdas(body)
    case other                       => other
  }

/** Extract parameter names and types from leading FunctionLiteral wrappers. */
def extractLeadingLambdaParams(expr: ExpressionValue): Seq[(String, Value)] =
  expr match {
    case FunctionLiteral(name, paramType, body) => (name, paramType) +: extractLeadingLambdaParams(body)
    case _                                      => Seq.empty
  }

/** Capture-avoiding substitution: replace all free occurrences of paramName with argValue. */
def substitute(body: ExpressionValue, paramName: String, argValue: ExpressionValue): ExpressionValue =
  body match {
    case ParameterReference(name, _) if name == paramName                 => argValue
    case ParameterReference(_, _)                                         => body
    case FunctionApplication(target, arg)                                 =>
      FunctionApplication(substitute(target, paramName, argValue), substitute(arg, paramName, argValue))
    case FunctionLiteral(name, paramType, innerBody) if name != paramName =>
      FunctionLiteral(name, paramType, substitute(innerBody, paramName, argValue))
    case _                                                                => body
  }
```

### Step 2: Update TypeEvaluator

**File:** `base/src/com/vanillasource/eliot/eliotc/monomorphize/processor/TypeEvaluator.scala`

- **Delete** `stripTypeParams` method (lines 125-130) entirely.
- **Simplify** `extractTypeParams` to delegate:
  ```scala
  def extractTypeParams(signature: ExpressionValue): Seq[String] =
    extractLeadingLambdaParams(signature).map(_._1)
  ```
  (Kept as thin wrapper since it's called from multiple places in MonomorphicTypeCheckProcessor.)
- Remove `scala.annotation.tailrec` import if no longer needed.

### Step 3: Update MonomorphicTypeCheckProcessor

**File:** `base/src/com/vanillasource/eliot/eliotc/monomorphize/processor/MonomorphicTypeCheckProcessor.scala`

- Change `TypeEvaluator.stripTypeParams(...)` calls (lines 37, 124) to
  `stripLeadingLambdas(...)` (already imports `ExpressionValue.*`).

### Step 4: Update UncurryingProcessor

**File:** `base/src/com/vanillasource/eliot/eliotc/uncurry/processor/UncurryingProcessor.scala`

- **Delete** `dropLambdas` method (lines 66-71).
- Replace call on line 34: `dropLambdas(typeCheckedValue.signature)` becomes
  `ExpressionValue.stripLeadingLambdas(typeCheckedValue.signature)`.
- Keep `scala.annotation.tailrec` import (still needed for `flattenApplication`/`flattenLambda`).

### Step 5: Update DataTypeEvaluator

**File:** `base/src/com/vanillasource/eliot/eliotc/eval/processor/DataTypeEvaluator.scala`

- **Delete** `extractParameters` method (lines 40-46).
- Replace call on line 33: `extractParameters(evaluated)` becomes
  `ExpressionValue.extractLeadingLambdaParams(evaluated)`.

### Step 6: Update Evaluator

**File:** `base/src/com/vanillasource/eliot/eliotc/eval/util/Evaluator.scala`

- **Delete** private `substitute` method (lines 128-137).
- Update call on line 97: `substitute(body, paramName, reducedArg)` -- since `ExpressionValue.*`
  is already imported, unqualified `substitute(body, paramName, reducedArg)` should resolve to
  the companion object method.

### Step 7: Move/update tests

- **Move** `stripTypeParams` tests from `TypeEvaluatorTest.scala` to a new
  `base/test/src/com/vanillasource/eliot/eliotc/eval/fact/ExpressionValueTest.scala`, renamed
  to test `stripLeadingLambdas`.
- **Add** tests for `extractLeadingLambdaParams` and `substitute` in the same new test file.
- **Keep** `extractTypeParams` tests in `TypeEvaluatorTest.scala` since the thin wrapper remains.

## Verification

1. `mill base.test` -- all existing tests pass
2. `mill __.compile` -- no compilation errors across all modules
3. `mill jvm.test` -- JVM backend tests still pass (uses expression types transitively)
4. `mill examples.run jvm exe-jar examples/src/ stdlib/src/ jvm/lib/ -m HelloWorld` -- end-to-end check

## Summary

| File | Change |
|------|--------|
| `ExpressionValue.scala` | +3 new methods (~25 lines) |
| `TypeEvaluator.scala` | Delete `stripTypeParams`, simplify `extractTypeParams` to 2-line delegate |
| `MonomorphicTypeCheckProcessor.scala` | Update 2 call sites |
| `UncurryingProcessor.scala` | Delete `dropLambdas`, update 1 call site |
| `DataTypeEvaluator.scala` | Delete `extractParameters`, update 1 call site |
| `Evaluator.scala` | Delete `substitute`, update 1 call site |
| `ExpressionValueTest.scala` (new) | Tests for the 3 new companion methods |
