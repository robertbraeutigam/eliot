# Consolidation Plan: eval, symbolic, and monomorphize Packages

## Overview

This document presents a refactoring plan to consolidate shared patterns and abstractions across the `eval`, `symbolic`, and `monomorphize` packages. These three packages form a coherent compilation pipeline with significant code duplication that can be eliminated through careful abstraction.

## Current State Analysis

### Package Purposes

| Package | Purpose | Input Fact | Output Fact |
|---------|---------|------------|-------------|
| `eval` | Runtime value evaluation using lambda calculus | `ResolvedValue` | `NamedEvaluable` |
| `symbolic` | Type checking with unification constraints | `ResolvedValue` | `TypeCheckedValue` |
| `monomorphize` | Generic function specialization | `TypeCheckedValue` | `MonomorphicValue` |

### Identified Duplication

1. **Expression Trees**: Three nearly identical sealed trait hierarchies
2. **Substitution Logic**: Three separate implementations of parameter substitution
3. **Tree Transformation**: Three similar tree traversal/transformation patterns
4. **Beta Reduction**: Duplicated reduction logic in `Evaluator` and `TypeEvaluator`

## Consolidation Strategy

The consolidation follows a bottom-up approach, starting with the most fundamental abstractions and building up to higher-level utilities.

---

## Phase 1: Generic Expression Tree

### Goal

Create a single parameterized expression tree that all three packages can use with different type annotations.

### Current Duplication

```scala
// eval/ExpressionValue.scala
sealed trait ExpressionValue
case class FunctionApplication(target: ExpressionValue, argument: ExpressionValue)
case class FunctionLiteral(paramName: String, paramType: Value, body: ExpressionValue)
case class ConcreteValue(value: Value)
case class ParameterReference(paramName: String, paramType: Value)
case class NativeFunction(paramType: Value, body: Value => ExpressionValue)

// symbolic/TypedExpression.scala
sealed trait Expression
case class FunctionApplication(target: TypeStack, argument: TypeStack)
case class FunctionLiteral(name: String, paramType: TypeStack, body: TypeStack)
case class ValueReference(valueName: ValueFQN)
case class ParameterReference(name: String)
case class IntegerLiteral(value: BigInt)
case class StringLiteral(value: String)

// monomorphize/MonomorphicExpression.scala
sealed trait Expression
case class FunctionApplication(target: MonomorphicExpression, argument: MonomorphicExpression)
case class FunctionLiteral(paramName: String, paramType: Value, body: MonomorphicExpression)
case class MonomorphicValueReference(valueName: ValueFQN, typeArguments: Seq[Value])
case class ParameterReference(name: String)
case class IntegerLiteral(value: BigInt)
case class StringLiteral(value: String)
```

### Proposed Abstraction

Create a new file in the `eval` package (as the most fundamental):

```scala
// eval/expression/GenericExpression.scala
package com.vanillasource.eliot.eliotc.eval.expression

/**
 * A generic expression tree parameterized by:
 * - T: The type annotation (Value, ExpressionValue, or TypeStack)
 * - R: The reference type for value references (can include type arguments)
 */
sealed trait GenericExpression[T, R]

object GenericExpression {
  case class FunctionApplication[T, R](
      target: GenericExpression[T, R],
      argument: GenericExpression[T, R]
  ) extends GenericExpression[T, R]

  case class FunctionLiteral[T, R](
      paramName: String,
      paramType: T,
      body: GenericExpression[T, R]
  ) extends GenericExpression[T, R]

  case class ValueReference[T, R](ref: R) extends GenericExpression[T, R]

  case class ParameterReference[T, R](
      name: String,
      paramType: Option[T]  // Some packages track type, others don't
  ) extends GenericExpression[T, R]

  case class IntegerLiteral[T, R](value: BigInt) extends GenericExpression[T, R]

  case class StringLiteral[T, R](value: String) extends GenericExpression[T, R]

  case class ConcreteValue[T, R](value: Value) extends GenericExpression[T, R]

  case class NativeFunction[T, R](
      paramType: T,
      body: Value => GenericExpression[T, R]
  ) extends GenericExpression[T, R]
}
```

### Type Aliases for Each Package

```scala
// eval package
type EvalExpression = GenericExpression[Value, ValueFQN]

// symbolic package
type SymbolicExpression = GenericExpression[ExpressionValue, ValueFQN]

// monomorphize package
case class MonomorphicRef(vfqn: ValueFQN, typeArgs: Seq[Value])
type MonomorphicExpr = GenericExpression[Value, MonomorphicRef]
```

### Migration Steps

1. Create `GenericExpression.scala` in `eval/expression/`
2. Add type aliases to each package
3. Migrate `ExpressionValue` to use `GenericExpression` (keep `ExpressionValue` as a wrapper for compatibility)
4. Migrate `TypedExpression.Expression` to use the alias
5. Migrate `MonomorphicExpression.Expression` to use the alias
6. Update pattern matches in all consumers

### Files to Modify

- `base/src/com/vanillasource/eliot/eliotc/eval/ExpressionValue.scala`
- `base/src/com/vanillasource/eliot/eliotc/symbolic/TypedExpression.scala`
- `base/src/com/vanillasource/eliot/eliotc/monomorphize/MonomorphicExpression.scala`
- All processors that pattern match on these types

### Estimated Impact

- **Lines removed**: ~80 (duplicated case class definitions)
- **Lines added**: ~50 (generic abstraction + type aliases)
- **Net reduction**: ~30 lines
- **Benefit**: Single place to add new expression variants

---

## Phase 2: Unified Substitution Framework

### Goal

Create a single substitution utility that can replace parameter references with bound values across all expression types.

### Current Duplication

```scala
// eval/Evaluator.scala
def substitute(expr: ExpressionValue, name: String, value: ExpressionValue): ExpressionValue

// symbolic/UnificationState.scala
def substitute(expr: ExpressionValue): ExpressionValue  // Uses internal substitution map

// monomorphize/TypeEvaluator.scala
def evaluateWithSubstitution(expr: ExpressionValue, substitution: Map[String, Value]): Value
```

### Proposed Abstraction

```scala
// eval/expression/Substitution.scala
package com.vanillasource.eliot.eliotc.eval.expression

object Substitution {

  /**
   * Substitute a single parameter reference with an expression.
   * Works on any GenericExpression type.
   */
  def single[T, R](
      expr: GenericExpression[T, R],
      paramName: String,
      replacement: GenericExpression[T, R],
      substituteInType: (T, String, GenericExpression[T, R]) => T = (t: T, _, _) => t
  ): GenericExpression[T, R] = expr match {
    case ParameterReference(name, _) if name == paramName =>
      replacement
    case FunctionApplication(target, arg) =>
      FunctionApplication(
        single(target, paramName, replacement, substituteInType),
        single(arg, paramName, replacement, substituteInType)
      )
    case FunctionLiteral(name, paramType, body) if name != paramName =>
      FunctionLiteral(
        name,
        substituteInType(paramType, paramName, replacement),
        single(body, paramName, replacement, substituteInType)
      )
    case lit: FunctionLiteral[T, R] =>
      lit  // Shadowed - don't substitute
    case other =>
      other
  }

  /**
   * Substitute multiple parameters at once.
   */
  def multiple[T, R](
      expr: GenericExpression[T, R],
      bindings: Map[String, GenericExpression[T, R]],
      substituteInType: (T, Map[String, GenericExpression[T, R]]) => T = (t: T, _) => t
  ): GenericExpression[T, R] = {
    bindings.foldLeft(expr) { case (e, (name, replacement)) =>
      single(e, name, replacement, (t, n, r) =>
        substituteInType(t, Map(n -> r))
      )
    }
  }
}
```

### Migration Steps

1. Create `Substitution.scala` in `eval/expression/`
2. Refactor `Evaluator.substitute()` to use `Substitution.single()`
3. Refactor `UnificationState.substitute()` to use the shared utility
4. Refactor `TypeEvaluator` to use the shared utility
5. Add package-specific type adapters where needed

### Files to Modify

- Create `base/src/com/vanillasource/eliot/eliotc/eval/expression/Substitution.scala`
- `base/src/com/vanillasource/eliot/eliotc/eval/Evaluator.scala`
- `base/src/com/vanillasource/eliot/eliotc/symbolic/UnificationState.scala`
- `base/src/com/vanillasource/eliot/eliotc/monomorphize/TypeEvaluator.scala`

### Estimated Impact

- **Lines removed**: ~60 (duplicated substitution logic)
- **Lines added**: ~40 (generic utility)
- **Net reduction**: ~20 lines
- **Benefit**: Guaranteed consistent substitution semantics

---

## Phase 3: Generic Tree Transformer

### Goal

Create a reusable tree transformation utility that can apply a function to all nodes in an expression tree.

### Current Duplication

```scala
// eval/ExpressionValue.scala
def transform(f: ExpressionValue => ExpressionValue): ExpressionValue

// symbolic/TypedExpression.scala
def transformTypes(f: ExpressionValue => ExpressionValue): TypedExpression

// monomorphize/MonomorphicTypeCheckProcessor.scala
def transformExpression(typed: TypedExpression): CompilerIO[MonomorphicExpression]
```

### Proposed Abstraction

```scala
// eval/expression/TreeTransformer.scala
package com.vanillasource.eliot.eliotc.eval.expression

trait TreeTransformer[F[_]] {
  /**
   * Transform an expression tree, applying functions to each node.
   * F is the effect type (Identity for pure, CompilerIO for effectful).
   */
  def transform[T1, R1, T2, R2](
      expr: GenericExpression[T1, R1]
  )(
      transformType: T1 => F[T2],
      transformRef: R1 => F[R2],
      transformExpr: GenericExpression[T1, R1] => F[GenericExpression[T2, R2]]
  ): F[GenericExpression[T2, R2]]
}

object TreeTransformer {
  /**
   * Pure transformer (no effects).
   */
  def pure: TreeTransformer[cats.Id] = new TreeTransformer[cats.Id] {
    def transform[T1, R1, T2, R2](expr: GenericExpression[T1, R1])(
        transformType: T1 => T2,
        transformRef: R1 => R2,
        transformExpr: GenericExpression[T1, R1] => GenericExpression[T2, R2]
    ): GenericExpression[T2, R2] = expr match {
      case FunctionApplication(target, arg) =>
        FunctionApplication(transform(target)(transformType, transformRef, transformExpr),
                           transform(arg)(transformType, transformRef, transformExpr))
      case FunctionLiteral(name, paramType, body) =>
        FunctionLiteral(name, transformType(paramType),
                       transform(body)(transformType, transformRef, transformExpr))
      case ValueReference(ref) =>
        ValueReference(transformRef(ref))
      case ParameterReference(name, pt) =>
        ParameterReference(name, pt.map(transformType))
      case IntegerLiteral(v) => IntegerLiteral(v)
      case StringLiteral(v) => StringLiteral(v)
      case ConcreteValue(v) => ConcreteValue(v)
      case NativeFunction(pt, body) =>
        // NativeFunction bodies can't be statically transformed
        throw new UnsupportedOperationException("Cannot transform NativeFunction")
    }
  }

  /**
   * Effectful transformer for CompilerIO.
   */
  def effectful: TreeTransformer[CompilerIO] = ???  // Similar but with traverse
}
```

### Migration Steps

1. Create `TreeTransformer.scala` in `eval/expression/`
2. Refactor `ExpressionValue.transform()` to use `TreeTransformer.pure`
3. Refactor `TypedExpression.transformTypes()` to use the utility
4. Refactor `MonomorphicTypeCheckProcessor.transformExpression()` to use `TreeTransformer.effectful`

### Files to Modify

- Create `base/src/com/vanillasource/eliot/eliotc/eval/expression/TreeTransformer.scala`
- `base/src/com/vanillasource/eliot/eliotc/eval/ExpressionValue.scala`
- `base/src/com/vanillasource/eliot/eliotc/symbolic/TypedExpression.scala`
- `base/src/com/vanillasource/eliot/eliotc/monomorphize/MonomorphicTypeCheckProcessor.scala`

### Estimated Impact

- **Lines removed**: ~50 (duplicated traversal logic)
- **Lines added**: ~60 (generic transformer with pure + effectful variants)
- **Net change**: +10 lines
- **Benefit**: Consistent traversal semantics, easier to add new transformations

---

## Phase 4: Shared Beta Reduction Engine

### Goal

Consolidate the beta reduction logic used by `Evaluator` and `TypeEvaluator` into a single engine.

### Current Duplication

```scala
// eval/Evaluator.scala
def reduce(expr: ExpressionValue): ExpressionValue = expr match {
  case FunctionApplication(FunctionLiteral(paramName, _, body), argument) =>
    reduce(substitute(body, paramName, argument))
  case FunctionApplication(NativeFunction(_, body), argument) =>
    reduce(body(toValue(reduce(argument))))
  // ...
}

// monomorphize/TypeEvaluator.scala
def evaluate(expr: ExpressionValue, typeArguments: Seq[Value]): Value = {
  // Similar reduction logic with type argument substitution
  Evaluator.reduce(substitutedExpr) match {
    case ConcreteValue(v) => v
    // ...
  }
}
```

### Proposed Abstraction

```scala
// eval/expression/BetaReducer.scala
package com.vanillasource.eliot.eliotc.eval.expression

object BetaReducer {

  /**
   * Reduce an expression to normal form using beta reduction.
   *
   * @param expr The expression to reduce
   * @param resolveValue Callback to resolve value references to their definitions
   * @param maxSteps Maximum reduction steps (prevents infinite loops)
   * @return Reduced expression, or error if stuck
   */
  def reduce[T, R](
      expr: GenericExpression[T, R],
      resolveValue: R => Option[GenericExpression[T, R]],
      maxSteps: Int = 1000
  ): Either[ReductionError, GenericExpression[T, R]] = {

    def step(e: GenericExpression[T, R], steps: Int): Either[ReductionError, GenericExpression[T, R]] = {
      if (steps >= maxSteps) Left(ReductionError.MaxStepsExceeded(expr))
      else e match {
        // Beta reduction: (λx.body) arg → body[x := arg]
        case FunctionApplication(FunctionLiteral(paramName, _, body), argument) =>
          step(Substitution.single(body, paramName, argument), steps + 1)

        // Delta reduction: unfold value reference
        case ValueReference(ref) =>
          resolveValue(ref) match {
            case Some(definition) => step(definition, steps + 1)
            case None => Right(e)  // Opaque/external value
          }

        // Reduce under application
        case FunctionApplication(target, arg) =>
          for {
            reducedTarget <- step(target, steps + 1)
            result <- (reducedTarget, arg) match {
              case (FunctionLiteral(_, _, _), _) =>
                step(FunctionApplication(reducedTarget, arg), steps + 1)
              case _ =>
                Right(FunctionApplication(reducedTarget, arg))
            }
          } yield result

        // Values are already reduced
        case _ => Right(e)
      }
    }

    step(expr, 0)
  }

  sealed trait ReductionError
  object ReductionError {
    case class MaxStepsExceeded(expr: Any) extends ReductionError
    case class StuckTerm(expr: Any) extends ReductionError
  }
}
```

### Migration Steps

1. Create `BetaReducer.scala` in `eval/expression/`
2. Refactor `Evaluator.reduce()` to use `BetaReducer.reduce()`
3. Refactor `TypeEvaluator.evaluate()` to use `BetaReducer.reduce()`
4. Add adapters for `NativeFunction` handling (eval-specific)

### Files to Modify

- Create `base/src/com/vanillasource/eliot/eliotc/eval/expression/BetaReducer.scala`
- `base/src/com/vanillasource/eliot/eliotc/eval/Evaluator.scala`
- `base/src/com/vanillasource/eliot/eliotc/monomorphize/TypeEvaluator.scala`

### Estimated Impact

- **Lines removed**: ~40 (duplicated reduction)
- **Lines added**: ~60 (generic reducer with error handling)
- **Net change**: +20 lines
- **Benefit**: Single reduction algorithm, better error handling, configurable limits

---

## Phase 5: Value Construction Utilities

### Goal

Consolidate scattered value construction patterns into a cohesive utility module.

### Current Duplication

```scala
// eval/Types.scala
def dataType(name: String): Value =
  Value.Direct(name, Value.Type)
def functionType(returnType: Value): ExpressionValue.InitialExpressionValue =
  ???

// eval/DataTypeEvaluator.scala
Value.Structure(
  resolvedDataType.typeReference.value.fields.map(...).toMap,
  Value.Direct(...)
)

// eval/FunctionDataTypeEvaluator.scala
Value.Direct("Function", Value.Type)
```

### Proposed Abstraction

```scala
// eval/expression/Values.scala
package com.vanillasource.eliot.eliotc.eval.expression

object Values {

  /**
   * Create a type value (the type of types).
   */
  val Type: Value = Value.Type

  /**
   * Create a named data type.
   */
  def dataType(vfqn: ValueFQN): Value =
    Value.Direct(vfqn, Value.Type)

  /**
   * Create a function type.
   */
  def functionType(paramType: Value, returnType: Value): Value =
    Value.Structure(
      Map("parameterType" -> paramType, "returnType" -> returnType),
      dataType(ValueFQN.fromString("Function"))
    )

  /**
   * Create a structure value.
   */
  def structure(typeRef: ValueFQN, fields: (String, Value)*): Value =
    Value.Structure(fields.toMap, dataType(typeRef))

  /**
   * Extract function type components.
   */
  object FunctionType {
    def unapply(value: Value): Option[(Value, Value)] = value match {
      case Value.Structure(fields, Value.Direct(vfqn, _))
        if vfqn.toString.endsWith("Function") =>
        for {
          paramType <- fields.get("parameterType")
          returnType <- fields.get("returnType")
        } yield (paramType, returnType)
      case _ => None
    }
  }
}
```

### Migration Steps

1. Create `Values.scala` in `eval/expression/`
2. Migrate `Types.scala` utilities to use `Values`
3. Update `DataTypeEvaluator` to use `Values.structure()`
4. Update `FunctionDataTypeEvaluator` to use `Values.functionType()`
5. Update pattern matches to use `Values.FunctionType` extractor

### Files to Modify

- Create `base/src/com/vanillasource/eliot/eliotc/eval/expression/Values.scala`
- `base/src/com/vanillasource/eliot/eliotc/eval/Types.scala` (deprecate or remove)
- `base/src/com/vanillasource/eliot/eliotc/eval/DataTypeEvaluator.scala`
- `base/src/com/vanillasource/eliot/eliotc/eval/FunctionDataTypeEvaluator.scala`

### Estimated Impact

- **Lines removed**: ~30 (scattered construction patterns)
- **Lines added**: ~50 (centralized utilities with extractors)
- **Net change**: +20 lines
- **Benefit**: Type-safe construction, pattern matching support

---

## Implementation Order

The phases should be implemented in this order due to dependencies:

```
Phase 1: GenericExpression (foundation)
    ↓
Phase 2: Substitution (uses GenericExpression)
    ↓
Phase 3: TreeTransformer (uses GenericExpression)
    ↓
Phase 4: BetaReducer (uses Substitution)
    ↓
Phase 5: Values (independent, can be done in parallel with 2-4)
```

## New Package Structure

After consolidation:

```
base/src/com/vanillasource/eliot/eliotc/
├── eval/
│   ├── expression/           # NEW: Shared abstractions
│   │   ├── GenericExpression.scala
│   │   ├── Substitution.scala
│   │   ├── TreeTransformer.scala
│   │   ├── BetaReducer.scala
│   │   └── Values.scala
│   ├── Value.scala          # Unchanged
│   ├── ExpressionValue.scala # Simplified, uses GenericExpression
│   ├── Evaluator.scala      # Uses shared utilities
│   └── ...
├── symbolic/
│   ├── TypedExpression.scala # Uses GenericExpression alias
│   └── ...
└── monomorphize/
    ├── MonomorphicExpression.scala # Uses GenericExpression alias
    └── ...
```

## Summary

| Phase | New Abstraction | Lines Removed | Lines Added | Net |
|-------|-----------------|---------------|-------------|-----|
| 1 | GenericExpression | ~80 | ~50 | -30 |
| 2 | Substitution | ~60 | ~40 | -20 |
| 3 | TreeTransformer | ~50 | ~60 | +10 |
| 4 | BetaReducer | ~40 | ~60 | +20 |
| 5 | Values | ~30 | ~50 | +20 |
| **Total** | | **~260** | **~260** | **0** |

While the net line count is roughly neutral, the benefits are:

1. **Single source of truth** for expression tree structure
2. **Consistent semantics** for substitution and reduction
3. **Easier to extend** - add new expression types in one place
4. **Better testability** - shared utilities can be tested in isolation
5. **Reduced cognitive load** - developers learn one pattern instead of three

## Risks and Mitigations

### Risk 1: Breaking Existing Functionality
**Mitigation**: Implement each phase behind feature flags or in parallel with existing code. Run full test suite after each phase.

### Risk 2: Performance Regression
**Mitigation**: Generic abstractions may add indirection. Profile before/after each phase. Optimize hot paths if needed.

### Risk 3: Complexity of Generic Types
**Mitigation**: Provide clear type aliases for each use case. Add comprehensive documentation with examples.

### Risk 4: Migration Burden
**Mitigation**: Implement incrementally. Keep backward-compatible shims during transition. Remove old code only after new code is validated.

## Success Criteria

1. All existing tests pass
2. No new code duplication across the three packages
3. Adding a new expression type requires changes in only one file
4. Clear documentation for the shared abstractions
5. Performance within 5% of current implementation
