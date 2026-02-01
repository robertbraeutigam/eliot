# Synergies Between eval, symbolic, and monomorphize Packages

## Executive Summary

This document analyzes potential synergies between three closely related packages in the ELIOT compiler:
- **eval**: Standard runtime evaluator for compile-time execution
- **symbolic**: Constraint-based type checker using unification
- **monomorphize**: Specializes generic polymorphic functions to concrete types

The main opportunities for synergy are:
1. Unifying type evaluation into a single reusable component
2. Having symbolic emit pre-processed type information to simplify monomorphize
3. Extracting common traversal patterns into shared utilities

---

## Current Architecture

### Package Overview

```
ResolvedValue (from resolve2)
        │
        ├──────────────────────────────────────────┐
        │                                          │
        ▼                                          ▼
┌───────────────────────┐               ┌──────────────────────────┐
│    eval/Evaluator     │               │  symbolic/TypeCheckProc  │
│                       │               │                          │
│  Expression -> EV     │               │  ResolvedValue -> TCV    │
│  (full beta-reduce)   │               │  (unification-based)     │
└───────────────────────┘               └──────────────────────────┘
        │                                          │
        ▼                                          ▼
  NamedEvaluable                           TypeCheckedValue
                                                   │
                                                   ▼
                                    ┌──────────────────────────────┐
                                    │  monomorphize/TypeChecker    │
                                    │                              │
                                    │  TCV + Args -> MonoValue     │
                                    │  (substitution-based)        │
                                    └──────────────────────────────┘
                                                   │
                                                   ▼
                                            MonomorphicValue

EV = ExpressionValue, TCV = TypeCheckedValue
```

### Key Data Types

| Package | Input | Output | Type Representation |
|---------|-------|--------|---------------------|
| eval | `Expression` | `ExpressionValue` | `ExpressionValue` (may contain unevaluated parts) |
| symbolic | `ResolvedValue` | `TypeCheckedValue` | `ExpressionValue` (fully resolved after unification) |
| monomorphize | `TypeCheckedValue` | `MonomorphicValue` | `Value` (ground types only) |

---

## Analysis of Evaluators

### 1. eval/Evaluator.scala

**Purpose**: Full expression evaluation with beta reduction

**Operation**: `Expression -> ExpressionValue`

**Key Features**:
- Evaluates resolved expressions to `ExpressionValue`
- Performs beta reduction (function application)
- Tracks recursion to detect infinite loops
- Uses `paramContext: Map[String, Value]` for parameter bindings
- Fetches `NamedEvaluable` facts for value references

**Code Pattern**:
```scala
def evaluate(expression: Sourced[Expression]): CompilerIO[ExpressionValue]
// Involves: evaluateToValue -> reduce -> applyOrKeep
```

### 2. monomorphize/TypeEvaluator.scala

**Purpose**: Evaluate symbolic types to ground types with substitution

**Operation**: `ExpressionValue -> Value` (with substitution map)

**Key Features**:
- Takes already-type-checked `ExpressionValue`
- Uses `substitution: Map[String, Value]` for type parameter bindings
- **No beta reduction** - just substitutes and constructs ground types
- Handles universal parameter extraction and stripping

**Code Pattern**:
```scala
def evaluate(expr: ExpressionValue, substitution: Map[String, Value]): CompilerIO[Value]
// Plus: extractUniversalParams, stripUniversalIntros, buildSubstitution
```

### Comparison

| Aspect | eval/Evaluator | monomorphize/TypeEvaluator |
|--------|----------------|---------------------------|
| Input | `Expression` | `ExpressionValue` |
| Output | `ExpressionValue` | `Value` |
| Parameter binding | `Map[String, Value]` | `Map[String, Value]` |
| Beta reduction | Yes | No |
| Fact fetching | Yes (`NamedEvaluable`) | No |
| Recursion detection | Yes | No |

**Current Duplication**:
- Both traverse expression/type structures
- Both handle parameter substitution
- Both convert to/from `Value`

---

## Proposed Synergies

### Synergy 1: Unified Type Evaluation

**Problem**: `TypeEvaluator` in monomorphize duplicates some logic from `Evaluator` but works on different input types.

**Proposal**: Create a shared type evaluation component in the `eval` package.

```scala
// eval/util/TypeEval.scala (new)
object TypeEval {
  /**
   * Evaluate an ExpressionValue to a ground Value with parameter substitution.
   * This is a simplified evaluation that doesn't do beta reduction.
   */
  def toValue(
      expr: ExpressionValue,
      substitution: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Value]
}
```

**Benefits**:
- Single source of truth for type evaluation
- `monomorphize/TypeEvaluator` becomes a thin wrapper or is eliminated
- Clear separation: `Evaluator` for full evaluation, `TypeEval` for type-level substitution

**Migration Path**:
1. Move `TypeEvaluator.evaluate` to `eval/util/TypeEval`
2. Keep universal param helpers in monomorphize (they're monomorphize-specific)
3. Have monomorphize import from eval

---

### Synergy 2: Pre-processed Type Information from Symbolic

**Problem**: Monomorphize must re-parse `TypeCheckedValue.signature` to:
1. Extract universal type parameters (`extractUniversalParams`)
2. Strip universal introductions (`stripUniversalIntros`)

This parsing duplicates knowledge about how symbolic encodes universal parameters.

**Current TypeCheckedValue**:
```scala
case class TypeCheckedValue(
    vfqn: ValueFQN,
    name: Sourced[String],
    signature: ExpressionValue,  // Contains universal intros baked in
    runtime: Option[Sourced[TypedExpression.Expression]]
)
```

**Proposal**: Extend `TypeCheckedValue` to include pre-extracted information:

```scala
case class TypeCheckedValue(
    vfqn: ValueFQN,
    name: Sourced[String],
    universalParams: Seq[String],     // NEW: Pre-extracted universal params
    signatureBody: ExpressionValue,   // NEW: Signature with universal intros stripped
    runtime: Option[Sourced[TypedExpression.Expression]]
)
```

**Alternative**: Keep `signature` as-is but add computed accessors:

```scala
case class TypeCheckedValue(...) {
  lazy val universalParams: Seq[String] = extractParams(signature)
  lazy val signatureBody: ExpressionValue = stripIntros(signature)
}
```

**Benefits**:
- Monomorphize doesn't need to understand how symbolic encodes universals
- Single point of definition for universal parameter encoding
- Cleaner separation of concerns

**Migration Path**:
1. Add `universalParams` and `signatureBody` fields to `TypeCheckedValue`
2. Update `SymbolicTypeCheckProcessor` to compute these when building the fact
3. Update `MonomorphicTypeCheckProcessor` to use pre-computed values
4. Remove `extractUniversalParams` and `stripUniversalIntros` from `TypeEvaluator`

---

### Synergy 3: Shared Expression Traversal Utilities

**Problem**: All three packages traverse `ExpressionValue` structures with similar patterns:
- `Evaluator.substitute`
- `TypeEvaluator.evaluate`
- `SymbolicUnification.unify`
- `ExpressionValue.transform`

**Proposal**: Enhance `ExpressionValue` companion with more powerful traversal utilities:

```scala
object ExpressionValue {
  // Already exists
  def transform(expr: ExpressionValue, f: ExpressionValue => ExpressionValue): ExpressionValue
  def containsVar(expr: ExpressionValue, varName: String): Boolean

  // NEW: Substitution helper
  def substitute(
      expr: ExpressionValue,
      substitution: Map[String, ExpressionValue]
  ): ExpressionValue

  // NEW: Collect all parameter references
  def collectParams(expr: ExpressionValue): Set[String]

  // NEW: Map over all parameter types
  def mapParamTypes(expr: ExpressionValue, f: Value => Value): ExpressionValue
}
```

**Benefits**:
- Single implementation of common traversals
- Less duplication across packages
- Easier to maintain consistency

---

### Synergy 4: Unified Parameter Context

**Problem**: Both `Evaluator` and `TypeEvaluator` use parameter binding maps:
- `Evaluator`: `paramContext: Map[String, Value]` (parameter -> its type)
- `TypeEvaluator`: `substitution: Map[String, Value]` (type param -> concrete type)

These serve the same purpose but are implemented separately.

**Proposal**: Create a shared `Bindings` type:

```scala
// eval/util/Bindings.scala (new)
case class Bindings(bindings: Map[String, Value]) {
  def get(name: String): Option[Value] = bindings.get(name)
  def +(binding: (String, Value)): Bindings = Bindings(bindings + binding)
  def ++(other: Bindings): Bindings = Bindings(bindings ++ other.bindings)
}

object Bindings {
  val empty: Bindings = Bindings(Map.empty)
  def from(pairs: (String, Value)*): Bindings = Bindings(pairs.toMap)
}
```

**Benefits**:
- Consistent handling of parameter bindings
- Can add common operations (lookup with error, scoping, etc.)
- Type safety for binding contexts

---

### Synergy 5: Better Separation of Value vs ExpressionValue

**Observation**: The system uses two type representations:
- `ExpressionValue`: May contain unevaluated parts (applications, parameters)
- `Value`: Fully evaluated ground types

**Current Issues**:
- `ExpressionValue.ConcreteValue(v: Value)` wraps `Value`
- `ExpressionValue.FunctionLiteral` uses `Value` for `parameterType`
- Conversion between them is scattered

**Proposal**: Make the relationship clearer:

```scala
// Value is the "normal form" of types
// ExpressionValue is "types under evaluation"

// Add explicit conversion utilities in eval/util/Types.scala
object Types {
  // Check if an ExpressionValue is fully ground (no params, no applications)
  def isGround(ev: ExpressionValue): Boolean

  // Convert to Value if ground, else None
  def toValue(ev: ExpressionValue): Option[Value]

  // Lift a Value to ExpressionValue
  def fromValue(v: Value): ExpressionValue = ConcreteValue(v)
}
```

---

## Implementation Roadmap

### Phase 1: Low-Risk Improvements (Recommended First)

1. **Add computed accessors to TypeCheckedValue**
   - Add `lazy val universalParams` and `lazy val signatureBody`
   - No structural changes, just convenience methods
   - Monomorphize can optionally use them

2. **Enhance ExpressionValue utilities**
   - Add `substitute` method to companion object
   - Add `collectParams` method
   - Both packages can use these

### Phase 2: Moderate Refactoring

3. **Move TypeEvaluator.evaluate to eval package**
   - Create `eval/util/TypeEval.scala`
   - Monomorphize imports from eval
   - Clearer dependency direction

4. **Create Bindings type**
   - New `eval/util/Bindings.scala`
   - Gradually migrate usages

### Phase 3: Structural Changes

5. **Restructure TypeCheckedValue**
   - Add explicit fields for universal params
   - Update SymbolicTypeCheckProcessor
   - Update MonomorphicTypeCheckProcessor
   - Remove extraction helpers from TypeEvaluator

---

## Detailed Design: TypeEval Component

```scala
// File: base/src/com/vanillasource/eliot/eliotc/eval/util/TypeEval.scala
package com.vanillasource.eliot.eliotc.eval.util

import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.processor.CompilerIO

/**
 * Evaluates ExpressionValue types to ground Value types.
 *
 * Unlike the full Evaluator which works on Expressions and does beta reduction,
 * TypeEval works on already-type-checked ExpressionValues and only performs
 * parameter substitution to produce ground types.
 */
object TypeEval {

  /**
   * Evaluate an ExpressionValue to a fully ground Value.
   *
   * @param expr The type expression to evaluate
   * @param bindings Type parameter bindings (name -> concrete type)
   * @param source Source location for error reporting
   * @return The ground type, or abort on error
   */
  def evaluate(
      expr: ExpressionValue,
      bindings: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Value] = ???

  /**
   * Create a function type Value from parameter and return types.
   */
  def functionType(paramType: Value, returnType: Value): Value = ???
}
```

---

## Detailed Design: Enhanced TypeCheckedValue

```scala
// File: base/src/com/vanillasource/eliot/eliotc/symbolic/fact/TypeCheckedValue.scala
package com.vanillasource.eliot.eliotc.symbolic.fact

case class TypeCheckedValue(
    vfqn: ValueFQN,
    name: Sourced[String],
    signature: ExpressionValue,
    runtime: Option[Sourced[TypedExpression.Expression]]
) extends CompilerFact {

  /**
   * Extract universal type parameter names from the signature.
   * Universal params are outer FunctionLiterals where paramType == Value.Type.
   */
  lazy val universalParams: Seq[String] = {
    def extract(ev: ExpressionValue): Seq[String] = ev match {
      case FunctionLiteral(name, paramType, body) if paramType == Value.Type =>
        name +: extract(body)
      case _ => Seq.empty
    }
    extract(signature)
  }

  /**
   * The signature with universal parameter introductions stripped away.
   * This is the "actual" type of the value once type parameters are instantiated.
   */
  lazy val signatureBody: ExpressionValue = {
    def strip(ev: ExpressionValue): ExpressionValue = ev match {
      case FunctionLiteral(_, paramType, body) if paramType == Value.Type =>
        strip(body)
      case other => other
    }
    strip(signature)
  }

  /**
   * Whether this value is polymorphic (has universal type parameters).
   */
  def isPolymorphic: Boolean = universalParams.nonEmpty

  override def key(): CompilerFactKey[TypeCheckedValue] = TypeCheckedValue.Key(vfqn)
}
```

---

## Summary of Benefits

| Synergy | Benefit | Effort | Risk |
|---------|---------|--------|------|
| Unified TypeEval | Single source of truth for type evaluation | Medium | Low |
| Pre-processed TCV | Cleaner separation, less parsing | Medium | Low |
| Shared traversals | Less code duplication | Low | Very Low |
| Unified Bindings | Consistent parameter handling | Low | Very Low |
| Value/EV separation | Clearer type semantics | Medium | Medium |

---

## Decision Points

1. **Should TypeEvaluator move to eval package?**
   - Pro: Better dependency direction (monomorphize depends on eval)
   - Con: It's quite monomorphize-specific (universal param handling)
   - Recommendation: Move `evaluate` to eval, keep universal helpers in monomorphize

2. **Should TypeCheckedValue store computed fields or compute lazily?**
   - Stored: Faster access, serialization-friendly
   - Lazy: Less memory, single source of truth
   - Recommendation: Start with lazy, refactor to stored if performance matters

3. **Should we create a shared Bindings type?**
   - Pro: Type safety, shared utilities
   - Con: Minor refactoring overhead
   - Recommendation: Yes, it clarifies intent and enables future enhancements

---

## Conclusion

The three packages have significant overlap in their handling of type evaluation and parameter substitution. The recommended approach is:

1. **Start with non-breaking enhancements**: Add lazy computed properties to `TypeCheckedValue`
2. **Extract common utilities**: Move type evaluation logic to `eval` package
3. **Improve type representations**: Make `Value` vs `ExpressionValue` relationship clearer

This will reduce code duplication, improve maintainability, and create clearer architectural boundaries between the packages.
