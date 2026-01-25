# Monomorphic Type Checker Design

## Overview

This document describes the design for a **monomorphic type checker** that operates after the symbolic type checker (`SymbolicTypeCheckProcessor`). Unlike the symbolic checker which verifies polymorphic code with free type variables, the monomorphic checker:

1. Takes a **main entrypoint** (`ValueFQN`) as input
2. **Recursively traverses** all reachable functions starting from the entrypoint
3. **Instantiates** all generic functions with concrete type arguments
4. **Evaluates** all type expressions to ground terms (no symbols/free variables)
5. **Verifies** that all concrete types are correctly typed

This is essentially the "monomorphization" phase where polymorphic code becomes concrete specialized code.

## Motivation

The symbolic type checker (`SymbolicTypeCheckProcessor`) verifies that polymorphic code is structurally sound:
- `identity[A](x: A): A = x` is valid because `A ~ A`
- `bad[A, B](x: A): B = x` is invalid because `A ~ B` fails for distinct universals

However, this doesn't catch errors that only manifest with specific type instantiations:
- Type-level computations that fail for certain inputs
- Dependent type constraints that aren't satisfiable for concrete types
- Generic code that assumes properties not universally true

The monomorphic checker performs **concrete type checking** by:
- Following the actual call graph from `main`
- Instantiating each polymorphic function with its specific type arguments
- Fully evaluating type expressions to ground terms
- Checking equality rather than unification

## Key Differences from Symbolic Checking

| Aspect | Symbolic (Current) | Monomorphic (New) |
|--------|-------------------|-------------------|
| **Scope** | Checks each function in isolation | Checks reachable functions from main |
| **Type Variables** | `UniversalVar`, `ParameterRef` remain symbolic | All variables substituted with concrete types |
| **Evaluation** | Partial (stops at free variables) | Full (evaluates to ground terms) |
| **Unification** | Generates constraints, solves symbolically | Checks structural equality |
| **Output** | `TypeCheckedValue` with symbolic types | `MonomorphicValue` with concrete types |
| **Trigger** | Per-function during compilation | Once, from main entrypoint |

## Architecture

### Input

```
MonomorphicTypeChecker.Key(mainValueFQN: ValueFQN)
```

The checker requires a single entrypoint - typically the `main` function of the program.

### Output Facts

#### 1. MonomorphicValue

Represents a fully type-checked, specialized function with all types concrete.

```scala
case class MonomorphicValue(
    key: MonomorphicValue.Key,
    name: Sourced[String],
    value: Sourced[ExpressionStack[MonomorphicExpression]]
) extends CompilerFact

object MonomorphicValue {
  // Key includes both the original function AND its type arguments
  case class Key(vfqn: ValueFQN, typeArguments: Seq[ConcreteType])
      extends CompilerFactKey[MonomorphicValue]
}
```

#### 2. ConcreteType

A fully evaluated type with no free variables.

```scala
sealed trait ConcreteType

object ConcreteType {
  case class ValueRef(vfqn: ValueFQN, arguments: Seq[ConcreteType]) extends ConcreteType
  case class IntLiteral(value: BigInt) extends ConcreteType
  case class StringLiteral(value: String) extends ConcreteType
  case class FunctionType(paramType: ConcreteType, returnType: ConcreteType) extends ConcreteType
}
```

Note: No `ParameterRef`, `UniversalVar`, `UnificationVar`, or `SymbolicApplication` - these cannot exist in concrete types.

#### 3. MonomorphicExpression

Expression tree annotated with concrete types.

```scala
case class MonomorphicExpression(
    expressionType: ConcreteType,
    expression: Expression
)

sealed trait Expression
object Expression {
  case class FunctionApplication(target: MonomorphicExpression, argument: MonomorphicExpression)
  case class IntegerLiteral(value: BigInt)
  case class StringLiteral(value: String)
  case class ParameterReference(name: String)  // Runtime parameter, type is concrete
  case class ValueReference(vfqn: ValueFQN, typeArgs: Seq[ConcreteType])  // Specialized reference
  case class FunctionLiteral(paramName: String, paramType: ConcreteType, body: MonomorphicExpression)
}
```

### Processor: MonomorphicTypeCheckProcessor

```scala
class MonomorphicTypeCheckProcessor
    extends SingleKeyTypeProcessor[MonomorphicValue.Key]
    with Logging {

  override protected def generateFact(key: MonomorphicValue.Key): CompilerIO[Unit] =
    for {
      // 1. Get the polymorphic typed value
      typedValue <- getFactOrAbort(TypeCheckedValue.Key(key.vfqn))

      // 2. Create substitution map from type parameters to concrete arguments
      typeParamMap <- extractTypeParamMap(typedValue, key.typeArguments)

      // 3. Evaluate all types with concrete substitutions
      monomorphicValue <- monomorphize(typedValue, typeParamMap)

      // 4. Register the specialized value
      _ <- registerFactIfClear(monomorphicValue)
    } yield ()
}
```

## Core Algorithm

### Phase 1: Entry Point Processing

Starting from `main`:

```
processEntryPoint(mainFQN: ValueFQN):
  1. Get TypeCheckedValue for mainFQN
  2. Verify main has no type parameters (it's already concrete)
  3. Create MonomorphicValue.Key(mainFQN, Seq.empty)
  4. Process the main function body
  5. For each function call discovered, enqueue specializations
```

### Phase 2: Specialization Queue

Maintain a work queue of functions to specialize:

```
SpecializationWork(vfqn: ValueFQN, typeArgs: Seq[ConcreteType])
```

Process until queue is empty:
```
while workQueue.nonEmpty:
  work = workQueue.dequeue()
  if not already processed(work.vfqn, work.typeArgs):
    specialize(work.vfqn, work.typeArgs)
```

### Phase 3: Function Specialization

For each `(vfqn, typeArgs)` pair:

```
specialize(vfqn: ValueFQN, typeArgs: Seq[ConcreteType]):
  1. Get TypeCheckedValue(vfqn)

  2. Extract type parameter names from signature
     - Each outer FunctionLiteral with empty param type introduces a type param
     - E.g., identity[A] has one param "A"

  3. Build substitution map: typeParamName -> ConcreteType
     - Zip parameter names with provided typeArgs
     - Error if counts don't match

  4. Monomorphize the signature:
     a. Substitute all UniversalVar/ParameterRef with concrete types
     b. Fully evaluate any type applications (beta-reduce)
     c. Result must be ConcreteType (no free variables)

  5. Monomorphize the body:
     a. Traverse expression tree
     b. For each subexpression, substitute and evaluate its type
     c. For function calls, determine concrete type arguments
     d. Enqueue any new specializations discovered

  6. Type check the monomorphized body:
     a. Infer concrete type of body
     b. Assert equality with declared return type
     c. Report error if mismatch

  7. Register MonomorphicValue(Key(vfqn, typeArgs), ...)
```

### Phase 4: Type Expression Evaluation

Evaluate `NormalizedExpression` to `ConcreteType`:

```scala
def evaluate(
    expr: NormalizedExpression,
    substitutions: Map[String, ConcreteType]
): CompilerIO[ConcreteType] = expr match {

  case ValueRef(vfqn, args) =>
    // Evaluate arguments, then check if this is a type function
    for {
      evalArgs <- args.traverse(evaluate(_, substitutions))
      result   <- lookupTypeDef(vfqn.value) match {
        case Some(typeLambda) =>
          // Type constructor with body - apply and reduce
          applyTypeLambda(typeLambda, evalArgs)
        case None =>
          // Opaque type or data constructor
          ConcreteType.ValueRef(vfqn.value, evalArgs).pure
      }
    } yield result

  case ParameterRef(name) =>
    substitutions.get(name.value) match {
      case Some(concrete) => concrete.pure
      case None => compilerError(s"Unbound type parameter: ${name.value}")
    }

  case UniversalVar(name) =>
    substitutions.get(name.value) match {
      case Some(concrete) => concrete.pure
      case None => compilerError(s"Uninstantiated universal: ${name.value}")
    }

  case IntLiteral(value) =>
    ConcreteType.IntLiteral(value.value).pure

  case StringLiteral(value) =>
    ConcreteType.StringLiteral(value.value).pure

  case FunctionType(param, ret, _) =>
    for {
      paramConcrete <- evaluate(param, substitutions)
      retConcrete   <- evaluate(ret, substitutions)
    } yield ConcreteType.FunctionType(paramConcrete, retConcrete)

  case SymbolicApplication(target, arg, _) =>
    for {
      targetConcrete <- evaluate(target, substitutions)
      argConcrete    <- evaluate(arg, substitutions)
      result         <- applyConcreteType(targetConcrete, argConcrete)
    } yield result

  case UnificationVar(id, source) =>
    // Should never happen after symbolic type checking
    compilerError(source.as(s"Unresolved unification variable: ?$id"))
}
```

### Phase 5: Concrete Type Equality

Unlike symbolic unification, monomorphic checking uses structural equality:

```scala
def assertEqual(
    expected: ConcreteType,
    found: ConcreteType,
    source: Sourced[?]
): CompilerIO[Unit] = (expected, found) match {

  case (ValueRef(v1, a1), ValueRef(v2, a2)) if v1 === v2 && a1.length === a2.length =>
    (a1 zip a2).traverse_ { case (e, f) => assertEqual(e, f, source) }

  case (FunctionType(p1, r1), FunctionType(p2, r2)) =>
    assertEqual(p1, p2, source) >> assertEqual(r1, r2, source)

  case (IntLiteral(v1), IntLiteral(v2)) if v1 === v2 =>
    ().pure

  case (StringLiteral(v1), StringLiteral(v2)) if v1 === v2 =>
    ().pure

  case _ =>
    compilerError(source.as("Type mismatch."), Seq(
      s"Expected: ${expected.show}",
      s"Found:    ${found.show}"
    ))
}
```

## Detailed Walkthrough: Specializing `apply`

Consider:
```
apply[A, B](f: Function[A, B], a: A): B = f(a)

// Call site:
apply[Int, String](toString, 42)
```

### Step 1: Discover Specialization Need

Processing `main` body, we encounter:
```
FunctionApplication(
  ValueReference(apply),
  typeArgs = [Int, String]  -- inferred from call
)
```

Enqueue: `SpecializationWork(apply, [Int, String])`

### Step 2: Specialize `apply[Int, String]`

1. Get `TypeCheckedValue(apply)`
2. Extract type params: `["A", "B"]`
3. Build substitution: `{A -> Int, B -> String}`
4. Evaluate signature:
   ```
   Original: [A][B] Function(Function(A, B), Function(A, B))
   After substitution: Function(Function(Int, String), Function(Int, String))
   After evaluation: (Int -> String) -> Int -> String
   ```
5. Evaluate body types:
   ```
   f: Function(Int, String)  -- after substitution
   a: Int
   f(a): String
   ```
6. Check return type: `String === String` ✓
7. Register `MonomorphicValue(Key(apply, [Int, String]), ...)`

### Step 3: Continue with `toString`

If `toString` is polymorphic, enqueue its specialization too.

## State Management

### Specialization Cache

Track which specializations have been processed:

```scala
case class MonomorphizationState(
    processed: Set[(ValueFQN, Seq[ConcreteType])],
    workQueue: Queue[SpecializationWork]
)
```

### Fact Dependencies

The monomorphic checker depends on:
- `TypeCheckedValue.Key(vfqn)` for each function
- `ResolvedValue.Key(vfqn)` for type definitions

It produces:
- `MonomorphicValue.Key(vfqn, typeArgs)` for each specialization

## Error Handling

### 1. Type Mismatch After Evaluation

When concrete types don't match:
```
Error at line 42, column 5:
  Type mismatch in specialized function apply[Int, String].
  Expected: String
  Found:    Int
```

### 2. Uninstantiated Type Variable

When a type variable remains unsubstituted:
```
Error at line 10, column 15:
  Uninstantiated type parameter 'A' in monomorphic context.
  This indicates a compiler bug or missing type argument.
```

### 3. Infinite Specialization

Guard against infinitely recursive type instantiations:
```
Error at line 5, column 1:
  Infinite type specialization detected.
  Specialization chain: f[Int] -> f[List[Int]] -> f[List[List[Int]]] -> ...
```

Detection: Limit specialization depth or detect cycles in type arguments.

### 4. Type-Level Computation Failure

When type-level functions don't reduce:
```
Error at line 20, column 10:
  Type expression did not reduce to a concrete type.
  Expression: Add(5, "hello")
  This may indicate a type-level type error.
```

## Integration with Pipeline

### Before Monomorphization

```
ResolvedValue (from resolve2)
       ↓
TypeCheckedValue (from typesystem2/SymbolicTypeCheckProcessor)
       ↓
MonomorphicValue (from MonomorphicTypeCheckProcessor) ← NEW
```

### After Monomorphization

```
MonomorphicValue
       ↓
UncurriedFunction (adapt uncurry to use MonomorphicValue)
       ↓
JvmClassGenerator (code generation)
```

The JVM code generator should be adapted to:
1. Query `MonomorphicValue` instead of `TypeCheckedValue`
2. Use concrete types directly (no generic handling needed)
3. Generate specialized code for each instantiation

## File Structure

```
base/src/com/vanillasource/eliot/eliotc/monomorphize/
├── fact/
│   ├── ConcreteType.scala          -- Fully evaluated type representation
│   ├── MonomorphicExpression.scala -- Expression with concrete types
│   └── MonomorphicValue.scala      -- Specialized function fact
├── processor/
│   ├── MonomorphicTypeCheckProcessor.scala -- Main processor
│   ├── TypeEvaluator.scala         -- NormalizedExpression -> ConcreteType
│   ├── SpecializationQueue.scala   -- Work queue management
│   └── ConcreteTypeEquality.scala  -- Equality checking
└── types/
    └── MonomorphizationState.scala -- Processing state
```

## Implementation Steps

### Step 1: Define Core Facts
1. Create `ConcreteType` sealed trait with all variants
2. Create `MonomorphicExpression` with concrete type annotations
3. Create `MonomorphicValue` fact with composite key `(ValueFQN, Seq[ConcreteType])`

### Step 2: Implement Type Evaluator
1. Create `TypeEvaluator` that converts `NormalizedExpression` to `ConcreteType`
2. Handle all cases: substitution, beta-reduction, type application
3. Error on unresolved variables

### Step 3: Implement Body Transformation
1. Create transformation from `TypedExpression` to `MonomorphicExpression`
2. Recursively process all subexpressions
3. Collect function references with their concrete type arguments

### Step 4: Implement Specialization Queue
1. Create work queue for pending specializations
2. Implement cycle detection / depth limiting
3. Process queue until empty

### Step 5: Implement Concrete Equality
1. Create `ConcreteTypeEquality` for structural comparison
2. Generate clear error messages on mismatch

### Step 6: Create Main Processor
1. Implement `MonomorphicTypeCheckProcessor`
2. Wire up fact dependencies
3. Integrate into `BasePlugin`

### Step 7: Testing
1. Unit tests for type evaluation
2. Unit tests for equality checking
3. Integration tests for full specialization
4. Error message quality tests

### Step 8: Adapt Downstream
1. Update `UncurriedFunction` to consume `MonomorphicValue`
2. Update `JvmClassGenerator` for concrete types
3. Remove generic handling from code generation (now unnecessary)

## Test Cases

### Basic Specialization
```eliot
identity[A](a: A): A = a
main: Unit = identity[Int](42)
```
Should produce `MonomorphicValue(identity, [Int])`.

### Nested Generics
```eliot
compose[A, B, C](f: B -> C, g: A -> B, a: A): C = f(g(a))
main: Unit = compose[Int, String, Bool](isEmpty, toString, 42)
```
Should produce `MonomorphicValue(compose, [Int, String, Bool])`.

### Type-Level Computation
```eliot
-- Assuming type-level Add is defined
Vec[N: Nat, A](...)
concat[N, M, A](v1: Vec[N, A], v2: Vec[M, A]): Vec[Add(N, M), A] = ...
main: Unit = concat(vec3, vec2)  -- Should produce Vec[5, Int]
```

### Error Case: Type Mismatch
```eliot
bad[A](a: A): Int = a  -- Symbolic checker passes (A could be Int)
main: Unit = bad[String]("hello")  -- Monomorphic checker fails!
```
After specialization:
- `a: String`
- Return type: `Int`
- Body type: `String`
- Error: `Type mismatch. Expected: Int, Found: String`

## Summary

The monomorphic type checker complements the symbolic checker by:

1. **Starting from main** - Only checks reachable code
2. **Full instantiation** - No type variables remain
3. **Complete evaluation** - Type expressions fully reduced
4. **Equality checking** - No constraint solving, just comparison
5. **Specialization** - Each generic use creates a concrete version

This enables:
- Catching errors that only manifest for specific type instantiations
- Generating efficient specialized code (no runtime type passing)
- Supporting dependent types with compile-time evaluation
- Simplifying code generation (all types known statically)
