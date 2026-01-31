# Nested Stack Type Checking Algorithm

## Problem Statement

In the ELIOT type system, expressions are organized in **stacks** where each level describes the type of the level below. The implicit top level is always `Type`. The current implementation has a critical flaw: both `TypeExpressionBuilder.buildFromStack()` and `BodyInferenceBuilder.buildFromStack()` take only `expressions.headOption` (the first element) and process it, completely ignoring the rest of the stack.

This is incorrect because:
1. All levels in a stack need to be checked in order, from top to bottom
2. Each level's expression must be evaluated to determine the expected type for the next level
3. An expression at any level may only be type-checked after the level above has been validated

### Current Broken Code

```scala
// TypeExpressionBuilder.scala:84-93
private def buildFromStack(
    stack: Sourced[ExpressionStack[Expression]]
): TypeGraphIO[TypedExpression] =
  stack.value.expressions.headOption match {
    case Some(expr) => build(expr)  // BUG: Ignores all levels except the first!
    case None       => generateUnificationVar(stack)
  }
```

The same pattern appears in `BodyInferenceBuilder`.

## Stack Structure Recap

```
ExpressionStack[E](expressions: Seq[E], hasRuntime: Boolean)

Index interpretation when hasRuntime = true:
  [0] = runtime expression (the actual value)
  [1] = signature (type of the runtime value)
  [2] = type of the signature
  [n] = type of level [n-1]

Index interpretation when hasRuntime = false:
  [0] = signature (abstract, no runtime)
  [1] = type of the signature
  [n] = type of level [n-1]

Implicit: The topmost level's type is always Type
```

**Example**: A value `zero: Int :: Type = 0` would have:
```
expressions = [
  IntegerLiteral(0),        // runtime (level 0)
  ValueReference(Int),      // signature (level 1)
  ValueReference(Type)      // meta-level (level 2)
]
hasRuntime = true

Implicit: Type is the type of level 2
```

## The Fractal Nature of Stacks

Expressions can contain nested stacks, creating a fractal structure:

```
FunctionLiteral(
  paramName: String,
  paramType: ExpressionStack[Expression],   // <-- nested stack!
  body: ExpressionStack[Expression]          // <-- nested stack!
)

FunctionApplication(
  target: ExpressionStack[Expression],       // <-- nested stack!
  argument: ExpressionStack[Expression]      // <-- nested stack!
)
```

Each nested stack requires the same top-down validation as the outermost stack.

## Correct Algorithm

### Core Principle

**An expression may only be type-checked after its expected type has been determined from the level above.**

This creates a top-down, recursive algorithm:

```
processStack(stack, outerExpectedType) -> (inferredType, typedExpressions):
  1. Extract type levels (exclude runtime if hasRuntime)
  2. Process levels top-to-bottom, starting with Type as expected type
  3. For runtime level (if present), process with signature type as expected
  4. Return the signature type and all typed levels
```

### Detailed Algorithm

```
Algorithm: processStackTopDown

Input:
  - stack: ExpressionStack[Expression]
  - outerExpectedType: Value (the type expected by the context, usually Type for top-level)

Output:
  - (signatureType: ExpressionValue, typedStack: ExpressionStack[TypedExpression])

Steps:

1. SEPARATE LEVELS
   typeLevels = if stack.hasRuntime then stack.expressions.drop(1) else stack.expressions
   runtimeExpr = if stack.hasRuntime then Some(stack.expressions(0)) else None

2. PROCESS TYPE LEVELS (top-to-bottom)
   expectedType = Type  // Implicit top level
   typedLevels = []

   FOR level IN reverse(typeLevels):  // Process from top (last) to bottom (first)

     2a. BUILD THE EXPRESSION (recursively processes nested stacks)
         typedExpr = buildExpression(level, expectedType)

     2b. VALIDATE TYPE MATCHES EXPECTED
         IF typedExpr.expressionType != ConcreteValue THEN
           IF this is NOT the signature level (last in processing order) THEN
             ERROR: "Higher levels must evaluate to concrete types"
         ELSE
           actualType = typedExpr.expressionType.value.valueType
           IF actualType != expectedType THEN
             ERROR: "Type mismatch: expected {expectedType}, got {actualType}"

     2c. EXTRACT VALUE FOR NEXT LEVEL
         expectedType = CASE typedExpr.expressionType OF
           ConcreteValue(v) => v
           other => Type  // For signature level with unification vars

     2d. ACCUMULATE
         typedLevels.prepend(typedExpr)

   signatureType = typedLevels.last.expressionType

3. PROCESS RUNTIME (if present)
   IF runtimeExpr.isDefined THEN
     typedRuntime = inferBody(runtimeExpr, signatureType)
     ADD constraint: typedRuntime.expressionType == signatureType
     typedLevels.prepend(typedRuntime)

4. RETURN
   (signatureType, ExpressionStack(typedLevels, stack.hasRuntime))
```

### Recursive Expression Building with Expected Type

```
Algorithm: buildExpression

Input:
  - expr: Expression
  - expectedType: Value (what we expect this expression's VALUE to have as type)

Output:
  - TypedExpression

The key change is that buildExpression must propagate expectedType into nested stacks:

CASE expr OF

  FunctionLiteral(paramName, paramTypeStack, bodyStack):
    // paramTypeStack describes a TYPE (whose type is Type)
    (paramType, typedParamStack) = processStackTopDown(paramTypeStack, Type)

    bindParameter(paramName, paramType)

    // bodyStack describes a VALUE (whose type is determined by context)
    // For type expressions: body is a TYPE, expected = Type
    // For runtime expressions: body has expected type from signature
    (bodyType, typedBodyStack) = processStackTopDown(bodyStack, expectedType)

    funcType = FunctionType(paramType, bodyType)
    RETURN TypedExpression(funcType, FunctionLiteral(paramName, typedParamStack, typedBodyStack))

  FunctionApplication(targetStack, argStack):
    // Both target and arg are expressions, process their stacks
    (targetType, typedTargetStack) = processStackTopDown(targetStack, Type)
    (argType, typedArgStack) = processStackTopDown(argStack, Type)

    resultType = applyType(targetType, argType)
    RETURN TypedExpression(resultType, FunctionApplication(typedTargetStack, typedArgStack))

  ValueReference(vfqn):
    // Look up the type from the resolved value
    resolvedType = lookupValueType(vfqn)
    RETURN TypedExpression(resolvedType, ValueReference(vfqn))

  ParameterReference(name):
    paramType = lookupParameter(name)
    RETURN TypedExpression(paramType, ParameterReference(name))

  IntegerLiteral(value):
    RETURN TypedExpression(ConcreteValue(IntType), IntegerLiteral(value))

  StringLiteral(value):
    RETURN TypedExpression(ConcreteValue(StringType), StringLiteral(value))
```

## Worked Example

Consider this ELIOT declaration:

```eliot
identity: (A: Type) -> (a: A) -> A = (A: Type) -> (a: A) -> a
```

The signature part `(A: Type) -> (a: A) -> A` has this structure:

```
FunctionLiteral(
  paramName = "A",
  paramType = Stack([ValueReference(Type)], hasRuntime=false),  // Level 1: Type
  body = Stack([
    FunctionLiteral(
      paramName = "a",
      paramType = Stack([ParameterReference(A)], hasRuntime=false),  // Level 1: A (which is a Type)
      body = Stack([ParameterReference(A)], hasRuntime=false)        // Level 1: A
    )
  ], hasRuntime=false)
)
```

### Processing Order

1. **Enter top FunctionLiteral** (expected: Type for type expression)

2. **Process paramType stack `[Type]`**:
   - Expected type: Type (this is the type of parameter types)
   - Level 0: `ValueReference(Type)`
   - Build: Results in `ConcreteValue(Type)` with type `Type` ✓
   - Bind: A → Type (A is a universal type variable)

3. **Process body stack** (expected: still Type, we're in type expression land):
   - Level 0: `FunctionLiteral(a, [A], [A])`

   3a. **Enter nested FunctionLiteral**

   3b. **Process paramType stack `[A]`**:
       - Expected type: Type
       - Level 0: `ParameterReference(A)`
       - Build: Results in `ParameterReference(A, Type)`
       - The type OF A is Type, which has valueType Type ✓

   3c. **Bind: a → A** (a has type A)

   3d. **Process body stack `[A]`**:
       - Expected type: Type (still in type expression)
       - Level 0: `ParameterReference(A)`
       - Build: Results in `ParameterReference(A, Type)`
       - Return type is A

   3e. **Result**: `FunctionType(A, A)` i.e., `(a: A) -> A`

4. **Final Result**: `FunctionType(Type, FunctionType(A, A))` i.e., `(A: Type) -> (a: A) -> A`

### Multi-Level Stack Example

Consider a value with explicit type annotation at multiple levels:

```eliot
myType: Type :: Type = Int
```

This would have:
```
Stack([
  ValueReference(Int),     // Level 0: runtime
  ValueReference(Type),    // Level 1: signature
  ValueReference(Type)     // Level 2: type of signature
], hasRuntime=true)
```

**Processing**:

1. **Separate levels**:
   - typeLevels = [Type, Type] (levels 1 and 2)
   - runtimeExpr = Int (level 0)

2. **Process type levels top-to-bottom**:

   2a. **Level 2**: `ValueReference(Type)`
       - Expected: Type (implicit top)
       - Build: `ConcreteValue(Type{valueType=Type})`
       - Check: Type.valueType == Type ✓
       - Next expected: Type

   2b. **Level 1**: `ValueReference(Type)`
       - Expected: Type (from level 2)
       - Build: `ConcreteValue(Type{valueType=Type})`
       - Check: Type.valueType == Type?
       - **Wait**: Type.valueType is Type, not Type!
       - This would be an **error** if Type's valueType wasn't Type

   Actually, let me reconsider. `Type` itself IS a type, and its value-type is `Type`. So when we have:
   - Level 2 says "the thing at level 1 has type Type"
   - Level 1 is `Type` itself
   - We need to check that `Type` (as a value) has valueType matching what level 2 expects

   Hmm, the semantics here are tricky. Let me think again...

   Actually the key insight is:
   - Level N's expression evaluates to some Value V
   - V.valueType must equal the Value that level N+1 evaluated to
   - The implicit top level evaluates to Type

   So for `myType: Type :: Type = Int`:
   - Level 2: evaluates to Type (a concrete Value)
     - Expected valueType: Type (implicit)
     - Actual: Type.valueType = Type ✓
   - Level 1: evaluates to Type (a concrete Value)
     - Expected valueType: Type (from level 2)
     - Actual: Type.valueType = Type ✗

   This would indeed be an error! The correct annotation would be just:
   ```eliot
   myType: Type = Int
   ```

   Where:
   - Level 1: evaluates to Type
     - Expected valueType: Type (implicit)
     - Actual: Type.valueType = Type ✓

## Key Insight: The Type-Of-Type Chain

The stack encodes a chain:
```
                                           (implicit)
Level N  ---type-of--->  Level N-1  ---type-of--->  ...  ---type-of--->  Level 0
   ↓                         ↓                                               ↓
evaluates to             evaluates to                                    runtime value
Value V_N                Value V_{N-1}
   ↓                         ↓
V_N.valueType            V_{N-1}.valueType
   =                         =
Type (implicit)      V_N
```

Each level validates that its value's `valueType` matches the value from the level above.

## Implementation Changes Required

### 1. Replace `buildFromStack` with Proper Stack Processing

Both `TypeExpressionBuilder` and `BodyInferenceBuilder` need:

```scala
def processStack(
    stack: Sourced[ExpressionStack[Expression]],
    expectedType: Value
): TypeGraphIO[(ExpressionValue, Sourced[ExpressionStack[TypedExpression]])]
```

This method:
1. Processes all levels from top to bottom
2. Validates each level against the expected type
3. Propagates evaluated values down the chain
4. Returns both the signature type and the fully typed stack

### 2. Modify Expression Building to Accept Expected Type

```scala
def build(
    expression: Expression,
    expectedType: Value
): TypeGraphIO[TypedExpression]
```

The expected type is used for:
- Validation that the expression's result matches expectations
- Propagating expectations into nested stacks

### 3. Recursive Stack Processing in Nested Expressions

When encountering `FunctionLiteral` or `FunctionApplication`:
- Their nested stacks must be processed with `processStack`
- Not just `expressions.head` extraction

### 4. Unification for Signature Level

The signature level (and runtime) may contain unification variables rather than concrete values. The algorithm must:
- Allow non-concrete types at signature level
- Generate constraints rather than immediate errors for type mismatches
- Let unification resolve unknowns later

## Distinction: Type Expressions vs Body Expressions

**Type Expressions** (processed by `TypeExpressionBuilder`):
- Appear in type annotation positions
- All levels should evaluate to types
- The overall result describes a type
- Expected type for the whole expression is typically Type

**Body Expressions** (processed by `BodyInferenceBuilder`):
- Appear in runtime/value positions
- The signature level gives the type of the runtime value
- The runtime level is the actual value
- May involve inference and unification variables

Both share the same recursive stack structure and require the same top-down processing.

## Summary

The correct algorithm is:
1. **Top-down processing**: Start from Type, work down through levels
2. **Validate at each level**: Each level's valueType must match the value from above
3. **Recursive on nested stacks**: FunctionLiteral and FunctionApplication contain stacks that need the same processing
4. **Propagate expected types**: Pass down the evaluated value as the expected type for the next level
5. **Allow unknowns at signature/runtime**: Use unification for the bottom levels

This ensures that every part of every expression is validated against its proper expected type, creating a sound type-checking algorithm.
