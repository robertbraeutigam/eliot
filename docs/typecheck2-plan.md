# Type Checking Plan for resolve2's Expression Stack Model

## Understanding the Current Structure

The `ResolvedValue` represents a value where:
- `typeExpression: Sourced[ExpressionStack]` - the type as an expression stack
- `value: Option[Sourced[Expression]]` - the optional implementation

The `ExpressionStack` model captures **types as values**:
- Bottom layer = runtime value
- Each layer above = the type of the layer below
- Example: `zero: Int : Type` has a stack of `[0, Int, Type]`

Key transformations from CoreProcessor tests:
- `f[A]: R` → typeStack becomes `Lambda("A", Empty, Ref("R"))` (generics become type-level lambdas)
- `A[B]` → becomes `App(Ref("A"), Ref("B"))` (type application = function application)
- Empty typeStack for type parameters means "unbounded" (like `∀A.`)

## The Two Type Checking Modes

### 1. Polymorphic Type Checking (Universal Quantification)

**Goal**: Verify code is valid for *all* possible instantiations of generic parameters.

**Characteristics**:
- Generic parameters are treated as opaque type variables
- Cannot evaluate expressions involving type parameters to concrete values
- Uses symbolic unification: constraints like `T ~ Int -> T` are carried forward
- Checks structural compatibility, not concrete type equality

**Example**:
```
identity[A](x: A): A = x
```
We check: the body `x` has type `A` (a parameter reference), and the declared return type is `A`. These unify symbolically.

### 2. Monomorphic Type Checking (After Instantiation)

**Goal**: Verify code with all concrete types known.

**Characteristics**:
- All generic parameters have been substituted with concrete types
- Can fully evaluate type expressions (beta reduction, etc.)
- Unification becomes equality checking on normalized types
- Happens during specialization/monomorphization before code generation

**Example**:
```
identity[Int](42)  -- A is now Int
```
We verify: `42 : Int`, parameter type is `Int`, they're equal.

## Expression Evaluation (Required for Both Phases)

Since types are expressions, the compiler needs an **evaluator** for pure expressions at compile time.

### One Evaluator, Two Modes

There is **one evaluator** that works the same way in both polymorphic and monomorphic modes. The difference is not in the evaluator itself, but in **what expressions it operates on**:

- **Polymorphic mode**: Expressions may contain `ParameterReference` nodes for type parameters. The evaluator reduces what it can, but these references remain as irreducible symbols.
- **Monomorphic mode**: All type parameters have been substituted with concrete types before evaluation. The evaluator can fully reduce to ground terms.

### Symbolic Evaluation Works Through Applications

A key insight: **substitution is mechanical** and works regardless of whether arguments are concrete or symbolic.

```
-- Type-level function (e.g., from "data Box[A](...)")
Box = Lambda("A", Empty, ...body using A...)

-- Concrete application:
App(Box, Ref("Int"))
  → substitute A → Int in body
  → eval(body[A := Int])
  → fully ground result

-- Symbolic application (T is a type parameter in scope):
App(Box, ParameterReference("T"))
  → substitute A → ParameterReference("T") in body
  → eval(body[A := T])
  → partially reduced result, still contains T
```

The evaluator performs beta reduction on *any* lambda application, even when the argument is symbolic. The result is an expression in normal form that may still contain free type variables.

### Evaluation Rules

```
eval(IntegerLiteral(n)) = IntegerLiteral(n)  -- literals are values
eval(StringLiteral(s)) = StringLiteral(s)
eval(ParameterReference(p)) = ParameterReference(p)  -- irreducible, stays as-is
eval(ValueReference(v)) = lookup(v) and evaluate if it has a body
eval(FunctionApplication(f, arg)) =
  let f' = eval(f)
  let arg' = eval(arg)
  if f' is FunctionLiteral(param, _, body):
    eval(substitute(body, param -> arg'))  -- beta reduction (works even if arg' is symbolic)
  else:
    FunctionApplication(f', arg')  -- can't reduce (f' is not a lambda)
eval(FunctionLiteral(p, t, b)) = FunctionLiteral(p, eval(t), b)  -- normalize param type, keep body unevaluated
```

### What the Modes Produce

| Mode | Input | Output |
|------|-------|--------|
| Polymorphic | Expressions with type parameter references | Normalized expressions, may contain free type variables |
| Monomorphic | Expressions after substituting concrete types | Fully ground normalized expressions |

The evaluator is identical; the difference is purely in the input.

## Unification Strategy

### Type Representation During Checking

Need a unified type representation:
```scala
sealed trait TypeExpr
case class TypeVar(name: String) extends TypeExpr           // Type variable (generic param)
case class TypeRef(vfqn: ValueFQN) extends TypeExpr         // Concrete type reference
case class TypeApp(fn: TypeExpr, arg: TypeExpr) extends TypeExpr  // Type application
case class TypeFn(param: String, body: TypeExpr) extends TypeExpr  // Type-level lambda
case class TypeLit(value: Any) extends TypeExpr             // Literal type (for dependently-typed values)
```

### Unification Rules

```
unify(TypeVar(a), T) =
  if a occurs in T: error (occurs check)
  else: {a -> T}  -- substitution

unify(TypeRef(v1), TypeRef(v2)) =
  if v1 == v2: {}  -- success, no substitution
  else: error

unify(TypeApp(f1, a1), TypeApp(f2, a2)) =
  let s1 = unify(f1, f2)
  let s2 = unify(apply(s1, a1), apply(s1, a2))
  compose(s1, s2)

unify(TypeFn(p1, b1), TypeFn(p2, b2)) =
  -- alpha-rename if needed, then unify bodies
  unify(b1, substitute(b2, p2 -> p1))
```

### How Unification Differs by Mode

Since both modes use the same evaluator, the difference in unification is about **what we're comparing**:

**Polymorphic unification** (on partially-reduced expressions):
- Expressions may contain `ParameterReference` nodes (free type variables)
- When unifying two different parameter references, generate a constraint: `T ~ U`
- When unifying a parameter reference with a concrete expression, generate a substitution: `T → ConcreteType`
- Result: a set of constraints/substitutions that must hold

**Monomorphic unification** (on fully-reduced expressions):
- All type variables have been substituted before evaluation
- Expressions are fully ground (no free variables)
- Unification becomes structural equality checking
- Result: success or type error

**Example**:
```
-- Polymorphic: checking "identity[A](x: A): A = x"
-- Body type: ParameterReference("A")
-- Declared return: ParameterReference("A")
-- Unify: ParameterReference("A") ~ ParameterReference("A") → success (same variable)

-- Polymorphic: checking "f[A, B](x: A): B = x"
-- Body type: ParameterReference("A")
-- Declared return: ParameterReference("B")
-- Unify: ParameterReference("A") ~ ParameterReference("B") → constraint: A ~ B
-- This constraint may be an error (A and B are different universal params) or deferred

-- Monomorphic: checking "identity[Int](42)"
-- After substitution: body type is Int, declared return is Int
-- Unify: Int ~ Int → success (equal ground types)
```

## Detailed Example: The `apply` Function

This section traces through type checking for a non-trivial generic function in both modes.

### The Function Definition

```
apply[A, B](f: Function[A, B], a: A): B = f(a)
```

After CoreProcessor, the `ExpressionStack` has:

- **Level 0 (runtime value)**:
  ```
  Lambda("f", Function(A, B), Lambda("a", A, App(Ref("f"), Ref("a"))))
  ```

- **Level 1 (type of the value)**:
  ```
  Lambda("A", Empty, Lambda("B", Empty, Function(Function(A, B), Function(A, B))))
  ```
  Where inside the inner `Function(...)`, `A` and `B` are `ParameterReference` nodes referring to the outer lambda bindings.

### Polymorphic Mode: Checking the Definition

When type-checking the *definition* of `apply`, we verify the body has the declared return type.

**Step 1: Enter the type-level lambda scopes**

The outer lambdas `Lambda("A", Empty, Lambda("B", Empty, ...))` introduce type parameters. We "open" them:
- Add `A` to scope as a type parameter (bound, value unknown)
- Add `B` to scope as a type parameter (bound, value unknown)
- Now working with the inner type: `Function(Function(A, B), Function(A, B))`

Where `A` and `B` are now `ParameterReference("A")` and `ParameterReference("B")` in scope.

**Step 2: Extract parameter types and return type**

The type `Function(Function(A, B), Function(A, B))` is a curried function type. Uncurrying:
- First parameter `f` has type: `Function(A, B)` (i.e., `A -> B`)
- Second parameter `a` has type: `A`
- Declared return type: `B`

**Step 3: Type-check the body `f(a)`**

```
infer(App(Ref("f"), Ref("a")))

  1. infer(Ref("f"))
     = lookup("f")
     = Function(ParameterRef("A"), ParameterRef("B"))

  2. This is a function type Function(X, Y), so we can apply it
     - expected argument type X = ParameterRef("A")
     - return type Y = ParameterRef("B")

  3. infer(Ref("a"))
     = lookup("a")
     = ParameterRef("A")

  4. Unify argument type with expected:
     unify(ParameterRef("A"), ParameterRef("A"))
     → Same parameter reference → success, no constraints

  5. Result type: ParameterRef("B")
```

**Step 4: Check return type matches declared**

```
Unify body type with declared return type:
  - body type: ParameterRef("B")
  - declared:  ParameterRef("B")
  → Same parameter reference → success
```

**What "evaluation" did here**: The type expression `Function(ParameterRef("A"), ParameterRef("B"))` is already in normal form. The evaluator, if invoked, returns it unchanged because `ParameterReference` nodes are irreducible. We worked entirely with symbolic types.

### Monomorphic Mode: At a Call Site

Now consider a call site:
```
apply[Int, String](myFunc, 42)
```

**Step 1: Apply concrete type arguments to the type-level lambdas**

Start with:
```
Lambda("A", Empty, Lambda("B", Empty, Function(Function(A, B), Function(A, B))))
```

Apply `Int` (first type argument):
```
App(Lambda("A", Empty, ...), Ref("Int"))
  → substitute A → Ref("Int") in body
  → Lambda("B", Empty, Function(Function(Int, B), Function(Int, B)))
```

Apply `String` (second type argument):
```
App(Lambda("B", Empty, ...), Ref("String"))
  → substitute B → Ref("String") in body
  → Function(Function(Int, String), Function(Int, String))
```

**Step 2: Evaluate to normal form**

`Function(Function(Int, String), Function(Int, String))` contains no `ParameterReference` nodes. It's fully ground.

Expanding the nested `Function` applications:
- Inner `Function(Int, String)` = the type `Int -> String`
- Outer = `(Int -> String) -> Int -> String`

This is the concrete type of `apply[Int, String]`.

**Step 3: Type-check the value arguments**

```
- myFunc must have type: Function(Int, String)
  → Check: does myFunc : Int -> String? (concrete check)

- 42 must have type: Int
  → Check: does 42 : Int? (concrete check)

- Result type: String (fully concrete)
```

### Summary: Key Differences

| Aspect | Polymorphic (checking definition) | Monomorphic (at call site) |
|--------|-----------------------------------|----------------------------|
| Type-level lambdas | Opened; params become `ParameterRef` in scope | Applied with concrete args; eliminated by beta reduction |
| Working type | `Function(ParameterRef("A"), ParameterRef("B"))` | `Function(Int, String)` |
| Evaluation result | Contains irreducible `ParameterRef` nodes | Fully ground, no free variables |
| Unification | Compares `ParameterRef` nodes symbolically (same ref = success) | Compares concrete types for equality |
| What we verify | Body type matches declared return type symbolically | Argument types match parameter types concretely |

The **same evaluator** handles both cases. In polymorphic mode, `ParameterRef` nodes are irreducible so they remain in the result. In monomorphic mode, concrete types are substituted first, so the evaluator produces fully ground types.

## Main Type Checking Algorithm (Pseudocode)

This section presents the core type checking logic in pseudocode.

### Entry Point

```
// Main entry point for type checking a resolved value
typecheckValue(rv: ResolvedValue) -> TypeCheckResult:
    scope = Scope.empty()

    // 1. Get the declared type (first layer of the type expression stack)
    declaredType = rv.typeExpression.expressions[0]

    // 2. Open type-level lambdas (generics) - adds type params to scope
    (scope, valueType) = openTypeLambdas(declaredType, scope)

    // 3. If abstract (no body), we're done
    if rv.value.isEmpty:
        return success(scope)

    // 4. Open value-level lambdas (function params) - checks param types, adds to scope
    (scope, body, returnType) = openValueLambdas(rv.value.get, valueType, scope)

    // 5. Infer type of the body expression
    inferredType = infer(body, scope)

    // 6. Unify inferred type with declared return type
    scope.unify(inferredType, returnType)

    return success(scope)
```

### Opening Lambda Scopes

```
// Opens type-level lambdas, bringing type parameters into scope
openTypeLambdas(typeExpr, scope) -> (Scope, Expression):
    while typeExpr is FunctionLiteral(typeParam, kind, body):
        // kind is Empty for unbounded, or a constraint expression
        scope = scope.addTypeParam(typeParam, kind)
        typeExpr = body.expressions[0]

    return (scope, typeExpr)


// Opens value-level lambdas, checking param types against the function type
openValueLambdas(valueExpr, expectedType, scope) -> (Scope, Expression, Expression):
    while valueExpr is FunctionLiteral(param, paramTypeStack, body):
        // Destructure expected function type: Function(ParamType)(ReturnType)
        (expectedParamType, restType) = expectFunctionType(expectedType, scope)

        // Evaluate declared param type and unify with expected
        declaredParamType = eval(paramTypeStack.expressions[0], scope)
        scope.unify(declaredParamType, expectedParamType)

        // Add param binding to scope
        scope = scope.addParam(param, expectedParamType)

        // Descend into body
        valueExpr = body.expressions[0]
        expectedType = restType

    return (scope, valueExpr, expectedType)  // expectedType is now return type
```

### Type Inference

```
// Main type inference - recursive descent on expressions
infer(expr, scope) -> Expression:
    match expr:
        case IntegerLiteral(_):
            return scope.intType

        case StringLiteral(_):
            return scope.stringType

        case ParameterReference(name):
            return scope.lookupParam(name)

        case ValueReference(vfqn):
            // Get the referenced value's type
            refType = lookupValueType(vfqn)
            // Instantiate generics as fresh type variables
            return instantiateGenerics(refType, scope)

        case FunctionApplication(targetStack, argStack):
            // Infer types of target and argument
            targetType = infer(targetStack.expressions[0], scope)
            argType = infer(argStack.expressions[0], scope)

            // Target must be a function type
            (paramType, returnType) = expectFunctionType(targetType, scope)

            // Unify argument with parameter type
            scope.unify(argType, paramType)

            // Return type (with substitutions applied)
            return scope.apply(returnType)

        case FunctionLiteral(param, paramTypeStack, bodyStack):
            // Evaluate the parameter type
            paramType = eval(paramTypeStack.expressions[0], scope)

            // Infer body type with param in scope
            innerScope = scope.addParam(param, paramType)
            bodyType = infer(bodyStack.expressions[0], innerScope)

            // Return function type: paramType -> bodyType
            return makeFunctionType(paramType, bodyType)
```

### Expression Evaluation (Beta Reduction)

```
// Evaluate an expression to normal form (beta reduction)
eval(expr, scope) -> Expression:
    match expr:
        case ParameterReference(name):
            // Irreducible if it's a type param, or lookup if substituted
            if scope.hasSubstitution(name):
                return eval(scope.getSubstitution(name), scope)
            else:
                return expr  // stays symbolic

        case ValueReference(vfqn):
            // Look up and evaluate if it has a body
            valueDef = lookupValue(vfqn)
            if valueDef.value.isDefined:
                return eval(valueDef.value.get, scope)
            else:
                return expr  // abstract value, can't reduce

        case FunctionApplication(targetStack, argStack):
            target = eval(targetStack.expressions[0], scope)
            arg = eval(argStack.expressions[0], scope)

            // Beta reduction if target is a lambda
            if target is FunctionLiteral(param, _, body):
                substituted = substitute(body.expressions[0], param -> arg)
                return eval(substituted, scope)
            else:
                return FunctionApplication(target, arg)  // can't reduce

        case FunctionLiteral(param, paramType, body):
            // Don't evaluate body until applied
            return FunctionLiteral(param, eval(paramType, scope), body)

        case _:
            return expr  // literals etc. are already values
```

### Helper Functions

```
// Destructure a function type into param and return types
expectFunctionType(typeExpr, scope) -> (Expression, Expression):
    // Evaluate to normal form first
    normalized = eval(typeExpr, scope)

    // Function types are: App(App(Function, ParamType), ReturnType)
    match normalized:
        case FunctionApplication(
               FunctionApplication(ValueReference(FunctionConstructor), paramType),
               returnType):
            return (paramType.expressions[0], returnType.expressions[0])

        case ParameterReference(t):
            // Type variable - generate fresh vars for param and return
            paramVar = scope.freshTypeVar()
            returnVar = scope.freshTypeVar()
            scope.unify(normalized, makeFunctionType(paramVar, returnVar))
            return (paramVar, returnVar)

        case _:
            error("Expected function type, found: " + normalized)
```

### Unification

```
// Unification - constrains two types to be equal
unify(t1, t2, scope):
    t1 = scope.apply(t1)  // apply current substitution
    t2 = scope.apply(t2)

    match (t1, t2):
        case (ParameterReference(a), ParameterReference(b)) if a == b:
            return  // same type variable, success

        case (ParameterReference(a), t) if scope.isTypeVar(a):
            if occursIn(a, t):
                error("Infinite type: " + a + " occurs in " + t)
            scope.addSubstitution(a -> t)

        case (t, ParameterReference(a)) if scope.isTypeVar(a):
            unify(t2, t1, scope)  // flip and retry

        case (ValueReference(v1), ValueReference(v2)) if v1 == v2:
            return  // same concrete type, success

        case (FunctionApplication(f1, a1), FunctionApplication(f2, a2)):
            unify(f1, f2, scope)
            unify(a1, a2, scope)

        case (FunctionLiteral(p1, k1, b1), FunctionLiteral(p2, k2, b2)):
            unify(k1, k2, scope)
            // Alpha rename p2 to p1 in b2, then unify bodies
            unify(b1, substitute(b2, p2 -> p1), scope)

        case _:
            error("Type mismatch: " + t1 + " vs " + t2)
```

### Summary of Key Functions

| Function | Purpose |
|----------|---------|
| `typecheckValue` | Entry point - orchestrates the phases |
| `openTypeLambdas` | Peels generic parameters from type, adds to scope |
| `openValueLambdas` | Peels function parameters, checks types match, adds to scope |
| `infer` | Recursive descent type inference, bottom-up |
| `eval` | Normalizes type expressions via beta reduction |
| `expectFunctionType` | Destructures `Function(A)(B)` into `(A, B)` |
| `unify` | Constrains types to be equal, builds substitution |

### Data Flow

```
ResolvedValue
    │
    ▼
┌─────────────────────┐
│  openTypeLambdas    │  ← Brings A, B, ... into scope as type params
└─────────────────────┘
    │
    ▼
┌─────────────────────┐
│  openValueLambdas   │  ← Brings f, a, ... into scope as value params
└─────────────────────┘     Checks declared param types against expected
    │
    ▼
┌─────────────────────┐
│  infer(body)        │  ← Recursively infers body type
└─────────────────────┘     Calls eval() for type normalization
    │                       Calls unify() at application sites
    ▼
┌─────────────────────┐
│  unify with return  │  ← Final check: body type = declared return type
└─────────────────────┘
    │
    ▼
TypeCheckResult (constraints, substitution)
```

## Where Type Checking Happens

For each `ResolvedValue`:

1. **Get the declared type** from `typeExpression`
   - If it contains lambdas at the outer level, those are generic parameters
   - The innermost expression is the "return type"

2. **Check the body** (if present)
   - Infer the type of the body expression
   - Unify with the declared return type

3. **For each expression in the body**:

   | Expression Type | Type Inference |
   |-----------------|----------------|
   | `IntegerLiteral` | Returns the integer type (e.g., `Int`) |
   | `StringLiteral` | Returns the string type |
   | `ParameterReference` | Lookup in scope, return its declared type |
   | `ValueReference` | Lookup the value, return its type (instantiating generics as fresh vars) |
   | `FunctionApplication(f, arg)` | Infer type of `f`, must be function type `(A -> B)`, infer type of `arg`, unify with `A`, return `B` |
   | `FunctionLiteral(p, pType, body)` | Add `p: pType` to scope, infer body type `B`, return `pType -> B` |

## Implementation Steps

### Phase 1: Core Infrastructure

1. **Create `TypeExpr` representation** (`typecheck2/fact/TypeExpr.scala`)
   - Sealed trait with cases for type variables, references, applications, lambdas
   - `Show` instance for error messages
   - Equality/comparison utilities

2. **Create expression evaluator** (`typecheck2/eval/ExpressionEvaluator.scala`)
   - Pure expression evaluator for compile-time computation
   - Support for beta reduction
   - Substitution mechanism
   - Normalization to head-normal form

3. **Create unification engine** (`typecheck2/unify/TypeUnification2.scala`)
   - Constraint-based unification
   - Occurs check
   - Substitution composition
   - Support for both modes via configuration

### Phase 2: Type Checking Processor

4. **Create type checking scope** (`typecheck2/processor/TypeCheckScope.scala`)
   - Track type variables in scope
   - Track value bindings with their types
   - Support scoped type variable introduction (for generics)

5. **Create polymorphic type checker** (`typecheck2/processor/PolymorphicTypeChecker.scala`)
   - Transform `ResolvedValue` → `PolymorphicTypedValue`
   - Introduce type variables for generics
   - Collect constraints through expression traversal
   - Produce substitution/constraints

6. **Create `PolymorphicTypedValue` fact** (`typecheck2/fact/PolymorphicTypedValue.scala`)
   - Contains the resolved value plus:
   - Inferred types for each subexpression
   - Remaining constraints/type variables
   - Substitution from unification

### Phase 3: Monomorphic Checking

7. **Create specialization request fact** (`typecheck2/fact/SpecializationRequest.scala`)
   - Contains: value to specialize + concrete type arguments
   - Triggered by monomorphization pass

8. **Create monomorphic type checker** (`typecheck2/processor/MonomorphicTypeChecker.scala`)
   - Takes `PolymorphicTypedValue` + concrete type args
   - Substitutes type args into all type expressions
   - Evaluates type expressions to normal form
   - Verifies all constraints are satisfied
   - Produces `MonomorphicTypedValue`

9. **Create `MonomorphicTypedValue` fact** (`typecheck2/fact/MonomorphicTypedValue.scala`)
   - Fully resolved types for every expression
   - No type variables remaining
   - Ready for code generation

### Phase 4: Integration

10. **Update processor pipeline**
    - Register new processors in `BasePlugin`
    - Define fact dependencies

11. **Error reporting**
    - Type mismatch errors with context
    - Show expected vs found types
    - Point to relevant source locations

12. **Testing**
    - Unit tests for evaluator
    - Unit tests for unification
    - Integration tests for both phases
    - Error message quality tests

## Key Design Decisions

### Expression Stack Handling

The `ExpressionStack` has multiple layers. For type checking:
- Layer 0 (bottom) is the runtime value
- Layer 1 is its type
- We primarily work with layer 1 for type checking
- Higher layers (kinds) can be checked similarly if needed

### Handling the Empty Type

For `Lambda("A", Empty, body)` (unbounded generic):
- `Empty` means "any type" or `Type` (the kind of types)
- In polymorphic mode, `A` is a fresh type variable with no constraints
- Could later support bounds: `Lambda("A", SomeConstraint, body)`

### Recursive Types and References

When looking up `ValueReference(vfqn)`:
- Get the value's type
- If the type contains generics, instantiate them as fresh type variables
- Cache to handle recursive definitions (tie the knot lazily)

### Function Type Representation

Functions are represented as:
```
Function(A)(B)  -- curried application
```
Where `Function` is a built-in type constructor.

In the expression model: `App(App(Ref("Function"), A), B)` represents `A -> B`.

## Summary

The type checking system needs:

1. **One expression evaluator** - normalizes type expressions via beta reduction; works identically on symbolic and concrete expressions
2. **Type unification** - compares normalized expressions; generates constraints when comparing symbolic terms, checks equality for ground terms
3. **Two-phase checking** (same evaluator, different inputs):
   - Polymorphic: evaluates expressions containing type parameter references → partially-reduced results → unification generates constraints
   - Monomorphic: substitutes concrete types first → evaluates to ground terms → unification checks equality
4. **Integration with fact system** - new facts for typed values

The key insight is that since types are values, type checking becomes expression equivalence checking. **Symbolic evaluation is not a separate evaluator** - it's the same beta-reduction process, but operating on expressions that contain free type variables. The evaluator mechanically substitutes and reduces; it simply can't eliminate `ParameterReference` nodes that refer to type parameters still in scope. After monomorphization substitutes concrete types for those parameters, the same evaluator produces fully ground results.
