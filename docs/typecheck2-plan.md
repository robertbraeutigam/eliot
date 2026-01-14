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

### Evaluation Rules

```
eval(IntegerLiteral(n)) = IntegerLiteral(n)  -- values are themselves
eval(StringLiteral(s)) = StringLiteral(s)
eval(ParameterReference(p)) = ParameterReference(p)  -- stays symbolic in poly mode
eval(ValueReference(v)) = lookup(v) and evaluate if it has a body
eval(FunctionApplication(f, arg)) =
  let f' = eval(f)
  let arg' = eval(arg)
  if f' is FunctionLiteral(param, _, body):
    eval(substitute(body, param -> arg'))
  else:
    FunctionApplication(f', arg')  -- can't reduce further
eval(FunctionLiteral(p, t, b)) = FunctionLiteral(p, eval(t), b)  -- don't eval body yet
```

**Key insight**: In polymorphic mode, `ParameterReference` for type parameters blocks evaluation (stays symbolic). In monomorphic mode after substitution, everything can be evaluated to normal form.

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

### Polymorphic vs Monomorphic Differences

**Polymorphic**:
- Type variables for generic params remain as `TypeVar`
- Unification produces constraints/substitutions
- Result may still contain `TypeVar` references

**Monomorphic**:
- All type variables have been substituted
- Can normalize types by evaluation
- Unification is essentially equality on normal forms

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

1. **Expression evaluator** - to normalize type expressions
2. **Type unification** - with support for type variables and substitution
3. **Two-phase checking**:
   - Polymorphic: symbolic, constraint-based
   - Monomorphic: concrete, evaluation-based
4. **Integration with fact system** - new facts for typed values

The key insight is that since types are values, type checking becomes expression equivalence checking, which requires evaluation capabilities. Polymorphic checking defers evaluation of expressions involving type variables, while monomorphic checking fully evaluates after substitution.
