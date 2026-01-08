# Action Plan: Unifying Types with Values and Functions

## Vision

Transform ELIOT's internal model so that types are values and type constructors are functions, while maintaining static type checking through compile-time partial evaluation. This creates a dependently-typed core that optimizes to efficient, statically-typed code.

## Guiding Principles

1. **Incremental transformation** - Each phase produces a working compiler
2. **Backwards compatibility** - Don't break existing code until necessary
3. **Test-driven** - Every change must have tests
4. **User-invisible initially** - Internal changes don't affect surface syntax at first
5. **Optimization-aware** - Design for partial evaluation from the start

---

## Phase 1: Unified Internal Representation (Foundation)

**Goal**: Represent types as expressions internally while keeping separate namespaces.

### Step 1.1: Create Expression-Based Type Representation

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/fact/UnifiedExpression.scala`

Create a unified expression type that can represent both values and types:

```scala
sealed trait UnifiedExpression

object UnifiedExpression {
  // Value-level constructs
  case class FunctionApplication(target: Sourced[UnifiedExpression], argument: Sourced[UnifiedExpression])
  case class FunctionLiteral(parameter: ArgumentDefinition, body: Sourced[UnifiedExpression])
  case class ParameterReference(name: Sourced[String])
  case class ConstantReference(fqn: Sourced[UnifiedFQN])
  case class IntegerLiteral(value: Sourced[BigInt])
  case class StringLiteral(value: Sourced[String])

  // Type-level constructs (represented as special constants)
  case class TypeConstant(typeFQN: Sourced[TypeFQN])  // e.g., String, Person
  case class UniverseConstant(level: Int)              // Type, Type₁, Type₂, ...
  case class GenericParameterConstant(name: Sourced[String])  // Type variables
}

// Unified FQN for both types and values
case class UnifiedFQN(moduleName: ModuleName, name: String)
```

**Tests**:
- Create `UnifiedExpressionTest.scala` testing construction of each variant
- Test equality and pattern matching

### Step 1.2: Convert TypeReference to UnifiedExpression

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/TypeReferenceConverter.scala`

Create bidirectional conversion between existing `TypeReference` and `UnifiedExpression`:

```scala
object TypeReferenceConverter {
  def toUnified(typeRef: TypeReference): UnifiedExpression
  def fromUnified(expr: UnifiedExpression): Option[TypeReference]
}
```

**Algorithm**:
- `DirectTypeReference(TypeFQN, generics)` → `FunctionApplication*(TypeConstant(TypeFQN), generics...)`
- `GenericTypeReference(name, generics)` → `FunctionApplication*(GenericParameterConstant(name), generics...)`

**Tests**:
- Round-trip conversion for all type forms
- Convert complex types like `Function[A, Function[B, C]]`
- Convert higher-kinded types like `F[A]` where `F` is a type parameter

### Step 1.3: Parallel Type Checking Path

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/UnifiedTypeChecker.scala`

Implement type checking using unified representation:

```scala
class UnifiedTypeChecker {
  def typeCheck(expr: UnifiedExpression, expectedType: UnifiedExpression): CompilationIO[UnifiedExpression]
  def inferType(expr: UnifiedExpression): CompilationIO[UnifiedExpression]
}
```

**Implementation**:
- Mirror the logic from existing `TypeCheckProcessor`
- Convert to `UnifiedExpression` at start, convert back at end
- Ensure identical results to existing type checker

**Tests**:
- Run all existing type checker tests through unified path
- Compare results with original type checker
- Test that both paths accept/reject the same programs

### Step 1.4: Feature Flag for Unified Path

**File**: `base/src/com/vanillasource/eliot/eliotc/CompilerOptions.scala`

Add compiler option to choose type checking implementation:

```scala
case class CompilerOptions(
  useUnifiedTypeChecker: Boolean = false,
  // ... existing options
)
```

**Tests**:
- Compile standard library with both paths
- Compile all examples with both paths
- Verify identical outputs

**Milestone**: Can type-check all existing code using unified representation internally.

---

## Phase 2: Unified Namespace (Merging Worlds)

**Goal**: Merge type and value namespaces so types can be referenced as values.

### Step 2.1: Introduce Universe Types

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/fact/Universe.scala`

```scala
case class Universe(level: Int) {
  require(level >= 0, "Universe level must be non-negative")

  def next: Universe = Universe(level + 1)
}

object Universe {
  val Type0: Universe = Universe(0)  // Type of ordinary values
  val Type1: Universe = Universe(1)  // Type of Type0
  val Type2: Universe = Universe(2)  // Type of Type1
}
```

**Change**: Every type now has a type!
- `String : Type`
- `Function : Type → Type → Type`
- `Type : Type₁`

**Tests**:
- Test universe level calculation
- Test that types have proper universe levels

### Step 2.2: Unified Scope with Single Dictionary

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/UnifiedScope.scala`

Merge `functionDictionary` and `typeDictionary`:

```scala
case class UnifiedScope(
  // Single namespace for both types and values
  dictionary: Map[String, UnifiedFQN],

  // Track what kind of thing each name refers to
  kinds: Map[UnifiedFQN, UnifiedExpression],  // The type/kind of each definition

  // Local bindings (parameters, let bindings)
  localBindings: Map[String, ArgumentDefinition],

  // Parent scope for lexical nesting
  parent: Option[UnifiedScope] = None
)
```

**Migration Strategy**:
1. Initially populate with both types and values
2. Types get universe types: `String : Type`, `Type : Type₁`
3. Values get their existing types: `println : Function[String, IO[Unit]]`

**Tests**:
- Test scope lookup with types and values
- Test shadowing behavior
- Test universe level tracking

### Step 2.3: Unified Resolver

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/UnifiedResolver.scala`

Create resolver that doesn't distinguish types from values:

```scala
class UnifiedResolver(scope: UnifiedScope) {
  def resolve(name: String): CompilationIO[UnifiedFQN]
  def resolveExpression(expr: ast.Expression): CompilationIO[UnifiedExpression]
}
```

**Key Change**: Resolution is purely name-based, not type-based.
- Look up name in single dictionary
- Return the resolved FQN
- Let type checking verify it's used correctly

**Tests**:
- Resolve type references and value references uniformly
- Test that qualified names work: `eliot.lang.String.String`
- Test import behavior

### Step 2.4: Compatibility Layer

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/CompatibilityLayer.scala`

Maintain backwards compatibility with existing pipeline:

```scala
object CompatibilityLayer {
  // Extract types from unified scope for old pipeline
  def extractTypeDictionary(scope: UnifiedScope): Map[String, TypeFQN]
  def extractFunctionDictionary(scope: UnifiedScope): Map[String, FunctionFQN]

  // Merge old dictionaries into unified scope
  def mergeIntoUnifiedScope(
    typeDictionary: Map[String, TypeFQN],
    functionDictionary: Map[String, FunctionFQN]
  ): UnifiedScope
}
```

**Tests**:
- Round-trip conversion between old and new scopes
- Verify no information loss

**Milestone**: Types and values share a namespace, but surface syntax unchanged.

---

## Phase 3: Type-Level Functions (Type Computation)

**Goal**: Allow functions that compute types, with normalization.

### Step 3.1: Expression Evaluator

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/Evaluator.scala`

Implement normalization/evaluation for expressions:

```scala
class Evaluator(environment: EvaluationEnvironment) {
  // Normalize an expression to weak head normal form (WHNF)
  def normalizeWHNF(expr: UnifiedExpression): CompilationIO[UnifiedExpression]

  // Normalize to full normal form
  def normalize(expr: UnifiedExpression): CompilationIO[UnifiedExpression]

  // Check if two expressions are definitionally equal (after normalization)
  def definitionallyEqual(e1: UnifiedExpression, e2: UnifiedExpression): CompilationIO[Boolean]
}

case class EvaluationEnvironment(
  // Map from FQN to definition
  definitions: Map[UnifiedFQN, UnifiedExpression],

  // Local bindings for evaluation
  bindings: Map[String, UnifiedExpression]
)
```

**Evaluation Rules**:
- `FunctionApplication(FunctionLiteral(param, body), arg)` → substitute `arg` for `param` in `body`
- `ConstantReference(fqn)` → look up definition and normalize
- Already normal forms: literals, type constants, universe constants

**Termination**: For now, use simple recursion depth limit. Later add termination checker.

**Tests**:
- Test beta reduction: `((x: A) -> body) arg` → `body[x := arg]`
- Test constant unfolding
- Test definitional equality
- Test infinite recursion detection

### Step 3.2: Convert Data Definitions to Type Functions

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/DataDefinitionConverter.scala`

Transform data definitions into functions that return types:

**Current**:
```scala
data Person(name: String, age: Byte)
```

**New Internal Representation**:
```scala
// Person is now a function that takes type parameters and returns a type
Person : Type -> Type -> Type
Person = (nameType: Type) -> (ageType: Type) -> <datatype constructor>

// The actual constructor function (previously auto-generated)
makePerson : nameType -> ageType -> Person nameType ageType

// Accessors
name : Person nameType ageType -> nameType
age : Person nameType ageType -> ageType
```

**For non-generic data**:
```scala
data String  // No fields, abstract type

// Becomes:
String : Type
String = <primitive type marker>
```

**Tests**:
- Convert simple data definitions
- Convert generic data definitions
- Test that constructor and accessor types are correct

### Step 3.3: Type-Level Function Definitions

**File**: Extend `base/src/com/vanillasource/eliot/eliotc/unified/UnifiedFunctionDefinition.scala`

Allow function definitions whose return type is `Type`:

```scala
// Example: A function that computes a type
def VectorType(n: Nat, elemType: Type): Type = ...

// Example: Conditional types
def IfThenElse(condition: Bool, thenType: Type, elseType: Type): Type = ...
```

**Implementation**:
- No special handling needed! Functions are functions.
- Type checker verifies return type is `Type` or `Type₁`, etc.

**Tests**:
- Define simple type-level functions
- Test application of type-level functions
- Test normalization of type function applications

### Step 3.4: Update Type Checker to Use Normalization

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/UnifiedTypeChecker.scala`

Update type checking to normalize before comparing:

```scala
def checkTypesEqual(t1: UnifiedExpression, t2: UnifiedExpression): CompilationIO[Boolean] = {
  for {
    n1 <- evaluator.normalize(t1)
    n2 <- evaluator.normalize(t2)
    equal <- evaluator.definitionallyEqual(n1, n2)
  } yield equal
}
```

**Tests**:
- Test that `Function[A, B]` equals `Function A B` (after normalization)
- Test that type synonyms work
- Test that computed types are recognized as equal

**Milestone**: Can define and use type-level functions internally.

---

## Phase 4: Dependent Types (Types Depend on Values)

**Goal**: Allow types to depend on runtime values, with compile-time evaluation.

### Step 4.1: Dependent Function Types

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/DependentTypes.scala`

Extend function types to allow dependency:

**Current**: `Function[A, B]` - type `B` cannot refer to the value of type `A`

**New**: `(x: A) -> B(x)` - type `B` can reference `x`

**Internal Representation**:
```scala
case class DependentFunctionType(
  parameter: ArgumentDefinition,  // (x: A)
  resultType: UnifiedExpression   // B, which may reference x
) extends UnifiedExpression
```

**Non-dependent functions are special case**: `Function[A, B]` is sugar for `(x: A) -> B` where `B` doesn't mention `x`.

**Tests**:
- Represent dependent function types
- Type check dependent functions
- Test substitution in dependent types

### Step 4.2: Dependent Data Types

**File**: Update `DataDefinitionConverter.scala`

Allow data types to be indexed by values:

**Example**:
```scala
// Vector indexed by length
data Vector(n: Nat, elemType: Type)(elements: ...)

// Creates:
Vector : Nat -> Type -> Type
Vector n elemType = <datatype with length n>
```

**Tests**:
- Define length-indexed vectors
- Define types indexed by other values
- Test type checking of indexed types

### Step 4.3: Dependent Pattern Matching

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/DependentPatternMatching.scala`

Extend pattern matching to refine types based on matched values:

**Example**:
```scala
def head(vec: Vector (Succ n) A): A = match vec {
  Cons(x, _) -> x  // Type checker knows vec is non-empty
}
```

**Implementation**:
- Track type refinements in each match branch
- Update environment with refined types
- Verify coverage accounting for impossible patterns

**Tests**:
- Test pattern matching on indexed types
- Test type refinement
- Test exhaustiveness checking with dependent types

### Step 4.4: Compile-Time Value Tracking

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/CompileTimeEvaluation.scala`

Track which values are known at compile time:

```scala
sealed trait ValueOrigin
object ValueOrigin {
  case object CompileTime    // Known at compile time (can use in types)
  case object Runtime        // Only known at runtime
}

case class TypedUnifiedExpression(
  expression: UnifiedExpression,
  exprType: UnifiedExpression,
  valueOrigin: ValueOrigin
)
```

**Rules**:
- Literals are compile-time
- Function parameters are generally runtime (unless in type position)
- Applications of pure functions to compile-time values are compile-time
- Type parameters are compile-time

**Tests**:
- Test compile-time value propagation
- Test that runtime values cannot appear in types (or are evaluated at compile time)
- Test error messages for invalid runtime dependencies in types

**Milestone**: Full dependent type system working internally.

---

## Phase 5: Optimization & Partial Evaluation (Making It Practical)

**Goal**: Prove purity and termination, enable compile-time evaluation, erase types at runtime.

### Step 5.1: Purity Analysis

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/PurityAnalysis.scala`

Analyze which functions are pure:

```scala
sealed trait PurityStatus
object PurityStatus {
  case object Pure           // No side effects, deterministic
  case object Impure        // Has side effects (IO)
  case object Unknown       // Not yet analyzed
}

class PurityAnalyzer {
  def analyzePurity(fqn: UnifiedFQN): CompilationIO[PurityStatus]
}
```

**Rules**:
- Literals are pure
- Lambdas are pure if body is pure
- Function application is pure if both function and argument are pure
- IO operations are impure
- Recursion is pure if recursive calls are pure

**Tests**:
- Test purity of simple functions
- Test that IO functions are marked impure
- Test purity propagation

### Step 5.2: Termination Checking

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/TerminationChecker.scala`

Ensure type-level functions terminate:

**Approach**: Use **structural recursion checking**:

```scala
class TerminationChecker {
  def checkTermination(fqn: UnifiedFQN): CompilationIO[TerminationStatus]
}

sealed trait TerminationStatus
object TerminationStatus {
  case object Terminates
  case object MayNotTerminate
  case class TerminationError(reason: String)
}
```

**Rules**:
- Non-recursive functions terminate
- Recursive functions terminate if all recursive calls are on structurally smaller arguments
- Use sized types for complex termination proofs

**Tests**:
- Test that simple recursion on naturals terminates
- Test that infinite loops are rejected
- Test structural recursion checker

### Step 5.3: Partial Evaluator

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/PartialEvaluator.scala`

Implement aggressive partial evaluation for compile-time code:

```scala
class PartialEvaluator {
  // Evaluate as much as possible given compile-time values
  def partialEval(
    expr: UnifiedExpression,
    compileTimeEnv: Map[String, UnifiedExpression]
  ): CompilationIO[UnifiedExpression]
}
```

**Optimizations**:
- Constant folding
- Function inlining
- Dead code elimination
- Specialization of polymorphic functions

**Tests**:
- Test that type computations reduce to constants
- Test that specialized code is generated
- Test performance improvement

### Step 5.4: Type Erasure

**File**: `base/src/com/vanillasource/eliot/eliotc/unified/TypeErasure.scala`

Erase type information after type checking:

```scala
class TypeEraser {
  // Remove all type abstractions and applications
  def erase(expr: TypedUnifiedExpression): CompilationIO[UntypedExpression]
}

sealed trait UntypedExpression {
  // Only runtime-relevant constructs remain
  case class FunctionApplication(target: UntypedExpression, argument: UntypedExpression)
  case class FunctionLiteral(body: UntypedExpression)  // No parameter type
  case class ConstantReference(fqn: UnifiedFQN)
  case class IntegerLiteral(value: BigInt)
  case class StringLiteral(value: String)
}
```

**Rules**:
- Remove type abstractions (type parameters)
- Remove type applications
- Remove proof terms (for dependent types)
- Keep only computationally relevant code

**Tests**:
- Test that erased code is smaller
- Test that erased code has correct runtime behavior
- Test integration with JVM backend

### Step 5.5: Update JVM Backend

**File**: Update `jvm/src/com/vanillasource/eliot/jvm/` files

Modify JVM backend to work with type-erased code:

**Changes**:
- Accept `UntypedExpression` instead of `TypedExpression`
- All type information already erased
- Generate efficient JVM bytecode without type overhead

**Tests**:
- Compile examples to JVM
- Run generated code
- Verify performance is comparable to current compiler

**Milestone**: Full dependently-typed compiler with compile-time evaluation and runtime type erasure.

---

## Phase 6: Surface Syntax & User-Facing Features (Optional)

**Goal**: Expose dependent types to users through syntax extensions.

### Step 6.1: Dependent Function Syntax

Allow users to write dependent function types:

**Syntax**:
```
// Current: Function[A, B]
// New: (x: A) -> B

def replicate(n: Nat, x: A): Vector n A = ...
```

**Parser Changes**:
- Allow `(name: type)` in type position
- Distinguish from regular function syntax

**Tests**:
- Parse dependent function types
- Test precedence and associativity

### Step 6.2: Indexed Data Types

Allow users to define indexed types:

**Syntax**:
```
data Vector(n: Nat, elemType: Type) {
  Nil: Vector Zero elemType
  Cons(x: elemType, rest: Vector n elemType): Vector (Succ n) elemType
}
```

**Tests**:
- Parse indexed data definitions
- Test constructor generation
- Test pattern matching

### Step 6.3: Compile-Time Computation Syntax

Add syntax for explicit compile-time evaluation:

**Syntax**:
```
const ten: Nat = 5 + 5  // Evaluated at compile time

def foo(): Vector ten String = ...  // Use compile-time constant in type
```

**Tests**:
- Test compile-time constant definitions
- Test error messages for non-terminating compile-time code

---

## Testing Strategy

### Regression Tests
- All existing tests must continue to pass after each phase
- Use feature flags to test both old and new implementations

### Property-Based Tests
- Type soundness: Well-typed programs don't go wrong
- Normalization: Evaluation is deterministic and terminating
- Erasure correctness: Erased code has same semantics

### Performance Tests
- Compile-time evaluation should be fast
- Generated code should be efficient
- Type checking should scale to large programs

### Integration Tests
- Compile standard library with new pipeline
- Compile example programs
- Generate and run JVM code

---

## Migration Strategy

### Phase-by-Phase Rollout

1. **Phase 1-2**: Internal changes only, no user-visible changes
   - Feature flag to enable unified pipeline
   - Both pipelines coexist
   - Thoroughly test equivalence

2. **Phase 3-4**: Power users can opt-in to dependent types
   - New syntax behind language version flag
   - Standard library remains simple

3. **Phase 5**: Optimization becomes default
   - All code uses new pipeline
   - Remove old pipeline

4. **Phase 6**: Full dependent types available
   - Document best practices
   - Provide examples and tutorials

### Backwards Compatibility

- **Old code continues to work**: Simple types are special case of dependent types
- **Gradual migration**: Modules can opt-in to dependent types individually
- **Error messages**: Maintain clear errors for common mistakes

---

## Open Questions & Research Areas

### 1. Universe Polymorphism
Should we support universe polymorphism (functions polymorphic over universe levels)?
- Pro: More expressive
- Con: More complex

### 2. Proof Irrelevance
Should proofs (evidence of properties) be erased separately from types?
- Pro: Smaller generated code
- Con: More complex type system

### 3. Compile-Time Performance
How to make compile-time evaluation fast enough for real programs?
- Caching normalized forms
- Incremental compilation
- Parallel evaluation

### 4. Error Messages
How to make type errors understandable in dependent type system?
- Show normalized vs. unnormalized types
- Highlight where types diverged
- Suggest fixes

### 5. Standard Library Design
How should standard library leverage dependent types?
- Length-indexed vectors
- Bounded integers
- Resource-tracked IO
- Memory region types (for microcontrollers!)

---

## Success Criteria

### Technical
- [ ] All existing tests pass with new pipeline
- [ ] Can define and use dependent types
- [ ] Type checking is sound (no false negatives)
- [ ] Generated code is efficient (comparable to current compiler)
- [ ] Compile times are reasonable (< 2x slowdown)

### User Experience
- [ ] Simple programs remain simple to write
- [ ] Error messages are understandable
- [ ] Documentation explains dependent types clearly
- [ ] Examples demonstrate power of dependent types

### Microcontroller Focus
- [ ] Can express resource constraints in types (buffer sizes, memory regions)
- [ ] Can verify resource usage at compile time
- [ ] Zero runtime overhead for type checking
- [ ] Generated code is suitable for embedded systems

---

## Timeline Estimate

**Phase 1**: 2-3 months - Unified internal representation
**Phase 2**: 2-3 months - Unified namespace
**Phase 3**: 3-4 months - Type-level functions
**Phase 4**: 4-5 months - Dependent types
**Phase 5**: 3-4 months - Optimization & partial evaluation
**Phase 6**: 2-3 months - Surface syntax

**Total**: 16-22 months for full implementation

**Note**: This assumes one full-time developer. Phases 1-3 can proceed with less risk. Phases 4-6 are more research-heavy.

---

## Resources & References

### Dependent Type Theory
- "Type Theory and Formal Proof" - Rob Nederpelt & Herman Geuvers
- "Dependent Types at Work" - Ana Bove & Peter Dybjer
- Idris documentation: https://idris-lang.org/
- Agda documentation: https://agda.readthedocs.io/

### Partial Evaluation
- "Partial Evaluation and Automatic Program Generation" - Jones, Gomard, Sestoft
- "Staged Compilation" - Taha & Sheard

### Implementation Techniques
- "Elaborating Dependent (Co)pattern Matching" - Cockx & Abel
- "A Tutorial Implementation of a Dependently Typed Lambda Calculus" - Löh, McBride, Swierstra

### Related Systems
- Idris: Dependently-typed with totality checking
- Agda: Proof assistant with dependent types
- Lean: Theorem prover with dependent types
- Coq: Proof assistant with universe hierarchy
- F*: Verification-oriented with dependent types

---

## Conclusion

This plan transforms ELIOT into a dependently-typed language where types are values and type constructors are functions. The key insight is that compile-time partial evaluation allows dependent types to remain zero-cost at runtime, making them practical for microcontrollers.

The incremental approach ensures each phase produces a working compiler, reducing risk and enabling continuous validation. The migration strategy maintains backwards compatibility while gradually exposing more power to users.

For a microcontroller-focused language, this opens up powerful possibilities: compile-time verification of buffer sizes, memory safety, resource usage, and protocol correctness—all without runtime overhead.
