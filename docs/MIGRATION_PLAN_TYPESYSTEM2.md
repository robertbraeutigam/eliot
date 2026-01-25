# Migration Plan: Replacing Old Typesystem with Typesystem2 for JVM Generation

## Executive Summary

This document outlines the plan to migrate the JVM code generation from the old typesystem (`typesystem/`) to the new typesystem2 (`typesystem2/`). The migration involves adapting `JvmClassGenerator` and `JvmProgramGenerator` to consume facts from the new processing track: `core` → `resolve2` → `typesystem2`.

---

## 1. Current Architecture Overview

### 1.1 Old Processing Track (Current)
```
SourceAST
    ↓ Parser
ParsedAST
    ↓ Resolver
ResolvedFunction (FunctionFQN, TypeReference-based)
    ↓ TypeCheckProcessor
TypeCheckedFunction (TypedExpression with TypeReference)
    ↓ UncurryingProcessor
UncurriedFunction (multi-param functions, TypeReference)
    ↓ UsedSymbolsProcessor
UsedSymbols (Seq[FunctionFQN], Seq[TypeFQN])
    ↓ JvmClassGenerator
GeneratedModule (bytecode)
    ↓ JvmProgramGenerator
JAR file
```

### 1.2 New Processing Track (Emerging)
```
SourceAST
    ↓ CoreProcessor
CoreAST (ExpressionStack, curried)
    ↓ ValueResolver (resolve2)
ResolvedValue (ValueFQN, ExpressionStack[Expression])
    ↓ SymbolicTypeCheckProcessor
TypeCheckedValue (ExpressionStack[TypedExpression], NormalizedExpression types)
    ↓ ??? (New Uncurry)
???
    ↓ ??? (New UsedSymbols)
???
    ↓ ??? (New JvmClassGenerator)
GeneratedModule
```

---

## 2. Key Structural Differences

### 2.1 Module System / Naming

| Aspect | Old System | New System |
|--------|------------|------------|
| Function identifier | `FunctionFQN(ModuleName, functionName)` | `ValueFQN(ModuleName, name)` |
| Type identifier | `TypeFQN(ModuleName, typeName)` | `ValueFQN(ModuleName, name)` with convention |
| Separator in show | `module.function` | `module::value` |
| Type convention | Separate namespace | Data types have `$DataType` suffix |

**Mismatch**: JVM generator uses `FunctionFQN` and `TypeFQN` throughout for method naming, class naming, and lookups.

**Solution Options**:
1. **Option A (Recommended)**: Create adapter layer that converts `ValueFQN` to old-style identifiers for JVM generation
2. **Option B**: Update all JVM naming logic to use `ValueFQN` directly, updating `NativeType` and `NativeImplementation` mappings

### 2.2 Type Representation

| Aspect | Old: TypeReference | New: NormalizedExpression |
|--------|-------------------|---------------------------|
| Concrete type | `DirectTypeReference(TypeFQN, params)` | `ValueRef(ValueFQN, args)` |
| Generic/param | `GenericTypeReference(name, params)` | `UniversalVar(name)` or `ParameterRef(name)` |
| Function type | Implicit in structure | `FunctionType(param, return, source)` |
| Type application | Nested in generic params | `SymbolicApplication(target, arg)` |
| Unification | N/A (post-resolution) | `UnificationVar(id)` |

**Mismatch**: `JvmClassGenerator` uses `simpleType(TypeReference): TypeFQN` to extract JVM types. This pattern doesn't work with `NormalizedExpression`.

**Solution Options**:
1. **Option A (Recommended)**: Create `evalToJvmType(NormalizedExpression): TypeFQN` that evaluates normalized expressions to concrete JVM types:
   - `ValueRef(vfqn, [])` → Extract TypeFQN from ValueFQN
   - `FunctionType(_, _)` → `systemLangType("Function")`
   - `UniversalVar(_)` → `systemAnyType` (type erasure)
   - `ParameterRef(_)` → Error (should not appear in fully-typed code)
   - `UnificationVar(_)` → Error (should be resolved)
2. **Option B**: Keep `NormalizedExpression` throughout and adapt all JVM operations to handle symbolic types

### 2.3 Expression Structure

| Aspect | Old: TypedExpression | New: TypedExpression |
|--------|---------------------|----------------------|
| Type annotation | `expressionType: TypeReference` | `expressionType: NormalizedExpression` |
| Structure | Flat, curried | `ExpressionStack[TypedExpression]` |
| Application args | Single-arg (curried) | Single-arg in stack |
| Lambda params | Single-param + nested type | Single-param with `ExpressionStack` param type |

**Mismatch**: Old expressions are flat; new expressions use `ExpressionStack` with multi-level representations.

**Solution Options**:
1. **Option A (Recommended)**: Create extraction utilities:
   ```scala
   def extractRuntime(stack: ExpressionStack[TypedExpression]): Option[TypedExpression]
   def extractSignature(stack: ExpressionStack[TypedExpression]): Option[TypedExpression]
   ```
2. **Option B**: Modify uncurrying to flatten stacks before JVM generation

### 2.4 Uncurrying

| Aspect | Old: UncurryingProcessor | New: ??? |
|--------|-------------------------|----------|
| Input | `TypeCheckedFunction` | `TypeCheckedValue` |
| Output | `UncurriedFunction` | `UncurriedValue` (new) |
| Parameter types | `ArgumentDefinition` with `TypeReference` | Needs adaptation |
| Return type | `TypeReference` | `NormalizedExpression` |

**Mismatch**: Uncurrying logic needs to work with `ExpressionStack` and `NormalizedExpression`.

**Solution**: Create new `UncurryingProcessor2` that:
- Takes `TypeCheckedValue` as input
- Produces `UncurriedValue` (new fact) with:
  ```scala
  case class UncurriedValue(
    vfqn: ValueFQN,
    name: Sourced[String],
    parameters: Seq[UncurriedParameter],  // New type with NormalizedExpression
    returnType: NormalizedExpression,
    body: Option[Sourced[UncurriedTypedExpression2]]  // New expression type
  )
  ```

### 2.5 Used Symbols Collection

| Aspect | Old: UsedSymbols | New: ??? |
|--------|-----------------|----------|
| Key | `FunctionFQN` | `ValueFQN` |
| Functions | `usedFunctions: Seq[Sourced[FunctionFQN]]` | N/A |
| Types | `usedTypes: Seq[Sourced[TypeFQN]]` | N/A |
| Unified | N/A | `usedValues: Seq[Sourced[ValueFQN]]` |

**Mismatch**: Old system tracks functions and types separately. New system unifies under `ValueFQN`.

**Solution Options**:
1. **Option A (Recommended)**: Create `UsedSymbols2` fact:
   ```scala
   case class UsedSymbols2(
     vfqn: ValueFQN,
     usedValues: Seq[Sourced[ValueFQN]]
   )
   ```
   Then derive function vs type distinction from naming convention (`$DataType` suffix).

2. **Option B**: Keep separate tracking but derive from unified values during collection.

### 2.6 Native Types and Implementations

**Current mappings** (in `jvm` module):

```scala
// NativeType.scala
val types: Map[TypeFQN, NativeType] = Map(
  systemLangType("String") -> ...,
  systemLangType("Function") -> ...,
  systemLangType("Unit") -> ...,
  systemLangType("Any") -> ...
)

// NativeImplementation.scala
val implementations: Map[FunctionFQN, NativeImplementation] = Map(
  FunctionFQN("String", "printlnInternal") -> ...,
  FunctionFQN("Unit", "unit") -> ...
)
```

**Mismatch**: These use old-style FQNs.

**Solution Options**:
1. **Option A (Recommended)**: Create parallel mappings for new system:
   ```scala
   // NativeType2.scala
   val types2: Map[ValueFQN, NativeType] = Map(...)

   // NativeImplementation2.scala
   val implementations2: Map[ValueFQN, NativeImplementation] = Map(...)
   ```

2. **Option B**: Create conversion utilities between old and new FQN types and reuse existing maps.

---

## 3. Key Questions and Decision Points

### Q1: Should we keep both systems running in parallel?

**Options**:
- **A (Recommended for migration)**: Yes, keep old system working while building new. Allows incremental migration and A/B testing.
- **B**: No, replace in-place. Faster but riskier.

### Q2: How to handle data types in the new system?

In the old system, data types produce:
1. A data class (inner class with fields)
2. A constructor function (module method)
3. Accessor functions (one per field)

The new system uses `ValueFQN` for everything.

**Options**:
- **A (Recommended)**: Use naming convention `$DataType` suffix to identify type definitions
- **B**: Add explicit metadata to `TypeCheckedValue` indicating if it's a data type
- **C**: Query for specific patterns in the type signature (e.g., presence of fields)

### Q3: How to extract JVM-compatible types from NormalizedExpression?

`NormalizedExpression` can represent symbolic types that have no JVM equivalent.

**Options**:
- **A (Recommended)**: Require all types to be fully resolved before JVM generation. Create `evaluateToJvmType` that errors on unresolved types.
- **B**: Use `Object` as fallback for unresolved types (loses type safety)
- **C**: Introduce a "JVM-ready" intermediate representation

### Q4: How to handle the ExpressionStack in code generation?

The new typed expressions are wrapped in `ExpressionStack`.

**Options**:
- **A (Recommended)**: Uncurrying flattens stacks to produce flat expressions for JVM
- **B**: JVM generator learns to navigate stacks
- **C**: Create separate "flattening" phase between typechecking and uncurrying

### Q5: Where should FQN conversion logic live?

**Options**:
- **A (Recommended)**: Create `FQNAdapter` in `jvm` module that converts between ValueFQN and TypeFQN/FunctionFQN
- **B**: Add conversion methods to ValueFQN class
- **C**: Refactor JVM generator to use ValueFQN natively

---

## 4. Proposed Migration Phases

### Phase 1: Create New Supporting Infrastructure

**New files to create**:

1. `base/src/.../uncurry2/UncurriedValue.scala` - New uncurried fact
2. `base/src/.../uncurry2/UncurriedTypedExpression2.scala` - New expression type using NormalizedExpression
3. `base/src/.../uncurry2/UncurryingProcessor2.scala` - New uncurrying processor
4. `base/src/.../used2/UsedSymbols2.scala` - New used symbols fact
5. `base/src/.../used2/UsedSymbolsProcessor2.scala` - New used symbols processor
6. `jvm/src/.../classgen/asm/JvmTypeEvaluator.scala` - Convert NormalizedExpression to JVM types
7. `jvm/src/.../classgen/processor/NativeType2.scala` - Native types with ValueFQN keys
8. `jvm/src/.../classgen/processor/NativeImplementation2.scala` - Native implementations with ValueFQN keys

### Phase 2: Create New JVM Generators

**New files to create**:

1. `jvm/src/.../classgen/processor/JvmClassGenerator2.scala` - New class generator consuming TypeCheckedValue
2. `jvm/src/.../jargen/JvmProgramGenerator2.scala` - New program generator

**Key changes**:
- Consume `UncurriedValue` instead of `UncurriedFunction`
- Use `UsedSymbols2` for symbol collection
- Use `JvmTypeEvaluator` for type conversions
- Update method/class naming for `ValueFQN`

### Phase 3: Wire Up Plugin System

**Modifications**:

1. `jvm/src/.../JvmPlugin.scala` - Register new processors
2. Ensure correct fact dependencies are declared

### Phase 4: Testing and Validation

1. Create parallel test suite for new generators
2. Compare outputs between old and new systems
3. Validate JAR files execute correctly

### Phase 5: Cleanup (Post-Migration)

1. Remove old typesystem processors
2. Remove old uncurrying
3. Remove old used symbols
4. Update JvmPlugin to only use new system

---

## 5. Detailed Component Specifications

### 5.1 JvmTypeEvaluator

```scala
// jvm/src/.../classgen/asm/JvmTypeEvaluator.scala
object JvmTypeEvaluator {
  def evalToTypeFQN(expr: NormalizedExpression): Either[String, TypeFQN] =
    expr match {
      case ValueRef(vfqn, _) =>
        Right(valueToTypeFQN(vfqn.value))
      case FunctionType(_, _, _) =>
        Right(systemLangType("Function"))
      case UniversalVar(_) =>
        Right(systemAnyType) // Type erasure
      case ParameterRef(name) =>
        Left(s"Cannot evaluate parameter reference ${name.value} to JVM type")
      case UnificationVar(id, _) =>
        Left(s"Unresolved unification variable: $id")
      case IntLiteral(_) =>
        Left("Integer literal cannot be a type")
      case StringLiteral(_) =>
        Left("String literal cannot be a type")
      case SymbolicApplication(_, _, _) =>
        Left("Symbolic application cannot be evaluated to JVM type")
    }

  private def valueToTypeFQN(vfqn: ValueFQN): TypeFQN = {
    // Convention: data types have $DataType suffix
    val typeName = if (vfqn.name.endsWith("$DataType"))
      vfqn.name.stripSuffix("$DataType")
    else
      vfqn.name
    TypeFQN(convertModuleName(vfqn.moduleName), typeName)
  }
}
```

### 5.2 UncurriedValue

```scala
// base/src/.../uncurry2/UncurriedValue.scala
case class UncurriedValue(
  vfqn: ValueFQN,
  name: Sourced[String],
  definition: UncurriedValueDefinition
) extends CompilerFact

case class UncurriedValueDefinition(
  parameters: Seq[UncurriedParameter],
  returnType: NormalizedExpression,
  body: Option[Sourced[UncurriedTypedExpression2]]
)

case class UncurriedParameter(
  name: Sourced[String],
  paramType: NormalizedExpression
)
```

### 5.3 UncurriedTypedExpression2

```scala
// base/src/.../uncurry2/UncurriedTypedExpression2.scala
case class UncurriedTypedExpression2(
  expressionType: NormalizedExpression,
  expression: Expression2
)

sealed trait Expression2
object Expression2 {
  case class FunctionApplication(
    target: Sourced[UncurriedTypedExpression2],
    arguments: Seq[Sourced[UncurriedTypedExpression2]]
  ) extends Expression2

  case class FunctionLiteral(
    parameters: Seq[UncurriedParameter],
    body: Sourced[UncurriedTypedExpression2]
  ) extends Expression2

  case class IntegerLiteral(value: Sourced[BigInt]) extends Expression2
  case class StringLiteral(value: Sourced[String]) extends Expression2
  case class ParameterReference(name: Sourced[String]) extends Expression2
  case class ValueReference(vfqn: Sourced[ValueFQN]) extends Expression2
}
```

### 5.4 UsedSymbols2

```scala
// base/src/.../used2/UsedSymbols2.scala
case class UsedSymbols2(
  vfqn: ValueFQN,
  usedValues: Seq[Sourced[ValueFQN]]
) extends CompilerFact {

  /** Values that are data type constructors (based on naming convention) */
  def usedDataTypes: Seq[Sourced[ValueFQN]] =
    usedValues.filter(_.value.name.endsWith("$DataType"))

  /** Values that are regular functions */
  def usedFunctions: Seq[Sourced[ValueFQN]] =
    usedValues.filterNot(_.value.name.endsWith("$DataType"))
}
```

---

## 6. Risk Assessment

### High Risk Areas

1. **Type Evaluation**: Converting `NormalizedExpression` to JVM types may fail for complex type expressions. Need comprehensive testing.

2. **Data Type Detection**: Relying on `$DataType` naming convention is fragile. Consider adding explicit metadata.

3. **Generic Parameters**: The old system tracks generic parameters explicitly. Need to ensure proper handling of `UniversalVar`.

4. **Lambda Closures**: Complex closure handling may break with new expression structure.

### Medium Risk Areas

1. **Native Mappings**: Parallel native type/implementation maps add maintenance burden.

2. **Module Name Differences**: Old uses `.` separator, new uses `::`. May cause issues in generated code.

3. **ExpressionStack Navigation**: Extracting runtime vs signature correctly is critical.

### Low Risk Areas

1. **Literal Handling**: Integer and string literals are straightforward in both systems.

2. **Parameter References**: Direct mapping between systems.

3. **JAR Generation**: No changes needed in JAR packaging logic.

---

## 7. Testing Strategy

### Unit Tests

1. `JvmTypeEvaluatorTest` - Test all NormalizedExpression variants
2. `UncurryingProcessor2Test` - Test flattening logic
3. `UsedSymbolsProcessor2Test` - Test symbol collection

### Integration Tests

1. Run both old and new pipelines on same source files
2. Compare generated bytecode structure
3. Execute generated JARs and compare outputs

### Regression Tests

1. All existing `examples/` must continue working
2. All existing tests must pass with new system

---

## 8. Open Questions for Discussion

1. **Should we unify ModuleName types?** Currently `module.fact.ModuleName` vs `module2.fact.ModuleName` exist separately.

2. **How should we handle data type field accessors?** Currently generated based on `ResolvedData.fields`. New system may need different approach.

3. **Is the `$DataType` convention the right approach?** Alternative: introduce `ValueKind` enum (Function, DataType, etc.) in `ResolvedValue`.

4. **Should JVM generators be refactored to be more modular?** Current `JvmClassGenerator` is 400+ lines and handles many concerns.

5. **How to handle the transition period?** Should both old and new pipelines be runnable simultaneously?

---

## 9. Recommended Implementation Order

1. **Week 1**: Create `JvmTypeEvaluator` and test thoroughly
2. **Week 2**: Create `UncurriedValue`, `UncurriedTypedExpression2`, and `UncurryingProcessor2`
3. **Week 3**: Create `UsedSymbols2` and `UsedSymbolsProcessor2`
4. **Week 4**: Create `NativeType2` and `NativeImplementation2` mappings
5. **Week 5**: Create `JvmClassGenerator2`
6. **Week 6**: Create `JvmProgramGenerator2` and wire up plugin
7. **Week 7**: Testing and bug fixes
8. **Week 8**: Cleanup old system (optional, can defer)

---

## 10. Success Criteria

1. All example programs compile and run correctly with new system
2. Generated bytecode is functionally equivalent to old system
3. No performance regression in compilation
4. Clean separation allows old system removal without breaking new system
