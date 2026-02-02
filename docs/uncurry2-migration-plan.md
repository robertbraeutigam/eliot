# Uncurry2 Migration Plan

This document outlines the plan to create a new `uncurry2` package that works with the new compilation pipeline (`symbolic` → `monomorphize` → `used2`) and ultimately replace the old pipeline (`typecheck` → `uncurry` → `used`) to connect to the JVM backend.

## Table of Contents

1. [Current Architecture Overview](#1-current-architecture-overview)
2. [New Pipeline Overview](#2-new-pipeline-overview)
3. [Uncurry2 Package Design](#3-uncurry2-package-design)
4. [Arity Selection Algorithm](#4-arity-selection-algorithm)
5. [JVM Backend Modifications](#5-jvm-backend-modifications)
6. [Migration Strategy](#6-migration-strategy)
7. [File Structure](#7-file-structure)
8. [Implementation Tasks](#8-implementation-tasks)

---

## 1. Current Architecture Overview

### Old Pipeline (BasePlugin)

```
Source → Tokenizer → ASTParser → ModuleProcessors → FunctionResolver → TypeResolver
       → TypeCheckProcessor → UncurryingProcessor → UsedSymbolsProcessor
                                    ↓
                            UncurriedFunction
                                    ↓
                            JvmClassGenerator
```

### Key Facts in Old Pipeline

| Fact | Location | Description |
|------|----------|-------------|
| `TypeCheckedFunction` | `typesystem/fact/` | Curried function with `TypedExpression` body |
| `UncurriedFunction` | `uncurry/` | Multi-parameter function with `UncurriedTypedExpression` body |
| `UsedSymbols` | `used/` | Simple list of used functions/types |

### JVM Backend Dependencies (Current)

The JVM backend (`JvmClassGenerator`) currently depends on:

1. **`UncurriedFunction`** - Provides multi-parameter function signatures
2. **`UncurriedTypedExpression`** - Expression tree with:
   - `FunctionApplication(target, arguments: Seq[...])` - multi-argument calls
   - `FunctionLiteral(parameters: Seq[...], body)` - multi-parameter lambdas
3. **`UsedSymbols`** - Determines which functions/types to generate
4. **`TypeReference`** - Type annotations using the old type system

---

## 2. New Pipeline Overview

### New Processing Chain

```
Source → Tokenizer → ASTParser → ModuleProcessors → ResolvedValue (resolve2)
       → SymbolicTypeCheckProcessor → MonomorphicTypeCheckProcessor → UsedNamesProcessor
                                              ↓
                                      MonomorphicValue
                                              ↓
                                         [uncurry2]
                                              ↓
                                     Uncurried2Value
                                              ↓
                                      JvmClassGenerator (modified)
```

### Key Facts in New Pipeline

| Fact | Location | Description |
|------|----------|-------------|
| `TypeCheckedValue` | `symbolic/fact/` | Polymorphic function with symbolic types |
| `MonomorphicValue` | `monomorphize/fact/` | Specialized function with concrete `Value` types |
| `UsedNames` | `used2/` | Statistics-driven usage information |

### UsedNames Statistics Structure

```scala
case class UsedNames(
    rootFQN: ValueFQN,
    usedNames: Map[ValueFQN, UsageStats]
)

case class UsageStats(
    // Type instantiations: e.g., id[Int], id[String] → Seq(Seq(Int), Seq(String))
    monomorphicTypeParameters: Seq[Seq[Value]],

    // Arity histogram: Map(arity → frequency)
    // Example: {0 → 5, 1 → 3, 2 → 10} means:
    //   - Used with 0 args 5 times (as value)
    //   - Used with 1 arg 3 times (partial application)
    //   - Used with 2 args 10 times (full application)
    directCallApplications: Map[Int, Int]
)
```

---

## 3. Uncurry2 Package Design

### 3.1 Package Location

```
base/src/com/vanillasource/eliot/eliotc/uncurry2/
├── fact/
│   ├── Uncurried2Value.scala          # Main output fact
│   └── Uncurried2Expression.scala     # Expression tree
├── processor/
│   └── Uncurrying2Processor.scala     # Main processor
└── AritySelector.scala                # Arity selection logic
```

### 3.2 Uncurried2Value (Output Fact)

```scala
package com.vanillasource.eliot.eliotc.uncurry2.fact

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/**
 * A monomorphic value that has been uncurried to its optimal arity
 * based on usage statistics.
 *
 * @param vfqn The fully qualified value name
 * @param typeArguments Concrete type arguments (from monomorphization)
 * @param name The sourced name
 * @param signature The concrete ground type (Value) after uncurrying
 * @param parameters The uncurried parameter list (optimal arity)
 * @param returnType The return type after uncurrying
 * @param body Optional uncurried expression body
 * @param targetArity The selected optimal arity for this function
 */
case class Uncurried2Value(
    vfqn: ValueFQN,
    typeArguments: Seq[Value],
    name: Sourced[String],
    signature: Value,
    parameters: Seq[Parameter2Definition],
    returnType: Value,
    body: Option[Sourced[Uncurried2Expression.Expression]],
    targetArity: Int
) extends CompilerFact {
  override def key(): CompilerFactKey[Uncurried2Value] =
    Uncurried2Value.Key(vfqn, typeArguments)
}

object Uncurried2Value {
  case class Key(vfqn: ValueFQN, typeArguments: Seq[Value])
      extends CompilerFactKey[Uncurried2Value]
}

case class Parameter2Definition(
    name: Sourced[String],
    parameterType: Value
)
```

### 3.3 Uncurried2Expression (Expression Tree)

```scala
package com.vanillasource.eliot.eliotc.uncurry2.fact

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/**
 * Expression tree after uncurrying, using concrete Value types.
 */
case class Uncurried2Expression(
    expressionType: Value,
    expression: Uncurried2Expression.Expression
)

object Uncurried2Expression {
  sealed trait Expression

  /**
   * Multi-argument function application.
   * Flattened from nested curried applications based on target arity.
   */
  case class FunctionApplication(
      target: Sourced[Uncurried2Expression],
      arguments: Seq[Sourced[Uncurried2Expression]]
  ) extends Expression

  /**
   * Multi-parameter function literal.
   * Flattened from nested lambdas up to optimal arity.
   */
  case class FunctionLiteral(
      parameters: Seq[Parameter2Definition],
      body: Sourced[Uncurried2Expression]
  ) extends Expression

  case class IntegerLiteral(value: Sourced[BigInt]) extends Expression

  case class StringLiteral(value: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  /**
   * Reference to an uncurried monomorphic value.
   */
  case class ValueReference(
      valueName: Sourced[ValueFQN],
      typeArguments: Seq[Value]
  ) extends Expression
}
```

### 3.4 Uncurrying2Processor

```scala
package com.vanillasource.eliot.eliotc.uncurry2.processor

import com.vanillasource.eliot.eliotc.monomorphize.fact.{MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.uncurry2.fact.{Uncurried2Expression, Uncurried2Value, Parameter2Definition}
import com.vanillasource.eliot.eliotc.used2.UsedNames
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor

/**
 * Processor that uncurries monomorphic values to their optimal arity
 * based on usage statistics from UsedNames.
 *
 * Input:  MonomorphicValue (curried form with single-argument applications)
 * Input:  UsedNames (usage statistics for arity selection)
 * Output: Uncurried2Value (multi-parameter form with optimal arity)
 */
class Uncurrying2Processor
    extends SingleKeyTypeProcessor[Uncurried2Value.Key] with Logging {

  override protected def generateFact(key: Uncurried2Value.Key): CompilerIO[Unit] =
    for {
      // Get the monomorphic value
      monomorphicValue <- getFactOrAbort(MonomorphicValue.Key(key.vfqn, key.typeArguments))

      // Get usage statistics (rooted from some entry point)
      // Note: We need to determine which UsedNames fact to use
      usageStats <- getUsageStatsFor(key.vfqn)

      // Select optimal arity based on statistics
      optimalArity = AritySelector.selectOptimalArity(usageStats, monomorphicValue.signature)

      // Extract parameters from signature up to optimal arity
      (parameters, returnType) = extractParameters(monomorphicValue.signature, optimalArity)

      // Convert body expression, uncurrying applications to optimal arities
      convertedBody = monomorphicValue.runtime.map(_.map(expr =>
        convertExpression(expr, usageStats)
      ))

      result = Uncurried2Value(
        vfqn = key.vfqn,
        typeArguments = key.typeArguments,
        name = monomorphicValue.name,
        signature = monomorphicValue.signature,
        parameters = parameters,
        returnType = returnType,
        body = convertedBody,
        targetArity = optimalArity
      )

      _ <- registerFactIfClear(result)
    } yield ()
}
```

---

## 4. Arity Selection Algorithm

### 4.1 Core Algorithm

The arity selection algorithm determines the optimal uncurry level for each function based on how it's most frequently called.

```scala
package com.vanillasource.eliot.eliotc.uncurry2

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.used2.UsedNames.UsageStats

object AritySelector {

  /**
   * Select the optimal arity for uncurrying based on usage statistics.
   *
   * Strategy: Choose the arity with the highest usage frequency,
   * but never exceed the function's maximum arity (from its type).
   *
   * @param stats Usage statistics from UsedNames
   * @param signature The function's type signature (to determine max arity)
   * @return The optimal arity to uncurry to
   */
  def selectOptimalArity(stats: Option[UsageStats], signature: Value): Int = {
    val maxArity = computeMaxArity(signature)

    stats match {
      case Some(usageStats) if usageStats.directCallApplications.nonEmpty =>
        // Find the arity with maximum usage count
        val (bestArity, _) = usageStats.directCallApplications
          .filter { case (arity, _) => arity <= maxArity }
          .maxByOption { case (_, count) => count }
          .getOrElse((maxArity, 0))

        bestArity

      case _ =>
        // No statistics available, default to full uncurrying
        maxArity
    }
  }

  /**
   * Compute the maximum possible arity from a function signature.
   * Counts nested Function types.
   *
   * Example: A -> B -> C -> D has max arity 3
   */
  def computeMaxArity(signature: Value): Int = {
    signature match {
      case Value.FunctionType(_, returnType) =>
        1 + computeMaxArity(returnType)
      case _ =>
        0
    }
  }
}
```

### 4.2 Arity Selection Examples

| Function Type | Usage Stats | Selected Arity | Rationale |
|---------------|-------------|----------------|-----------|
| `A -> B -> C -> D` | `{0→5, 1→3, 2→10, 3→2}` | 2 | Most frequent (10 times) |
| `A -> B -> C` | `{0→1, 2→1}` | 2 | Tie-break: prefer higher arity |
| `A -> B -> C` | `{}` (none) | 2 | Default to full arity |
| `A -> B` | `{0→100, 1→1}` | 0 | Function used mostly as value |

### 4.3 Handling Partial Applications

When a function is uncurried to arity N but called with M < N arguments:

1. **Generate a lambda wrapper** that captures the M arguments and returns a function taking N-M arguments
2. **Alternative**: Generate multiple entry points (overloads) for common arities

```
// Original: add: Int -> Int -> Int
// Selected arity: 2 (uncurried to add(Int, Int))
// Call site: add 1 → generates: x => add(1, x)
```

---

## 5. JVM Backend Modifications

### 5.1 Overview

The JVM backend needs to be modified to:

1. Use `Uncurried2Value` instead of `UncurriedFunction`
2. Use `UsedNames` instead of `UsedSymbols`
3. Work with `Value` types instead of `TypeReference`
4. Handle monomorphic specializations (different type arguments = different methods)

### 5.2 New JvmClassGenerator2

Create a new processor that works with the new pipeline:

```scala
package com.vanillasource.eliot.eliotc.jvm.classgen.processor

class JvmClassGenerator2 extends SingleKeyTypeProcessor[GeneratedModule2.Key] with Logging {

  override protected def generateFact(key: GeneratedModule2.Key): CompilerIO[Unit] =
    for {
      // Use UsedNames instead of UsedSymbols
      usedNames <- getFactOrAbort(UsedNames.Key(key.rootVfqn))

      // Filter to this module's values
      moduleValues = usedNames.usedNames
        .filter { case (vfqn, _) => vfqn.moduleName == key.moduleName }

      // For each (vfqn, typeArgs) combination, generate specialized code
      mainClassGenerator <- createClassGenerator[CompilerIO](key.moduleName)

      classFiles <- moduleValues.toSeq.flatTraverse { case (vfqn, stats) =>
        // Generate for each monomorphic specialization
        stats.monomorphicTypeParameters.flatTraverse { typeArgs =>
          createModuleMethod(mainClassGenerator, vfqn, typeArgs)
        }
      }

      mainClass <- mainClassGenerator.generate[CompilerIO]()
      _ <- registerFactIfClear(GeneratedModule2(key.moduleName, key.rootVfqn, classFiles :+ mainClass))
    } yield ()

  private def createModuleMethod(
      classGenerator: ClassGenerator,
      vfqn: ValueFQN,
      typeArgs: Seq[Value]
  ): CompilerIO[Seq[ClassFile]] =
    for {
      // Get the uncurried value for this specialization
      uncurriedValue <- getFactOrAbort(Uncurried2Value.Key(vfqn, typeArgs))

      // Generate method with mangled name for specialization
      methodName = mangleMethodName(vfqn.valueName, typeArgs)

      classFiles <- uncurriedValue.body match {
        case Some(body) =>
          classGenerator
            .createMethod[CompilerIO](
              methodName,
              uncurriedValue.parameters.map(p => valueToNativeType(p.parameterType)),
              valueToNativeType(uncurriedValue.returnType)
            )
            .use { methodGenerator =>
              createExpressionCode(classGenerator, methodGenerator, body.value)
            }
        case None =>
          // Handle native implementations
          handleNativeImplementation(classGenerator, vfqn, typeArgs)
      }
    } yield classFiles
}
```

### 5.3 Type Mapping Changes

Update `NativeType` to work with `Value` instead of `TypeReference`:

```scala
package com.vanillasource.eliot.eliotc.jvm.classgen.asm

object NativeType2 {

  /**
   * Map Value types to JVM types.
   */
  def valueToNativeType(value: Value): TypeFQN = value match {
    case Value.ConcreteType(typeFQN) =>
      types.getOrElse(typeFQN, typeFQN)

    case Value.FunctionType(_, _) =>
      // All functions map to java.util.function.Function
      TypeFQN(ModuleName(Seq("java", "util", "function"), "Function"), "Function")

    case Value.DataType(typeFQN, _) =>
      typeFQN

    case _ =>
      // Default to Object for complex/unknown types
      TypeFQN(ModuleName(Seq("java", "lang"), "Object"), "Object")
  }

  // Built-in type mappings
  private val types: Map[TypeFQN, TypeFQN] = Map(
    TypeFQN.eliotString -> TypeFQN.javaString,
    TypeFQN.eliotUnit -> TypeFQN.javaVoid,
    TypeFQN.eliotNumber -> TypeFQN.javaInteger
    // ... etc
  )
}
```

### 5.4 Expression Code Generation

Update expression handling for `Uncurried2Expression`:

```scala
private def createExpressionCode(
    classGenerator: ClassGenerator,
    methodGenerator: MethodGenerator,
    expr: Uncurried2Expression
): CompilationTypesIO[Seq[ClassFile]] =
  expr.expression match {
    case Uncurried2Expression.FunctionApplication(target, arguments) =>
      // Multi-argument application already flattened
      generateFunctionApplication(classGenerator, methodGenerator, target.value, arguments)

    case Uncurried2Expression.FunctionLiteral(parameters, body) =>
      // Multi-parameter lambda
      generateLambda(classGenerator, methodGenerator, parameters, body)

    case Uncurried2Expression.ValueReference(valueName, typeArgs) =>
      // Look up the uncurried value's arity and generate appropriate call
      for {
        uncurriedTarget <- getFact(Uncurried2Value.Key(valueName.value, typeArgs)).liftToTypes
        _ <- uncurriedTarget match {
          case Some(target) if target.targetArity == 0 =>
            // Zero-arity: direct call
            methodGenerator.addCallTo[CompilationTypesIO](
              mangleMethodName(valueName.value, typeArgs),
              Seq.empty,
              valueToNativeType(target.returnType)
            )
          case Some(target) =>
            // Non-zero arity but no args provided: need to create partial application wrapper
            generatePartialApplication(classGenerator, methodGenerator, target, Seq.empty)
          case None =>
            compilerError(valueName.as("Could not find uncurried value.")).liftToTypes
        }
      } yield Seq.empty

    case Uncurried2Expression.IntegerLiteral(intLit) =>
      methodGenerator.addLdcInsn[CompilationTypesIO](intLit.value.intValue).as(Seq.empty)

    case Uncurried2Expression.StringLiteral(strLit) =>
      methodGenerator.addLdcInsn[CompilationTypesIO](strLit.value).as(Seq.empty)

    case Uncurried2Expression.ParameterReference(paramName) =>
      for {
        index <- getParameterIndex(paramName.value)
        paramType <- getParameterType(paramName.value)
        _ <- methodGenerator.addLoadVar[CompilationTypesIO](
          valueToNativeType(paramType),
          index
        )
      } yield Seq.empty
  }
```

### 5.5 Method Name Mangling

For monomorphic specializations, mangle method names to include type arguments:

```scala
/**
 * Mangle a method name to include type argument information.
 * This ensures different specializations have unique JVM method names.
 *
 * Example: id with [Int] → id$Int
 * Example: map with [Int, String] → map$Int$String
 */
def mangleMethodName(baseName: String, typeArgs: Seq[Value]): String = {
  if (typeArgs.isEmpty) {
    baseName
  } else {
    val typeNames = typeArgs.map(valueToTypeName).mkString("$")
    s"${baseName}$$${typeNames}"
  }
}

def valueToTypeName(value: Value): String = value match {
  case Value.ConcreteType(tfqn) => tfqn.typeName
  case Value.DataType(tfqn, _) => tfqn.typeName
  case Value.FunctionType(_, _) => "Fn"
  case _ => "Unknown"
}
```

---

## 6. Migration Strategy

### Phase 1: Create Uncurry2 Package (Parallel Development)

1. Create `uncurry2/fact/` with new data structures
2. Implement `AritySelector` with tests
3. Implement `Uncurrying2Processor`
4. Add processor to BasePlugin (alongside existing processors)
5. Write comprehensive tests

### Phase 2: Create JVM Backend v2 (Parallel Development)

1. Create `GeneratedModule2` fact with `ValueFQN`-based key
2. Create `JvmClassGenerator2` working with new facts
3. Create `NativeType2` mapping for `Value` types
4. Update `NativeImplementation` for new type system
5. Create `JvmProgramGenerator2` using new pipeline

### Phase 3: Integration Testing

1. Run both pipelines in parallel
2. Compare outputs for correctness
3. Benchmark performance differences
4. Fix any discrepancies

### Phase 4: Switchover

1. Add command-line flag to select pipeline (e.g., `--use-new-pipeline`)
2. Make new pipeline the default
3. Deprecate old pipeline
4. Remove old pipeline after validation period

### Phase 5: Cleanup

1. Remove old `typecheck/` package
2. Remove old `uncurry/` package
3. Remove old `used/` package
4. Remove old JVM generators
5. Rename `uncurry2` → `uncurry`, etc. (optional)

---

## 7. File Structure

### New Files to Create

```
base/src/com/vanillasource/eliot/eliotc/uncurry2/
├── fact/
│   ├── Uncurried2Value.scala
│   ├── Uncurried2Expression.scala
│   └── Parameter2Definition.scala
├── processor/
│   └── Uncurrying2Processor.scala
└── AritySelector.scala

base/test/src/com/vanillasource/eliot/eliotc/uncurry2/
├── AritySelectorTest.scala
└── Uncurrying2ProcessorTest.scala

jvm/src/com/vanillasource/eliot/eliotc/jvm/
├── classgen2/
│   ├── processor/
│   │   ├── JvmClassGenerator2.scala
│   │   └── TypeState2.scala
│   ├── asm/
│   │   └── NativeType2.scala
│   └── fact/
│       └── GeneratedModule2.scala
└── jargen2/
    ├── JvmProgramGenerator2.scala
    └── GenerateExecutableJar2.scala

jvm/test/src/com/vanillasource/eliot/eliotc/jvm/
├── classgen2/
│   └── JvmClassGenerator2Test.scala
└── jargen2/
    └── JvmProgramGenerator2Test.scala
```

### Files to Modify

```
base/src/com/vanillasource/eliot/eliotc/plugin/BasePlugin.scala
  - Add Uncurrying2Processor to processor list

jvm/src/com/vanillasource/eliot/eliotc/jvm/plugin/JvmPlugin.scala
  - Add option to select between old and new pipeline
  - Register new processors

jvm/src/com/vanillasource/eliot/eliotc/jvm/classgen/processor/NativeImplementation.scala
  - Add support for new ValueFQN-based lookups (or create NativeImplementation2)
```

### Files to Eventually Remove

```
base/src/com/vanillasource/eliot/eliotc/typesystem/         # Old type checker
base/src/com/vanillasource/eliot/eliotc/uncurry/            # Old uncurry
base/src/com/vanillasource/eliot/eliotc/used/               # Old used symbols
jvm/src/com/vanillasource/eliot/eliotc/jvm/classgen/        # Old JVM class generator
jvm/src/com/vanillasource/eliot/eliotc/jvm/jargen/          # Old JAR generator (except shared utilities)
```

---

## 8. Implementation Tasks

### Task 1: Uncurry2 Facts

- [ ] Create `Parameter2Definition.scala`
- [ ] Create `Uncurried2Expression.scala` with all expression variants
- [ ] Create `Uncurried2Value.scala` with key structure
- [ ] Ensure compatibility with `Value` type system from `eval/fact/`

### Task 2: Arity Selection

- [ ] Implement `AritySelector.selectOptimalArity()`
- [ ] Implement `AritySelector.computeMaxArity()`
- [ ] Write unit tests for various usage patterns
- [ ] Handle edge cases (no stats, ties, zero arity)

### Task 3: Uncurrying2Processor

- [ ] Implement basic structure extending `SingleKeyTypeProcessor`
- [ ] Implement parameter extraction from `Value` types
- [ ] Implement expression conversion from `MonomorphicExpression` to `Uncurried2Expression`
- [ ] Implement application flattening based on target arity
- [ ] Implement lambda flattening
- [ ] Handle partial application cases
- [ ] Write integration tests

### Task 4: JVM Backend v2 - Types

- [ ] Create `NativeType2` with `Value` → JVM type mapping
- [ ] Implement method name mangling for specializations
- [ ] Update or create `NativeImplementation2` for new types

### Task 5: JVM Backend v2 - Code Generation

- [ ] Create `GeneratedModule2` fact
- [ ] Create `JvmClassGenerator2` processor
- [ ] Implement expression code generation for `Uncurried2Expression`
- [ ] Implement multi-argument function calls
- [ ] Implement multi-parameter lambda generation
- [ ] Handle partial application wrapper generation

### Task 6: JVM Backend v2 - JAR Generation

- [ ] Create `GenerateExecutableJar2` fact
- [ ] Create `JvmProgramGenerator2` processor
- [ ] Wire up entry point generation
- [ ] Handle main method generation

### Task 7: Plugin Integration

- [ ] Add `Uncurrying2Processor` to `BasePlugin`
- [ ] Add pipeline selection flag to `JvmPlugin`
- [ ] Register new JVM processors
- [ ] Ensure proper processor ordering

### Task 8: Testing

- [ ] Port existing uncurry tests to uncurry2
- [ ] Port existing JVM tests to new generator
- [ ] Add tests for arity-based optimization
- [ ] Add tests for monomorphic specialization
- [ ] End-to-end tests with real ELIOT programs

### Task 9: Migration & Cleanup

- [ ] Run parallel pipelines in CI
- [ ] Switch default to new pipeline
- [ ] Remove old pipeline code
- [ ] Update documentation

---

## Appendix A: Key Type Mappings

### MonomorphicExpression → Uncurried2Expression

| MonomorphicExpression | Uncurried2Expression |
|----------------------|---------------------|
| `FunctionApplication(target, arg)` | `FunctionApplication(target, Seq(arg, ...))` (flattened) |
| `FunctionLiteral(name, type, body)` | `FunctionLiteral(Seq(Param(...), ...), body)` (flattened) |
| `IntegerLiteral(value)` | `IntegerLiteral(value)` |
| `StringLiteral(value)` | `StringLiteral(value)` |
| `ParameterReference(name)` | `ParameterReference(name)` |
| `MonomorphicValueReference(vfqn, typeArgs)` | `ValueReference(vfqn, typeArgs)` |

### Value Types → JVM Types

| Value | JVM Type |
|-------|----------|
| `ConcreteType("eliot.lang.String")` | `java.lang.String` |
| `ConcreteType("eliot.lang.Unit")` | `java.lang.Void` |
| `ConcreteType("eliot.lang.Number")` | `java.lang.Integer` |
| `FunctionType(A, B)` | `java.util.function.Function<A, B>` |
| `DataType(tfqn, fields)` | Generated inner class |

---

## Appendix B: Example Transformation

### Source Code

```eliot
add: Int -> Int -> Int
add = a -> b -> plus a b

main: Unit
main = println (add 1 2)
```

### Usage Statistics (from UsedNames)

```
add: {
  monomorphicTypeParameters: [],  // Not generic
  directCallApplications: {2 → 1}  // Called with 2 args once
}
```

### Old Pipeline Output (UncurriedFunction)

```scala
UncurriedFunction(
  ffqn = FunctionFQN("example", "add"),
  definition = UncurriedTypedFunctionDefinition(
    name = "add",
    parameters = Seq(
      ArgumentDefinition("a", DirectTypeReference(Int)),
      ArgumentDefinition("b", DirectTypeReference(Int))
    ),
    returnType = DirectTypeReference(Int),
    body = Some(FunctionApplication(
      ValueReference("plus"),
      Seq(ParameterReference("a"), ParameterReference("b"))
    ))
  )
)
```

### New Pipeline Output (Uncurried2Value)

```scala
Uncurried2Value(
  vfqn = ValueFQN("example", "add"),
  typeArguments = Seq.empty,  // Not generic
  name = "add",
  signature = FunctionType(Int, FunctionType(Int, Int)),
  parameters = Seq(
    Parameter2Definition("a", ConcreteType(Int)),
    Parameter2Definition("b", ConcreteType(Int))
  ),
  returnType = ConcreteType(Int),
  body = Some(FunctionApplication(
    ValueReference("plus", Seq.empty),
    Seq(ParameterReference("a"), ParameterReference("b"))
  )),
  targetArity = 2
)
```

### Generated JVM Bytecode

```java
// Old pipeline: same as new for this case
public static Integer add(Integer a, Integer b) {
    return plus(a, b);
}

// New pipeline: identical, but derived from usage statistics
public static Integer add(Integer a, Integer b) {
    return plus(a, b);
}
```

---

## Appendix C: Partial Application Handling

When a function with target arity N is called with M < N arguments:

### Strategy 1: Generate Wrapper Lambda at Call Site

```scala
// add has targetArity = 2
// Call site: add 1

// Generated: creates a lambda that captures 1 and takes remaining arg
val captured = 1
new Function[Int, Int] {
  def apply(b: Int): Int = add(captured, b)
}
```

### Strategy 2: Generate Multiple Entry Points

```scala
// Generate both:
public static Integer add(Integer a, Integer b) { ... }
public static Function<Integer, Integer> add(Integer a) {
    return b -> add(a, b);
}
```

The implementation should use **Strategy 1** initially (simpler) and potentially optimize to Strategy 2 later for frequently used partial applications.
