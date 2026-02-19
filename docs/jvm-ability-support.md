# JVM Backend Ability Support via Dictionary Passing

## Context

The JVM backend generates **type-erased** JVM bytecode from `UncurriedValue` facts, which are
derived from `AbilityCheckedValue` (the pre-monomorphization stage). This means the JVM backend
operates on **generic, symbolic code** — not monomorphized specializations. One JVM method is
generated per ELIOT function (not one per type instantiation).

For ability calls with **concrete types**, `AbilityCheckProcessor` already resolves the ability
reference to the concrete implementation before the uncurry stage. These flow through to the JVM
backend as normal static method calls and already work.

For constrained generic functions, e.g.:

```eliot
ability Show[A] { def show(x: A): A }
def f[A ~ Show[A]](x: A): A = show(x)
```

`AbilityCheckProcessor` leaves the `show` reference **unresolved** (as a `ValueReference` with
`Qualifier.Ability` in the FQN). This propagates to `UncurriedExpression` and then to the JVM
backend, which has no way to handle it yet.

The monomorphizer (Phase 4 of the ability-constraints-threading plan) resolves ability refs for
concrete instantiations, which lets `UsedNames` trace through the resolved call graph correctly.
But the JVM backend reads the **generic** `UncurriedValue`, not the monomorphized form.

## Core Design: Dictionary Passing

This follows the same pattern as Haskell typeclass dictionaries and is the standard approach for
compiling type classes to a type-erased target.

### 1. Ability → JVM Interface

Each ability becomes a JVM interface with one method per ability member, all types erased to
`Object`:

```java
// Generated for: ability Show[A] { def show(x: A): A }
// Inner class of the ability's module's JVM class
interface Show$vtable {
    Object show(Object x);
}
```

### 2. Ability Implementation → Singleton Class

Each ability implementation becomes a singleton JVM class implementing the interface and
delegating to the already-generated concrete static method:

```java
// Generated for: implement Show[Int] { def show(x: Int): Int = x }
// Inner class of the implementation's module's JVM class
class Show$Int$impl implements ShowModule$Show$vtable {
    static final ShowModule$Show$vtable INSTANCE = new Show$Int$impl();

    @Override
    public Object show(Object x) {
        return ImplModule.show(x);  // INVOKESTATIC to concrete generated method
    }
}
```

### 3. Constrained Function → Dictionary Parameters

Each constrained type parameter `A ~ Show[A]` produces an extra implicit dictionary parameter
prepended to the regular parameter list. Dictionaries come first, regular parameters follow.

```java
// Generated for: def f[A ~ Show[A]](x: A): A = show(x)
// Original arity 1; with dict becomes arity 2 at JVM level
static Object f(ShowModule$Show$vtable $Show$A, Object x) {
    return $Show$A.show(x);  // INVOKEINTERFACE
}
```

### 4. Ability Call → INVOKEINTERFACE Dispatch

When the body of a constrained function contains a `ValueReference` with `Qualifier.Ability`, the
JVM backend:
1. Identifies which constraint covers the ability (from `ResolvedValue.paramConstraints`)
2. Loads the corresponding dictionary parameter
3. Loads the arguments
4. Emits `INVOKEINTERFACE` on the dictionary

### 5. Call Site → Dictionary Injection

When generating a call to a constrained function, the JVM backend injects the appropriate
dictionary before the regular arguments:

- **Concrete call site** (concrete type known): look up the `AbilityImplementation` fact, emit
  `GETSTATIC ImplModule$Show$Int$impl.INSTANCE`
- **Generic call site** (caller is also constrained with matching ability): load the caller's own
  dictionary parameter and pass it through

## Worked Example

```eliot
ability Show[A] { def show(x: A): A }
data Int
implement Show[Int] { def show(x: Int): Int = x }

def f[A ~ Show[A]](x: A): A = show(x)  -- constrained generic
def g(x: Int): Int = f(x)              -- concrete call site
def h[A ~ Show[A]](x: A): A = f(x)    -- generic call site (pass-through)
```

Generated JVM (pseudocode):

```java
// ShowModule.class
interface Show$vtable {
    Object show(Object x);
}

// ImplModule.class
static Object show(Object x) { return x; }   // from: implement Show[Int]

class Show$Int$impl implements ShowModule$Show$vtable {
    static final ShowModule$Show$vtable INSTANCE = new Show$Int$impl();
    public Object show(Object x) { return ImplModule.show(x); }
}

// UserModule.class
static Object f(ShowModule$Show$vtable $Show$A, Object x) {
    return $Show$A.show(x);                   // INVOKEINTERFACE
}

static Object g(Object x) {
    return f(ImplModule$Show$Int$impl.INSTANCE, x);  // inject concrete dict
}

static Object h(ShowModule$Show$vtable $Show$A, Object x) {
    return f($Show$A, x);                     // pass through dict
}
```

## Naming Conventions

| Eliot construct | JVM name |
|---|---|
| `ability Show[A]` in module `M` | `M$Show$vtable` (inner class of `M`) |
| `implement Show[Int]` in module `I` | `I$Show$Int$impl` (inner class of `I`) |
| Dictionary param for `A ~ Show[A]` | `$Show$A` |
| INSTANCE field on singleton | `static final Show$vtable INSTANCE` |

For multi-param abilities (`ability Combine[A, B]`) and constraint `A ~ Combine[A, B]`, the
dictionary parameter is named `$Combine$A$B`.

## New Facts and Processors

### `JvmAbilityInterface` — Interface Class Fact

```
lang/... (or jvm/...)/fact/JvmAbilityInterface.scala
```

```scala
case class JvmAbilityInterface(
    abilityFQN: AbilityFQN,
    interfaceInternalName: String,       // e.g. "my/Module$Show$vtable"
    methods: Seq[JvmAbilityInterface.Method],
    classFile: ClassFile                 // the generated .class bytes
) extends CompilerFact

object JvmAbilityInterface {
  case class Method(name: String, arity: Int)
  case class Key(abilityFQN: AbilityFQN) extends CompilerFactKey[JvmAbilityInterface]
}
```

**Processor**: `JvmAbilityInterfaceProcessor`

- **Input key**: `JvmAbilityInterface.Key(abilityFQN)`
- **Reads**: All `AbilityCheckedValue` facts for the ability's module whose FQN has
  `Qualifier.Ability(abilityFQN.abilityName)`. This gives the method names and their uncurried
  arities.
- **Generates**: A JVM interface class using `ClassGenerator` with the `interfaces` parameter set
  appropriately, plus one abstract method per ability method (all params/returns as `Object`).
- **Output**: `JvmAbilityInterface` fact containing the `ClassFile` bytes.

> **How to enumerate ability methods**: Scan `UnifiedModuleValue` (or `UncurriedValue`) facts for
> the ability's module. Values with `Qualifier.Ability(name)` that match the ability name are the
> methods. `UncurriedValue.Key(abilityMethodVfqn, arity)` gives the arity.

### `JvmAbilityImplSingleton` — Implementation Singleton Fact

```
jvm/src/.../fact/JvmAbilityImplSingleton.scala
```

```scala
case class JvmAbilityImplSingleton(
    abilityFQN: AbilityFQN,
    typeArgs: Seq[Value],
    singletonInternalName: String,       // e.g. "my/ImplModule$Show$Int$impl"
    classFile: ClassFile
) extends CompilerFact

object JvmAbilityImplSingleton {
  case class Key(abilityFQN: AbilityFQN, typeArgs: Seq[Value])
      extends CompilerFactKey[JvmAbilityImplSingleton]
}
```

**Processor**: `JvmAbilityImplSingletonProcessor`

- **Input key**: `JvmAbilityImplSingleton.Key(abilityFQN, typeArgs)`
- **Reads**:
  - `AbilityImplementation.Key(abilityMethodVfqn, typeArgs)` — maps ability method to concrete
    implementation FQN
  - `JvmAbilityInterface.Key(abilityFQN)` — to know the interface and method signatures
  - `UncurriedValue.Key(implMethodVfqn, arity)` — to know parameter types for the delegation call
- **Generates**: A JVM class `implementing Show$vtable` with:
  - `static final Show$vtable INSTANCE = new Show$Int$impl();`
  - One `@Override` method per ability method, delegating to the concrete `INVOKESTATIC`
- **Output**: `JvmAbilityImplSingleton` fact containing the `ClassFile` bytes.

> Note: The ability may have multiple methods. The singleton implements ALL of them, each
> delegating to the corresponding `AbilityImplementation`.

## Modifications to Existing Code

### `JvmClassGenerator`

This is the central location for all ability-dispatch logic. The key changes are:

#### A. Dictionary parameters in method signatures

When `createModuleMethod` builds parameters for an `UncurriedValue`, it first checks
`ResolvedValue.paramConstraints` for the value's FQN (using `getFact`, not `getFactOrAbort`,
because synthesized values have no `ResolvedValue`). For each constraint
`paramName -> Seq(ResolvedAbilityConstraint(abilityFQN, ...))`, a dictionary parameter is
**prepended** before the regular parameters:

```
$Show$A: Show$vtable, x: Object   (instead of just: x: Object)
```

Dictionary parameters are tracked in `TypeState` as regular parameters (they have an index and
a type). Their type is represented as the interface `ValueFQN` (see NativeType changes below).

#### B. Ability ref detection and INVOKEINTERFACE dispatch

In `generateFunctionApplication`, when `typedTarget.expression` is
`ValueReference(sourcedVfqn)` and `sourcedVfqn.value.name.qualifier` is `Qualifier.Ability(name)`:
1. Extract `abilityFQN = AbilityFQN(sourcedVfqn.value.moduleName, name)`
2. Look up which dictionary parameter covers this ability from the enclosing function's
   `paramConstraints` — find the dict param name (e.g. `$Show$A`)
3. Look up the dict param's index in `TypeState`
4. Emit `ALOAD <dictParamIndex>` to load the dictionary
5. Generate code for arguments (regular `createExpressionCode` calls)
6. Look up `JvmAbilityInterface.Key(abilityFQN)` to get the interface's internal name and method
   signature
7. Emit `INVOKEINTERFACE` to call the method on the dictionary

#### C. Dictionary injection at call sites

In `generateFunctionApplication`, when `typedTarget.expression` is `ValueReference(f)` and `f`
is a regular (non-ability) value: check if `f` has ability constraints.

Read `ResolvedValue.paramConstraints` for `f`. For each constraint:
1. Determine the concrete binding for the constrained type param by matching against argument
   types:
   - Walk `f`'s `UncurriedValue.parameters` and look for which parameter has type
     `ParameterReference(paramName)` for each constrained param `paramName`
   - The corresponding actual argument's `expressionType` gives the binding
   - E.g., if `f`'s first param has type `ParameterReference("A")` and the actual argument has
     `expressionType = ConcreteValue(IntValue)` → `A = Int`
2. Determine what dictionary to pass:
   - If binding is `ConcreteValue(v)`: look up `JvmAbilityImplSingleton.Key(abilityFQN, Seq(v))`
     and emit `GETSTATIC singletonInternalName.INSTANCE`
   - If binding is `ParameterReference(p)`: find the caller's own dict param for `p`'s ability
     and emit `ALOAD <callerDictParamIndex>` (pass-through)
3. Inject dictionary arguments **before** the regular argument code

#### D. Collect ability class files in module output

`createModuleMethod` (and related code) must accumulate ability-related `ClassFile` objects
(from `JvmAbilityInterface` and `JvmAbilityImplSingleton` facts) and include them in the
final `Seq[ClassFile]` returned for the module. These facts produce the interface and singleton
class files that must appear in the JAR.

### `JvmProgramGenerator`

No core changes needed if ability class files are included in the `GeneratedModule` facts (via
`JvmClassGenerator`). Alternative: add a separate pass collecting all `JvmAbilityInterface` and
`JvmAbilityImplSingleton` facts encountered during generation and writing them separately to the
JAR.

### `NativeType` / Type Mapping

Ability dictionary parameters have a JVM type that is the interface itself. A new helper is needed:

```scala
def abilityInterfaceVfqn(abilityFQN: AbilityFQN): ValueFQN =
  ValueFQN(abilityFQN.moduleName,
    QualifiedName(abilityFQN.abilityName + "$vtable", Qualifier.Default))

def abilityInterfaceInternalName(abilityFQN: AbilityFQN): String =
  NativeType.convertToNestedClassName(abilityInterfaceVfqn(abilityFQN))
```

`javaSignatureName` and `javaInternalName` must handle these interface VFQNs correctly (they
follow the same pattern as data class inner names, so they should work without special-casing).

### `MethodGenerator` / `ClassGenerator`

New methods needed on `ClassGenerator`:

```scala
// Generate an interface (instead of a concrete class)
def createInterfaceGenerator[F[_]: Sync](name: String): F[ClassGenerator]
// (uses ACC_INTERFACE | ACC_ABSTRACT instead of ACC_FINAL | ACC_STATIC)

// Generate an abstract interface method
def createAbstractMethod[F[_]: Sync](name: String, paramCount: Int): F[Unit]
```

New method on `MethodGenerator`:

```scala
// Emit INVOKEINTERFACE for a dictionary method call
def addCallToAbilityMethod[F[_]: Sync](
    interfaceInternalName: String,
    methodName: String,
    arity: Int
): F[Unit]
// signature: "(Ljava/lang/Object;...N times...)Ljava/lang/Object;"

// Emit GETSTATIC to load a singleton dictionary instance
def addGetStaticInstance[F[_]: Sync](singletonInternalName: String): F[Unit]
```

### `TypeState`

Dictionary parameters are regular parameters in `TypeState`. No structural changes needed — they
are added like any other parameter via `addParameterDefinition`. Their `parameterType` uses the
ability interface `ValueFQN`.

To distinguish them from user parameters (for logging / debugging), they can be named with a
`$` prefix (e.g., `$Show$A`).

### `JvmPlugin`

Register the two new processors:

```scala
JvmAbilityInterfaceProcessor(),
JvmAbilityImplSingletonProcessor(),
JvmClassGenerator(),
JvmProgramGenerator(...)
```

## Implementation Phases

All phases must individually compile and have all existing tests pass.

### Phase 1 — Ability Interface Class Generation

**Goal**: Produce correct JVM interface `.class` files for each ability, driven lazily by demand.

**New files**:
- `jvm/src/.../fact/JvmAbilityInterface.scala`
- `jvm/src/.../processor/JvmAbilityInterfaceProcessor.scala`

**Changed files**:
- `jvm/src/.../classgen/asm/ClassGenerator.scala` — add `createInterfaceGenerator` and
  `createAbstractMethod`
- `jvm/src/.../plugin/JvmPlugin.scala` — register `JvmAbilityInterfaceProcessor`

**What to test**: Write a unit test or integration test that for an ability definition the
processor produces a `JvmAbilityInterface` fact with an interface class that, when loaded by the
JVM, is a valid Java interface with the correct method(s).

**Verification**: `mill lang.test && mill jvm.test` — all tests pass.

### Phase 2 — Ability Implementation Singleton Generation

**Goal**: Produce a singleton `.class` for each ability implementation, implementing the
interface and delegating to the concrete static method.

**New files**:
- `jvm/src/.../fact/JvmAbilityImplSingleton.scala`
- `jvm/src/.../processor/JvmAbilityImplSingletonProcessor.scala`

**Changed files**:
- `jvm/src/.../classgen/asm/MethodGenerator.scala` — add `addGetStaticInstance`,
  `addCallToAbilityMethod`
- `jvm/src/.../plugin/JvmPlugin.scala` — register `JvmAbilityImplSingletonProcessor`

**What to test**: For a concrete ability implementation, the processor produces a
`JvmAbilityImplSingleton` whose class implements the ability interface and whose `show` method
delegates to the concrete impl. Can be verified by loading the bytecode with ASM and inspecting
instructions, or by running a small integration test.

**Verification**: `mill lang.test && mill jvm.test` — all tests pass.

### Phase 3 — Dictionary Parameters in Method Signatures

**Goal**: Functions with ability constraints get extra dictionary parameters prepended to their
JVM method signatures.

**Changed files**:
- `jvm/src/.../classgen/processor/JvmClassGenerator.scala` — modify `createModuleMethod` to read
  `ResolvedValue.paramConstraints` and prepend dict params; modify `TypeState` initialization to
  include them

**Key concern**: `getFact` (not `getFactOrAbort`) for `ResolvedValue` since synthesized values
(default ability implementations) have no `ResolvedValue`.

**What to test**: A constrained function `f[A ~ Show[A]](x: A): A = x` (body doesn't call show
yet — just returns x) should generate a JVM method with signature
`(ShowModule$Show$vtable, Object) Object`. The dict param is present but unused in this phase.

**Verification**: `mill lang.test && mill jvm.test`.

### Phase 4 — Dispatch Ability Calls Through Dictionary

**Goal**: When the body of a constrained function contains a `ValueReference` with
`Qualifier.Ability`, emit `INVOKEINTERFACE` on the dictionary instead of `INVOKESTATIC`.

**Changed files**:
- `jvm/src/.../classgen/processor/JvmClassGenerator.scala` — add ability ref detection in
  `generateFunctionApplication`; request `JvmAbilityInterface` fact; emit `INVOKEINTERFACE`

**Include interface class file in output**: When the `JvmAbilityInterface` fact is first
requested for module `M`'s generation, add its `classFile` to the output `ClassFile` sequence
for that module. Use a `Set` to deduplicate if encountered multiple times.

**What to test**: Full pipeline integration test: `def f[A ~ Show[A]](x: A): A = show(x)` should
generate a JVM method that calls `INVOKEINTERFACE` on the first parameter, and the ability
interface class should appear in the generated module's class files.

**Verification**: `mill lang.test && mill jvm.test && mill examples.run ...`.

### Phase 5 — Dictionary Injection at Call Sites

**Goal**: When calling a constrained function, inject the appropriate dictionary argument(s)
before the regular arguments.

**Changed files**:
- `jvm/src/.../classgen/processor/JvmClassGenerator.scala` — extend `generateFunctionApplication`
  for the `ValueReference` case: after fetching `UncurriedValue`, check `ResolvedValue.paramConstraints`
  for the called function; match argument types to constrained type params; inject dict args

**Include singleton class file in output**: When a `JvmAbilityImplSingleton` fact is requested,
add its `classFile` to the current module's output.

**Concrete call site** (e.g., `g(x: Int): Int = f(x)`):
1. Detect `f` has constraint `A ~ Show[A]`
2. Match `f`'s first param type `ParameterReference("A")` against argument's `expressionType`
   `ConcreteValue(Int)`
3. `A = Int`
4. Request `JvmAbilityImplSingleton.Key(ShowFQN, Seq(IntValue))`
5. Emit `GETSTATIC Show$Int$impl.INSTANCE`

**Generic call site** (e.g., `h[A ~ Show[A]](x: A): A = f(x)`):
1. Detect `f` has constraint `A ~ Show[A]`
2. Match `f`'s param type `ParameterReference("A")` against argument's `expressionType`
   `ParameterReference("A")`
3. `h` has constraint `A ~ Show[A]` → `h` has dict param `$Show$A`
4. Emit `ALOAD <index of $Show$A>` (pass-through)

**What to test**: End-to-end tests:
- `g(x: Int): Int = f(x)` compiles, runs, and returns `42` for `g(42)`
- `h[A ~ Show[A]](x: A): A = f(x)` called from `g` works correctly

**Verification**: `mill lang.test && mill jvm.test && mill examples.run jvm exe-jar examples/src/ -m HelloWorld`.

## Key Invariants and Design Decisions

### JVM backend remains generic (type-erased)

The JVM backend generates **one method per ELIOT function**, regardless of how many monomorphic
instantiations exist. Type parameters are erased to `Object`. This means ability dispatch MUST
be dynamic (via interfaces) — not static method selection.

### Dictionary parameters are an implementation detail

Dictionary parameters (`$Show$A`) are invisible to ELIOT programmers. They only appear in the
generated JVM bytecode. Existing function call conventions (arity, `UsedNames.directCallApplications`)
track ELIOT-level arities and must not be changed to count dict params.

The `UncurriedValue.arity` and `UsedNames.highestArity` remain ELIOT-level (not including dict
params). Dict params are **injected by the JVM backend** when it finds constraints, based on
`ResolvedValue.paramConstraints`.

### `getFact` vs `getFactOrAbort` for `ResolvedValue`

Synthesized values (default ability implementations created by `AbilityImplementationProcessor`)
have no corresponding `ResolvedValue`. Always use `getFact` (returns `Option`) for
`ResolvedValue.paramConstraints` in the JVM backend, defaulting to `Map.empty`.

### Ability interface class file deduplication

If multiple modules in the same compilation unit use the same ability, multiple `GeneratedModule`
facts may include the same interface `.class` file (e.g., `ShowModule$Show$vtable.class`). Since
the bytes are identical (same interface, same module path), the JVM's class loader handles this
correctly — the last entry in the JAR wins, but since they're identical it doesn't matter. Future
optimization: deduplicate in `JvmProgramGenerator` before writing.

### Singleton class file placement

`JvmAbilityImplSingleton` class files are included in the `GeneratedModule` of whatever module
**first requests them during code generation**. Their JVM class name already encodes the
implementation module (e.g., `ImplModule$Show$Int$impl`), so they're correctly scoped regardless
of where the `.class` file appears in the JAR.

### Multi-parameter abilities

For `ability Combine[A, B] { def combine(x: A, y: B): A }`, the interface has method
`Object combine(Object x, Object y)`. The dict param name would be `$Combine$A$B`. Type matching
at call sites handles multiple type params simultaneously.

This design works for multi-parameter abilities without modification to the core approach.

### Abilities vs higher-kinded constraints (future)

This plan covers single-level ability constraints (e.g., `A ~ Show[A]`). Nested constraints
(e.g., `F[A] ~ Functor[F]`) are out of scope for now and require additional design.

### No changes to existing fact keys or `UsedNames` tracing

The `UsedNames` processor continues to trace from `MonomorphicValue` and collect
`monomorphicTypeParameters`. The JVM backend continues to read `UncurriedValue` at
`UsedNames.highestArity`. No changes to these pipeline stages are required.
