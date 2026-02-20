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

Each ability becomes a JVM interface with one method per ability member. Only generic type
parameters are erased to `Object`; concrete types retain their JVM representations:

```java
// Generated for: ability Show[A] { def show(x: A): A }
// A is a generic type parameter → erased to Object
// Inner class of the ability's module's JVM class
interface Show$vtable {
    Object show(Object x);
}

// Generated for: ability Serialize[A] { def serialize(x: A): String }
// A is generic → Object; String is concrete → String
interface Serialize$vtable {
    String serialize(Object x);
}
```

### 2. Ability Implementation → Singleton Class

Each ability implementation becomes a singleton JVM class implementing the interface and
delegating to the already-generated concrete static method. The delegation method signature
matches the interface (generic params erased, concrete types preserved):

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

## Modifications to Existing Code

### `JvmClassGenerator`

This is the central location for **all** ability-related generation. Just as it already generates
data class constructor methods and accessor inner classes, it also generates ability interfaces and
implementation singletons as part of module code generation. No separate processors are needed.

#### A. Ability interface generation

When processing a module's values, `JvmClassGenerator` filters for values whose FQN has
`Qualifier.Ability(abilityName)`. For each distinct ability name it finds, it generates a JVM
interface inner class using new helper classes in `classgen` (see `AbilityInterfaceGenerator`
below). Each ability method becomes one abstract interface method. Only generic type parameters
(those that appear as `ParameterReference` in the `UncurriedValue` signature) are erased to
`Object`; concrete types use their JVM type directly.

The resulting `ClassFile` is added to the module's output `Seq[ClassFile]`, exactly as data inner
classes are today.

#### B. Ability implementation singleton generation

When processing a module's values, `JvmClassGenerator` also filters for values whose FQN has
`Qualifier.Implementation(abilityFQN, typeArgs)`. For each distinct `(abilityFQN, typeArgs)`
combination it finds, it generates a singleton class using a new helper `AbilityImplGenerator`.
The singleton:
- Implements the ability's JVM interface (whose internal name is computed from `abilityFQN`)
- Has `static final <Interface> INSTANCE = new <Impl>();`
- Has one `@Override` method per ability method, delegating via `INVOKESTATIC` to the concrete
  method already generated in the same module

The resulting `ClassFile` is added to the module's output, again exactly as data inner classes.

#### C. Dictionary parameters in method signatures

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

#### D. Ability ref detection and INVOKEINTERFACE dispatch

In `generateFunctionApplication`, when `typedTarget.expression` is
`ValueReference(sourcedVfqn)` and `sourcedVfqn.value.name.qualifier` is `Qualifier.Ability(name)`:
1. Extract `abilityFQN = AbilityFQN(sourcedVfqn.value.moduleName, name)`
2. Look up which dictionary parameter covers this ability from the enclosing function's
   `paramConstraints` — find the dict param name (e.g. `$Show$A`)
3. Look up the dict param's index in `TypeState`
4. Emit `ALOAD <dictParamIndex>` to load the dictionary
5. Generate code for arguments (regular `createExpressionCode` calls)
6. Compute the interface's internal name from `abilityFQN` using the naming convention; use
   method signature already known from the enclosing module's generated interface
7. Emit `INVOKEINTERFACE` to call the method on the dictionary

#### E. Dictionary injection at call sites

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
   - If binding is `ConcreteValue(v)`: compute the singleton's internal name from `abilityFQN`
     and type args using the naming convention, and emit `GETSTATIC singletonInternalName.INSTANCE`
   - If binding is `ParameterReference(p)`: find the caller's own dict param for `p`'s ability
     and emit `ALOAD <callerDictParamIndex>` (pass-through)
3. Inject dictionary arguments **before** the regular argument code

### `JvmProgramGenerator`

No changes needed. Ability interface and singleton class files are included in `GeneratedModule`
facts by `JvmClassGenerator` and flow through to the JAR automatically.

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

// Generate an abstract interface method (with proper JVM types — Object for generic params, concrete otherwise)
def createAbstractMethod[F[_]: Sync](name: String, paramTypes: Seq[String], returnType: String): F[Unit]
```

New method on `MethodGenerator`:

```scala
// Emit INVOKEINTERFACE for a dictionary method call
def addCallToAbilityMethod[F[_]: Sync](
    interfaceInternalName: String,
    methodName: String,
    paramTypes: Seq[String],
    returnType: String
): F[Unit]

// Emit GETSTATIC to load a singleton dictionary instance
def addGetStaticInstance[F[_]: Sync](singletonInternalName: String): F[Unit]
```

### New classgen helpers: `AbilityInterfaceGenerator` and `AbilityImplGenerator`

Two new classes (not processors) in the `classgen` package, used directly by `JvmClassGenerator`:

**`AbilityInterfaceGenerator`**: Given an ability name and its methods (with their
`UncurriedValue` type information), produces a `ClassFile` for the JVM interface. It maps each
method's parameter and return types to JVM types — resolving `ParameterReference` to `Object`
and concrete type FQNs to their JVM internal names.

**`AbilityImplGenerator`**: Given an `(abilityFQN, typeArgs)` combination and the list of
method implementations, produces a `ClassFile` for the singleton class. It generates the
`INSTANCE` field, constructor, and one `@Override` method per ability method delegating via
`INVOKESTATIC`.

### `TypeState`

Dictionary parameters are regular parameters in `TypeState`. No structural changes needed — they
are added like any other parameter via `addParameterDefinition`. Their `parameterType` uses the
ability interface `ValueFQN`.

To distinguish them from user parameters (for logging / debugging), they can be named with a
`$` prefix (e.g., `$Show$A`).

### `JvmPlugin`

No new processors to register. `JvmClassGenerator` already handles ability interface and
implementation singleton generation internally.

## Implementation Phases

All phases must individually compile and have all existing tests pass.

### Phase 1 — Ability Interface Class Generation

**Goal**: Produce correct JVM interface `.class` files for each ability as part of module
generation in `JvmClassGenerator`.

**New files**:
- `jvm/src/.../classgen/AbilityInterfaceGenerator.scala` — helper class that takes an ability's
  methods (with their `UncurriedValue` type info) and produces a `ClassFile` for the JVM
  interface. Concrete types map to their JVM names; `ParameterReference` maps to `Object`.

**Changed files**:
- `jvm/src/.../classgen/asm/ClassGenerator.scala` — add `createInterfaceGenerator` and
  `createAbstractMethod`
- `jvm/src/.../classgen/JvmClassGenerator.scala` — filter ability interface methods by
  `Qualifier.Ability`, group by ability name, call `AbilityInterfaceGenerator` per ability,
  append resulting `ClassFile` objects to module output

**What to test**: Integration test that for a module containing an ability definition,
`JvmClassGenerator` produces an additional inner `ClassFile` that is a valid JVM interface with
the correct method signature(s). Verify that concrete return types (e.g. `String`) are preserved
and generic params become `Object`.

**Verification**: `mill lang.test && mill jvm.test` — all tests pass.

### Phase 2 — Ability Implementation Singleton Generation

**Goal**: Produce a singleton `.class` for each ability implementation as part of module
generation in `JvmClassGenerator`.

**New files**:
- `jvm/src/.../classgen/AbilityImplGenerator.scala` — helper class that takes an
  `(abilityFQN, typeArgs)` combination and the concrete method FQNs, and produces a `ClassFile`
  for the singleton: `INSTANCE` field, constructor, and `@Override` methods delegating via
  `INVOKESTATIC`.

**Changed files**:
- `jvm/src/.../classgen/asm/MethodGenerator.scala` — add `addGetStaticInstance` and
  `addCallToAbilityMethod`
- `jvm/src/.../classgen/JvmClassGenerator.scala` — filter implementation values by
  `Qualifier.Implementation`, group by `(abilityFQN, typeArgs)`, call `AbilityImplGenerator`
  per group, append resulting `ClassFile` objects to module output

**What to test**: Integration test that for a module containing a concrete ability
implementation, `JvmClassGenerator` produces a singleton inner `ClassFile` that implements the
ability interface and whose override method delegates to the concrete static method.

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
- `jvm/src/.../classgen/JvmClassGenerator.scala` — add ability ref detection in
  `generateFunctionApplication`; compute interface internal name from `abilityFQN` via naming
  convention; emit `INVOKEINTERFACE`

**Interface class file in output**: Already handled in Phase 1 — the interface `ClassFile` is
included in the module output. No additional work needed here.

**What to test**: Full pipeline integration test: `def f[A ~ Show[A]](x: A): A = show(x)` should
generate a JVM method that calls `INVOKEINTERFACE` on the first parameter, and the ability
interface class should appear in the generated module's class files.

**Verification**: `mill lang.test && mill jvm.test && mill examples.run ...`.

### Phase 5 — Dictionary Injection at Call Sites

**Goal**: When calling a constrained function, inject the appropriate dictionary argument(s)
before the regular arguments.

**Changed files**:
- `jvm/src/.../classgen/JvmClassGenerator.scala` — extend `generateFunctionApplication`
  for the `ValueReference` case: after fetching `UncurriedValue`, check `ResolvedValue.paramConstraints`
  for the called function; match argument types to constrained type params; inject dict args

**Singleton class file in output**: Already handled in Phase 2 — singleton `ClassFile` objects
are included in the module output. No additional work needed here.

**Concrete call site** (e.g., `g(x: Int): Int = f(x)`):
1. Detect `f` has constraint `A ~ Show[A]`
2. Match `f`'s first param type `ParameterReference("A")` against argument's `expressionType`
   `ConcreteValue(Int)`
3. `A = Int`
4. Compute singleton internal name from `(ShowFQN, Seq(IntValue))` using naming convention
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

The ability interface `ClassFile` is generated as part of the ability's defining module
(`JvmClassGenerator` processes that module). Since there is exactly one defining module per
ability, the interface class is only emitted once. No deduplication needed.

### Singleton class file placement

Singleton `ClassFile` objects are generated as part of the module where the `implement` block
lives (`JvmClassGenerator` processes that module's implementation values). Their JVM class name
already encodes the implementation module (e.g., `ImplModule$Show$Int$impl`), so they're
correctly scoped.

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
