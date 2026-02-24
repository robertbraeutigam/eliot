# Refactoring Proposal: Symbolic Type Checking Package

## Summary

The symbolic package implements constraint-based type checking via unification. It works
correctly and the individual pieces are well-structured, but the central `TypeStackBuilder`
object conflates two distinct interpretation modes of the same AST into a single 344-line file.
Splitting this along its natural seam — type-level evaluation vs. body-level inference — would
make the code significantly easier to follow for someone encountering it for the first time.

## Problem Analysis

### 1. `TypeStackBuilder` has two parallel interpretations with no structural separation

`TypeStackBuilder` contains two top-level recursive dispatches over `Expression`:

- **`buildExpression`** (line 214): Interprets an expression *in type position*. A `ValueReference`
  becomes a `ConcreteValue` (a known data type) or stays a `ParameterReference` (a universal
  variable). A `FunctionLiteral` either introduces a universal type parameter
  (`buildUniversalIntro`) or constructs a `FunctionType` (`buildFunctionType`). Integer and
  string literals produce `ConcreteValue(Value.Direct(...))` — compile-time constants.

- **`inferBody`** (line 40): Interprets an expression *in runtime/body position*. A
  `ValueReference` loads the referenced value's resolved type and instantiates its type
  parameters as fresh unification variables. A `FunctionApplication` generates unification
  variables for argument and return types and emits constraints. Literals produce
  `ConcreteValue(Types.dataType(...))` — runtime type references.

These two functions do fundamentally different things — one *evaluates* type syntax into
`ExpressionValue`, the other *infers* types of runtime code by emitting constraints. But they
live side by side with similar-sounding names (`build*` vs `infer*`), making it hard to tell
which context you're in when reading a helper method.

The dependency between them is one-directional: body inference calls into type evaluation
(via `processStack`) when it needs to evaluate a type annotation (e.g., the parameter type in
a lambda, or the type of a referenced value). Type evaluation never calls into body inference.

### 2. Mode flags create hidden behavioral branching

`TypeCheckState` carries two mode flags — `instantiationMode` and `pendingTypeArgs` — that
change the behavior of `buildUniversalIntro`. When `instantiationMode` is `true`, a universal
introduction (`[A] ->`) binds `A` to a fresh unification variable (or an explicit type
argument); when `false`, it records `A` as a universal variable and wraps the result in
`FunctionLiteral`.

This branching is driven by whether we're processing a *referenced* value's type (instantiation
mode, triggered from `inferValueReference`) vs. the *current* value's type declaration
(declaration mode). The mode flag is set via `withInstantiationMode { ... }` in
`inferValueReference`, creating a non-local effect that influences deeply nested behavior in
`buildUniversalIntro`.

This pattern is correct but hard to trace: you have to know that `buildExpression` behaves
differently depending on who called it, and that information is only visible in the mutable
state.

### 3. The 7-tuple in `typeCheckWithBody` is a readability bottleneck

`SymbolicTypeCheckProcessor.typeCheckWithBody` runs a `TypeGraphIO` computation that produces
7 values needed after the state is discarded. These are collected into an anonymous tuple and
destructured on the next line. This is the most complex method in the processor and the tuple
makes it hard to name and reason about the intermediate state.

### 4. Constraint container and solver are merged

`SymbolicUnification` serves as both the accumulated constraint set (via its `Monoid` instance)
and the constraint solver (via the `solve` method with its 120-line `unify` implementation).
Accumulation and solving are fundamentally different lifecycle phases — constraints are collected
during the `TypeGraphIO` state pass, then solved outside it. Separating these would make the
lifecycle more explicit.

## Proposed Refactoring

### Step 1: Split `TypeStackBuilder` into `TypeExpressionEvaluator` and `BodyTypeInferrer`

**`TypeExpressionEvaluator`** — evaluates type-position expressions into `ExpressionValue`:
- `processStack` (public entry point — used by both type evaluation and body inference)
- `processLevels`
- `buildExpression` (renamed to `evaluateExpression` or kept internal)
- `buildUniversalIntro`, `buildFunctionType`, `buildValueReference`, `buildTypeApplication`
- `extractConcreteValue`
- `isKindAnnotation`, `isKindExpression`, `isFunctionReference`
- `handleParameterReference` (shared — lives here since it's type-level)

**`BodyTypeInferrer`** — infers types of runtime expressions via constraint generation:
- `inferBody` (public entry point)
- `inferLiteral`, `inferValueReference`, `inferFunctionApplication`, `inferFunctionLiteral`
- `inferBodyStack`

`BodyTypeInferrer` depends on `TypeExpressionEvaluator` (calls `processStack` for type
annotations). `TypeExpressionEvaluator` has no dependency on `BodyTypeInferrer`.

This split follows the existing naming convention (`build*` methods stay together, `infer*`
methods stay together) and makes the one-directional dependency explicit in the file structure.

### Step 2: Replace the mode flags with explicit method variants

Instead of a boolean `instantiationMode` flag that silently changes `buildUniversalIntro`
behavior, `TypeExpressionEvaluator` could expose two methods:

```scala
/** Process a type stack for a declaration (universal vars preserved as FunctionLiteral). */
def processStackForDeclaration(stack: Sourced[TypeStack[Expression]]): TypeGraphIO[...]

/** Process a type stack for instantiation (universal vars become unification vars). */
def processStackForInstantiation(
    stack: Sourced[TypeStack[Expression]],
    explicitTypeArgs: Seq[ExpressionValue] = Seq.empty
): TypeGraphIO[...]
```

The mode and pending-type-args state would become parameters rather than mutable state fields.
`buildUniversalIntro` would receive an explicit parameter indicating which behavior to use
rather than consulting the state.

This eliminates `instantiationMode`, `pendingTypeArgs`, `withInstantiationMode`,
`withPendingTypeArgs`, `consumeNextPendingTypeArg`, `getRemainingPendingTypeArgCount`, and
`isInstantiationMode` from `TypeCheckState`, simplifying it to just the core type-checking
state (variable bindings, constraints, id generation).

### Step 3: Extract a result case class for `typeCheckWithBody`

Replace the 7-tuple with a named case class:

```scala
private case class TypeCheckResult(
    declaredType: ExpressionValue,
    typedLevels: Seq[TypedExpression],
    typedBody: TypedExpression,
    constraints: SymbolicUnification,
    universalVars: Set[String],
    unificationVars: Set[String],
    qualifierParams: Seq[ExpressionValue]
)
```

This makes `typeCheckWithBody` read as: collect a `TypeCheckResult`, then solve and apply
substitutions.

### Step 4 (optional): Separate `ConstraintSolver` from `SymbolicUnification`

Split into:
- **`SymbolicUnification`** — pure constraint container with `Monoid` and `Show`. Just data.
- **`ConstraintSolver`** — takes a `SymbolicUnification`, universal/unification var sets,
  runs Robinson's algorithm, returns `UnificationState`. Contains `unify`, `solveConstraint`,
  `isOccursCheck`, and `issueError`.

This is a smaller win but would make the two-phase lifecycle (accumulate, then solve) explicit
in the type structure.

## What Not to Change

- **The fact types** (`TypeCheckedValue`, `TypedExpression`, `QualifiedName`, `Qualifier`) are
  clean and appropriately scoped. No changes needed.
- **`ShortUniqueIdentifiers`** is a self-contained utility. Fine as-is.
- **`UnificationState`** is minimal and focused. Fine as-is.
- **The test file** tests through the processor interface and would not need changes for any of
  the proposed refactoring (all changes are internal restructuring).

## Impact Assessment

| Change | Files affected | Risk | Readability gain |
|--------|---------------|------|-----------------|
| Split TypeStackBuilder | 3 new/modified files in processor/ | Low — method bodies unchanged | High |
| Explicit method variants | TypeExpressionEvaluator, TypeCheckState, SymbolicTypeCheckProcessor | Medium — changes call signatures | High |
| Result case class | SymbolicTypeCheckProcessor only | Very low | Medium |
| Separate ConstraintSolver | 2 new/modified files in types/ | Low | Low-Medium |

Steps 1 and 3 carry low risk and high readability benefit. Step 2 is the most
architecturally impactful but removes a source of non-local reasoning. Step 4 is
optional polish.
