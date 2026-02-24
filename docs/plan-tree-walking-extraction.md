# Plan: Tree-Walking Extraction

## Context

This document analyses how expression and tree-like structures are used across the
`eval`, `symbolic`, `monomorphize`, and `uncurry` packages, and proposes what
generic tree-walking abstractions to extract and where.

---

## Expression Hierarchy

The compiler has five distinct expression/value tree types, used in sequence:

| Type | Package | Children carry |
|------|---------|---------------|
| `resolve.fact.Expression` | lang/resolve | raw AST with type stacks |
| `eval.fact.ExpressionValue` | lang/eval | compile-time values & types |
| `symbolic.fact.TypedExpression` | lang/symbolic | ExpressionValue-typed expressions |
| `monomorphize.fact.MonomorphicExpression` | lang/monomorphize | ground-typed expressions |
| `uncurry.fact.UncurriedExpression` | lang/uncurry | multi-arg applications/lambdas |

`ExpressionValue` is special: it represents *types and values at compile time* and
is referenced by the later expression types as the type annotation carrier.

---

## Inventory of Tree-Walking Code

### In `eval.fact.ExpressionValue` (companion object)

| Function | Kind | Notes |
|----------|------|-------|
| `containsVar` | fold (→ Boolean) | Manual recursion: FunctionApplication (OR), FunctionLiteral (body), leaves (false) |
| `substitute` | transform | Variable-capturing-aware: stops at shadowing lambda |
| `transform` | bottom-up map | General structural transformation, same type in/out |
| `matchTypeVarBindings` | fold over two trees | Pattern-matches type vars by name set |
| `stripLeadingLambdas` | linear tail-rec | Not a tree walk |
| `extractLeadingLambdaParams` | linear | Not a tree walk |
| `Show` instances | fold (→ String) | Manual recursion |

### In `eval.util.Evaluator`

| Function | Kind | Notes |
|----------|------|-------|
| `toExpressionValue` | catamorphism on `resolve.Expression` → `ExpressionValue` (CompilerIO) | All 6 cases, threads `CompilerIO` |
| `reduce` | structural recursion on `ExpressionValue` (CompilerIO) | Beta reduction; special post-recursion logic |

### In `monomorphize.processor.MonomorphicTypeCheckProcessor`

| Function | Kind | Notes |
|----------|------|-------|
| `transformExpression` | catamorphism on `TypedExpression.Expression` → `MonomorphicExpression.Expression` (CompilerIO) | All 6 cases |
| `matchTypes` | fold over two trees (→ Map) | **Private duplicate** of `matchTypeVarBindings` |

### In `monomorphize.processor.TypeEvaluator`

| Function | Kind | Notes |
|----------|------|-------|
| `resolveDataTypeRefs` | monadic structural recursion on `ExpressionValue` (CompilerIO) | Treats FunctionApplication target differently from arg |

### In `uncurry.processor.UncurryingProcessor`

| Function | Kind | Notes |
|----------|------|-------|
| `convertExpression` | catamorphism on `TypedExpression` → `UncurriedExpression` (pure) | All 6 cases; delegates FunctionApplication to `convertApplication` |
| `flattenApplication` | tail-recursive traversal | Flattens curried applications |
| `flattenLambda` | tail-recursive traversal | Flattens nested lambdas |
| `stripLambdas` | structural recursion (CompilerIO) | Strips N leading lambdas |

### In `symbolic.fact.TypedExpression`

| Function | Kind | Notes |
|----------|------|-------|
| `transformTypes` | tree walk applying `ExpressionValue => ExpressionValue` to all type fields | Does NOT recurse into ValueReference; only into FunctionApplication and FunctionLiteral |

### In `used.UsedNamesProcessor`

| Function | Kind | Notes |
|----------|------|-------|
| `processExpression` | fold on `MonomorphicExpression.Expression` (→ UsedNamesIO) | Manual recursion with `@tailrec` on application chains |

---

## Identified Duplication and Extraction Opportunities

### 1. `matchTypes` vs `matchTypeVarBindings` — Clear Duplication

`MonomorphicTypeCheckProcessor.matchTypes` (private) and
`ExpressionValue.matchTypeVarBindings` (public) perform structurally identical
recursive matching of two `ExpressionValue` trees to extract type variable bindings.

Differences:
- `matchTypes` matches ANY `ParameterReference`; `matchTypeVarBindings` filters by a
  name set.
- `matchTypes` recurses into `FunctionLiteral` bodies; `matchTypeVarBindings` does not.

**Proposal:** Replace both with a single `ExpressionValue.matchTypes` (name TBD) in
the `ExpressionValue` companion. The unified version handles all structural cases
(including `FunctionLiteral` bodies), and takes an optional predicate
`String => Boolean` defaulting to `_ => true`. Then:

- `matchTypeVarBindings` becomes a one-liner calling the unified function with
  `typeParamNames.contains`.
- `MonomorphicTypeCheckProcessor.matchTypes` is deleted; the processor calls the
  public one instead.

### 2. `ExpressionValue.fold[A]` — Missing Generic Query Primitive

`containsVar` does manual structural recursion. Any future query (find all free
variables, find all value references, count nodes, collect leaves, …) would have to
duplicate the same skeleton. The existing `transform` covers structural
transformations but there is no generic fold.

**Proposal:** Add `ExpressionValue.fold[A]` to the companion:

```scala
def fold[A](
    onConcrete:  Value => A,
    onNative:    Value => A,
    onParamRef:  (String, Value) => A,
    onFunApp:    (A, A) => A,
    onFunLit:    (String, Value, A) => A
)(expr: ExpressionValue): A
```

Rewrite `containsVar` using `fold`. `substitute` and `transform` deliberately cannot
use `fold` (they short-circuit at binding sites or produce the same type via
structural identity), so leave them as-is.

### 3. Catamorphism on `TypedExpression` — Repeated Phase-Boundary Pattern

Two separate processors translate the whole `TypedExpression` tree to a new
expression type:

- `UncurryingProcessor.convertExpression`: `TypedExpression → UncurriedExpression`
  (pure, all 6 cases)
- `MonomorphicTypeCheckProcessor.transformExpression`: `TypedExpression.Expression →
  MonomorphicExpression.Expression` (`CompilerIO`, all 6 cases)

Both exhaustively pattern-match all 6 `TypedExpression.Expression` variants.
Every future backend or pass that transforms `TypedExpression` will write the same
skeleton.

**Proposal:** Add a generic catamorphism to `TypedExpression`:

```scala
def foldExpression[F[_]: Monad, A](
    onIntLit:    Sourced[BigInt] => F[A],
    onStrLit:    Sourced[String] => F[A],
    onParamRef:  Sourced[String] => F[A],
    onValRef:    Sourced[ValueFQN] => F[A],
    onFunApp:    (Sourced[TypedExpression], Sourced[TypedExpression]) => F[A],
    onFunLit:    (Sourced[String], Sourced[ExpressionValue], Sourced[TypedExpression]) => F[A]
)(self: TypedExpression.Expression): F[A]
```

The pure case is the special case with `F = Id` (cats `Id`).

`UncurryingProcessor.convertExpression` becomes a call to `foldExpression[Id, UncurriedExpression]`.
`MonomorphicTypeCheckProcessor.transformExpression` becomes a call to
`foldExpression[CompilerIO, MonomorphicExpression.Expression]`.

Note: the callbacks for `FunctionApplication` and `FunctionLiteral` still receive the
raw `Sourced[TypedExpression]` children (not pre-recursed results), because the
callers need to control recursion themselves (e.g. flattening in `convertApplication`,
type-inference in `transformFunctionApplication`). This is a "dispatch catamorphism"
rather than a fully recursive one, and it is intentionally limited to avoid losing
flexibility.

---

## What NOT to Extract

- **`Evaluator.reduce`**: Beta reduction has post-recursion logic (type checking,
  substitution) that does not fit any standard pattern. Keep as-is.
- **`TypeEvaluator.resolveDataTypeRefs`**: Treats `FunctionApplication.target` and
  `.argument` asymmetrically; cannot be expressed with `transform` or `transformM`.
  Keep as-is.
- **`UsedNamesProcessor.processExpression`**: The `@tailrec` application-chain logic
  is bespoke; a fold on `MonomorphicExpression` would not simplify it materially.
  Only one caller exists. Keep as-is.
- **`TypedExpression.transformTypes`**: Already a well-named, single-purpose helper.
  Keep as-is.

---

## Implementation Plan

### Step 1 — Unify `matchTypes` / `matchTypeVarBindings`

**File:** `lang/src/.../eval/fact/ExpressionValue.scala`

1. Replace `matchTypeVarBindings` with a more general `matchTypes` that accepts an
   optional name-filter predicate (default `_ => true`) and handles `FunctionLiteral`
   bodies.
2. Delete the private `matchTypes` in `MonomorphicTypeCheckProcessor`; update its call
   sites to use `ExpressionValue.matchTypes`.
3. Any existing callers of `matchTypeVarBindings` call
   `ExpressionValue.matchTypes(..., typeParamNames.contains)`.

### Step 2 — Add `ExpressionValue.fold[A]`

**File:** `lang/src/.../eval/fact/ExpressionValue.scala`

1. Add `fold[A]` method to the companion object as described above.
2. Rewrite `containsVar` as a one-liner using `fold`.

### Step 3 — Add `TypedExpression.foldExpression`

**File:** `lang/src/.../symbolic/fact/TypedExpression.scala`

1. Add `foldExpression[F[_]: Monad, A]` to the `TypedExpression` companion as
   described above (dispatch, not fully recursive).
2. Rewrite `UncurryingProcessor.convertExpression` using `foldExpression[Id, ...]`.
3. Rewrite `MonomorphicTypeCheckProcessor.transformExpression` using
   `foldExpression[CompilerIO, ...]`.

### Ordering

Steps 1, 2, and 3 are independent and can be done in any order or in parallel
branches. Step 1 has the highest risk/reward ratio because it removes actual
duplication. Step 2 is the lowest-risk change. Step 3 has the widest impact and
should include corresponding test coverage verification.

---

## File Impact Summary

| File | Change |
|------|--------|
| `eval/fact/ExpressionValue.scala` | Add `fold`, replace `matchTypeVarBindings` with `matchTypes` |
| `monomorphize/processor/MonomorphicTypeCheckProcessor.scala` | Remove private `matchTypes`, call `ExpressionValue.matchTypes` |
| `symbolic/fact/TypedExpression.scala` | Add `foldExpression` |
| `uncurry/processor/UncurryingProcessor.scala` | Use `foldExpression` in `convertExpression` |
| `monomorphize/processor/MonomorphicTypeCheckProcessor.scala` | Use `foldExpression` in `transformExpression` |
| All test files for the above | Update any test cases that directly construct expression trees if APIs change |
