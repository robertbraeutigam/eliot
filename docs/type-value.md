# Type-Level and Value-Level Unification

## Principle

In Eliot, type constructors are normal functions in a separate namespace. For example,
`data Box[A: Type](content: A)` produces two functions:

- **Value constructor**: `Box(content: A)` — a normal function in the default namespace
- **Type constructor**: `Box^Type(A: Type)` — a normal function in the `^Type` namespace

Both should be handled identically by the compiler pipeline. The only difference is the
namespace qualifier (`Qualifier.Default` vs `Qualifier.Type`).

## Evaluation by Processor

| Processor | Type/Value Differentiation | Violation? |
|-----------|---------------------------|------------|
| core | `typeContext` flag assigns qualifiers based on syntax (`[]` vs `()`) | No — syntactic namespace resolution |
| core | `DataDefinitionDesugarer` creates two functions per data definition | No — correct by design |
| core | Generic params → `FunctionLiteral(name, Some(Type), body)`, value params → `Function^Type` application | No — structural encoding |
| module | None — treats all `QualifiedName`s uniformly | No |
| resolve | `runtime` flag controls type stack level resolution | No — distinguishes meta-levels |
| eval | Three evaluators split by qualifier | No — implementation strategy split (native vs interpreted), not type/value |
| operator | None — types flow through as expressions | No |
| symbolic | Separate `TypeExpressionEvaluator` and `BodyTypeInferrer` | No — evaluate vs infer, not type vs value |
| **symbolic** | **`stripUniversalIntros` removes type param wrappers before constraining** | **Yes** |
| matchdesugar | Separate `TypeMatchDesugarer` and `DataMatchDesugarer` | No — types are open unions (`typeMatch*`), data uses closed `handleWith` |
| matchdesugar | `ConstructorTypeAnalyzer` checks `Qualifier.Type` for data type name lookup | No — namespace-aware lookup |
| matchdesugar | `ConstructorTypeAnalyzer` skips `FunctionLiteral` when counting fields | Cascading |
| **uncurry** | **`stripLeadingLambdas` excludes type params from uncurried parameter list** | **Yes** |
| implementation | `stripUniversalTypeIntros()` before substitution | Cascading |
| abilitycheck | `stripUniversalTypeIntros()` + `matchTypes()` | Cascading |
| monomorphize | `extractBodyTypeParams` filters by `Value.Type` | Cascading |
| used | None — post-monomorphization, all concrete | No |

## The Stripping Problem

All violations and cascading issues stem from one root cause: type parameters are encoded as
leading `FunctionLiteral` wrappers inside the `signature` field:

```
FunctionLiteral("A", Type,           -- type param wrapper
  Function^Type(A, A)                -- actual function signature
)
```

Every downstream processor needs to peel these off. They all perform the same extract + strip
dance to get at two things:

| Processor | What it extracts | Purpose |
|-----------|-----------------|---------|
| symbolic (line 70) | Inner signature | Constrain against body's inferred type |
| implementation (line 85) | Inner signature | Substitute type args, produce concrete signature |
| implementation (line 122) | Type param names | Zip with type arguments to build substitution map |
| implcheck (lines 122, 128, 132) | Both | Substitute and compare abstract vs impl signatures |
| abilitycheck (lines 127-128) | Both | Match inner sig against concrete type to extract type args |
| monomorphize (lines 34, 50) | Both | Build substitution map, evaluate signature with type args |
| monomorphize (line 105) | Type param names | Check if a referenced value is generic |
| monomorphize (line 138) | Inner signature | Match against call-site type to infer type args |
| uncurry (line 39) | Inner signature | Extract value-level parameters for uncurrying |
| matchdesugar | Inner signature | Count constructor fields, extract data type name |

Every single site wants either: (a) the type parameter names, (b) the inner function signature
without type param wrappers, or (c) both.

## Proposed Fix

Store type parameters separately in the value facts instead of encoding them as wrappers
in the expression tree:

```scala
case class TypeCheckedValue(
    vfqn: ValueFQN,
    name: Sourced[QualifiedName],
    typeParameters: Seq[(String, Value)],  // extracted once at creation
    signature: ExpressionValue,            // already the inner function type
    runtime: Option[Sourced[TypedExpression.Expression]]
)
```

This would:
- Eliminate all `stripUniversalTypeIntros` / `stripLeadingLambdas` / `extractLeadingLambdaParams`
  calls across 6+ processors
- Make type param names directly accessible via `typeCheckedValue.typeParameters`
- Make the signature already be the inner function type without wrappers
- Simplify substitution to `typeParameters.map(_._1).zip(typeArgs).toMap`
- Remove `extractBodyTypeParams`, `stripNonBodyUniversals` from monomorphize

The type parameters are still regular parameters — they are just stored as structured data on
the value definition rather than flattened into the expression tree where every consumer must
re-extract them.
