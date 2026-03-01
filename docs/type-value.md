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

## Violations

### 1. Symbolic: Universal Type Intro Stripping

`stripUniversalIntros` removes leading `FunctionLiteral(_, Value.Type, _)` before constraining
declared type against inferred body type. If type params were genuine function parameters, they
would unify naturally with the body's lambda structure instead of being stripped as a special wrapper.

### 2. Uncurry: Type Parameter Stripping

Type parameters are stripped from the uncurried parameter list via `stripLeadingLambdas`.
If type constructors are normal functions, their params should uncurry like any other parameters.

## Cascading Pattern: `stripUniversalTypeIntros`

The `strip` pattern appears in 5 processors: symbolic, implementation, abilitycheck, monomorphize,
uncurry. This pervasive stripping treats type parameters as a removable annotation layer rather
than genuine function parameters. Fully implementing the unification principle would likely
eliminate this pattern — type parameters would flow through the pipeline as regular parameters
whose type happens to be `Type`.
