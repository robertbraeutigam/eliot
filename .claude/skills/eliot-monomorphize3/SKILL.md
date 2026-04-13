---
name: eliot-monomorphize3
description: Use when editing, debugging, or reasoning about code under `lang/src/com/vanillasource/eliot/eliotc/monomorphize3/` — the NbE (Normalisation by Evaluation) type checker for Eliot. Covers the semantic domain (SemValue), the bidirectional checker, the evaluator, pattern unification, and the TypeStackLoop that processes type stacks uniformly without any concept of "generic parameters."
---

# monomorphize3: NbE type checker

## Scope of this skill

This skill governs all code under:

```
lang/src/com/vanillasource/eliot/eliotc/monomorphize3/
├── fact/
│   ├── GroundValue.scala              (output: Direct, Structure, Type)
│   ├── Monomorphic3Value.scala        (output fact: signature + runtime)
│   ├── Monomorphic3Expression.scala   (output expression ADT)
│   └── NativeBinding.scala            (CompilerFact: vfqn → SemValue)
├── domain/
│   ├── SemValue.scala                 (VType, VConst, VLam, VPi, VNative,
│   │                                    VTopDef, VMeta, VNeutral, Spine)
│   ├── Env.scala                      (Vector[SemValue] + names; de Bruijn levels)
│   └── MetaStore.scala                (IntMap[Option[SemValue]]; fresh/solve/lookup)
├── eval/
│   ├── Evaluator.scala                (eval, applyValue, force, semToGround)
│   └── Quoter.scala                   (SemValue → GroundValue read-back)
├── processor/
│   ├── SystemNativesProcessor.scala   (Function → VPi, Type → VType)
│   ├── DataTypeNativesProcessor.scala (data decls → VNative chains → VConst)
│   ├── UserValueNativesProcessor.scala(user defs → VTopDef with lazy thunk)
│   └── Monomorphic3Processor.scala    (entry point, delegates to TypeStackLoop)
├── unify/
│   └── Unifier.scala                  (pattern unification + postponement)
└── check/
    ├── Checker.scala                  (bidirectional check/infer)
    ├── CheckState.scala               (env, nameLevels, unifier)
    └── TypeStackLoop.scala            (uniform top-down fold over type stack)
```

and its sibling tests under `lang/test/src/com/vanillasource/eliot/eliotc/monomorphize3/`.

`monomorphize3` is **independent** of `monomorphize` and `monomorphize2`. Do not touch those packages when the task is about `monomorphize3`, and do not try to unify them.

## How NbE works

The core idea: ORE (syntax) is evaluated into `SemValue` (semantics) via Scala closures. Type equality becomes structural equality of normal forms. There is no constraint set, no worklist, no separate solving phase — unification happens locally as the checker walks the ORE.

### The semantic domain (`SemValue`)

| Variant | What it represents | Produced by |
|---|---|---|
| `VType` | The type of all types | Constant |
| `VConst(ground)` | Non-function ground values: concrete types (`BigInteger`, `Person`), literals (`42`) | Evaluator (literals), DataTypeNativesProcessor (fully applied constructors) |
| `VLam(name, closure)` | Runtime lambda closure | Evaluator (always, for `FunctionLiteral`) |
| `VPi(domain, codomain)` | Function type (dependent or non-dependent) | Checker (when checking `FunctionLiteral` against `VType`), `Function` native |
| `VNative(paramType, fire)` | Primitive awaiting a concrete argument | SystemNativesProcessor, DataTypeNativesProcessor |
| `VTopDef(fqn, cached, spine)` | Lazy top-level definition | UserValueNativesProcessor |
| `VMeta(id, spine, expected)` | Unsolved metavariable | Checker (fresh allocation via `MetaStore.fresh`) |
| `VNeutral(head, spine, tpe)` | Stuck application on a variable head | Evaluator (unresolved `ParameterReference`) |

### VLam vs VPi — who produces what

This is the single most important invariant:

- **The evaluator always produces `VLam` for `FunctionLiteral`.** A lambda is a callable closure, regardless of whether it appears in a type or value position. `eval(env, FunctionLiteral(pn, _, body))` → `VLam(pn, arg => eval(env.bind(pn, arg), body))`.
- **The Checker produces `VPi` when checking a `FunctionLiteral` against `VType`.** In this context, the expression IS a function type (a dependent product).
- **The Checker produces `VLam` when checking a `FunctionLiteral` against `VPi(d, c)`.** The expression is a runtime lambda.
- **The `Function` native produces `VPi`.** `Function[A, B]` evaluates via `apply(apply(nativeFunction, A), B)` → `VPi(A, _ => B)`.

Both are applicable: `apply(VLam(_, c), x)` invokes the closure; `apply(VPi(_, c), x)` returns `c(x)`. `VConst` never represents function types.

### Env and closures

`Env` is `Vector[SemValue]` indexed by de Bruijn level. Variable lookup is by level, not by name. Closures are native Scala functions that capture the current env and body ORE. No ORE substitution ever happens.

`Spine` is a reversed cons list (`SNil | SApp(tail, head)`) for O(1) append.

### How computation runs

`eval` encounters `FunctionApplication(target, arg)` → calls `applyValue(eval(target), eval(arg))`.

- `VLam` → invoke closure (beta-reduction)
- `VPi` → invoke codomain closure (type-level computation)
- `VNative` → `fire(arg)` if arg is concrete, else stuck as `VNeutral`
- `VNeutral`/`VTopDef`/`VMeta` → append arg to spine

`force(v, metaStore)` walks solved metas and unfolds `VTopDef` when the cached body is available.

## Pipeline

### Entry point

`Monomorphic3Processor.generateFromKeyAndFact` delegates to `TypeStackLoop.process`.

### NativeBinding injection

Three processors emit `NativeBinding` facts that map `ValueFQN → SemValue`:

1. **`SystemNativesProcessor`** — `Function` → curried `VNative` producing `VPi(A, _ => B)`; `Type` → `VType`.
2. **`DataTypeNativesProcessor`** — Data type constructors → `VNative` chains that collect type args and build `VConst(Structure(...))`.
3. **`UserValueNativesProcessor`** — User-defined values → `VTopDef(fqn, Lazy(eval(body)), SNil)`. Uses a concurrent `generating` guard to prevent mutual-recursion deadlocks.

### TypeStackLoop — the uniform fold

`TypeStackLoop.process` is the core algorithm:

1. **Pre-fetch** all `NativeBinding`s referenced in type stack levels and runtime body.
2. **Walk type stack top-down**: reverse the levels, fold with `expectedType = VType`. For each level: `checker.check(level, expectedType)`, then `expectedType = eval(level)`.
3. **Apply explicit type args**: fold `apply(sig, eval(typeArg))` for each, binding the type parameter name in the checker's state.
4. **Instantiate remaining**: peel any leftover `VLam` closures (phantom type parameters) by applying fresh metas.
5. **Check runtime body** against the monomorphized signature.
6. **Drain unifier**, report errors, quote signature to `GroundValue`.

**The fold body is identical for every level.** There is no branch for "is this level a type parameter?" Generics emerge naturally from `FunctionLiteral` levels being checked against `VPi` kinds from above.

### Bidirectional checker

Two modes:

- `check(tm, expected)` — checks a term against a known type. The `check` fallback path calls `infer`, then `instantiatePolymorphic` (peels leading `VLam`s with fresh metas), then unifies.
- `infer(tm)` — infers a term's type.

Key cases:

- **`ValueReference`**: fetch binding, fetch evaluated signature, apply explicit `typeArgs` via `applyValue`. Resolve ability methods via `tryResolveAbility`.
- **`FunctionApplication`**: infer target, then `applyInferred`. If target type is `VPi`, extract domain/codomain. If `VLam` (polytype at term level), instantiate with fresh meta and recurse. Otherwise unify against fresh `VPi(?dom, ?cod)`.
- **`FunctionLiteral` with annotation**: eval parameter type, bind, check body against return type.
- **`FunctionLiteral` without annotation, checked against `VPi`**: use `VPi`'s domain directly.

### Unifier

Pattern unification on `SemValue`s with postponement:

1. Force both sides.
2. `VType` vs `VType` → success.
3. `VConst(g1)` vs `VConst(g2)` → structural `GroundValue` equality.
4. `VPi`/`VLam` → extend with fresh variable, unify under binder.
5. Eta: `VLam(_, c)` vs other → unify `c(fresh)` against `apply(other, fresh)`.
6. `VNeutral` vs `VNeutral` with same head → zip spines.
7. `VMeta` → pattern rule: empty spine → solve directly; non-empty → postpone.
8. `VTopDef` vs `VTopDef` with same FQN → zip spines.
9. Otherwise → error.

`drain()` retries postponed constraints until stable (no progress → stop).

### Quoting (`forceAndConst` / `Quoter`)

Two quoting paths exist:

- **`Checker.forceAndConst`**: the primary path used during type checking. Force, then pattern-match: `VConst` → ground, `VType` → `GroundValue.Type`, `VPi` → `Structure(Map("$typeName" → Function, "A" → dom, "B" → cod))`, fallback → `GroundValue.Type`.
- **`Quoter.quote`**: stricter read-back that fails on `VNeutral`/`VMeta`/`VLam`/`VNative`/`VTopDef`. Currently used less frequently.

## The hard rules

1. **No concept of "generic parameters."** Do NOT extract, count, or classify type stack levels as "type parameters" vs "signature." The TypeStackLoop treats all levels identically. Do NOT use `TypeParameterAnalysis`, do NOT walk leading `FunctionLiteral`s in a type to count type params, do NOT introduce any structure that distinguishes generic from non-generic levels.

2. **The evaluator produces VLam; the Checker produces VPi.** Do not produce VPi in the evaluator. Do not produce VLam in the Checker when checking against VType (that should be VPi). The `Function` native is a VNative that fires to produce VPi.

3. **No ORE rewriting.** ORE is read once into `SemValue` and then forgotten. There is no `substitute(ore, name, replacement)` anywhere in monomorphize3. All substitution happens via closure application.

4. **No constraint set.** There is no list of `Constraint(left, right)` objects. Unification happens immediately and locally when the checker encounters a type mismatch between inferred and expected.

5. **NativeBinding pre-fetching.** The evaluator is synchronous and pure — it cannot do `CompilerIO`. All `NativeBinding` lookups must happen before evaluation via `prefetchBindings` / `collectBindings`, which populate a mutable cache the evaluator reads from.

6. **Mutable state management.** `Checker` has a `var state: CheckState` and `Unifier` has `var metaStore/depth/postponed/errors`. State mutations happen in-place. When binding a parameter, always use `state.bind(name, value)` which extends both `env` and `nameLevels` atomically.

## Diagnosing failures

**"Type mismatch" on a correct program.** Check that all referenced values have `NativeBinding` facts. If a `ValueReference` has no binding, the evaluator produces `VNeutral` (a stuck term), and unification fails when comparing it against the expected concrete type. Add the missing case to the relevant processor.

**Infinite loop / stack overflow.** Likely `force` unfolding a `VTopDef` that recursively references itself without the spine growing. `UserValueNativesProcessor` uses a `generating` guard for fact-level recursion, but `force` itself has no depth limit. Check that `force` only unfolds when the cached body is `Some`.

**"Cannot quote lambda"** from `Quoter`. A `VLam` survived to quoting time. This means a polymorphic value wasn't fully instantiated. Check that `TypeStackLoop.instantiateRemaining` peels all `VLam` closures after applying explicit type args.

**Wrong type for a generic value.** Check `fetchEvaluatedSignature` in `Monomorphic3Processor` — it evaluates only the last level of the type stack (via `foldLeft` that discards intermediate results). The signature `SemValue` should be a `VLam` for generic values. Then `applyTypeArgs` in `TypeStackLoop` applies explicit type args and `instantiateRemaining` handles the rest.

**Meta not solved.** After `drain()`, unsolved metas fall back to `GroundValue.Type` via `forceAndConst`'s fallback case. If this produces wrong types, the unifier likely postponed a constraint that should have been solved. Check that `solveMeta` handles the empty-spine case correctly.

## Anti-patterns (reject in review)

- **Inspecting ORE to count or classify type parameters.** No `extractLeadingLambdaParams`, no `TypeParameterAnalysis`, no "how many type parameters does this value have?" question. The type stack fold is uniform.
- **Producing `VPi` in the evaluator.** The evaluator always produces `VLam` for `FunctionLiteral`. Only the Checker and the `Function` native produce `VPi`.
- **Producing `VLam` in the Checker when the expected type is `VType`.** That should be `VPi`. A `FunctionLiteral` checked against `VType` is a type expression (dependent product), not a runtime lambda.
- **ORE substitution inside monomorphize3.** There is no `OperatorResolvedExpression.substitute` call anywhere. All binding is via closure capture in Scala.
- **Calling `CompilerIO` from the evaluator.** The evaluator is synchronous and pure. All IO (fact fetching) must be done before evaluation via pre-fetching.
- **Adding a constraint list or worklist.** Unification is immediate and local, not deferred. The only "deferred" mechanism is the unifier's `postponed` queue for non-pattern meta spines.
- **Eagerly allocating metas for type parameters at `ValueReference`.** Implicit instantiation is driven lazily by `FunctionApplication` / `instantiatePolymorphic` / `instantiateRemaining`, not by peeking at the referenced value's structure.
- **Using `Map[String, SemValue]` for the environment hot path.** Env is `Vector[SemValue]` with de Bruijn level indexing. Name-to-level mapping is in `CheckState.nameLevels`, maintained by the Checker, not the Evaluator.
- **Pattern-matching on `SemValue` to count leading `VPi` binders or `VLam` closures.** The checker never asks "how many type parameters does this value have?" It applies what's given and lets unification figure out the rest.
- **Skipping `prefetchBindings` before evaluating.** The evaluator reads from a mutable cache. If bindings aren't pre-fetched, the evaluator produces `VNeutral` for unresolved references, leading to spurious type mismatches.
- **Forgetting to call `drain()` after the check/infer walk.** Postponed unification constraints accumulate silently. Without `drain()`, metas that depend on other metas being solved first will never be resolved.

## Testing

Tests live at:

- `lang/test/src/com/vanillasource/eliot/eliotc/monomorphize3/processor/Monomorphic3ProcessorTest.scala`
- `lang/test/src/com/vanillasource/eliot/eliotc/monomorphize3/processor/Monomorphic3TypeCheckTest.scala`

Run them:

```bash
./mill lang.test 2>&1 | grep -v DEBUG | grep "Monomorphic3"
```

Tests construct source text inline. Follow the project testing conventions: single-line asserts, assert the `Seq` itself, prefer `.asserting(_ ...)`.

## Facts and keys

- `Monomorphic3Value.Key(vfqn, specifiedTypeArguments)` — composite key. Same `vfqn` + different type args → different specialization.
- `Monomorphic3Value` fields: `vfqn`, `specifiedTypeArguments`, `signature: GroundValue`, `runtime: Option[Sourced[Monomorphic3Expression.Expression]]`. No `calculatedTypeArguments` — NbE folds them into the signature directly.
- `NativeBinding.Key(vfqn)` — maps a value FQN to its `SemValue` for the NbE evaluator.
- `Monomorphic3Expression.expressionType: GroundValue` — always fully ground, no free variables or unsolved metas.
