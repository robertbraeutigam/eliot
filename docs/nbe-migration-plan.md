# Plan: NbE-based monomorphize2

A migration plan to replace `monomorphize2`'s constraint-extract / solve / processor pipeline with a single-pass bidirectional elaborator backed by **Normalization by Evaluation** (NbE). New `eval2` package provides the semantic domain, evaluator, and unifier with metavariables. The existing `eval` package and `ORE` are not touched. Output (`MonomorphicValue` / `MonomorphicExpression`) keeps its current shape, with the bonus that `calculatedTypeArguments` finally gets populated.

This document is a **plan / outline**, not a spec. The intent is enough detail that someone with the existing codebase in front of them can sit down and start implementing, not a finished design.

## 0. Goals

- Eliminate `ConstraintExtract`, `ConstraintSolver`, `Constraints`, `Solution`, `SolverState`, `TypeCheckState`, `ShortUniqueIdentifiers`, and the `WalkIO` replay in the processor.
- Replace them with a bidirectional NbE-based elaborator that walks the ORE once and produces `MonomorphicExpression` directly.
- Type-level computation is handled by the evaluator instead of by an `evalAndCompare` fallback.
- Polytype instantiation becomes function application at the semantic level — no separate "instantiation rule" needed.
- ORE, the existing `eval` package, and downstream consumers (`uncurry`, `used`) are untouched.
- `MonomorphicExpression` shape stays the same; `MonomorphicValue.calculatedTypeArguments` finally gets populated with the inferred type arguments per call site.

## 1. Why NbE

A constraint-based unification engine over a syntactic representation is fine for HM. It strains as soon as types can compute (type-level functions, applied data type constructors, etc.) — that's why `monomorphize2` already needed an `evalAndCompare` fallback. The fallback is a hint that the underlying model wants reduction baked in, not bolted on.

NbE-based elaborators (Lean 4, Idris 2, Agda, [smalltt](https://github.com/AndrasKovacs/smalltt)) handle this by making the evaluator the canonical normalizer. Type equality is structural equality of normal forms. Polytype instantiation is just function application at the value level. Unification is local: when two values meet during elaboration, they unify on the spot (with metavariables for unknowns). There is no constraint set, no worklist, no separate solving phase, no replay.

The architecture is also a much better fit for where Eliot is heading: dependent types and compile-time guarantees about resource usage. Both of those *require* type-level reduction as a first-class operation.

## 2. The new `eval2` package

`lang/src/com/vanillasource/eliot/eliotc/eval2/` — parallel to the existing `eval`. Different in two essential ways:

1. The semantic domain (`Sem`) carries metavariables as first-class neutral heads.
2. The evaluator is the canonical normalizer; eval2 always evaluates, eagerly, and only stops at neutral terms (free parameters, unsolved metas, blocked top-level definitions).

### 2.1 The semantic domain (`eval2/fact/Sem.scala`)

```scala
sealed trait Sem

object Sem {
  /** A literal value: BigInt, String, etc. Wraps the existing eval `Value.Direct`. */
  case class Lit(direct: Value.Direct) extends Sem

  /** The universe of types. */
  case object TypeUniv extends Sem

  /** A fully-evaluated structure (data type instance, function type, etc). */
  case class Struct(typeFqn: ValueFQN, fields: Map[String, Sem]) extends Sem

  /** A closure: a lambda value waiting to be applied. The body is ORE — the closure
    * captures the env at construction time and re-evaluates the body when applied. */
  case class Lam(paramName: String, dom: Sem, body: Closure) extends Sem

  /** A neutral term: a head (variable, meta, or stuck top-level reference) followed
    * by a spine of arguments. Cannot be reduced further until something fills in the head. */
  case class Neut(head: Head, spine: Seq[Sem]) extends Sem
}

sealed trait Head
object Head {
  /** A bound parameter that escaped its binder (free in the current scope). */
  case class Param(name: String) extends Head

  /** A unification metavariable. */
  case class Meta(id: MetaId) extends Head

  /** A top-level value reference whose body is currently blocked from reducing
    * (e.g. recursive definition, opaque definition). */
  case class Ref(vfqn: ValueFQN) extends Head
}

case class Closure(env: Env, body: OperatorResolvedExpression)

case class Env(params: Map[String, Sem]) {
  def extend(name: String, sem: Sem): Env = copy(params + (name -> sem))
}
object Env { def empty: Env = Env(Map.empty) }

opaque type MetaId = Int
```

Key relationships:
- `Value` (existing) is essentially the **fully-ground** subset of `Sem` — everything except `Lam` (which needs more work) and `Neut` (which needs more bindings to reduce).
- `quote: Sem => Option[Value]` (see §2.4) is the projection back to `Value` for output, used after all metas are solved.

### 2.2 The evaluator (`eval2/util/Evaluator2.scala`)

```scala
object Evaluator2 {
  def eval(env: Env, expr: OperatorResolvedExpression): EvalIO[Sem]
  def apply(f: Sem, arg: Sem): EvalIO[Sem]
}
```

`EvalIO[A] = StateT[CompilerIO, MetaState, A]` — the evaluator threads the metacontext so that reductions across metavariables produce the right thing.

`eval` cases:

| ORE | Sem result |
|---|---|
| `IntegerLiteral(v)` | `Lit(Direct(v, bigIntType))` |
| `StringLiteral(v)` | `Lit(Direct(v, stringType))` |
| `ParameterReference(name)` | `env.params(name)` if bound, else `Neut(Param(name), [])` |
| `ValueReference(vfqn, typeArgs)` | look up the resolved value; eval its **runtime** in `Env.empty` to get a Sem (or `Neut(Ref(vfqn), [])` if blocked); then `apply` it to each evaluated `typeArg` |
| `FunctionApplication(target, arg)` | `apply(eval(env, target), eval(env, arg))` |
| `FunctionLiteral(paramName, paramTypeOpt, body)` | `Lam(paramName, eval(env, paramTypeOpt), Closure(env, body))` (fresh meta if `paramTypeOpt` is `None`) |

`apply(f, arg)`:

| Shape of `f` | Result |
|---|---|
| `Lam(name, _, Closure(closureEnv, body))` | `eval(closureEnv.extend(name, arg), body)` — the only place reduction actually happens |
| `Neut(head, spine)` | `Neut(head, spine :+ arg)` — deferred application; will fire when `head` becomes known |
| `Struct(...)` representing a function *type* | compile error: applying a type to a value |
| anything else | compile error |

**Top-level definition reduction.** When eval'ing `ValueReference(vfqn, _)`, the evaluator looks up the resolved value's runtime body and re-evaluates it in `Env.empty`. To avoid loops on recursive defs, the evaluator tracks a "currently evaluating" set in `MetaState` (or carries it as an extra reader). On re-entry, it returns `Neut(Ref(vfqn), spine)` instead of recursing — the reference becomes blocked (neutral), which is exactly the behaviour you want for `def f: T = f`-style definitions: the type-checker still sees `f` as having the right type via its signature, but the runtime body doesn't get inlined into itself.

Same applies to mutually recursive defs.

This is the eval2 equivalent of the existing `evaluateValueType` in the OLD monomorphize package, but generalized to all reduction.

### 2.3 The metacontext (`eval2/util/MetaState.scala`)

```scala
case class MetaInfo(
    name: String,           // for error messages and debug printing
    expectedType: Sem,      // for occurs check and zonking
    solution: Option[Sem]   // None until solved
)

case class MetaState(
    metas: Map[MetaId, MetaInfo] = Map.empty,
    nextId: Int = 0,
    inProgress: Set[ValueFQN] = Set.empty  // for top-level recursion guard
)

object MetaState {
  def freshMeta(name: String, expectedType: Sem): EvalIO[MetaId]
  def lookupMeta(id: MetaId): EvalIO[MetaInfo]
  def solveMeta(id: MetaId, sem: Sem): EvalIO[Unit]
  def withInProgress[A](vfqn: ValueFQN)(action: EvalIO[A]): EvalIO[A]
}
```

Solving a meta is destructive (after the occurs check passes). The cost is that any cached `Sem` containing that meta is now stale; the way to deal with it is **never cache forced Sems** — always re-force when reading. `force` walks meta solutions.

```scala
def force(s: Sem): EvalIO[Sem] = s match {
  case Sem.Neut(Head.Meta(id), spine) =>
    lookupMeta(id).flatMap {
      case MetaInfo(_, _, Some(solution)) =>
        spine.foldLeftM(solution)(Evaluator2.apply).flatMap(force)
      case _ => s.pure
    }
  case other => other.pure
}
```

### 2.4 The quoter (`eval2/util/Quoter.scala`)

```scala
object Quoter {
  /** Convert a fully-solved Sem to a Value. Returns None if any meta is unsolved
    * or any free parameter remains. The output Value is what gets stored in
    * MonomorphicExpression.expressionType. */
  def quote(sem: Sem): EvalIO[Option[Value]]
}
```

Walks the Sem, forcing each metavariable, and projects each `Sem` constructor to its `Value` counterpart:

| Sem | Value |
|---|---|
| `Lit(direct)` | `direct` |
| `TypeUniv` | `Value.Type` |
| `Struct(fqn, fields)` | `Value.Structure(fields.mapValues(quote), …)` (recursively quote each field) |
| `Lam(_, _, _)` | error if encountered after zonking — runtime functions aren't supposed to appear in monomorphized types. (If they're needed for some reason, we'd extend `Value` separately, but Eliot's `Value` model is "no lambdas in types after monomorphization", so this should never happen.) |
| `Neut(_, _)` | `None` — there's an unresolved meta or free param, that's a "couldn't fully infer" error |

### 2.5 The unifier (`eval2/util/Unifier.scala`)

```scala
object Unifier {
  def unify(s1: Sem, s2: Sem): EvalIO[Unit]
}
```

After **forcing both sides** (chasing meta solutions to head normal form), case-split:

| Left | Right | Action |
|---|---|---|
| `Lit(v1)` | `Lit(v2)` | succeed iff `v1 == v2`, else error |
| `TypeUniv` | `TypeUniv` | succeed |
| `Struct(t1, f1)` | `Struct(t2, f2)` | check `t1 == t2` and key sets match; recursively unify fields |
| `Lam(_, dom1, c1)` | `Lam(_, dom2, c2)` | unify domains; pick a fresh free `Param`; apply both closures to it; recursively unify results |
| `Neut(h1, sp1)` | `Neut(h2, sp2)` (`h1 == h2`) | unify spines pointwise |
| `Neut(Meta(id), spine)` | other | **pattern unification**: if `spine` is a list of distinct `Neut(Param(_), [])`s, abstract `other` over those params and `solveMeta(id, λ params. other)`. Otherwise defer or fail (we don't support full higher-order unification). |
| other | `Neut(Meta(id), spine)` | symmetric |
| any | any (mismatched) | type error |

**Pattern unification** is the standard restricted form of higher-order unification (Miller). It's decidable, simple to implement, and covers ~95% of real-world cases. The rest fail with "couldn't infer" — which is the same outcome the current solver reaches in hard cases (it just defers forever).

**Occurs check** is part of `solveMeta`: walking `λ params. other` to ensure `id` doesn't appear in it. If it does, we have a recursive type, which is an error.

### 2.6 Why a separate package, not extending `eval`

Two reasons:

1. **Metavariables.** `ExpressionValue` doesn't have a meta constructor and shouldn't grow one — it would force every existing user of `eval` to handle a case that only the type checker cares about. eval2's `Sem` is a fresh hierarchy that bakes in metas from the start.
2. **Reduction posture.** The existing `Evaluator` is cautious — it doesn't always reduce, and it has special cases for what to evaluate eagerly vs. lazily (`Evaluator.applyTypeArgs` vs. `applyTypeArgsStripped`, etc). eval2 is the opposite: always reduce, always force, only stop at neutrals. The two postures don't compose well; better to have separate codepaths.

eval2 will share infrastructure with `eval` where convenient (the `Value.Direct` type, the `Types.bigIntType` constants, etc.) — those are stable types that both packages can depend on. What's *not* shared is the recursive evaluation logic.

## 3. The new monomorphize2 elaborator

### 3.1 Top-level structure

```scala
class MonomorphicTypeCheckProcessor extends ... {
  override protected def generateFromKeyAndFact(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[MonomorphicValue] =
    Elaborator.elaborate(key, resolvedValue).map { case (signature, calculatedArgs, runtime) =>
      MonomorphicValue(
        key.vfqn,
        key.specifiedTypeArguments,
        calculatedArgs,
        signature,
        runtime
      )
    }
}
```

All the work is in `Elaborator`. The processor file becomes ~30 lines.

### 3.2 Elaborator interface (`monomorphize2/typecheck/elab/Elaborator.scala`)

```scala
object Elaborator {
  type ElabIO[A] = StateT[CompilerIO, MetaState, A]

  /** Top-level entry point. Evaluates the signature, elaborates the runtime body
    * (if present) against it, zonks everything, and returns the final monomorphized
    * shape plus the calculated type arguments for `key.vfqn` itself. */
  def elaborate(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[(Value, Seq[Value], Option[Sourced[MonomorphicExpression.Expression]])]

  /** Bidirectional checking: ORE in scope must produce a value of the given expected type. */
  def check(env: Env, ore: Sourced[OperatorResolvedExpression], expected: Sem): ElabIO[ElabExpr]

  /** Bidirectional inference: synthesize the type from the ORE. */
  def infer(env: Env, ore: Sourced[OperatorResolvedExpression]): ElabIO[(ElabExpr, Sem)]
}
```

`ElabExpr` is an internal mirror of `MonomorphicExpression` whose type field is `Sem` instead of `Value`:

```scala
case class ElabExpr(expressionType: Sem, expression: ElabExpr.Expression)
object ElabExpr {
  sealed trait Expression
  case class IntegerLiteral(value: Sourced[BigInt]) extends Expression
  case class StringLiteral(value: Sourced[String]) extends Expression
  case class ParameterReference(name: Sourced[String]) extends Expression
  case class MonomorphicValueReference(vfqn: Sourced[ValueFQN], typeArgs: Seq[Sem]) extends Expression
  case class FunctionApplication(target: Sourced[ElabExpr], argument: Sourced[ElabExpr]) extends Expression
  case class FunctionLiteral(paramName: Sourced[String], paramType: Sem, body: Sourced[ElabExpr]) extends Expression
}
```

The structure mirrors `MonomorphicExpression` exactly. After elaboration, **zonking** walks the `ElabExpr`, forces every `Sem`, quotes to `Value`, and produces a real `MonomorphicExpression`.

### 3.3 Per-ORE-case rules

**Literals** (`infer` produces concrete types; `check` defers to infer + unify):

```scala
infer IntegerLiteral(v)  → (IntegerLiteral(v), Lit(Direct(0, bigIntType)))   // type is BigInteger
infer StringLiteral(v)   → (StringLiteral(v),  Lit(Direct("", stringType)))  // type is String

check x against expected → (e, t) = infer x; unify(t, expected); return e
```

(For literals where the expected type is something like `Int` instead of `BigInteger`, unify reports the mismatch.)

**`ParameterReference(name)`** is always inferred:

```scala
infer ParameterReference(name) →
  let t = env.params(name) (or error)
  (ParameterReference(name), t)
```

**`ValueReference(vfqn, explicitTypeArgs)`** — the polytype instantiation site:

```scala
infer ValueReference(vfqn, explicitTypeArgs) →
  let resolved   = getFactOrAbort(OperatorResolvedValue.Key(vfqn))
  let sigSem     = Evaluator2.eval(Env.empty, resolved.typeStack.signature)

  // Apply explicit type arguments first.
  let (afterExplicit, usedExplicitArgs) = foldM (sigSem, []) (explicitTypeArgs) { (acc, ta) =>
    let taSem = Evaluator2.eval(env, ta)
    let next  = Evaluator2.apply(acc, taSem)   // this peels one Lam from the polytype
    return (next, usedExplicitArgs :+ taSem)
  }

  // Any remaining leading Lams in the polytype are inferred via fresh metas.
  let mut current = afterExplicit
  let mut metaArgs = []
  while (force(current) is Lam(_, dom, _)) {
    let m = freshMeta("typeArg", dom)
    metaArgs :+= Neut(Meta(m), [])
    current = Evaluator2.apply(current, Neut(Meta(m), []))
  }

  let allArgs = usedExplicitArgs ++ metaArgs
  return (MonomorphicValueReference(vfqn, allArgs), current)
```

This is the *only* place polytype instantiation happens. There is no separate "instantiation rule" in the unifier — by the time the unifier sees a value reference's type, it's already been fully instantiated.

The `metaArgs` here are what eventually become `MonomorphicValue.calculatedTypeArguments` for *referenced* values, after zonking. The current `Seq.empty` TODO finally gets filled.

Explicit type-arg arity check: if `explicitTypeArgs.length` exceeds the number of leading Lams in the signature, report `Too many type arguments`. The current TODO + manual count goes away — eval2 tells you naturally when you've run out of Lams.

**`FunctionApplication(target, arg)`** is inferred (synthesizes its own type):

```scala
infer FunctionApplication(target, arg) →
  let (targetExpr, targetType) = infer target
  let funType = forceUntilFunction(targetType)
    where forceUntilFunction repeatedly forces and instantiates leading Lams of the
    polytype until the result is `Sem.Struct(functionDataTypeFQN, ...)` (or fails)
  let (paramType, returnType) = (funType.fields("A"), funType.fields("B"))
  let argExpr = check(env, arg, paramType)
  return (FA(targetExpr, argExpr), returnType)
```

The call to `forceUntilFunction` is the eval2 way of "instantiate any leftover polytype layers before unifying". In practice it's almost always a no-op because `infer ValueReference` already instantiated everything.

**`FunctionLiteral(paramName, paramTypeOpt, body)`** is bidirectional — *check* is the natural mode (parameter type comes from the expected type), *infer* needs an annotation:

```scala
check FunctionLiteral(paramName, paramTypeOpt, body) against expected →
  let funType = forceUntilFunction(expected)
  let expectedParamType = funType.fields("A")
  let expectedReturnType = funType.fields("B")
  let paramType = match paramTypeOpt {
    case Some(pt) => let p = Evaluator2.eval(env, pt.signature); unify(p, expectedParamType); p
    case None     => expectedParamType
  }
  let env'  = env.extend(paramName, paramType)
  let bodyExpr = check(env', body, expectedReturnType)
  return (FunctionLiteral(paramName, paramType, bodyExpr), expected)

infer FunctionLiteral(paramName, Some(pt), body) →
  let paramType = Evaluator2.eval(env, pt.signature)
  let env'  = env.extend(paramName, paramType)
  let (bodyExpr, bodyType) = infer(env', body)
  let funType = Sem.Struct(functionDataTypeFQN, Map("A" -> paramType, "B" -> bodyType, "$typeName" -> ...))
  return (FunctionLiteral(paramName, paramType, bodyExpr), funType)

infer FunctionLiteral(_, None, _) →
  error "Cannot infer type of unannotated lambda; provide a type annotation or use it in a checked context"
```

Note that the *check* mode is exactly what handles the failing-test scenario `def f(s: String): String = id(s)` cleanly: the body's expected type is the function's signature, the FL's parameter type comes from there, and so on down the tree.

### 3.4 Elaboration top level

```scala
def elaborate(key, resolvedValue) = StateT.run(MetaState.empty) {
  for {
    // 1. Eval the signature in empty env.
    sigSem      <- Evaluator2.eval(Env.empty, resolvedValue.typeStack.signature)

    // 2. Apply the explicit type arguments from the key (these instantiate the
    //    OUTER value's polytype, the same way `key.specifiedTypeArguments` does today).
    appliedSig  <- key.specifiedTypeArguments.foldLeftM(sigSem) { (acc, arg) =>
                     Evaluator2.eval(Env.empty, arg).flatMap(Evaluator2.apply(acc, _))
                   }

    // 3. Elaborate the runtime body against the applied signature, if present.
    elabRuntime <- resolvedValue.runtime.traverse { body =>
                     Elaborator.check(Env.empty, body, appliedSig)
                   }

    // 4. Compute the calculatedTypeArguments — this is whatever fresh metas were
    //    introduced for the OUTER value's own type parameters that weren't covered
    //    by key.specifiedTypeArguments. (Often empty unless we're elaborating a
    //    generic def with no explicit type args, e.g. for testing.)
    calcArgs    <- collectOuterCalculatedArgs(sigSem, key.specifiedTypeArguments.length)

    // 5. Assert all metas have been solved.
    _           <- assertAllMetasSolved

    // 6. Zonk: walk ElabExpr → MonomorphicExpression, quote each Sem to Value.
    finalSig    <- zonkValue(appliedSig)
    finalCalc   <- calcArgs.traverse(zonkValue)
    finalBody   <- elabRuntime.traverse(zonkExpr)
  } yield (finalSig, finalCalc, finalBody)
}
```

### 3.5 Zonking

```scala
object Zonk {
  def zonkValue(sem: Sem): EvalIO[Value]
  def zonkExpr(elab: ElabExpr): EvalIO[MonomorphicExpression]
}
```

`zonkValue` is just `Quoter.quote` with a `compilerAbort` if the result is `None`.

`zonkExpr` walks `ElabExpr` and converts each level:
- `expressionType: Sem` → `Value` via `zonkValue`.
- `MonomorphicValueReference.typeArgs: Seq[Sem]` → `Seq[Value]` via `zonkValue` per element.
- Children recursively zonked.

**Unsolved metas at zonk time** are a hard error (`compilerAbort`). They mean elaboration finished without enough information to pin down a type. The error should print which meta is unsolved and where it was introduced (its `name`).

## 4. Output

`MonomorphicValue` and `MonomorphicExpression` keep their current shape, but with two improvements:

- **`MonomorphicValue.calculatedTypeArguments`** is no longer `Seq.empty` — it's the inferred type arguments for `key.vfqn`'s own type parameters, computed as the solutions to the metas introduced when we eval'd its signature without enough explicit type args.
- **`MonomorphicExpression.MonomorphicValueReference.typeArgs`** is no longer `Seq.empty` — it's the full set of type arguments (explicit + inferred) for the referenced value at this call site. This matches the original design intent and makes the downstream `uncurry` and `used` phases more useful.

Downstream consumers (`uncurry.processor.MonomorphicUncurryingProcessor`, `used.UsedNamesProcessor`) will see the same `MonomorphicValue` shape they see today, plus type-args fields that are actually populated. That should be a strict improvement, not a regression.

## 5. What stays vs. what changes

### Stays untouched
- `OperatorResolvedExpression` and the entire `operator` package.
- `OperatorResolvedValue` and earlier phases (`token`, `ast`, `core`, `module`, `resolve`, `matchdesugar`, `operator`, `implementation`).
- The existing `eval` package — `Value`, `ExpressionValue`, `Evaluator`, etc. eval2 is parallel.
- `MonomorphicValue` and `MonomorphicExpression` shapes (only `calculatedTypeArguments` semantics improve).
- Downstream consumers (`uncurry`, `used`).
- `LangPlugin` wiring — the production path is still `monomorphize` (the old package), which is unchanged.
- `monomorphize2/fact/` — `MonomorphicValue.scala`, `MonomorphicExpression.scala` are unchanged.

### Deleted
- `monomorphize2/typecheck/constraints/` — entire directory: `Constraints.scala`, `ConstraintExtract.scala`, `ShortUniqueIdentifiers.scala`, `TypeCheckState.scala`.
- `monomorphize2/typecheck/solution/` — entire directory: `ConstraintSolver.scala`, `Solution.scala`, `SolverState.scala`.

### New
- `eval2/fact/Sem.scala`, `eval2/util/Evaluator2.scala`, `eval2/util/Quoter.scala`, `eval2/util/Unifier.scala`, `eval2/util/MetaState.scala`.
- `monomorphize2/typecheck/elab/Elaborator.scala`, `monomorphize2/typecheck/elab/ElabExpr.scala`, `monomorphize2/typecheck/elab/Zonk.scala`.

### Rewritten
- `monomorphize2/processor/MonomorphicTypeCheckProcessor.scala` — becomes ~30 lines, just calls `Elaborator.elaborate`.

### Estimated size delta
Roughly:
- ~700 lines new in `eval2/`.
- ~500 lines new in `monomorphize2/typecheck/elab/`.
- ~30 lines in the rewritten processor.
- ~900 lines deleted (constraint extract, solver, processor walk, supporting state).

Net: similar total size, dramatically simpler architecture. The new code has zero replay coupling and one well-defined responsibility per file.

## 6. Migration strategy

### Phase 1 — eval2 in isolation
- Implement `Sem`, `Evaluator2`, `Quoter`, `MetaState` (no unifier yet, no metavariables yet).
- Write unit tests against simple ORE expressions: literals, parameter refs, FAs, FLs, top-level value lookups.
- Verify the evaluator handles recursive defs without looping (via the in-progress set).
- Verify `quote` round-trips simple `Value`s correctly.

### Phase 2 — metavariables and unifier
- Add `Head.Meta` to `Sem`.
- Implement `freshMeta`, `solveMeta`, `force`.
- Implement `Unifier.unify` with pattern unification for the meta-vs-other case.
- Unit tests: unify `Lit == Lit`, `Struct == Struct`, `Lam == Lam`, `?m == concrete`, `?m(x, y) == f(x, y)` (pattern), `?m == Lit` (trivial), occurs check, etc.

### Phase 3 — elaborator
- Create `monomorphize2/typecheck/elab/`. Implement `ElabExpr`, `Elaborator.check` / `Elaborator.infer` per the rules in §3.3, `Zonk`.
- **Don't wire it in yet.** Add a separate test class that bypasses the existing constraint pipeline and goes through the elaborator directly. Iterate against the existing test suite; expect surprises with abilities, recursive types, edge cases.

### Phase 4 — switch over
- Replace `MonomorphicTypeCheckProcessor`'s body with `Elaborator.elaborate`.
- Run the full test suite. Some tests will surface NbE-vs-constraint differences (e.g., NbE catches some errors earlier, with different messages). Update test expectations accordingly.
- Delete the old constraint code.

### Phase 5 — cleanup
- Delete the now-unused `monomorphize2/typecheck/constraints/` and `monomorphize2/typecheck/solution/` directories.
- Update `SKILL.md` from the constraint-based model to the NbE model. Probably half its current content disappears.
- Decide whether `eval2` should eventually replace `eval` (likely yes, but not part of this migration).

## 7. Risks and open questions

### 7.1 Recursive top-level definitions
The evaluator needs to handle `def f: T = f` and mutual recursion without looping. Mitigation: the `inProgress: Set[ValueFQN]` guard in `MetaState`. On re-entry, return `Neut(Ref(vfqn), [])` — the reference becomes blocked, the type checker still sees `f` as having type `T` (from its signature), and the runtime body doesn't get inlined into itself.

This is the same trick the existing `monomorphize` package uses; just spelled out at a different layer.

### 7.2 Phantom type parameters
If a value's signature has type parameters that don't appear in either the runtime type or any explicit type argument, the metas for them won't be touched by anything and will be unsolved at zonk time. Options:
- Error: "Cannot infer type argument for phantom parameter X" — same behaviour as the existing solver.
- Default: leave unsolved, fill with `Value.Type` or some sentinel and warn.

The existing tests have one such case (`def f[I: BigInteger]: String` where `I` is phantom). Match the existing behaviour: explicit type args required for phantom params, error otherwise.

### 7.3 Pattern unification limits
Pattern unification only handles `?m(x, y, …) == other` when the spine consists of distinct bound parameters. Real programs occasionally need more (`?m(f(x)) == g(x)` style). Practical experience from smalltt / Lean / Idris: pattern unification covers ~95% of cases naturally, the rest can be deferred to a later "post-pass" attempt or reported as "couldn't infer".

For initial migration, fail fast on non-pattern cases; revisit if real programs need it.

### 7.4 Ability resolution timing
Today, ability resolution happens in a separate phase (`implementation`). With NbE, we *could* resolve abilities during elaboration (since we know the concrete types when an ability function is referenced). But that's a bigger change and orthogonal to this migration. Initial migration: produce `MonomorphicValueReference` for ability calls with the inferred types and let the existing implementation phase deal with them later.

### 7.5 Performance of always-on reduction
NbE evaluates aggressively. For a deeply nested or recursive type, that could be slow. Mitigations available later if needed:
- Memoize closure applications.
- Mark some defs as `opaque` (don't reduce).
- Lazy `Sem` constructors where useful.

For initial migration, just do the eager thing and measure.

### 7.6 Source positions for error messages
ORE nodes carry `Sourced` positions. The elaborator should plumb these through to the `Sem` level (or at least keep them in `ElabExpr` for the build phase). Errors should report positions matching the existing tests' expectations (e.g., `"Type mismatch." at "id"`).

This is mostly a matter of being disciplined: pass `Sourced[ORE]` to `check` / `infer`, not bare ORE, and use the source position when reporting unification failures.

### 7.7 The `compilerAbort` vs. `compileError` distinction
The existing pipeline uses both (abort = die now, error = collect and continue). The elaborator should follow the same convention: `compileError` for type mismatches that downstream nodes can still be checked around, `compilerAbort` for situations where there's no point continuing (couldn't find a referenced value, couldn't quote a fully-unsolved type, etc.).

### 7.8 Interaction with `MonomorphicValue.Key` and the fact cache
The fact cache distinguishes specializations by `(vfqn, specifiedTypeArguments)`. Two calls to `id` with different inferred type arguments produce different `MonomorphicValueReference`s, but they *look up* the same `MonomorphicValue.Key(id, [])` since neither specifies type args explicitly.

This is fine — the cached `MonomorphicValue` for `id` represents `id`'s polytype itself, and each call site holds its own instantiated `typeArgs` in the `MonomorphicValueReference`. The downstream `uncurry` / `used` phases consume the call-site type args, not the polytype's. This matches the current intent.

If the downstream phases actually want monomorphized specializations (one `MonomorphicValue` per concrete instantiation), that's a separate change to the keying scheme — not something this migration needs to do.

## 8. Why this is the right move

This migration removes the most fragile coupling in the current code (the `ShortUniqueIdentifiers` replay), eliminates the constraint-extract / solve / replay-substitute pipeline entirely, and replaces it with a single bidirectional walk. The same code handles polytype instantiation (it's just function application now), type-level computation (it's just evaluation now), and elaboration (the walk produces the output directly).

It also positions Eliot for the dependent-type direction the language is heading toward. Lean, Idris 2, Agda, and smalltt all use NbE-based elaborators because that's the only known approach that handles type-level computation cleanly. The current hybrid is fine for HM-level programs but will get harder and harder to extend as the type system grows.

The cost is roughly proportional to the win: one new package, one rewritten elaborator, ~900 lines deleted, ~1200 lines added, ~5 phases of careful work. For a type-checker rewrite at this scale, that's cheap.
