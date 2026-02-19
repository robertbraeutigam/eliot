# Plan: Thread Ability Constraints from GenericParameter to AbilityCheckProcessor

## Context

`GenericParameter.abilityConstraints` (AST stage) allows syntax like `def foo[A ~ Show[A]](x: A): A = show(x)`. Currently, calling an ability with a generic type parameter causes an immediate error in `AbilityCheckProcessor`: "Cannot prove ability 'Show' is available for given type." The constraint information from the AST is discarded in `CoreProcessor` and never reaches the ability checker.

The goal is to thread the constraint data through the pipeline so `AbilityCheckProcessor` can prove an ability call is valid when the type argument is a generic parameter covered by a constraint, and then resolve it to a concrete implementation in the monomorphizer when actual types are known.

## Design Decisions

- **Don't touch TypeCheckedValue or the symbolic type checker** — constraints live below and above that stage.
- **Thread constraints through NamedValue** (additive field with default empty) to avoid complex infrastructure for bridging back to SourceAST later. This keeps the flow linear: AST → Core → Resolve → AbilityCheck.
- **Constraints in ResolvedValue** (also additive field with default empty) — `AbilityCheckProcessor` reads this alongside `TypeCheckedValue`. The symbolic stage ignores the new field entirely.
- **Lazy resolution in monomorphizer** — ability refs that passed through AbilityCheckProcessor (because covered by constraints) are resolved to concrete implementations when monomorphizing with concrete type arguments.

## Critical Files

| File | Change |
|------|--------|
| `lang/src/.../core/fact/NamedValue.scala` | Add `paramConstraints: Map[String, Seq[NamedValue.CoreAbilityConstraint]]` + nested case class |
| `lang/src/.../core/processor/CoreProcessor.scala` | Populate `paramConstraints` from `GenericParameter.abilityConstraints` |
| `lang/src/.../resolve/fact/ResolvedValue.scala` | Add `paramConstraints: Map[String, Seq[ResolvedValue.ResolvedAbilityConstraint]]` + nested case class |
| `lang/src/.../resolve/processor/ValueResolver.scala` | Resolve constraint ability names and type args; add to `ResolvedValue` |
| `lang/src/.../abilitycheck/AbilityCheckProcessor.scala` | Read `ResolvedValue` for constraints; allow covered ability calls |
| `lang/src/.../monomorphize/processor/MonomorphicTypeCheckProcessor.scala` | Resolve ability refs via `AbilityImplementation.Key` with concrete type args |
| `lang/test/.../abilitycheck/AbilityCheckProcessorTest.scala` | Add passing test for constrained generic |

## Reusable Utilities

- `ValueResolver.resolveAbilityName(name: Sourced[String]): ScopedIO[AbilityFQN]` — already handles ability name → AbilityFQN lookup; reuse for constraint resolution
- `ValueResolver.resolveExpression(expr: CoreExpression, runtime: Boolean): ScopedIO[Expression]` — already resolves core expressions; reuse for constraint typeArgs (type params resolve to `ParameterReference`)
- `AbilityCheckProcessor.extractAbilityTypeArgs` / `containsParameterRef` / `isAbilityRef` — reuse; only add a constraint-check branch before the current error
- `AbilityCheckProcessor.resolveAbilityRef` → `AbilityImplementation.Key` lookup — replicate logic in monomorphizer for the constrained case
- `TypeEvaluator.evaluateWithSubstitution` — already used in monomorphizer to evaluate type args to `Value`; reuse to get concrete type args for `AbilityImplementation.Key`

## Phase 1 — Core data structures (compile + tests pass)

**`NamedValue.scala`**: Add companion with nested type and new field:
```scala
case class NamedValue(
    qualifiedName: Sourced[QualifiedName],
    runtime: Option[Expression],
    typeStack: TypeStack[Expression],
    paramConstraints: Map[String, Seq[NamedValue.CoreAbilityConstraint]] = Map.empty
)
object NamedValue {
  case class CoreAbilityConstraint(abilityName: Sourced[String], typeArgs: Seq[Expression])
  // Keep existing signatureEquality — it must NOT compare paramConstraints
}
```

**`CoreProcessor.scala`** in `transformFunction`, after building `typeStack`:
```scala
val constraints = function.genericParameters.map { gp =>
  gp.name.value -> gp.abilityConstraints.map { c =>
    NamedValue.CoreAbilityConstraint(c.abilityName, c.typeParameters.map(toTypeExpression(_).value))
  }
}.filter(_._2.nonEmpty).toMap
NamedValue(convertQualifiedName(function.name), curriedValue.map(_.value), typeStack, constraints)
```

Note: `toTypeExpression` already converts `TypeReference` to `core.fact.Expression`. A generic param `A` in the constraint's `typeParameters` produces `NamedValueReference(QualifiedName("A", Qualifier.Type))`, which the resolver will correctly turn into `ParameterReference("A")`.

**Verification**: `mill lang.test` — all existing tests pass.

## Phase 2 — Thread through resolve stage (compile + tests pass)

**`ResolvedValue.scala`**: Add companion with nested type and new field (default empty):
```scala
case class ResolvedValue(
    vfqn: ValueFQN,
    name: Sourced[QualifiedName],
    runtime: Option[Sourced[Expression]],
    typeStack: Sourced[TypeStack[Expression]],
    paramConstraints: Map[String, Seq[ResolvedValue.ResolvedAbilityConstraint]] = Map.empty
)
object ResolvedValue {
  case class ResolvedAbilityConstraint(abilityFQN: AbilityFQN, typeArgs: Seq[Expression])
}
```

**`ValueResolver.scala`** in `generateFromKeyAndFact`, inside `resolveProgram`:
```scala
resolvedConstraints <- resolveParamConstraints(namedValue.paramConstraints)
```
And update the `yield` to include `resolvedConstraints`.

Add private helper:
```scala
private def resolveParamConstraints(
    paramConstraints: Map[String, Seq[NamedValue.CoreAbilityConstraint]]
): ScopedIO[Map[String, Seq[ResolvedValue.ResolvedAbilityConstraint]]] =
  paramConstraints.toSeq.traverse { case (paramName, constraints) =>
    constraints.traverse { c =>
      for {
        abilityFQN   <- resolveAbilityName(c.abilityName)
        resolvedArgs <- c.typeArgs.traverse(resolveExpression(_, false))
      } yield ResolvedValue.ResolvedAbilityConstraint(abilityFQN, resolvedArgs)
    }.map(paramName -> _)
  }.map(_.toMap)
```

`resolveAbilityName` is already in `ValueResolver` and works via `ScopedIO`. Generic type params in constraint `typeArgs` (e.g., `NamedValueReference(QualifiedName("A", Qualifier.Type))`) resolve to `Expression.ParameterReference(Sourced("A"))` because `A` is in `genericParams` scope.

**Verification**: `mill lang.test` — all existing tests pass (SymbolicTypeCheckProcessor ignores the new field).

## Phase 3 — Allow constrained ability calls in AbilityCheckProcessor (compile + tests pass + new tests)

**`AbilityCheckProcessor.scala`**:

1. In `generateFromKeyAndFact`, optionally read `ResolvedValue` for the current vfqn:
```scala
resolvedValue <- getFact(ResolvedValue.Key(fact.vfqn))
val paramConstraints = resolvedValue.map(_.paramConstraints).getOrElse(Map.empty)
```
Use `getFact` (not `getFactOrAbort`) because synthesized values from `AbilityImplementationProcessor.handleDefaultImplementation` have no `ResolvedValue`.

2. Pass `paramConstraints` down through `resolveAbilityRefs` and `resolveAbilityRef`.

3. In `resolveAbilityRef`, before the error on `containsParameterRef`:
```scala
_ <- typeArgExprs.traverse_ { arg =>
       if (containsParameterRef(arg))
         if (isProvedByConstraint(vfqn.value, typeArgExprs, paramConstraints))
           ().pure[CompilerIO]  // constraint proves this ability
         else
           compilerAbort[Unit](vfqn.as("Cannot prove ability ..."))
       else ().pure[CompilerIO]
     }
```

4. When all type args are concrete AND constraint-proved: skip `AbilityImplementation.Key` lookup and **keep the ability ref as-is** (return original `vfqn.value` unchanged). The monomorphizer will resolve it.

5. Add private helper:
```scala
private def isProvedByConstraint(
    vfqn: ValueFQN,
    typeArgExprs: Seq[ExpressionValue],
    paramConstraints: Map[String, Seq[ResolvedValue.ResolvedAbilityConstraint]]
): Boolean = {
  val calledAbilityFQN = AbilityFQN(vfqn.moduleName,
    vfqn.name.qualifier.asInstanceOf[CoreQualifier.Ability].name)
  paramConstraints.values.flatten.exists { c =>
    c.abilityFQN == calledAbilityFQN &&
    c.typeArgs.length == typeArgExprs.length &&
    c.typeArgs.zip(typeArgExprs).forall {
      case (ResolveExpr.ParameterReference(nameSrc), ExpressionValue.ParameterReference(evName, _)) =>
        nameSrc.value == evName
      case _ => false
    }
  }
}
```

**`AbilityCheckProcessorTest.scala`**: Add test:
```scala
it should "succeed when calling ability with generic type parameter covered by constraint" in {
  runEngineForErrors(
    "ability Show[A] { def show(x: A): A }\ndef foo[A ~ Show[A]](x: A): A = show(x)"
  ).asserting(_ shouldBe Seq.empty)
}
```

**Verification**: `mill lang.test` — all tests pass including new one.

## Phase 4 — Resolve ability refs in monomorphizer (compile + tests pass)

**`MonomorphicTypeCheckProcessor.scala`**:

In `transformValueReference`, after inferring `typeArgs`, check if the reference is an ability ref:
```scala
finalVfqn <- if (isAbilityRef(vfqn.value) && typeArgs.nonEmpty)
               getFactOrAbort(AbilityImplementation.Key(vfqn.value, typeArgs))
                 .map(impl => vfqn.as(impl.implementationFQN))
             else
               vfqn.pure[CompilerIO]
finalTypeChecked <- getFactOrAbort(AbilityCheckedValue.Key(finalVfqn.value))
finalTypeParams   = TypeEvaluator.extractTypeParams(finalTypeChecked.signature)
finalTypeArgs     = Seq.empty[Value]  // impl has no type params
```
Produce `MonomorphicValueReference(finalVfqn, finalTypeArgs)`.

Helper (replicate from AbilityCheckProcessor):
```scala
private def isAbilityRef(vfqn: ValueFQN): Boolean =
  vfqn.name.qualifier.isInstanceOf[CoreQualifier.Ability]
```

Add a full-pipeline test (or update example) to verify `foo[A ~ Show[A]]` works end-to-end.

**Verification**: `mill lang.test && mill jvm.test && mill examples.run jvm exe-jar examples/src/ -m HelloWorld`

## Key Invariants

- **Symbolic type checker untouched** — reads `ResolvedValue` but new `paramConstraints` field is ignored.
- **`NamedValue.signatureEquality`** must NOT include `paramConstraints` (constraints don't affect the type signature, only implementation search).
- **`getFact` (not `getFactOrAbort`)** for `ResolvedValue` in AbilityCheckProcessor — synthesized ability implementations have no `ResolvedValue`.
- **Constraints only matched on ParameterReference** for now — concrete type args in constraints are not matched (natural extension later).
- After Phase 3, ability refs for constrained generics remain in `AbilityCheckedValue.runtime` as-is; Phase 4 ensures the monomorphizer resolves them correctly.
