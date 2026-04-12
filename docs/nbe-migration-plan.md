# Plan: Build `monomorphize3` — a standalone NbE type checker

## Strategy

Build NbE as a **completely independent** `monomorphize3` package alongside the existing `monomorphize` and `monomorphize2` packages. Neither is touched, deleted, or compared against at runtime. `monomorphize3` has its own semantic domain, evaluator, native-value processors, ground value type, and output fact shapes. The only dependency on existing code is consuming `OperatorResolvedExpression` / `OperatorResolvedValue` / `TypeStack` as read-only input.

Each step compiles clean, leaves all existing test suites green, and adds its own tests for the new code it introduces. There is no "swap" step — `monomorphize3` lives alongside the others indefinitely.

## Key design decisions

1. **No dependency on `eval/` at runtime.** `monomorphize3` has its own semantic domain, evaluator, native-value fact and processors, ground `Value` type, and output fact shapes. The only things copied from `eval/` are the *patterns* of how native system values are injected via `CompilerFact` processors, and the well-known `ValueFQN`s for built-ins like `Function`, `Type`, `BigInteger`, `String`.

2. **The output fact shape is free to change.** Since no downstream consumer (`uncurry`, `used`, JVM backend) depends on `monomorphize3`, the result type can be shaped for clarity and NbE fidelity.

3. **The evaluator is designed for NbE from day one.** De Bruijn levels in the env, Scala-closure closures, lazy top-def thunks, `IntMap` meta store, spine-based neutrals. No ORE rewriting ever — ORE is read once into `SemValue` and then forgotten.

## Target file layout

```
lang/src/com/vanillasource/eliot/eliotc/monomorphize3/
├── fact/
│   ├── GroundValue.scala              (own Value replacement: Direct, Structure, Type)
│   ├── Monomorphic3Value.scala        (output fact)
│   ├── Monomorphic3Expression.scala   (output expression ADT)
│   └── NativeBinding.scala            (CompilerFact: vfqn → SemValue for natives)
├── domain/
│   ├── SemValue.scala                 (VType, VConst, VLam, VPi, VNative,
│   │                                    VNeutral, VTopDef, VMeta, Spine, Closure)
│   ├── Env.scala                      (Vector[SemValue] + level; de Bruijn levels)
│   └── MetaStore.scala                (IntMap[MetaEntry]; fresh/solve/lookup)
├── eval/
│   ├── Evaluator.scala                (eval, apply, force, quote)
│   └── Quoter.scala                   (SemValue → GroundValue projection)
├── processor/
│   ├── SystemNativesProcessor.scala   (built-in Function/Type/etc. → NativeBinding)
│   ├── DataTypeNativesProcessor.scala (data declarations → NativeBinding)
│   ├── UserValueNativesProcessor.scala (user def → NativeBinding, cached via fact system)
│   └── Monomorphic3Processor.scala    (the monomorphize3 entry point)
├── unify/
│   └── Unifier.scala                  (pattern unification + postponement)
└── check/
    ├── Checker.scala                  (bidirectional check / infer)
    ├── CheckState.scala               (env, meta store, pending, errors)
    └── TypeStackLoop.scala            (top-down walk over OperatorResolvedValue.typeStack)

lang/test/src/com/vanillasource/eliot/eliotc/monomorphize3/
├── eval/        NbeEvaluatorTest, NbeQuoterTest
├── unify/       NbeUnifierTest
├── check/       NbeCheckerTest
└── processor/   Monomorphic3ProcessorTest, Monomorphic3TypeCheckTest
```

## How NbE works in this design

### The semantic domain

The core idea: ORE (syntax) is evaluated into `SemValue` (semantics) via closures. Type equality becomes structural equality of normal forms. Polytype instantiation becomes function application at the value level. There is no constraint set, no worklist, no separate solving phase — unification happens locally as the checker walks the ORE.

**SemValue variants:**
- `VType` — the type of all types.
- `VConst(ground: GroundValue)` — non-function ground values: concrete types like `BigInteger`, `String`, `Person`, and concrete runtime values like `42`, `"hello"`.
- `VLam(name: String, closure: SemValue => SemValue)` — a runtime lambda closure. Produced by the Checker when checking a `FunctionLiteral` against a function type (`VPi`).
- `VPi(domain: SemValue, codomain: SemValue => SemValue)` — a function type (dependent or non-dependent). ALL function types are `VPi`, including `Function[Int, String]`.
- `VNative(paramType: SemValue, fire: GroundValue => SemValue)` — primitive that fires on concrete arguments.
- `VTopDef(fqn: ValueFQN, cached: Lazy[SemValue], spine: Spine)` — lazy top-level definition.
- `VMeta(id: MetaId, spine: Spine, expected: SemValue)` — unsolved metavariable.
- `VNeutral(head: NeutralHead, spine: Spine, tpe: SemValue)` — stuck application with a variable head.

**VLam vs VPi — no mode flag needed.** Both are closures internally (`SemValue => SemValue`), but they serve different roles. The distinction falls out naturally from position — the evaluator never decides:

- **The Checker decides.** When the Checker encounters `FunctionLiteral` being checked against `VType` (a type expression), it constructs `VPi(domain, codomainClosure)`. When it encounters `FunctionLiteral` being checked against a `VPi(domain, codomain)` (a runtime lambda), it constructs `VLam`. The expected type tells the Checker which to produce.
- **The native `Function` constructor decides.** `Function[A, B]` is evaluated via `apply(apply(nativeFunction, A), B)`. The native fires and directly produces `VPi(A, _ => B)`, not `VConst(Structure(...))`. This makes all function types — dependent and non-dependent — uniformly `VPi`.
- **The evaluator doesn't handle `FunctionLiteral` at all.** All ORE walks go through the Checker's `check`/`infer`, which handles `FunctionLiteral` directly based on the expected type. The evaluator only handles literals, parameter refs, value refs, and application — cases where the VLam/VPi question doesn't arise.

This means `VConst` never represents function types. Function types are always `VPi`. `VConst` is exclusively for non-function ground values (concrete data types, literals, structures).

**Env** is `Vector[SemValue]` indexed by de Bruijn level. Variable lookup is by level, not by name — no `Map[String, SemValue]` in the hot path.

**Closures** are native Scala functions. When the Checker produces a `VLam`, it captures the current env and body ORE in a Scala closure: `VLam(pn.value, arg => ...)`. Same for `VPi`. No ORE substitution ever happens — variable resolution is by env lookup at evaluation time.

**Spine** is a reversed cons list for O(1) append: `sealed trait Spine; case object SNil; case class SApp(tail: Spine, head: SemValue)`.

### How computation runs

Type-level computation (like `1+1` in a type position) runs via `apply`. When `eval` encounters `FunctionApplication(target, arg)`, it calls `apply(eval(target), eval(arg))`. For primitives, `apply(VNative(_, fire), VConst(ground))` invokes the Scala function `fire(ground)` — that's real JVM code executing `BigInt.+` or `String.substring`. For user-defined functions, `apply(VLam(_, closure), arg)` invokes the closure, which re-enters `eval` with the extended env.

When an argument is still symbolic (a meta or neutral), the application stays stuck as a `VNeutral`/`VMeta` with the argument appended to the spine. When unification later solves the meta, `force` re-triggers the application.

### Bidirectional checking

The checker has two modes:
- `check(tm: ORE, expected: SemValue) -> Monomorphic3Expression` — checks a term against a known type.
- `infer(tm: ORE) -> (Monomorphic3Expression, SemValue)` — infers a term's type.

At each ORE node, the checker either knows the expected type (from a parent check) or infers it, producing the `Monomorphic3Expression` as it descends. No side-table, no replay pass — the typed tree is built in one walk.

**No shape assumptions on SemValue.** The checker never peeks at a SemValue to count leading VPi binders, walk leading VLam closures, or otherwise assume a specific structure. Instead:
- **Explicit type args** at a `ValueReference` are applied via `apply(sig, eval(typeArg))` one by one. If `sig` is a VLam, `apply` beta-reduces. If it's a VTopDef or VNeutral, `apply` appends to the spine. No shape assumption needed.
- **Implicit type args** are NOT eagerly allocated at the `ValueReference` site. Instead, instantiation is driven lazily by `FunctionApplication`: when the checker infers `id(42)`, it infers `id`'s type (whatever SemValue shape it happens to be), then `infer(FunctionApplication)` forces the target type and unifies it against a fresh `VPi(?domain, ?codomain)`. The meta is created at the point of *use*, not by peeking at the referenced value's structure.

This means the checker never asks "how many type parameters does this value have?" — it applies what's given and lets unification figure out the rest.

### The type stack

Each value has a `TypeStack` of ORE levels. The checker walks them top-down:
1. Start at the topmost level; check against `VType` (implicit).
2. Evaluate the level to a `SemValue` — this becomes the expected type of the next level.
3. Continue until the signature level is reached.
4. Apply specified type arguments via `apply(sig, eval(typeArg))`.
5. Check the runtime body against the monomorphized signature.

This is "consume the stack, running each level against the level above, to get the type of the level below" — no assumptions about ORE shape, no lambda stripping, no structural pattern-matching on the levels.

---

## Step 0 — Baseline

**Goal.** Confirm the current tree compiles clean and all existing tests pass.

**Actions.**
- `./mill lang.compile` — clean.
- `./mill lang.test` — all pass.
- Record the current pass/fail state of `MonomorphicTypeCheckProcessorTest` + `MonomorphicTypeCheckTest` as the contract monomorphize3 must eventually match.

---

## Step 1 — Package skeleton and data shapes

**Goal.** Create the `monomorphize3` package tree with fact types, ground value type, and a stub processor that aborts. Must compile with no effect on existing tests.

**New files:**

- `fact/GroundValue.scala` — conceptual copy of `eval.fact.Value` renamed for independence:
  ```scala
  sealed trait GroundValue { def valueType: GroundValue }
  object GroundValue {
    case class Direct(value: Any, override val valueType: GroundValue) extends GroundValue
    case class Structure(fields: Map[String, GroundValue],
                         override val valueType: GroundValue) extends GroundValue
    case object Type extends GroundValue { override def valueType = this }
  }
  ```

- `fact/Monomorphic3Value.scala` — output fact. Drops `calculatedTypeArguments` (NbE folds concrete type args into the signature directly):
  ```scala
  case class Monomorphic3Value(
    vfqn: ValueFQN,
    specifiedTypeArguments: Seq[Sourced[OperatorResolvedExpression]],
    signature: GroundValue,
    runtime: Option[Sourced[Monomorphic3Expression.Expression]]
  ) extends CompilerFact
  ```

- `fact/Monomorphic3Expression.scala` — same shape as `monomorphize2.fact.MonomorphicExpression` but over `GroundValue`.

- `fact/NativeBinding.scala` — `CompilerFact: vfqn -> SemValue` for native built-in values.

- `domain/SemValue.scala` — empty `sealed trait SemValue` placeholder (real ADT in Step 2).

- `processor/Monomorphic3Processor.scala` — stub `TransformationProcessor` that aborts.

- Empty `Monomorphic3ProcessorTest.scala` scaffold with the `ProcessorTest` harness but no real assertions.

**Verification.** `./mill lang.compile` passes. Existing tests untouched.

---

## Step 2 — Semantic domain, evaluator, and native processors

**Goal.** Replace the `SemValue` placeholder with the full NbE domain, implement `eval`/`apply`/`force`/`quote`, and wire up the native-binding processors. At the end, `NativeBinding` facts exist for every well-known FQN and for user-defined values, and the evaluator can reduce closed ORE expressions to `SemValue`s.

**New files:**

- `domain/SemValue.scala` — the full ADT (VType, VConst, VLam, VPi, VNative, VTopDef, VMeta, VNeutral, NeutralHead, Spine).

- `domain/Env.scala` — `Env(bindings: Vector[SemValue], names: Vector[String])`. Lookup by de Bruijn level (index into `bindings`). Name-to-level resolution happens at Checker time, not inside Evaluator.

- `domain/MetaStore.scala` — `IntMap[Option[SemValue]]` backed. Allocation via next-free-int counter.

- `eval/Evaluator.scala`:
  - `eval(env, tm: ORE) -> SemValue` — synchronous, pure. Does NOT handle `FunctionLiteral` — that is the Checker's job (the Checker decides VLam vs VPi based on expected type). Cases: IntegerLiteral -> VConst, StringLiteral -> VConst, ParameterReference -> env.lookupByLevel, ValueReference -> fetched VTopDef, FunctionApplication -> apply(eval(t), eval(a)). If `FunctionLiteral` is encountered, it is an error (the Checker should have handled it).
  - `apply(f, x) -> SemValue` — VLam -> invoke closure, VPi -> error (applying a value to a type), VNative -> fire if VConst else stuck, VNeutral -> append to spine, VTopDef -> append to spine (unfolds only when forced), VMeta -> append to spine.
  - `force(v) -> SemValue` — walks solved metas and unfolds VTopDef when spine is fully concrete.

- `eval/Quoter.scala` — `quote(depth, SemValue) -> GroundValue`. Fails on VNeutral/VMeta/VLam (those should be resolved before quoting).

- `processor/SystemNativesProcessor.scala` — emits `NativeBinding` for `functionDataTypeFQN`, `typeFQN`, etc. Copies the shape of `eval.processor.SystemValueEvaluator`.

- `processor/DataTypeNativesProcessor.scala` — emits `NativeBinding` for data type/constructor FQNs. Copies shape of `eval.processor.DataTypeEvaluator`.

- `processor/UserValueNativesProcessor.scala` — `TransformationProcessor[OperatorResolvedValue.Key, NativeBinding.Key]`. Compiles each user-defined value's runtime body and type signature into a `SemValue` via the new evaluator. Simple `Set[ValueFQN]` recursion guard for now (replaced in Step 9).

**New tests:**

- `eval/NbeEvaluatorTest.scala`:
  - eval of IntegerLiteral(42) -> VConst(Direct(42, bigIntType))
  - eval of ValueReference(functionDataTypeFQN) applied to Int and String -> VPi(VConst(intType), _ => VConst(stringType))
  - De Bruijn level lookup with shadowing
  - Native partial application: Function(Int) produces a VNative waiting for second arg
  - eval of FunctionLiteral -> error (Checker handles FunctionLiteral, not evaluator)

- `eval/NbeQuoterTest.scala`:
  - quote(VConst(bigIntType)) === bigIntType as GroundValue
  - quote(Function[Int, String]) produces expected GroundValue.Structure
  - quote(VNeutral) fails with clear error

**Verification.** `./mill lang.compile`, new unit tests pass, existing tests untouched.

---

## Step 3 — Unifier

**Goal.** Add pattern unification on `SemValue`s with a postponement queue.

**New files:**

- `unify/Unifier.scala`:
  1. `force(l)`, `force(r)`.
  2. `VType` vs `VType` -> success.
  3. `VConst(g1)` vs `VConst(g2)` -> structural `GroundValue` equality.
  4. `VPi(d1, c1)` vs `VPi(d2, c2)` -> unify domains, extend env with fresh VVar at current depth, unify codomains applied to it.
  5. `VLam(_, c1)` vs `VLam(_, c2)` -> same trick as pi.
  6. Eta: `VLam(_, c)` vs other -> unify `c(fresh var)` against `apply(other, fresh var)`.
  7. `VNeutral(h1, sp1)` vs `VNeutral(h2, sp2)` with matching heads and spine lengths -> zip and unify.
  8. Pattern rule: `VMeta(?m, sp)` vs rhs where sp is distinct VVars, rhs doesn't mention ?m -> solve.
  9. Non-pattern fallback: postpone.
  10. Otherwise -> mismatch error with source position.
  - `drain()` — worklist loop until stable.

**New tests:**

- `unify/NbeUnifierTest.scala`:
  - VType vs VType
  - VConst(bigIntType) vs VConst(stringType) -> mismatch
  - VMeta(?a) vs VConst(bigIntType) -> solves
  - VPi with metas -> solves
  - Occurs check -> error
  - Eta: VLam wrapping apply equals the function itself
  - Late firing after meta solve

**Verification.** All NbE unit tests pass, existing tests untouched.

---

## Step 4 — Bidirectional checker and first end-to-end tests

**Goal.** Add `check`/`infer`, `TypeStackLoop`, and plug into `Monomorphic3Processor`. Port the non-generic subset of monomorphize2 tests.

**New files:**

- `check/CheckState.scala` — `CheckState(env, metaStore, pending, errors, nameLevels: Map[String, Int])`, `CheckIO[T] = StateT[CompilerIO, CheckState, T]`.

- `check/Checker.scala`:
  - `check(tm, expected) -> CheckIO[Monomorphic3Expression]`
  - `infer(tm) -> CheckIO[(Monomorphic3Expression, SemValue)]`
  - Non-generic cases: literals, ParameterReference, ValueReference (no typeArgs), FunctionApplication, FunctionLiteral with annotation.

- `check/TypeStackLoop.scala` — walks `typeStack.levels.reverse` top-down. Each level is checked against the level above, evaluated to SemValue, becomes the expected type for the next level. Applies specifiedTypeArguments. Checks runtime body. Drains unifier. Quotes signature.

- `processor/Monomorphic3Processor.scala` — replace stub with real call to TypeStackLoop.

**Tests ported (from monomorphize2):**

Into `Monomorphic3ProcessorTest`:
- monomorphize non-generic value
- monomorphize function literal in body
- monomorphize integer/string literal in body
- monomorphize value reference to non-generic value

Into `Monomorphic3TypeCheckTest`:
- function call (3 cases)
- functions without body (non-generic: simple return, one param, multi params)
- parameter usage (3 cases)
- top level functions
- literals (2)
- value references (non-generic)
- error reporting (undefined function, fail only once, wrong parameter type)

**Verification.** `./mill lang.test 2>&1 | grep Monomorphic3` — all ported tests pass. Original monomorphize2 tests untouched.

---

## Step 5 — Generics and polytype instantiation

**Goal.** Generic values, implicit/explicit type arguments, usage-driven polytype instantiation.

**Changes:**

- `infer(ValueReference(fqn, typeArgs))`: Fetch the referenced value's signature SemValue (by evaluating its type stack signature ORE in TypeLevel mode). Apply each explicit typeArg via `apply(sig, eval(typeArg))` — this beta-reduces through VPi closures naturally, without inspecting the SemValue's shape. Do NOT eagerly allocate metas for type parameters not covered by explicit typeArgs — let FunctionApplication drive that. If explicit typeArgs outnumber the VPi binders that `apply` can consume (i.e., `apply` hits a non-VPi, non-VLam value), report `"Too many type arguments"`.
- `infer(FunctionApplication(target, arg))`: infer the target's type, force it. If it's a VPi, extract domain/codomain directly. If it's a VLam (a polytype encountered at term level — e.g., a reference to a generic value being applied), `apply(VLam, freshMeta)` to instantiate it, then re-force and expect a VPi. If it's neither, unify against a fresh `VPi(?domain, ?codomain)` to let metas drive the inference. This is where implicit type parameters get instantiated — lazily, at the point of use.
- `check(FunctionLiteral, VPi)` without annotation: use the VPi's domain directly as the parameter type, extend env, recurse on body with codomain.

**Tests ported:**

From ProcessorTest:
- identity function with Int/String
- phantom type parameter
- multiple type parameters
- type argument count mismatch
- function application (generic id)

From TypeCheckTest:
- generic types (5)
- multi-parameter unification (2)
- apply
- function application
- explicit type arguments (9)
- functions without body (generic variants)
- parameter usage (wrong parameter in another function)

---

## Step 6 — Higher-kinded types and explicit kind restrictions

**Goal.** Type constructors, type-constructor parameters (`F[_]`, `C[_, _]`), explicit kind annotations (`F: Function[Type, Type]`).

**Changes.** No domain changes — `VPi` is fully general. Main work is ensuring `Quoter.quote` produces correct `GroundValue.Structure` for partially applied native type constructors, and that native wiring works at higher arities.

**Tests ported:**
- higher-kinded types (6 cases, including the ignored nested HK case)
- explicit type restrictions (5 cases)

---

## Step 7 — Type-level computation

**Goal.** `Box[one]` and `Box[oneDifferently]` unify when both reduce to `Box[1]`. Reject when they differ.

**Changes.** `force` unfolds `VTopDef` when spine is fully VConst. `unify` on `VConst(g1)` vs `VConst(g2)` does structural GroundValue equality. Postponement handles metas that aren't yet solved.

**Tests ported:**
- type level functions: non-type type parameters, concrete literal values, reject differing literals, concrete data values, reject differing data, type-level function calls

---

## Step 8 — Lambda inference and ability implementation resolution

**Goal.** Unannotated lambdas infer parameter type from context. Ability method references resolve to concrete implementations.

**Changes.**
- `check(FunctionLiteral(pn, None, body), VPi(d, c))` — use `d` directly as parameter type.
- `infer(ValueReference(fqn))` checks if the resolved value is an ability method dispatched to a concrete implementation during the `implementation` phase. Emits `MonomorphicValueReference(implementationFqn, ...)`.

**Tests ported:**
- lambda type inference (2)
- resolve ability ref to concrete implementation

---

## Step 9 — Recursion via lazy `VTopDef`

**Goal.** Direct and mutual recursion terminate during type checking.

**Changes.**
- `UserValueNativesProcessor` represents each value as `VTopDef(fqn, lazy thunk, SNil)`. Thunk evaluates body on demand, memoized at fact level.
- `force(VTopDef)` unfolds only when spine is fully VConst or unification demands structural comparison.
- `apply(VTopDef, x)` appends to spine without unfolding.
- Two `VTopDef`s with same FQN unify by FQN equality + spine unification — `def f = f` terminates because the inner `f` has the same cached type without unfolding.

**Tests ported:**
- direct recursion without infinite loop (both files)
- mutual recursion without infinite loop (both files)

---

## Step 10 — Final parity audit and cleanup

**Goal.** Confirm monomorphize3 handles everything monomorphize2's tests handle.

**Actions:**
- Walk through both original test files. For each test, verify a corresponding Monomorphic3 test exists and passes.
- Write `.claude/skills/eliot-monomorphize3/SKILL.md` describing the NbE architecture.
- `./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __`
- `./mill __.compile && ./mill __.test`

**Verification.** All monomorphize3 tests green. All monomorphize/monomorphize2/eval tests still green and untouched.

---

## Test-porting cross-reference

| Source test (in monomorphize2) | Ported in step |
|---|---|
| monomorphize non-generic value | 4 |
| monomorphize identity function with Int / with String | 5 |
| monomorphize value with phantom type parameter | 5 |
| monomorphize function with multiple type parameters | 5 |
| fail on type argument count mismatch | 5 |
| monomorphize function literal in body | 4 |
| monomorphize integer literal in body / string literal in body | 4 |
| monomorphize value reference to non-generic value | 4 |
| monomorphize function application | 5 |
| handle direct / mutual recursion (both files) | 9 |
| resolve ability ref to concrete implementation | 8 |
| function call (3 cases) | 4 |
| generic types (5) | 5 |
| multi-parameter unification (2) | 5 |
| higher-kinded types (6) | 6 |
| explicit type restrictions (5) | 6 |
| functions without body (5, split by genericity) | 4 / 5 |
| parameter usage (3) | 4 / 5 |
| top level functions | 4 |
| apply | 5 |
| lambda type inference (2) | 8 |
| literals (2) | 4 |
| value references | 4 |
| function application | 5 |
| error reporting (4) | 4 / 5 |
| explicit type arguments (9) | 5 |
| type level functions (6) | 7 |
| recursion (2) | 9 |

---

## Risk ledger

- **Native binding parity.** The three native processors must together cover every ValueFQN that the checker will encounter. Step 2 should include a smoke test enumerating all FQNs from the module pipeline.
- **De Bruijn level bookkeeping.** A helper `withBinder(name, sem)(body: CheckIO[T])` that extends env and nameLevels at the next free level and restores on exit makes this safe.
- **Error position threading.** Preserving exact `"Type mismatch." at "<expr>"` positions (which monomorphize2 tests assert verbatim) requires threading `Sourced[ORE]` through the unifier's context. Pick the threading strategy in Step 4 and commit — changing it later is expensive.
- **Closure allocation cost.** Every FunctionLiteral becomes a Scala closure allocation. For monomorphize3's workloads (one value at a time) this is negligible.
- **Eta at the unifier boundary.** Enabling eta from day one buys extensional function equality but adds a subtle recursion — increase depth before recursing to guarantee termination.
- **GroundValue drift vs Value.** Since `GroundValue` is a snapshot of `Value`, changes to `Value` in `eval` do not propagate. For monomorphize3 as a standalone experiment this is fine.
- **FunctionLiteral must never reach eval.** The evaluator does not handle `FunctionLiteral` — the Checker must always intercept it via `check`/`infer` and decide VLam vs VPi based on the expected type. If a FunctionLiteral leaks through to `eval`, it's a bug in the Checker's coverage. The evaluator should fail loudly if it encounters one.
- **Function native produces VPi, not VConst.** The `SystemNativesProcessor` must wire `Function` to produce `VPi(domain, _ => codomain)`. This is a departure from the existing `eval` package where `Function` produces `VConst(Structure($typeName=Function, A=..., B=...))`. The Quoter must handle `VPi` → `GroundValue` conversion for the final output, reconstructing the `Structure` form that downstream consumers expect.

## What does NOT change

- `monomorphize/`, `monomorphize2/`, `eval/`, `uncurry/`, `used/`, `jvm/`, plugin wiring, and all existing test files. All existing tests keep running against their original implementations.
- `OperatorResolvedExpression` / `OperatorResolvedValue` / `TypeStack` fact shapes — consumed read-only.
- The `CompilerIO` / `CompilerFact` / processor infrastructure — monomorphize3 just registers new processors alongside existing ones.
