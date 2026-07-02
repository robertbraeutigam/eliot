---
name: eliot-monomorphize
description: Use when editing, debugging, or reasoning about code under `lang/src/com/vanillasource/eliot/eliotc/monomorphize/` — the NbE (Normalisation by Evaluation) type checker for Eliot. Covers the semantic domain (SemValue), the bidirectional checker, the three evaluators, pattern unification, the two platform tracks (runtime + compiler), and the TypeStackLoop that processes type stacks uniformly without any concept of "generic parameters."
---

# monomorphize: NbE type checker

## Scope of this skill

This skill governs all code under `lang/src/com/vanillasource/eliot/eliotc/monomorphize/`:

```
monomorphize/
├── domain/
│   ├── SemValue.scala          (VType, VConst, VLam, VPi, VNative, VTopDef, VStuckNative, VMeta, VNeutral, Spine)
│   ├── Env.scala               (Vector[SemValue] + names; lookup by de Bruijn level AND by name)
│   ├── MetaStore.scala         (IntMap[Option[SemValue]]; fresh/solve/lookup)
│   └── MetaRole.scala          (D2: one role per meta id — Plain / Instantiation / AbstractAssoc)
├── eval/
│   ├── NbeEvaluator.scala      (the ONE shared pure traversal; abstract `decompose`)
│   ├── Evaluator.scala         (NbeEvaluator over ORE; hosts applyValue/force/renormalize/groundToSem + object helpers)
│   ├── SemExpressionEvaluator.scala  (NbeEvaluator over the checker's SemExpression output)
│   ├── MonomorphicEvaluator.scala    (NbeEvaluator over reduced MonomorphicExpression; DROPS erased type args)
│   └── Quoter.scala            (strict SemValue → GroundValue read-back; fails loudly on stuck forms)
├── check/
│   ├── Checker.scala           (bidirectional check/infer; definitional-equality core; builds 4 collaborators)
│   ├── CheckIO.scala           (StateT[CompilerIO, CheckState, *])
│   ├── CheckState.scala        (env, unifier, bindingCache, abilityResolutions, typeStackValueParams, sawGuardReturn)
│   ├── SemExpression.scala     (checker output ADT; type slots are SemValue, not GroundValue)
│   ├── TypeStackLoop.scala     (uniform top-down fold + the D1 post-drain pipeline + defaults + postcondition)
│   ├── PostDrainQuoter.scala   (the SOLE SemValue→GroundValue transition; reification gate; fold selection; reduceSourced)
│   ├── CalculatedReturnResolver.scala (D7 back-edge + W2b guard discharge)
│   ├── CarrierKindChecker.scala (D8 HKT kind seeding + verification)
│   └── AbilityResolver.scala   (ability-ref collection + resolve-abilities saturation pass)
├── refine/
│   └── RefinementSolver.scala  (D4 refinement lattice: Coerce widening + Combine join + upper-bounds)
├── unify/
│   ├── Unifier.scala           (pattern unification; pure definitional equality; MetaRole map)
│   ├── UnifyResult.scala       (Unified / Contradiction)
│   ├── UnifyError.scala        (context + optional expected/actual)
│   └── SemValuePrinter.scala   (human-readable SemValue rendering for error messages)
├── lowering/
│   └── RepresentationLowering.scala (Phase 3: unfold opaque types to machine representation via TransparentBinding)
├── fact/
│   ├── GroundValue.scala       (output: Direct, Structure, Type)
│   ├── MonomorphicValue.scala  (runtime output fact: signature + runtime, keyed by (vfqn, typeArgs))
│   ├── CompilerMonomorphicValue.scala (compiler-track output fact — a DISTINCT type; cannot name MonomorphicValue.Key)
│   ├── MonomorphicExpression.scala (output expression ADT; type slots are ground)
│   ├── NativeBinding.scala     (Platform-keyed: vfqn → SemValue for the evaluator)
│   ├── ContributedBinding.scala (Key(vfqn, label); NOT platform-keyed — one contribution serves both tracks)
│   ├── BindingContribution.scala (Leaf(SemValue) | Body(SaturatedValue))
│   └── TransparentBinding.scala (like NativeBinding but WITHOUT the opaque guard, for lowering)
└── processor/
    ├── MonomorphicTypeCheckProcessor.scala   (runtime entry point → TypeStackLoop, Platform.Runtime)
    ├── CompilerMonomorphicTypeCheckProcessor.scala (compiler entry point → TypeStackLoop, Platform.Compiler; native-leaf boundary)
    ├── SystemNativesProcessor.scala          (Function → VNative→VPi, Type → VType, Bool true/false/fold)
    ├── DataTypeNativesProcessor.scala        (body-less Type-qualified names → inert VTopDef; pool-guarded)
    ├── MatchNativesProcessor.scala           (handleCases/typeMatch impls → VNative; pool-guarded)
    ├── DeclaringPool.scala                   (quiet two-pool membership probe used by the two leaf contributors above)
    ├── UserValueNativesProcessor.scala       (thin BodyContributorProcessor over the runtime user pool)
    ├── CompilerNativesProcessor.scala        (compiler-pool body supplier; ability-using values → reduced Leaf)
    ├── BodyContributorProcessor.scala        (shared base for the two user-category body suppliers)
    ├── BindingMergerProcessor.scala          (SINGLE owner of NativeBinding; category precedence + dependency closure)
    ├── BindingClosure.scala                  (closes a selected Body over its deps; reifyingWrap via binderRoles)
    ├── ReducedBindingClosure.scala           (BindingClosure's twin over a reduced MonomorphicExpression)
    └── TransparentBindingProcessor.scala     (emits TransparentBinding via BindingClosure with runtime bodies)
```

and its sibling tests under `lang/test/src/com/vanillasource/eliot/eliotc/monomorphize/`.

`monomorphize` is the sole monomorphic type-checker package. There is **one** evaluator traversal (`NbeEvaluator`)
and **one** semantic domain (`SemValue`) shared by types and values — never a second, weaker compile-time interpreter.

## How NbE works

ORE (operator-resolved expressions) are evaluated into `SemValue` (semantics) via Scala closures. Type equality
becomes structural equality of normal forms. Unification happens locally as the checker walks the term — no constraint
set, no worklist. All read-back to `GroundValue` is deferred to a single post-drain pass; the checker never converts to
`GroundValue` mid-flight, so there is **no silent `Type` fallback anywhere**.

### The semantic domain (`SemValue`)

| Variant | What it represents | Produced by |
|---|---|---|
| `VType` | The type of all types | Constant (`Type` native, checker defaults) |
| `VConst(ground)` | Non-function ground values: literals (`42`, `"hi"`), compile-time `Bool` (`Direct(true/false)`), the unit placeholder | Evaluator (literals), Bool natives, MatchNativesProcessor |
| `VLam(name, closure)` | Runtime lambda closure | Evaluator (always, for `FunctionLiteral`) |
| `VPi(domain, codomain)` | Function type (dependent or non-dependent) | Checker (checking a `FunctionLiteral` against `VType`), the `Function` native |
| `VNative(paramType, fire)` | Primitive awaiting a concrete argument; `fire` yields its own stuck form when the arg is not concrete | SystemNativesProcessor (Function, Bool ops), MatchNativesProcessor (handleCases/typeMatch), StdlibNativesProcessor (arithmetic) |
| `VTopDef(fqn, cached, spine)` | Lazy top-level definition (cached=Some) **and** applied type/value constructor (cached=None) | BindingClosure (cached body), DataTypeNativesProcessor (body-less), the FQN-preserving missing-binding fallback |
| `VStuckNative(fqn, spine)` | A **non-injective** native application (`add(x,y)`, `min`, `&&`, `fold`, `inc`) stuck on not-yet-concrete args | a `VNative`'s `fire` when an argument is abstract |
| `VMeta(id, spine)` | Unsolved metavariable (there is **no** `expected` field) | Checker (`MetaStore.fresh`) |
| `VNeutral(head, spine)` | Stuck application on a rigid variable head (`NeutralHead.VVar(level, name)`) | Evaluator (unresolved `ParameterReference`), the `$bad-apply` fail-safe fallback |

`VStuckNative` is deliberately **distinct from `VTopDef`** (D3): a native application is *not injective* (`add(1,3)` =
`add(2,2)`), so the unifier must never injectivity-decompose it and the quoter must fail loudly if one survives read-back.
`Evaluator.renormalize` re-fires a `VStuckNative` once its arguments become concrete.

### `applyValue` — the one application primitive (fail-safe fallback, F1)

`Evaluator.applyValue(f, x)` is exhaustive over every *applicable* head: `VLam`/`VPi` β-reduce, `VNative` fires (even
on a non-concrete argument — the native produces its own `VStuckNative`), and `VNeutral`/`VTopDef`/`VStuckNative`/`VMeta`
grow their spine. The two remaining heads — `VConst` and `VType` — are **not applicable**; reaching them can only happen
in an **ill-typed** program (one the checker has already, or is about to, reject). This case returns a **loud stuck
form** — a `VNeutral` on the reserved head `NeutralHead.VVar(-1, "$bad-apply")` carrying the argument — *never* the old
identity-ish fallback that returned `x` (which silently collapsed `F[A]` to `A`). If a `$bad-apply` value survives to
read-back the strict `Quoter` fails it ("Cannot quote neutral value") ⤳ `PostDrainQuoter`'s "Cannot resolve type."; but a
program with a genuine type error aborts *before* quoting and reports that real diagnostic. Fail-safe: never a silent
wrong value. (Do not reintroduce the identity fallback — see anti-patterns.)

### Type/value constructors are body-less `VTopDef`s

`DataTypeNativesProcessor` binds every body-less `Type`-qualified name (an abstract `type`, or a `data` type's
constructor) to `VTopDef(fqn, None, SNil)`. Applying type args just grows its spine. So `Int[2,5]` is
`VTopDef(IntFQN, None, [VConst(2), VConst(5)])` — **not** a `VConst(Structure)`. `Structure(...)` only appears after
read-back. This is load-bearing: `typeMatch`/`handleCases` dispatch on the `VTopDef(head, None, spine)` shape, and the
unifier's same-FQN case operates on these `VTopDef`s.

### VLam vs VPi — who produces what

The single most important invariant:

- **The evaluator always produces `VLam` for `FunctionLiteral`** (`NbeEvaluator.eval`), regardless of position.
- **The Checker produces `VPi`** when checking a `FunctionLiteral` against `VType` (the term IS a function type).
- **The Checker produces `VLam`** when checking a `FunctionLiteral` against `VPi(d, c)` (the term is a runtime lambda).
- **The `Function` native produces `VPi`.** `Function[A, B]` = `apply(apply(functionNative, A), B)` → `VPi(A, _ => B)`.

Both are applicable. `VConst`/`VStuckNative` never represent function types.

### Env, closures, and name lookup

`Env` is `Vector[SemValue]` (indexed by de Bruijn level) plus a parallel `names` vector. The evaluator resolves a
`ParameterReference` by **name** (`Env.lookupByName`, last-bound-wins), *not* by a pre-computed level — there is no
`nameLevels` table on `CheckState`. Closures are native Scala functions capturing the current env and body; no ORE
substitution ever happens. `Spine` is a reversed cons list (`SNil | SApp(tail, head)`) for O(1) append.

### Three evaluators, one traversal

`NbeEvaluator[E]` is the single pure, syntax-directed traversal. Its subclasses differ **only** in `decompose` (how a
node projects onto `NbeEvaluator.Term`):

- `Evaluator` (over ORE) — re-evaluates ORE type-argument sub-terms.
- `SemExpressionEvaluator` (over the checker's `SemExpression`) — reads the already-evaluated `SemValue` type args.
- `MonomorphicEvaluator` (over a reduced `MonomorphicExpression`) — **drops** the erased type arguments of a value
  reference (applying them would mis-bind a value parameter to an erased type arg).

Missing-binding fallback for a **value reference** is an FQN-preserving stuck `VTopDef(vfqn, None, SNil)` — **not** a
`VNeutral` (which would drop the FQN and corrupt read-back/codegen/`Coerce`-constructor recognition). A missing
**parameter** reference falls back to `VNeutral`.

### How computation runs

`force(v, metaStore)` walks solved metas and unfolds `VTopDef` when the cached body is present.
`renormalize(v, metaStore, lookupNative, deep)` additionally re-fires `VStuckNative`s whose args have concretised (and,
with `deep=true`, descends under `VPi` binders — used **only** post-drain at quote time, where every meta is solved).

## Pipeline

### Entry points (two tracks)

- `MonomorphicTypeCheckProcessor` → `TypeStackLoop.process(..., platform = Platform.Runtime)` → `MonomorphicValue`.
- `CompilerMonomorphicTypeCheckProcessor` → the **same** `TypeStackLoop.process(..., platform = Platform.Compiler)` →
  `CompilerMonomorphicValue` (a *distinct* fact type, so the compiler track cannot even name `MonomorphicValue.Key`;
  the `compiler-mono → runtime-mono` edge is impossible by construction — acyclic).

`NativeBinding`, `SaturatedValue`, `OperatorResolvedValue`, `UnifiedModuleNames`, `ModuleConstructors`, etc. are all
`Platform`-keyed (Runtime / Compiler). Each track unifies base + user program + its own layer independently.

### NativeBinding injection (ContributedBinding + BindingMergerProcessor)

`NativeBinding.Key(vfqn, platform)` maps a value FQN to its evaluator `SemValue`. It is produced by a **single owner**,
`BindingMergerProcessor` — replacing any first-registration race. The flow:

1. Every supplier emits a **total** `ContributedBinding` fact under its `label` — a `BindingContribution` when it defines
   a reduction for `vfqn`, else `None` (never declines). `ContributedBinding.Key(vfqn, label)` is **not** platform-keyed:
   one contribution serves both track merges. Labels:
   - **native category** (`langNativeLabels` = `system` / `datatype` / `match` / `compiler`, plus any `extraNativeLabelsKey`
     labels added by layers, e.g. stdlib's arithmetic natives):
     - `SystemNativesProcessor` (`system`) — `Function` → `VNative`→`VPi`; `Type` → `VType`; the compile-time `Bool`
       primitives `true`/`false` (`VConst(Direct(Boolean))`) and `fold` (selects a branch on a concrete `Bool`, else stuck).
     - `DataTypeNativesProcessor` (`datatype`) — body-less `Type`-qualified constructors → `VTopDef(fqn, None, SNil)`,
       excluding the system-owned `Function`/`Type`. **Pool-guarded** (see `DeclaringPool`).
     - `MatchNativesProcessor` (`match`) — the `PatternMatch.handleCases` / `TypeMatch.typeMatch` impl FQNs → `VNative`s
       that reduce a concrete `VTopDef(ctor, None, spine)` scrutinee during checking. **Pool-guarded**.
     - `CompilerNativesProcessor` (`compiler`) — the compiler platform's Eliot-bodied reductions (the `add` pattern:
       ranked as a native so its reduction wins for *checking* while the runtime body is still used for codegen). A
       compiler-only value whose checking body performs ability dispatch is contributed as the **reduced**
       `CompilerMonomorphicValue` body wrapped by `ReducedBindingClosure` (a `Leaf`), because its raw body would stall on
       the abstract ability method.
   - **user category** (`userLabel`): `UserValueNativesProcessor`, a thin `BodyContributorProcessor` subclass over the
     runtime pool. A body-ful value contributes a `BindingContribution.Body(saturated)`; a body-less one contributes
     `None` (its checking impl is a native, or the evaluator's stuck fallback — never an empty binding that would shadow a
     reducing native, the `add` bug). There is **no** `generating` concurrent-guard mutable state.
2. `BindingMergerProcessor` selects by **category precedence**: the first native `Some`, else — *only on the Runtime
   platform* — the first user `Some`, else abort (no binding; the evaluator stalls at the use site). The Compiler track
   never consults the user pool (native-leaf boundary). For a selected `Body` it runs `BindingClosure`, closing the body
   over its dependencies' `NativeBinding`s. The dependency walk lives **here** (the merger is the sole owner of the
   `NativeBinding` recursion); suppliers never read back the fact they feed. The mutual-recursion guard is the runtime's
   active fact-request chain (`activeFactKeys`), not mutable state. Reified type-stack binders are wrapped as leading
   lambdas via `BindingClosure.reifyingWrap`, driven by the precomputed `SaturatedValue.binderRoles` (D6).

`TransparentBindingProcessor` emits `TransparentBinding` (like `NativeBinding` but *without* the opaque guard, so
`opaque type Int = <repr>` unfolds) via the same `BindingClosure`, for `RepresentationLowering`.

### TypeStackLoop — the uniform fold

`TypeStackLoop.processIO`:

1. `walkTypeStack` — reverse the type-stack levels, fold with `expectedType = VType`; for each level `check(level,
   expected)` then `expected = eval(level)`. **The fold body is identical for every level** — no "is this a type
   parameter?" branch. Generics emerge from `FunctionLiteral` levels checked against `VPi` kinds from above.
2. `applyTypeArgs` — apply explicit ground type args (a type-level arg via `groundToSem` into an applicable
   `VTopDef`/`VType`; a value-level arg stays a `VConst`). Over-application records one "Too many type arguments." error.
3. `instantiateRemaining` — peel leftover `VLam` closures (phantom / implicit type params) with fresh metas.
4. `pinCompilerCarriers` (compiler track only) — a `{Throw[E]}` carrier is fixed to the compile-time carrier `Either[E]`.
5. Return-position resolution (mutually exclusive): a *calculated* return (`installReturnMeta`, W3), an *effectful guard*
   return (`dischargeGuardedSignature`, W2b — **runtime track only**; the compiler track is the guard's *producer* and
   leaves the carrier signature as-is), or an ordinary explicit return.
6. `check` the runtime body against the signature.
7. `runPostDrainPipeline` (D1, below), report unifier errors, abort before quoting if any error exists.
8. Read back via `PostDrainQuoter` — for the compiler track, the body is **reduced** (`reduceSourced`); for the runtime
   track it is structurally quoted (`quoteSourced`).

### Post-drain pipeline (D1)

A tiered structure (there is no external design doc — it is described here and in `TypeStackLoop`'s doc comments):

- **Saturation tier** (fixed-point loop, drain interleaved as the equality core settles): resolve-abilities and
  resolve-`Combine`s, re-draining after each until stable.
- **Finalization tier** (linear, once): upper-bounds discharge (`RefinementSolver.resolveUpperBounds`), carrier-kind
  verification (`CarrierKindChecker.verifyCarrierKinds`), and the calculated-return fail-safe.
- **Finalizer** (`defaultUnsolvedMetas`): a **total match on `MetaRole`** with no catch-all — `AbstractAssoc` stays
  unsolved (quotes as abstract), `Plain` and `Instantiation` default to `VType`. A future role added to the ADT forces an
  explicit decision here.
- **Postcondition** (`assertEveryMetaResolvedOrAbstract`): every meta is solved or an `AbstractAssoc` placeholder.

### Bidirectional checker & its collaborators

`Checker` is the **definitional-equality core only**: `check`/`infer`/`applyInferred`/`peelLams`/`instantiatePolymorphic`
plus the shared primitives (`force`, `freshMeta`, `doUnify`, `evalExpr`, `ensureBinding`, `prefetchBindings`). Four
non-equality concerns live in collaborator modules, each constructed at the top of `Checker` with **exactly the checker
primitives it needs** (that narrow surface is the module boundary), and invoked from named hook points:

| Collaborator — field | Concern | Hook points |
|---|---|---|
| `refine/RefinementSolver` — `checker.solver` (D4) | refinement lattice: `Coerce` widening + `Combine` join + upper-bounds | `Checker.check` → `unifyOrCoerce`; `TypeStackLoop` post-drain `resolve-combines` / `upper-bounds` |
| `check/CalculatedReturnResolver` — `checker.calcReturns` (D7 + W2b) | non-local inference (fill a bare return from the callee's mono body) **and** effectful-guard discharge | `Checker.infer`/`applyInferred`; `TypeStackLoop` `installReturnMeta` / `dischargeGuardedSignature` |
| `check/CarrierKindChecker` — `checker.carriers` (D8) | HKT kind seeding + verification | `Checker.instantiatePolymorphic` → `recordCarrierMetas`; `TypeStackLoop` `carrier-kinds` pass → `verifyCarrierKinds` |
| `check/AbilityResolver` — `checker.abilityResolver` | ability-ref collection + the `resolve-abilities` saturation pass (resolve each ability-qualified ref to its impl; inject associated types) | `TypeStackLoop.processIO` → `collectAbilityRefs`; `TypeStackLoop` post-drain `resolve-abilities` pass → `resolveAbilities` |

Per-meta data (combinable / candidates / carrier-kind / upper-bounds / abstract-assoc) all lives in the **single**
`MetaRole` map on the `Unifier` (D2); the collaborators own the *algorithm*, not a private store.

### MetaRole (D2)

One `Map[Int, MetaRole]` on the `Unifier` replaces the former six scattered side-tables. Absence ⇒ `Plain`.

- `Plain` — solved by ordinary unification, else defaulted to `VType`. The default for any unclassified meta.
- `Instantiation(combinable, candidates, combineResolved, upperBounds, carrierKind)` — an implicit type-parameter meta
  peeled from a polytype's leading binders. Combinable (covariant) unless tainted by a `VPi` domain; a higher-kinded one
  additionally carries a `carrierKind` verified post-drain.
- `AbstractAssoc(fqn)` — a placeholder for an abstract associated ability type (`type X` inside `ability …`); the one role
  the finalizer protects from defaulting to `Type`, so a constraint-covered reference can stay abstract through quoting.

### Guard discharge (W2b) — `CalculatedReturnResolver`

Sits beside the calc-return back-edge because both are "run a compile-time computation to obtain the return type." A
return-type expression may be a `{Throw[String]}` computation on the compile-time `Either[String, _]` carrier (the
compiler is the handler). Three hook points recognise the carrier's `Right`/`Left` **by FQN**
(`WellKnownTypes.eitherFQN` / `rightFQN` / `leftFQN`) and reduce `fold`/`foldEither` via the merged `NativeBinding`:
`isGuardCarrier` (the *kind* position — accept an `Either[..]`-valued return where a bare `Type` is expected),
`dischargeGuardedSignature` (the *callee* side — `Right(t)` ⤳ the published type `t`, `Left(msg)` ⤳ a diagnostic, an
abstract-bounds guard deferred to the body via a return meta), and `dischargeGuardedReturn` (the *applied / by-name read*
sides). The compiler track skips discharge (it is the *producer*: `pure(A)` ⤳ `Right(A)`, publishing the undischarged
carrier signature for a consumer to discharge).

### PostDrainQuoter — the sole SemValue→GroundValue transition

`Checker.forceAndConst` no longer exists. Read-back happens exactly once, post-drain, in `PostDrainQuoter`:

- `quoteSem` deeply renormalises (so a stuck native in an intermediate `VPi` codomain re-fires) then calls the strict
  `Quoter.quote`. On a `Left` it aborts with `"Cannot resolve type."` (detail = the quoter message) — **no** silent
  `Type` fallback.
- The strict `Quoter` succeeds only on `VType`, `VConst`, `VPi` (→ Function `Structure`), and body-less
  `VTopDef(fqn, None, spine)` (→ `Structure`). It **fails loudly** on `VNeutral`, `VMeta`, `VLam`, `VNative`,
  `VStuckNative`, and an unapplied cached `VTopDef`.
- `PostDrainQuoter` also hosts the **compile→runtime reification gate** (materialising erased-parameter-determined
  sub-terms into literal/constructor trees), the `Bool` `fold` **branch selection** (`trySelectFold`), and the compiler
  track's `reduceSourced` (evaluate + read back the reduced compile-time body, folding resolved ability impls in).

### Unifier — pure definitional equality

Pattern unification on `SemValue`s with a `postponed` queue (`drain()` retries until stable):

1. Force both sides. `VType`~`VType` ✓. `VConst`~`VConst` → structural `GroundValue` equality.
2. `VPi`/`VLam` → extend with a fresh rigid var, unify under the binder. Eta: `VLam` vs other → unify `c(fresh)` against
   `apply(other, fresh)`.
3. `VNeutral`~`VNeutral` same head → zip spines. `VTopDef`~`VTopDef` same FQN → zip spines (definitional equality; e.g.
   `Int[0,5]` ≠ `Int[0,10]` and are *rejected* — directional widening is `Coerce`'s job in check mode, never here).
4. `VStuckNative`~`VStuckNative` same FQN → zip spines; it is **never** injectivity-decomposed against a meta (non-injective).
5. `VMeta` → pattern rule: empty spine → solve directly (with an **occurs-check**); non-empty spine → `tryDecomposeApplied`
   (injectivity decomposition against a *rigid* `VTopDef(None)` / `VNeutral` head, equal-arity or partial-application),
   else postpone. A carrier meta postponed against a non-rigid `VPi` (`?F[A,B] ~ Function[A,B]`) is **deliberately not
   decomposed** — `CarrierKindChecker` documents this as higher-order unification the pattern unifier cannot solve
   (see the diagnosing note on the two-arg HKT-over-Function limitation).

There is **no** assignability / widening / `refinements` map in `unify`. Coercion is the `Coerce` ability inserted in the
checker's check mode (`RefinementSolver`), recognised by protocol name (`WellKnownTypes.coerceFQN`), never by type.

### Two-pool membership guards (leaf contributors)

`ContributedBinding` is not platform-keyed, but a leaf contributor must read a *platform-keyed* source fact (a value, its
constructors) to build a reduction. Requesting that fact on a fixed pool a name is absent from would trip
`UnifiedModuleValueProcessor`'s "Could not find" build error. So `DataTypeNativesProcessor` and `MatchNativesProcessor`
ask `DeclaringPool.of(vfqn)` first — a quiet `UnifiedModuleNames` (name-set) probe that returns the pool a name is
declared in (preferring `Runtime`, falling back to `Compiler`, else `None`) — and read the source fact only on that
pool. `CompilerNativesProcessor.inCompilerPool` and `CompilerMonomorphicTypeCheckProcessor.inRuntimePool` use the same
quiet-probe pattern.

## The hard rules

1. **No concept of "generic parameters" in the checker's fold.** `TypeStackLoop`/`Checker` never extract, count, or
   classify type-stack levels as "type params" vs "signature"; the fold is uniform (no `TypeParameterAnalysis`, no
   walking leading `FunctionLiteral`s to count type params). *However*, three sanctioned **peripheral** readers legitimately
   read binder structure through `SignatureView` / saturation metadata — do **not** mistake them for a violation:
   `CarrierKindChecker.recordCarrierMetas` (seeds carrier kinds off the referenced value's `SignatureView`),
   `TypeStackLoop.abilityArity` (reads an ability-marker's binder count for impl queries), and
   `SaturatedValue.binderRoles` feeding `BindingClosure.reifyingWrap` (which leading binders a body reifies). None of
   these drives a definitional-equality decision.
2. **The evaluator produces VLam; the Checker produces VPi.** Never produce `VPi` in the evaluator. The `Function` native
   is a `VNative` that fires to `VPi`.
3. **No ORE rewriting.** ORE is read once into `SemValue` and forgotten. All substitution is closure application.
4. **No constraint set.** Unification is immediate and local; the only deferral is the unifier's `postponed` queue.
5. **NativeBinding pre-fetching.** The evaluator is synchronous and pure. All bindings are prefetched into
   `CheckState.bindingCache` (via `prefetchBindings`/`BindingClosure.collectBindings`) before evaluation.
6. **State via `CheckIO`.** State is threaded through `StateT[CompilerIO, CheckState, *]` — `get`/`modify`/`inspect`,
   not in-place mutation. Bind a parameter with `state.bind(name, value)` (runtime param, env binding = its *type*) or
   `state.bindTypeStackParam(name, value)` (erased type-stack param, env binding = its *value*).
7. **No silent read-back fallback.** `applyValue` on a non-applicable head yields a loud `$bad-apply` stuck neutral; the
   quoter fails loudly on every stuck form; `defaultUnsolvedMetas` is a total match on `MetaRole`. Nowhere does a stuck
   value silently become `Type`.
8. **Two acyclic tracks.** The compiler track produces `CompilerMonomorphicValue` and cannot name `MonomorphicValue.Key`.
   Its `fetchBinding` enforces the native-leaf boundary: a runtime-concrete + compiler-absent name is a hard error
   ("Cannot use runtime-only value … at compile time"), never a fall-through to the runtime body.

## Diagnosing failures

- **"Type mismatch" on a correct program.** Check every referenced value has a `NativeBinding` (right platform). A
  missing *value* binding evaluates to a stuck `VTopDef` (FQN-preserving); a missing *parameter* to a `VNeutral`; either
  fails to unify against a concrete expected type. Add the missing case to the relevant supplier / prefetch.
- **"Cannot resolve type."** A stuck `SemValue` survived to `PostDrainQuoter.quoteSem`. Inspect *which* stuck form
  (`Quoter`'s `Left` message names it): an unsolved `VMeta` (a constraint that should have solved postponed and was
  dropped), a `VNeutral` (an unresolved parameter, or the `$bad-apply` fail-safe = an argument applied to a
  non-applicable head in an ill-typed program), a `VLam` (a polymorphic value not fully instantiated —
  `instantiateRemaining` should peel it), or a `VStuckNative` (see next).
- **"Cannot quote stuck native application."** A native's arguments stayed abstract at read-back. Check that
  `Evaluator.renormalize` (with the checker's binding cache as `lookupNative`) re-fires it once its metas solve, and that
  the native's `fire` produces a `VStuckNative` (not `false`) on non-concrete args.
- **Two-arg HKT carrier over `Function`** (`?F[A,B] ~ Function[A,B]`): the `Function` carrier normalises to a `VPi`,
  which the unifier deliberately does not decompose, so `?F` stays unsolved, defaults to `VType`, and the later `F[A,B]`
  applies args to `VType` → the loud `$bad-apply` fallback → "Cannot resolve type." This is a **known inference
  limitation** (surfaced, not silently miscompiled — a `data`-carrier `?F := Box` *does* decompose). See
  `MonomorphicTypeCheckTest`'s "surface (not silently miscompile) a two-arg HKT carrier over the Function type".
- **Infinite loop / stack overflow.** Likely `force` unfolding a self-referential `VTopDef` whose spine never grows.
  `BindingClosure` guards fact-level mutual recursion via `activeFactKeys`; `force` itself has no depth limit, so ensure
  it only unfolds when the cached body is `Some`. The unifier's occurs-check prevents cyclic meta solutions.
- **`data` type declared only in the compiler pool has no constructor/match native.** Check `DeclaringPool.of` finds the
  compiler pool (name must be in the compiler `UnifiedModuleNames`).

## Anti-patterns (reject in review)

- **Inspecting the type-stack to count or classify type parameters in the checker's fold.** The fold is uniform. (The
  three sanctioned peripheral readers in Hard Rule 1 are the *only* binder-structure reads, and none drives equality.)
- **Producing `VPi` in the evaluator**, or `VLam` in the Checker when the expected type is `VType`.
- **ORE substitution inside monomorphize.** All binding is closure capture in Scala.
- **Calling `CompilerIO` from an evaluator.** The evaluators are synchronous and pure; prefetch bindings first.
- **Adding a constraint list / worklist.** Unification is immediate; the only deferral is the unifier's `postponed` queue.
- **Eagerly allocating metas for type parameters at `ValueReference`.** Implicit instantiation is driven lazily by
  `FunctionApplication` / `instantiatePolymorphic` / `instantiateRemaining`.
- **Using `Map[String, SemValue]` for the environment hot path.** Env is `Vector[SemValue]`; the evaluator resolves
  parameters by name via `Env.lookupByName`.
- **Re-introducing assignability / widening / a `refinements` map into `unify`.** `unify` is pure definitional equality;
  directional coercion is the `Coerce` ability's job in check mode (`RefinementSolver`).
- **Re-inlining a non-equality side-car into `Checker`.** A new lattice relation / inference back-edge / kind rule gets
  its *own* collaborator constructed with injected primitives — never grown into `Checker`, never folded into `unify`.
- **Special-casing a concrete type (e.g. `Int`) in the checker/unifier.** Recognise the *ability protocol* by name
  (`coerceFQN`, `eitherFQN`, `PatternMatch`/`TypeMatch`), never the type.
- **Returning `false` (not a `VStuckNative`) from a native on non-concrete args.** `&&`/`fold`/arithmetic must stay stuck
  so the unifier still solves metavariables; `false` would wrongly reject generic/open comparisons.
- **Injectivity-decomposing a `VStuckNative` against a meta.** A native application is non-injective; it must postpone.
- **Re-introducing a silent fallback in `applyValue` or any read-back path.** Applying an argument to a non-applicable
  head must yield the loud `$bad-apply` stuck form; stuck values must fail the quoter loudly; unsolved metas must default
  by role, never silently become `Type`.
- **Reading a runtime-pool fact in a leaf contributor without a membership probe.** A `ContributedBinding` is not
  platform-keyed; probe `DeclaringPool` / `UnifiedModuleNames` before requesting a platform-keyed value, or a
  compiler-pool-only name pollutes the build with a spurious "Could not find" error.
- **Naming `MonomorphicValue.Key` from the compiler track.** It would create the forbidden `compiler-mono → runtime-mono`
  edge; the compiler track produces `CompilerMonomorphicValue` only.

## Testing

Tests live under `lang/test/src/com/vanillasource/eliot/eliotc/monomorphize/`:

- `processor/MonomorphicTypeCheckTest.scala` — the main end-to-end checker cases (inference, HKT decomposition, carrier
  kinds, `Coerce`/`Combine`, calculated returns W3/W4, guard signatures). **Extend these named end-to-end cases** rather
  than mock-unit-testing a collaborator (the collaborators were pure moves; their behaviour is pinned here).
- `processor/MonomorphicTypeCheckProcessorTest.scala` — processor-level cases.
- `processor/MatchNativesProcessorTest.scala` — `match` reduction via `handleCases`/`typeMatch` natives.
- `processor/MonomorphizationVersioningTest.scala` — per-instantiation keying.
- `processor/ReificationTest.scala` — the compile→runtime reification gate.
- `processor/ComputedTypeArgumentReadbackTest.scala` — computed type-argument read-back.
- `processor/CompilerMonomorphicTypeCheckProcessorTest.scala`, `processor/CompilerNativesProcessorTest.scala`,
  `processor/CompilerNativeLeafBoundaryTest.scala`, `processor/CompilerEitherCarrierTest.scala`,
  `processor/CompilerAbilityResolutionTest.scala` — the compiler-as-platform track (register a `Platform.Compiler`
  `PathScan` for the compiler pool; see `CompilerNativesProcessorTest`'s `twoPoolFacts` template).
- `processor/CompilerOnlyDataNativesTest.scala` — the `DeclaringPool` compiler-pool fallback for the datatype native.
- `eval/EvaluatorApplyValueTest.scala` — the F1 loud `$bad-apply` fallback shape + quoter failure.
- `unify/UnifyResultTest.scala`, `unify/OccursCheckTest.scala`, `unify/StuckNativeUnifyTest.scala`,
  `unify/UnifierRoleTest.scala` — pure unifier unit tests (construct `SemValue`s directly; `AnyFlatSpec`).

Most processor tests run the whole pipeline via `ProcessorTest(LangProcessors()*)` and construct source text inline.
Follow the project testing conventions: single-line asserts, assert the `Seq` itself, prefer `.asserting(_ ...)`.

```bash
./mill lang.test 2>&1 | grep -v DEBUG | grep "MonomorphicTypeCheck"
```

## Facts and keys

- `MonomorphicValue.Key(vfqn, typeArguments)` / `CompilerMonomorphicValue.Key(vfqn, typeArguments)` — composite;
  same `vfqn` + different type args → different specialization. The compiler key is a *distinct* type (acyclicity).
- `MonomorphicValue` fields: `vfqn`, `typeArguments`, `name`, `signature: GroundValue`, `runtime:
  Option[Sourced[MonomorphicExpression.Expression]]`. `CompilerMonomorphicValue` mirrors it with `reduced` instead of
  `runtime` (the reduced compile-time body plugged in as a native, not codegen input).
- `NativeBinding.Key(vfqn, platform)` — value FQN → evaluator `SemValue`, per platform. `TransparentBinding.Key(vfqn)` —
  the opaque-unfolding twin (runtime only), for representation lowering.
- `ContributedBinding.Key(vfqn, label)` — **not** platform-keyed; one supplier's total answer per label.
- `MonomorphicExpression.expressionType: GroundValue` — always fully ground; no free variables or unsolved metas.
