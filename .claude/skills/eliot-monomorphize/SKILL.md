---
name: eliot-monomorphize
description: Use when editing, debugging, or reasoning about code under `lang/src/com/vanillasource/eliot/eliotc/monomorphize/` — the NbE (Normalisation by Evaluation) type checker for Eliot. Covers the semantic domain (SemValue), the bidirectional checker, the three evaluators, pattern unification, the two platform tracks (runtime + compiler), and the TypeStackLoop that kind-checks a value's signature against its derived kind and drives monomorphization.
---

# monomorphize: NbE type checker

## Scope of this skill

This skill governs all code under `lang/src/com/vanillasource/eliot/eliotc/monomorphize/`:

```
monomorphize/
├── domain/
│   ├── SemValue.scala          (VType, VConst, VLam, VPi, VNative, VTopDef, VStuckNative, VMeta, VNeutral, Spine)
│   ├── Env.scala               (Vector[SemValue] + names; lookup by name, last-bound-wins; `level` mints neutrals)
│   └── MetaStore.scala         (IntMap[Option[SemValue]]; fresh/solve/lookup)
├── eval/
│   ├── NbeEvaluator.scala      (the ONE shared pure traversal; abstract `decompose`)
│   ├── Evaluator.scala         (NbeEvaluator over ORE; hosts applyValue/force/renormalize/groundToSem + object helpers)
│   ├── SemExpressionEvaluator.scala  (NbeEvaluator over the checker's SemExpression output)
│   ├── MonomorphicEvaluator.scala    (NbeEvaluator over reduced MonomorphicExpression; DROPS erased type args)
│   └── Quoter.scala            (strict SemValue → GroundValue read-back; fails loudly on stuck forms)
├── check/
│   ├── Checker.scala           (bidirectional check/infer; definitional-equality core; builds 4 collaborators;
│   │                            inferSpine = whole-spine argument resolution with Phase-A/B flex-slot deferral)
│   ├── CheckIO.scala           (StateT[CompilerIO, CheckState, *])
│   ├── CheckState.scala        (gamma Γ + rho ρ, unifier, bindingCache, abilityResolutions, sawGuardReturn,
│   │                            ambientCarriers + liftCounter for the effect lift)
│   ├── SemExpression.scala     (checker output ADT; type slots are SemValue, not GroundValue)
│   ├── TypeStackLoop.scala     (signature kind-check + the post-drain resolution sequence + defaults + postcondition)
│   ├── PostDrainQuoter.scala   (the SOLE SemValue→GroundValue transition; reification gate; integerLiteral rewrite;
│   │                            reduceSourced)
│   ├── CalculatedReturnResolver.scala (D7 back-edge + W2b guard discharge)
│   ├── MarkerGuardSignature.scala (ability-impl marker detection + the parameter-stripped guard view)
│   ├── GuardChannel.scala      (the Left/Right guard read-back protocol shared with the ability processor)
│   ├── CarrierKindChecker.scala (D8 HKT kind seeding + verification; flags every HKT instantiation meta effectCarrier)
│   ├── EffectLifter.scala      (the effect auto-lift: bind-lift/pure-wrap arms + Effect.flatMap/map/pure splices)
│   ├── AbilityResolver.scala   (ability-ref collection + resolve-abilities saturation pass)
│   └── Track.scala             (Runtime/Compiler strategy: platform + 4 per-track hooks, no platform match in the core)
├── channel/
│   ├── RefinementChannelProcessor.scala (post-pass flow analysis over MonomorphicValue: ^Meta transfers/merges,
│   │                                     ^Where precondition demands)
│   └── RefinementTable.scala   (per-node meta values keyed by source position; read by reconcile/backend/LSP)
├── unify/
│   ├── Unifier.scala           (pattern unification; pure definitional equality; carrierRoles map; flushPostponed)
│   ├── UnifyResult.scala       (Unified / Contradiction)
│   ├── UnifyError.scala        (context + optional expected/actual)
│   └── SemValuePrinter.scala   (human-readable SemValue rendering for error messages)
├── fact/
│   ├── GroundValue.scala       (output: Direct, Structure, Type)
│   ├── MonomorphicValue.scala  (runtime output fact: signature + runtime, keyed by (vfqn, typeArgs))
│   ├── CompilerMonomorphicValue.scala (compiler-track output fact — a DISTINCT type; cannot name MonomorphicValue.Key)
│   ├── MonomorphicExpression.scala (output expression ADT; type slots are ground)
│   ├── NativeBinding.scala     (Platform-keyed: vfqn → SemValue for the evaluator)
│   ├── ContributedBinding.scala (Key(vfqn, label); NOT platform-keyed — one contribution serves both tracks)
│   ├── BindingContribution.scala (Leaf(SemValue) | Body(SaturatedValue))
│   └── BodyValueReferences.scala (memoized per-value body reference set for transitive binding prefetch)
└── processor/
    ├── MonomorphicTypeCheckProcessor.scala   (runtime entry point → TypeStackLoop, Track.Runtime)
    ├── CompilerMonomorphicTypeCheckProcessor.scala (compiler entry point → TypeStackLoop, Track.Compiler; native-leaf boundary)
    ├── SystemNativesProcessor.scala          (Function → VNative→VPi, Type → VType, Bool true/false constants,
    │                                          integerLiteral, the Eq[Type] equals leaf)
    ├── DataTypeNativesProcessor.scala        (body-less Type-qualified names → inert VTopDef; pool-guarded)
    ├── MatchNativesProcessor.scala           (handleCases/typeMatch impls → VNative; pool-guarded)
    ├── DeclaringPool.scala                   (quiet two-pool membership probe used by the two leaf contributors above)
    ├── UserValueNativesProcessor.scala       (thin BodyContributorProcessor over the runtime user pool)
    ├── CompilerNativesProcessor.scala        (compiler-pool body supplier; ability-using values → reduced Leaf)
    ├── BodyContributorProcessor.scala        (shared base for the two user-category body suppliers)
    ├── BodyValueReferencesProcessor.scala    (emits BodyValueReferences off the SaturatedValue body walk)
    ├── BindingMergerProcessor.scala          (SINGLE owner of NativeBinding; category precedence + dependency closure)
    ├── BindingClosure.scala                  (closes a selected Body over its deps; reifyingWrap via binderRoles)
    └── ReducedBindingClosure.scala           (BindingClosure's twin over a reduced MonomorphicExpression)
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
| `VNative(paramType, fire)` | Primitive awaiting a concrete argument; `fire` yields its own stuck form when the arg is not concrete | SystemNativesProcessor (Function, integerLiteral, the Eq[Type] equals leaf), MatchNativesProcessor (handleCases/typeMatch), StdlibNativesProcessor (arithmetic, Bool `fold`/`&&`/comparisons) |
| `VTopDef(fqn, cached, spine)` | Lazy top-level definition (cached=Some) **and** applied type/value constructor (cached=None) | BindingClosure (cached body), DataTypeNativesProcessor (body-less), the FQN-preserving missing-binding fallback |
| `VStuckNative(fqn, spine)` | A **non-injective** native application (`add(x,y)`, `min`, `&&`, `fold`) stuck on not-yet-concrete args | a `VNative`'s `fire` when an argument is abstract |
| `VMeta(id, spine)` | Unsolved metavariable (there is **no** `expected` field) | Checker (`MetaStore.fresh`) |
| `VNeutral(head, spine)` | Stuck application on a rigid head. `NeutralHead` is **structural** (identity by constructor): `Param(level, name)` (bound vars), `Fresh(Origin.Unify/Quote, depth)` (binder-descending probes), `Reserved(Marker.BadApply/GuardProbe/Match)` (scope-less markers) | Evaluator (unresolved `ParameterReference` → `Param`), the `$bad-apply` fail-safe (`Reserved(BadApply)`) |

`VStuckNative` is deliberately **distinct from `VTopDef`** (D3): a native application is *not injective* (`add(1,3)` =
`add(2,2)`), so the unifier must never injectivity-decompose it and the quoter must fail loudly if one survives read-back.
`Evaluator.renormalize` re-fires a `VStuckNative` once its arguments become concrete.

### `applyValue` — the one application primitive (fail-safe fallback, F1)

`Evaluator.applyValue(f, x)` is exhaustive over every *applicable* head: `VLam`/`VPi` β-reduce, `VNative` fires (even
on a non-concrete argument — the native produces its own `VStuckNative`), and `VNeutral`/`VTopDef`/`VStuckNative`/`VMeta`
grow their spine. The two remaining heads — `VConst` and `VType` — are **not applicable**; reaching them can only happen
in an **ill-typed** program (one the checker has already, or is about to, reject). This case returns a **loud stuck
form** — a `VNeutral` on the reserved head `NeutralHead.Reserved(Marker.BadApply)` carrying the argument — *never* the old
identity-ish fallback that returned `x` (which silently collapsed `F[A]` to `A`). If a `$bad-apply` value survives to
read-back the strict `Quoter` fails it ("Cannot quote neutral value") ⤳ `PostDrainQuoter`'s "Cannot resolve type."; but a
program with a genuine type error aborts *before* quoting and reports that real diagnostic. Fail-safe: never a silent
wrong value. (Do not reintroduce the identity fallback — see anti-patterns.)

### Type/value constructors are body-less `VTopDef`s

`DataTypeNativesProcessor` binds every body-less `Type`-qualified name (an abstract `type`, or a `data` type's
constructor) to `VTopDef(fqn, None, SNil)`. Applying type args just grows its spine. So `Box[Int]` is
`VTopDef(BoxFQN, None, [VTopDef(IntFQN, …)])` — **not** a `VConst(Structure)`. `Structure(...)` only appears after
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

**Γ and ρ are separate (the textbook NbE-checker shape).** `CheckState` holds two `Env`s, grown in lockstep:

- **`gamma` (Γ)** — name → its **type**. Read only by `Checker.infer`'s `ParameterReference` (`state.gamma.lookupByName`).
- **`rho` (ρ)** — name → its **value**, the env the evaluator consumes (`makeEvaluator.eval(rho, …)`). An erased
  type parameter binds its `groundToSem` value; a runtime value parameter binds a **fresh neutral**
  (`paramNeutral`, `Param(rho.level, name)`) standing for its unknown runtime value; a peeled instantiation meta binds
  the meta.

Three `bind*` methods keep them in sync: `bindValueParam(name, type)` (Γ=type, ρ=neutral — runtime value params),
`bindTypeStackParam(name, type, value)` (Γ=type, ρ=value — erased explicit type args, both computed from the ground arg
in `applyTypeArgs`), `bindTypeParam(name, meta)` (Γ=ρ=meta — peeled leftover type params). `Checker.check`
(`FunctionLiteral` vs `VPi`) binds the neutral in ρ and checks the body against **`codomain(paramNeutral)`** — genuine
dependent Π, never `codomain(paramType)`. (Every current-Eliot `VPi` codomain is constant, so the two agree today; the
neutral is correct once runtime-value-dependent types land.) `monoEnv` (the reification env in `PostDrainQuoter`) is
just ρ captured before the body check; `PostDrainQuoter.isRuntimeParam` reads runtime-ness off ρ (a name bound to a
neutral) rather than threading a separate `runtimeParams` set.

### Three evaluators, one traversal

`NbeEvaluator[E]` is the single pure, syntax-directed traversal. Its subclasses differ **only** in `decompose` (how a
node projects onto `NbeEvaluator.Term`):

- `Evaluator` (over ORE) — re-evaluates ORE type-argument sub-terms.
- `SemExpressionEvaluator` (over the checker's `SemExpression`) — reads the already-evaluated `SemValue` type args.
- `MonomorphicEvaluator` (over a reduced `MonomorphicExpression`) — **drops** the erased type arguments of a value
  reference (applying them would mis-bind a value parameter to an erased type arg).

Missing-binding fallback for a **value reference** is an FQN-preserving stuck `VTopDef(vfqn, None, SNil)` — **not** a
`VNeutral` (which would drop the FQN and corrupt read-back/codegen). A missing **parameter** reference falls back to
`VNeutral`.

### How computation runs

`force(v, metaStore)` walks solved metas and unfolds `VTopDef` when the cached body is present.
`renormalize(v, metaStore, lookupNative, deep)` additionally re-fires `VStuckNative`s whose args have concretised (and,
with `deep=true`, descends under `VPi` binders — used **only** post-drain at quote time, where every meta is solved).
`Checker.ensureBinding` prefetches transitively through the memoized `BodyValueReferences` fact (walked once per value),
so a native reached only through a bodied helper is already in the flat `bindingCache` the re-fire lookup consults.

## Pipeline

### Entry points (two tracks)

- `MonomorphicTypeCheckProcessor` → `TypeStackLoop.process(..., track = Track.Runtime)` → `MonomorphicValue`.
- `CompilerMonomorphicTypeCheckProcessor` → the **same** `TypeStackLoop.process(..., track = Track.Compiler)` →
  `CompilerMonomorphicValue` (a *distinct* fact type, so the compiler track cannot even name `MonomorphicValue.Key`;
  the `compiler-mono → runtime-mono` edge is impossible by construction — acyclic).

Both entry points first strip an ability-implementation *marker*'s pattern arguments
(`MarkerGuardSignature.strippedForGuard`) — a marker is monomorphized only to discharge its `where` guard, and its
pattern arguments are not real value parameters (they need not be of kind `Type`).

`NativeBinding`, `SaturatedValue`, `OperatorResolvedValue`, `UnifiedModuleNames`, `ModuleConstructors`, etc. are all
`Platform`-keyed (Runtime / Compiler). Each track unifies base + user program + its own layer independently.

### NativeBinding injection (ContributedBinding + BindingMergerProcessor)

`NativeBinding.Key(vfqn, platform)` maps a value FQN to its evaluator `SemValue`. It is produced by a **single owner**,
`BindingMergerProcessor` — replacing any first-registration race. The flow:

1. Every supplier emits a **total** `ContributedBinding` fact under its `label` — a `BindingContribution` when it defines
   a reduction for `vfqn`, else `None` (never declines). `ContributedBinding.Key(vfqn, label)` is **not** platform-keyed:
   one contribution serves both track merges. Labels:
   - **native category** (`langNativeLabels` = `system` / `datatype` / `match` / `compiler`, plus any `extraNativeLabelsKey`
     labels added by layers, e.g. stdlib's arithmetic/Bool natives):
     - `SystemNativesProcessor` (`system`) — `Function` → `VNative`→`VPi`; `Type` → `VType`; the compile-time `Bool`
       constants `true`/`false` (`VConst(Direct(Boolean))`); the value-position literal protocol `integerLiteral[V]`
       (reduces to `V`, so a literal feeding a compile-time computation — e.g. a `^Where` bound — evaluates); and the
       `Eq[Type]::equals` structural-equality leaf, bound to the impl-method FQN through the ability-impl marker. The
       `Bool` eliminator `fold` and the arithmetic natives are *stdlib*-owned (`StdlibNativesProcessor`) — the compiler
       special-cases nothing about them.
     - `DataTypeNativesProcessor` (`datatype`) — body-less `Type`-qualified constructors → `VTopDef(fqn, None, SNil)`,
       excluding the system-owned `Function`/`Type`. **Pool-guarded** (see `DeclaringPool`).
     - `MatchNativesProcessor` (`match`) — the `PatternMatch.handleCases` / `TypeMatch.typeMatch` impl FQNs → `VNative`s
       that reduce a concrete `VTopDef(ctor, None, spine)` scrutinee during checking. **Pool-guarded**.
     - `CompilerNativesProcessor` (`compiler`) — the compiler platform's Eliot-bodied reductions (the `add` pattern:
       ranked as a native so its reduction wins for *checking* while the runtime body is still what codegen reads). A
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
   active fact-request chain (`activeFactKeys`), not mutable state. Reified generic binders are wrapped as leading
   lambdas via `BindingClosure.reifyingWrap`, driven by the precomputed `SaturatedValue.binderRoles` (D6).

### TypeStackLoop — the signature kind-check

`TypeStackLoop.processIO` (`Checker` and `TypeStackLoop` hold a **`Track`**, not a bare `Platform` — see below; there
is **no `platform match` anywhere in the checking core**, only `track.<hook>` dispatch):

1. `walkTypeStack` — **derive the kind** from the signature's generic binders (`SignatureView.of(sig).binders` folded
   into `Function[K1,…,Type]`; a binderless signature has kind `Type`, checked directly), then fold `[kind, signature]`
   with `expectedType = VType`: for each level `check(level, expected)` then `expected = eval(level)`. **The fold body is
   identical for every level** — no "is this a type parameter?" branch. Generics emerge from `FunctionLiteral` levels
   checked against `VPi` kinds from above.
2. `applyTypeArgs` — apply explicit ground type args, every argument injected through the **one canonical
   `groundToSem` conversion** (a type or data value ⟹ its applicable constructor `VTopDef`, a `Direct` literal ⟹
   `VConst`) — the same form is applied to the signature closure and bound into ρ, so no ground value ever has two
   semantic forms. Over-application records one "Too many type arguments." error.
3. `instantiateRemaining` — peel leftover `VLam` closures (phantom / implicit type params) with fresh metas.
4. `recordAmbientCarriers` — record the value's own ambient effect-carrier heads (`SignatureView` carrier binders ∩
   `paramConstraints`, each looked up in ρ and recorded by forced head) into `CheckState.ambientCarriers` for the lift.
5. `track.pinCarriers` (compiler track only) — a `{Throw[E]}` carrier is fixed to the compile-time carrier `Either[E]`.
6. `track.settleReturnPosition` (mutually exclusive): a *calculated* return (`installReturnMeta`, W3 — track-independent),
   an *effectful guard* return (`dischargeGuardedSignature`, W2b — **runtime track only**, and never for an ability-impl
   marker: the marker's guard verdict must survive undischarged for the ability processor to interpret per candidate),
   or an ordinary explicit return.
7. `check` the runtime body against the signature.
8. `runPostDrainResolution` (D1, below), report unifier errors, abort before quoting if any error exists.
9. Fetch `track.implBindings` (compiler-only impl-body fetch); for a body-less *guarded marker*, reduce the guard's
   bodied sub-values per instantiation (`reduceGuardSubValues` via `ReducedBindingClosure.reduceInstance`) and
   re-evaluate the guard return against them (ability-guards Stage 4 — so a guard reaching its ability *through* an
   operator, `where E1 != E2` via `!=`, still collapses to a concrete verdict). Then read back via `PostDrainQuoter`
   through `track.readBackBody` — the compiler track **reduces** the body (`reduceSourced`), the runtime track
   structurally quotes it (`quoteSourced`).

### Track — the per-track strategy (no `platform match` in the core)

`check/Track` is a sealed trait with two case objects, `Track.Runtime` / `Track.Compiler`, each carrying its `platform:
Platform` plus the **four** places the two tracks genuinely differ — every former `platform match` conditional in
`TypeStackLoop`, extracted 1:1:

| Hook | Runtime | Compiler |
|---|---|---|
| `settleReturnPosition` (shared calc-return branch is `final`; `settleGuardedReturn` is the abstract hook) | `dischargeGuardedSignature` (W2b use-site discharge; skipped for ability-impl markers — `MarkerGuardSignature.isMarker`) | pass-through (producer leaves the undischarged carrier signature) |
| `pinCarriers` (+ `throwCarrierErrorType` / `pinCarrierToEither` / `throwAbilityFQN` live here) | no-op | pin `{Throw[E]}` carrier meta to `Either[E]` |
| `implBindings` | empty | fetch each drain-resolved ability impl's `NativeBinding` body |
| `readBackBody` | `quoter.quoteSourced` | `quoter.reduceSourced` |

`TypeStackLoop` / `Checker` take the `Track`; **fact keys read `track.platform`** (`Checker` keeps a private `val
platform = track.platform`, so the collaborators are still constructed with a bare `Platform`). The two entry
processors pass `Track.Runtime` / `Track.Compiler`. The `resolveAbility` seam is **two-arg** `(ValueFQN,
Seq[GroundValue]) => …`. `reduceSourced` physically lives in `PostDrainQuoter` (a read-back variant sharing its
machinery; `readBackBody` dispatches to it).

### Post-drain resolution (D1)

A fixed sequence, `TypeStackLoop.runPostDrainResolution` (no external design doc — it is described here and in its doc
comments):

- **Saturation** (`resolveAbilitiesToFixedPoint`): drain the unifier — so the resolver sees every solution the previous
  round injected — then try to resolve the still-unresolved ability references; loop while any reference newly resolves.
- **Finalization** (once): carrier-kind verification (`CarrierKindChecker.verifyCarrierKinds`), the
  calculated-return fail-safe, and **effect verification** (`EffectResidualChecker`, value monos only, not the
  signature twin): compute the value's residual effect set — the abilities demanded on its own ambient carrier — and
  require `residual ⊆ declared`. Runs here, after ability resolution and the final drain but before defaulting, so a
  reference's carrier argument is solved to the ambient (concrete `IO` or a still-abstract carrier meta) with the
  ambient identity intact. A finalization step can commit new solutions (the kind check unifies a solution's kind
  against its expectation), so the runner drains once more before defaulting.
- **Finalizer** (`defaultUnsolvedMetas`): every still-unsolved meta defaults to `VType` — what remains unsolved after
  the fixed point is an unconstrained (phantom) instantiation.
- **Postponement flush** (`Unifier.flushPostponed`): any constraint still postponed after the finalizer is an equality
  obligation the check never discharged — flushed to a hard mismatch error (after a triage re-drain; the one exemption
  is a `$bad-apply`-headed constraint, owned by a more precise fail-safe).
- **Postcondition** (`assertEveryMetaResolved`): every meta is solved — the compiler-bug backstop, `compilerAbort`ing
  on an internal invariant violation.

### Bidirectional checker & its collaborators

`Checker` is the **definitional-equality core only**: `check`/`infer`/`inferSpine`/`applyInferred`/`peelLams`/
`instantiatePolymorphic` plus the shared primitives (`force`, `freshMeta`, `doUnify`, `evalExpr`, `ensureBinding`,
`prefetchBindings`). Application checking is **spine-level** (`inferSpine`): the whole curried spine is decomposed at
the root and its argument slots resolved in two phases — **Phase A** (left to right) runs the resolution ladder per
slot, *deferring* a slot whose domain is a bare flex meta receiving an effect-carrier-headed argument; **Phase B**
re-forces each deferred domain — rigidified ⟹ full ladder (the `readLine.f` bind); still flex ⟹ the
**transparent/non-transparent split** on `occursIn(domainMeta, slotResultType)`: **occurs** (a *transparent* callee
whose result flows from the domain meta — `identity`, `const`, a data ctor) ⟹ pass-through *adoption* (solve the bare
domain meta to the carrier type via ordinary `doUnify`), letting the parent's slot decide; **absent** (a
*non-transparent* callee whose result carrier is independent of the domain — `putState[S, F](s: S): F[Unit]`, `S` not
in `F[Unit]`) ⟹ **bind-lift at this slot**, since adoption would strand the argument's carrier inside the type
parameter (never grounded → "contains unresolved variable"). This is what lets a direct-style `putState(f(state))` /
`updateState(f) = putState(f(state))` type-check. The check-mode **resolution ladder** per slot is: unify → bind-lift →
pure-wrap → mismatch, with *pre-arms* for the one shape unification can only postpone, never fail
(`?F[T'] ~ rigid-under-applied` and its pure-wrap dual). Four non-equality concerns live in collaborator modules, each
constructed at the top of `Checker` with **exactly the checker primitives it needs** (that narrow surface is the module
boundary), and invoked from named hook points:

| Collaborator — field | Concern | Hook points |
|---|---|---|
| `check/CalculatedReturnResolver` — `checker.calcReturns` (D7 + W2b) | non-local inference (fill a bare return from the callee's mono body) **and** effectful-guard discharge | `Checker.infer`/`applyInferred`; `TypeStackLoop` `installReturnMeta` / `dischargeGuardedSignature` |
| `check/CarrierKindChecker` — `checker.carriers` (D8) | HKT kind seeding + verification | `Checker.instantiatePolymorphic` → `recordCarrierMetas`; `TypeStackLoop` post-drain → `verifyCarrierKinds` |
| `check/EffectResidualChecker` — `checker.effectResidual` | effect *verification*: the residual `⊆` declared subset check (`Inf` included) + the declared-pure fail-safe. Discharge falls out structurally — a discharged effect rides an inner transformer carrier, not the ambient, so it is simply absent from the residual. This replaced the deleted pre-mono `effect/` phase; there is no `-E` syntax or discharge summary | `TypeStackLoop.runPostDrainResolution` post-drain (value monos only) |
| `check/AbilityResolver` — `checker.abilityResolver` | ability-ref collection + the `resolve-abilities` saturation pass (resolve each ability-qualified ref to its impl) | `TypeStackLoop.processIO` → `collectAbilityRefs`; `TypeStackLoop` post-drain → `resolveAbilities` |
| `check/EffectLifter` — `checker.lifter` | the effect auto-lift (docs/effect-lift-in-checker.md): the bind-lift / pure-wrap ladder arms + their doomed-postponement pre-arms, `effectCarrierSplit` (flagged metas + re-forced `ambientCarriers` heads), and the `Effect.flatMap`/`map`/`pure` `SemExpression` splices (`[C, T', R]` type args; `$eff$N` binders off `CheckState.liftCounter`; `bindWrap` unifies the bind's carrier with the core's) | the one shared `Checker.resolveLadder`/`resolveFailureLadder`, with the bind-lift arm gated by `allowBindLift` (`true` only at argument slots — a return boundary never strips a carrier, so it passes `false` and the doomed shape commits the eager mismatch) and pure-wrap on both; `typeImmediateLambda` (the let-bind rule: effectful bound value ⟹ sequence, with the continuation *inferred*, never checked against the carrier expectation); `inferSpine` → `wrapBinds` |

Per-meta carrier data (carrier kind / effect-carrier flag) lives in the **single** `CarrierRole` map on the `Unifier`;
the collaborators own the *algorithm*, not a private store.

### Carrier bookkeeping (`Unifier.CarrierRole`)

One `Map[Int, CarrierRole]` on the `Unifier` holds all per-metavariable bookkeeping. Only *higher-kinded*
instantiation metas (`[F[_]]` carriers) get an entry, seeded by `CarrierKindChecker.recordCarrierMetas`:

- `carrierKind` — the binder's expected kind plus a call-site context; verified post-drain (a carrier solved to a
  value of the wrong kind is rejected rather than silently accepted).
- `effectCarrier` — the effect lift's *callee-side* carrier notion, deliberately **unfiltered** (an effectful result
  rides *any* of the callee's own HKT binders, including `runStateToPair`'s unconstrained `G[_]`; the
  ability-constraint filter applies only to a value's own *ambient* carriers in `CheckState.ambientCarriers`).

Every meta without an entry is plain: solved by ordinary unification, else defaulted to `VType` by
`defaultUnsolvedMetas`.

### Guard discharge (W2b) — `CalculatedReturnResolver`

Sits beside the calc-return back-edge because both are "run a compile-time computation to obtain the return type." A
return-type expression may be a `{Throw[String]}` computation on the compile-time `Either[String, _]` carrier (the
compiler is the handler). Three hook points recognise the carrier's `Right`/`Left` **by FQN**
(`WellKnownTypes.eitherFQN` / `rightFQN` / `leftFQN`) and reduce `fold`/`foldEither` via the merged `NativeBinding`:
`isGuardCarrier` (the *kind* position — accept an `Either[..]`- or `Bool`-valued return where a bare `Type` is
expected), `dischargeGuardedSignature` (the *callee* side — `Right(t)` ⤳ the published type `t`, `Left(msg)` ⤳ a
diagnostic, an abstract-bounds guard deferred to the body via a return meta), and `dischargeGuardedReturn` (the
*applied / by-name read* sides). The `Left`/`Right` payload convention and the fallback rejection message live in
`GuardChannel`, shared with the ability processor's `where`-verdict interpreter so the two cannot drift. The compiler
track skips discharge (it is the *producer*: `pure(A)` ⤳ `Right(A)`, publishing the undischarged carrier signature for
a consumer to discharge); the runtime track also skips it for an ability-impl *marker* (`MarkerGuardSignature`), whose
verdict the ability processor interprets per candidate.

### The refinement channel (`channel/`) — downstream of typing

`RefinementChannelProcessor` is a **post-pass** over each runtime `MonomorphicValue` (producing the `RefinementTable`
fact), not part of checking: it walks the fully-ground body bottom-up and computes each node's **meta value** (an
opaque domain `GroundValue`, e.g. `Int$Meta(Interval(lo, hi))`) by flow. A literal seeds via the `integerLiteral^Meta`
companion; a call whose callee declares a `^Meta` companion computes a **transfer** (the `Numeric[Int]` `add^Meta`) or a **merge**
(`fold^Meta`, `Meta.join` over the arms) by reducing that companion through the one NbE evaluator — no leaf and no
branch construct is named, the companion is the sole recognition point. A def's `where` precondition (its `^Where`
companion) is demanded at every full call over the arguments' metas — an unknown (⊤) range or a `false` verdict is a
hard use-site error. Everything else is ⊤ (no entry; bignum layout — sound, wide, never wrong); lambda bodies are
walked (so nested `where` demands fire) but never record. Consumers: the reconcile pass stamps the table onto the body,
the JVM backend decodes machine widths from it, the LSP hover shows value ranges. Refinements flow strictly
**downstream** of type formation — they never feed back into the checker or unifier.

### PostDrainQuoter — the sole SemValue→GroundValue transition

`Checker.forceAndConst` no longer exists. Read-back happens exactly once, post-drain, in `PostDrainQuoter`:

- `quoteSem` deeply renormalises (so a stuck native in an intermediate `VPi` codomain re-fires) then calls the strict
  `Quoter.quote`. On a `Left` it aborts with `"Cannot resolve type."` (detail = the quoter message) — **no** silent
  `Type` fallback.
- The strict `Quoter` succeeds only on `VType`, `VConst`, `VPi` (→ Function `Structure`), and body-less
  `VTopDef(fqn, None, spine)` (→ `Structure`). It **fails loudly** on `VNeutral`, `VMeta`, `VLam`, `VNative`,
  `VStuckNative`, and an unapplied cached `VTopDef`.
- `PostDrainQuoter` also hosts the **compile→runtime reification gate** (materialising erased-parameter-determined
  sub-terms into literal/constructor trees), the **`integerLiteral[V]` quote-time rewrite** (a value-position literal
  reads back as a plain `IntegerLiteral` node, so no `integerLiteral` reference survives into the monomorphic tree),
  and the compiler track's `reduceSourced` (evaluate + read back the reduced compile-time body, folding resolved
  ability impls in).

### Unifier — pure definitional equality

Pattern unification on `SemValue`s with a `postponed` queue (`drain()` retries until stable):

1. Force both sides. `VType`~`VType` ✓. `VConst`~`VConst` → structural `GroundValue` equality.
2. `VPi`/`VLam` → extend with a fresh rigid var, unify under the binder. Eta: `VLam` vs other → unify `c(fresh)` against
   `apply(other, fresh)`.
3. `VNeutral`~`VNeutral` same head → zip spines. `VTopDef`~`VTopDef` same FQN → zip spines (definitional equality: a
   constructor applied to differing arguments is rejected).
4. `VStuckNative`~`VStuckNative` same FQN → zip spines; it is **never** injectivity-decomposed against a meta (non-injective).
5. `VMeta` → pattern rule: empty spine → solve directly (with an **occurs-check**); non-empty spine → `tryDecomposeApplied`
   (injectivity decomposition against a *rigid* `VTopDef(None)` / `VNeutral` head, equal-arity or partial-application),
   else postpone. A carrier meta postponed against a non-rigid `VPi` (`?F[A,B] ~ Function[A,B]`) is **deliberately not
   decomposed** — `CarrierKindChecker` documents this as higher-order unification the pattern unifier cannot solve
   (see the diagnosing note on the two-arg HKT-over-Function limitation). **Flex-flex `?F[a..] ~ ?B`** (applied meta vs a
   *bare, unsolved* meta) solves the bare meta **in either orientation** — `?B := ?F[a..]` — rather than postponing when
   the applied side is on the left. Without it, a carrier application aliased through a polymorphic combinator (the `.`
   operator's result `?B := ?carrier[payload]`) stays hidden behind `?B`, so the effect-lift never sees the argument as
   carrier-headed — the bug that broke dot-chained effect discharge (`x.provide(a).provide(b)` fully discharged to a pure
   result). Pinned by `unify/MetaApplicationUnifyTest`.

There is **no** assignability / widening / `refinements` map in `unify` — and no directional coercion anywhere in the
checker since the refinement channel's flag day (`Int` is nullary; value ranges ride the `channel/` post-pass, strictly
downstream of typing). The one deliberate side-door is the Phase-B pass-through *adoption*
(`Checker.resolveDeferredSlot`): solving a bare domain meta to the carrier-headed argument type via ordinary `doUnify`,
taken only for a *transparent* callee — one whose domain meta occurs in the call's result; a non-transparent callee
bind-lifts at the slot instead.

`flushPostponed` runs after the finalizer: still-postponed constraints become hard mismatch errors (fail-safe — see the
post-drain sequence above). Pinned by `unify/PostponedFlushTest`.

### Two-pool membership guards (leaf contributors)

`ContributedBinding` is not platform-keyed, but a leaf contributor must read a *platform-keyed* source fact (a value, its
constructors) to build a reduction. Requesting that fact on a fixed pool a name is absent from would trip
`UnifiedModuleValueProcessor`'s "Could not find" build error. So `DataTypeNativesProcessor` and `MatchNativesProcessor`
ask `DeclaringPool.of(vfqn)` first — a quiet `UnifiedModuleNames` (name-set) probe that returns the pool a name is
declared in (preferring `Runtime`, falling back to `Compiler`, else `None`) — and read the source fact only on that
pool. `CompilerNativesProcessor.inCompilerPool` and `CompilerMonomorphicTypeCheckProcessor.inRuntimePool` use the same
quiet-probe pattern.

## The hard rules

1. **No concept of "generic parameters" in the checker's fold.** `TypeStackLoop`/`Checker` never classify a signature's
   binders as "type params" vs "value" to drive a *typing or equality* decision; the fold over `[kind, signature]` is
   uniform (no `TypeParameterAnalysis`). *However*, sanctioned **peripheral** readers legitimately read binder structure
   through `SignatureView` — do **not** mistake them for a violation:
   `walkTypeStack` itself (reconstructs the kind `Function[K1,…,Type]` from the signature's binders to kind-check
   against — the kind is a projection of the signature, never stored),
   `CarrierKindChecker.recordCarrierMetas` (seeds carrier kinds off the referenced value's `SignatureView`),
   `AbilityResolver.abilityArity` (reads an ability-marker's binder count for impl queries),
   `SaturatedValue.binderRoles` feeding `BindingClosure.reifyingWrap` (which leading binders a body reifies), and
   `TypeStackLoop.recordAmbientCarriers` (the value's own ability-constrained HKT binders, seeding
   `CheckState.ambientCarriers` for the effect lift). None of these drives a definitional-equality decision.
2. **The evaluator produces VLam; the Checker produces VPi.** Never produce `VPi` in the evaluator. The `Function` native
   is a `VNative` that fires to `VPi`.
3. **No ORE rewriting.** ORE is read once into `SemValue` and forgotten. All substitution is closure application.
4. **No constraint set.** Unification is immediate and local; the only deferral is the unifier's `postponed` queue.
5. **NativeBinding pre-fetching.** The evaluator is synchronous and pure. All bindings are prefetched into
   `CheckState.bindingCache` (via `prefetchBindings`/`ensureBodyBindings`/`BindingClosure.collectBindings`) before
   evaluation.
6. **State via `CheckIO`.** State is threaded through `StateT[CompilerIO, CheckState, *]` — `get`/`modify`/`inspect`,
   not in-place mutation. Bind a parameter with `state.bindValueParam(name, type)` (runtime param: Γ=type, ρ=neutral),
   `state.bindTypeStackParam(name, type, value)` (erased type-stack param: Γ=type, ρ=value), or
   `state.bindTypeParam(name, meta)` (peeled type param: Γ=ρ=meta). Never overload one env for type and value — Γ
   (`gamma`) and ρ (`rho`) are separate.
7. **No silent read-back fallback.** `applyValue` on a non-applicable head yields a loud `$bad-apply` stuck neutral; the
   quoter fails loudly on every stuck form; still-postponed constraints are flushed to hard errors (`flushPostponed`).
   Nowhere does a stuck value silently become `Type`.
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
- **A `where` precondition silently not demanded.** The demand fires only at a *full* call (`MonomorphicValue.
  naturalArity`) of a callee whose module declares the `^Where` companion; check `MetaWhereDesugarer` emitted it and the
  compiler pool sees the module.

## Anti-patterns (reject in review)

- **Classifying a signature's binders as type-params-vs-value to drive a typing or equality decision.** The fold is
  uniform. (The sanctioned peripheral `SignatureView` readers in Hard Rule 1 — including `walkTypeStack`'s kind
  derivation — are the *only* binder-structure reads, and none drives equality.)
- **Producing `VPi` in the evaluator**, or `VLam` in the Checker when the expected type is `VType`.
- **ORE substitution inside monomorphize.** All binding is closure capture in Scala.
- **Calling `CompilerIO` from an evaluator.** The evaluators are synchronous and pure; prefetch bindings first.
- **Adding a constraint list / worklist.** Unification is immediate; the only deferral is the unifier's `postponed` queue.
- **Eagerly allocating metas for type parameters at `ValueReference`.** Implicit instantiation is driven lazily by
  `FunctionApplication` / `instantiatePolymorphic` / `instantiateRemaining`.
- **Using `Map[String, SemValue]` for the environment hot path.** Env is `Vector[SemValue]`; the evaluator resolves
  parameters by name via `Env.lookupByName`.
- **Re-introducing assignability / widening / a `refinements` map into `unify`.** `unify` is pure definitional equality.
  Value-range refinements live in the `channel/` post-pass, strictly *downstream* of typing — never in the unifier or
  the checker.
- **Making bind/`pure` decisions from declared signatures outside the checker.** Whether an effectful term in a slot is
  sequenced (`Effect.flatMap`/`map`) or passed through is undecidable from a callee's declaration (the same generic slot
  needs opposite answers at different instantiations — the `readLine.f` probe pair); it is `EffectLifter`'s check-mode
  elaboration per concrete instantiation. Never grow a pre-typing phase (or any consumer of `CalleeSignatures`) a
  bind-position/branch-position/machinery-sniffing heuristic — that is the shadow type system the single-evaluator rule
  bans, and it is exactly what docs/effect-lift-in-checker.md removed.
- **Re-inlining a non-equality side-car into `Checker`.** A new lattice relation / inference back-edge / kind rule gets
  its *own* collaborator constructed with injected primitives — never grown into `Checker`, never folded into `unify`.
- **Special-casing a concrete type (e.g. `Int`) in the checker/unifier.** Recognise the *ability protocol* by name
  (`eitherFQN`, `PatternMatch`/`TypeMatch`, the `^Meta`/`^Where` companion namespaces), never the type.
- **Returning `false` (not a `VStuckNative`) from a native on non-concrete args.** `&&`/`fold`/arithmetic must stay stuck
  so the unifier still solves metavariables; `false` would wrongly reject generic/open comparisons.
- **Injectivity-decomposing a `VStuckNative` against a meta.** A native application is non-injective; it must postpone.
- **Re-introducing a silent fallback in `applyValue` or any read-back path.** Applying an argument to a non-applicable
  head must yield the loud `$bad-apply` stuck form; stuck values must fail the quoter loudly; unsolved metas default to
  `VType` only through the finalizer, with the postponement flush as the equality backstop.
- **Reading a runtime-pool fact in a leaf contributor without a membership probe.** A `ContributedBinding` is not
  platform-keyed; probe `DeclaringPool` / `UnifiedModuleNames` before requesting a platform-keyed value, or a
  compiler-pool-only name pollutes the build with a spurious "Could not find" error.
- **Naming `MonomorphicValue.Key` from the compiler track.** It would create the forbidden `compiler-mono → runtime-mono`
  edge; the compiler track produces `CompilerMonomorphicValue` only.

## Testing

Tests live under `lang/test/src/com/vanillasource/eliot/eliotc/monomorphize/`:

- `processor/MonomorphicTypeCheckTest.scala` — the main end-to-end checker cases (inference, HKT decomposition, carrier
  kinds, dependent bounds, calculated returns W3/W4, guard signatures). **Extend these named end-to-end cases** rather
  than mock-unit-testing a collaborator (the collaborators were pure moves; their behaviour is pinned here).
- `processor/MonomorphicTypeCheckProcessorTest.scala` — processor-level cases.
- `processor/MatchNativesProcessorTest.scala` — `match` reduction via `handleCases`/`typeMatch` natives.
- `processor/MonomorphizationVersioningTest.scala` — per-instantiation keying.
- `processor/ReificationTest.scala` — the compile→runtime reification gate.
- `processor/ComputedTypeArgumentReadbackTest.scala` — computed type-argument read-back.
- `processor/EqOperatorResolutionTest.scala`, `processor/EqTypeLeafTest.scala` — the `Eq[Type]` structural-equality leaf
  and its resolution through `==`/`!=`.
- `processor/CompilerMonomorphicTypeCheckProcessorTest.scala`, `processor/CompilerNativesProcessorTest.scala`,
  `processor/CompilerNativeLeafBoundaryTest.scala`, `processor/CompilerEitherCarrierTest.scala`,
  `processor/CompilerAbilityResolutionTest.scala` — the compiler-as-platform track (register a `Platform.Compiler`
  `PathScan` for the compiler pool; see `CompilerNativesProcessorTest`'s `twoPoolFacts` template).
- `processor/CompilerOnlyDataNativesTest.scala` — the `DeclaringPool` compiler-pool fallback for the datatype native.
- `check/CarrierBookkeepingTest.scala` — ambient-carrier heads + effect-carrier meta flagging, via the
  `TypeStackLoop.processWithState` test seam.
- `check/EffectLifterTest.scala` — the effect auto-lift end-to-end cases (bind/pure arms, deferral, let-bind rule).
- `channel/RefinementChannelProcessorTest.scala` — the refinement channel's flow analysis and `^Where` demands.
- `eval/EvaluatorApplyValueTest.scala` — the F1 loud `$bad-apply` fallback shape + quoter failure.
- `unify/UnifyResultTest.scala`, `unify/OccursCheckTest.scala`, `unify/StuckNativeUnifyTest.scala`,
  `unify/MetaApplicationUnifyTest.scala`, `unify/PostponedFlushTest.scala`, `unify/CarrierRoleTest.scala` — pure unifier
  unit tests (construct `SemValue`s directly; `AnyFlatSpec`).

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
- `NativeBinding.Key(vfqn, platform)` — value FQN → evaluator `SemValue`, per platform.
- `ContributedBinding.Key(vfqn, label)` — **not** platform-keyed; one supplier's total answer per label.
- `BodyValueReferences.Key(vfqn, platform)` — the memoized reference set of a value's checking body (transitive
  binding prefetch).
- `RefinementTable.Key(vfqn, typeArguments)` — keyed exactly like `MonomorphicValue.Key`; the per-node meta values of
  one monomorphic instance (the refinement channel's output).
- `MonomorphicExpression.expressionType: GroundValue` — always fully ground; no free variables or unsolved metas.
