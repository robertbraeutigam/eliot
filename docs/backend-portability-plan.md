# Extracting Generic Mechanisms From the JVM Backend Into `lang`

Status: **In progress.** W1, W2, W4, and W6 are implemented (see their entries); W3, W5, W8 remain, and W7's
recommended shared-key portion is already satisfied by W6 (only the optional pre-dedup fact is outstanding). This is
a survey of what currently lives in the `jvm` module but is platform-independent, and a sequenced plan for moving
it into `lang` so that writing a second backend (the microcontroller target) is easier and safer.

> **Update (post-`2590fc06`).** Since this plan was written, the `ModuleConstructors` index landed (commit
> `2590fc06`, "Introduce ModuleConstructors/ModuleAbilities indices; collapse UnifiedModuleNames fan-in"). It is
> exactly W2's constructor-grouping fact — realized as a **module-keyed** index rather than a data-type-FQN-keyed
> `DataTypeLayout`, but with the same effect: one ordering authority (`ModuleConstructorsProcessor`) now feeds the
> backend, `MatchNativesProcessor`, and `DataMatchDesugarer`, and the backend's signature-stripping reconstruction
> is deleted. W2 is **DONE**; findings 4–5 and the inventory below are updated accordingly.

## Goal

Make the JVM backend as thin as it can be, so a new backend is a small, low-risk amount of code. Concretely:
identify every mechanism, algorithm, or piece of logic in `jvm/src` that does **not** depend on JVM specifics
(ASM, class-file format, `java.lang.*`) and move it into `lang`, leaving the backend to do only what is
irreducibly platform-bound.

### The "ideal backend" target

Recent front-end work established a repeatable pattern: **reduce a language feature to ordinary core terms, or
to a single named leaf, *before* code generation.**

- **Reification** (`PostDrainQuoter` + `SemExpressionEvaluator`) materialises erased value-position constants
  into plain `IntegerLiteral` / constructor-tree nodes at the `SemExpression → MonomorphicExpression` boundary,
  so the backend sees literals, never the compile-time computation that produced them.
- **Coercion** (`Checker.unifyOrCoerce` / `coercionPayload` / `buildCoercedExpr`) resolves `Coerce` in check
  mode and splices the instance's `some` payload — an ordinary `nativeWiden` call — at the use site. The
  backend sees a normal call to one leaf, not a conversion protocol.
- **Combine** (`WellKnownTypes.combinedFQN`, resolved in the checker) is type-only; the backend never sees it.
- **`RepresentationLowering`** already runs generically in `lang` (folded into `uncurry`) and rewrites
  `Int[MIN, MAX]` to a `Jvm*` representation type before any descriptor is computed.

Measured against that target, the *ideal* backend handles only:

1. **literals** (integer, string),
2. **constructor allocation** and field access,
3. **lambda** creation (closure conversion) and **application**,
4. a **representation-FQN → machine-type** map, and
5. a small set of named **intrinsic leaf** emitters (`nativeAdd…`, `nativeWiden`, `intToString`, `println`).

Everything else the current backend does is generic and should move. This document enumerates the gaps between
today's backend and that target.

## Key findings from the codebase

1. **The architecture already endorses this direction.** `RepresentationLowering.scala:12-21` states the rule
   is "general and never names `Int`"; `NativeType.scala:36-47` notes the five `Jvm*` entries are "the entire
   backend type knowledge of integer widths." The (now-complete) jvm-`Int` representation work achieved "no
   `Int`-specific code in the backend" — all width policy lives in the jvm layer's `Int.els`, the backend only maps
   representation types to layouts and realises fixed leaf natives. The work below continues that trajectory for the
   *non-Int* mechanisms.

2. **`WellKnownTypes` is the established home for by-name protocol constants.** It already centralises
   `coerceFQN`, `combinedFQN`, `someFQN`/`noneFQN`, `boolFoldFQN`, `integerLiteralFQN`, and the `BigInteger`
   compile-time natives. But the two *older* dispatch protocols — `PatternMatch` and `TypeMatch` — are **not**
   there; they are hardcoded string literals in the backend (see finding 4).

3. **The IR the backend consumes is already mostly representation-typed.** `UncurriedMonomorphicValue` /
   `UncurriedMonomorphicExpression` carry `GroundValue` types that have passed through `RepresentationLowering`.
   `GroundValue` exposes exactly the generic queries a backend needs (`asFunctionType`, `typeFQN`,
   `deepReturnType`, `functionArity`, `extractParamAndReturnTypes` — `GroundValue.scala:32-69`).

4. **Several backend mechanisms re-derive or re-encode information that is generic.** The large ones (the first is
   now **resolved** — see W2):
   - ~~`JvmClassGenerator` reconstructs constructor → data-type grouping by re-parsing `OperatorResolvedValue`
     signatures.~~ **Resolved (W2).** The backend now reads `ModuleConstructors.byDataType` for grouping/ordering;
     the `evaluateConstructorDataType` / `strip*` / `extractReturnTypeRef` helpers are deleted from the backend.
   - `JvmClassGenerator` / `ExpressionCodeGenerator` recognise `PatternMatch`/`TypeMatch` by hardcoded strings
     (`JvmClassGenerator.scala:59,79,86`; `ExpressionCodeGenerator.scala:123,135,204`).
   - `DataClassGenerator` hand-builds a Church/Scott encoding of the data eliminator in bytecode
     (`DataClassGenerator.scala:188-321`).
   - `LambdaGenerator` computes a lambda's captured free variables (`LambdaGenerator.scala:163-175`) and peels
     N-ary lambdas into nested single-arg closures (`LambdaGenerator.scala:43-56`).

5. **The match-dispatch logic the plan wants to extract is already written once in `lang` — and now the ordering
   authority is genuinely shared (W2 done).** The constructor grouping/ordering — a data type's value constructors
   in source-declaration order, read from `RoleHint.ValueConstructor` — now lives in **one** place,
   `ModuleConstructorsProcessor`, producing the `ModuleConstructors` fact. Its three former re-derivers all consume
   it: `MatchNativesProcessor.orderedConstructors` (`monomorphize/processor/MatchNativesProcessor.scala:70-71`, the
   native-emitting half that builds `handleCases`/`typeMatch` as pure `VNative` `SemValue`s), `DataMatchDesugarer`
   (the syntactic half), and the JVM backend (`JvmClassGenerator`). The **caveat this finding raised — `RoleHint`
   does not reach the backend, so the layout must surface in a fact the backend already reads — was the design
   driver**: `ModuleConstructors` is that fact. What W3 still targets is the *other* half of the duplication:
   `MatchNativesProcessor` encodes the Church selector (`churchSelector`) and field application
   (`applyHandlerToFields`) as `SemValue`s, while the backend's `DataClassGenerator` re-encodes the same selector
   chain in **bytecode**. Sharing that *shape* (not just the ordering) is what remains.

## Solution-strategy preference

The dispositions below are chosen against an explicit preference ordering for *where* extracted logic should
live, from most to least preferred:

1. **A method/function on an existing fact** (or an extension on an IR type). Best when the logic is a pure
   function of data already on the fact — no new fact, no storage, no staleness. Applies to W4, W6 (both **DONE**).
2. **Denormalized data carried on an existing fact.** Use when the value is needed repeatedly downstream and
   recomputing is wasteful, or when it must be fixed at an earlier phase that has information later phases lack.
   Applies to W5 (capture sets per peeled frame).
3. **A new fact + processor.** Acceptable *only* when it offloads genuinely new, closed functionality — a
   self-contained computation that no existing fact can host as a method (typically a cross-value aggregation or
   a synthesis step). Applies to W2, W3, W8, and the optional form of W7.

Each work item records its **Disposition** (the chosen strategy and any change from this document's original
sketch) and its **Open questions**.

## Inventory: generic vs. genuinely backend-specific

| Mechanism | Location | Verdict |
|---|---|---|
| Constructor → data-type grouping, return-type extraction | `JvmClassGenerator.scala:48-56` | **DONE** → `ModuleConstructors` fact (W2) |
| PatternMatch/TypeMatch recognition by name | `JvmClassGenerator.scala:56-99`; `ExpressionCodeGenerator.scala:119-160` | **generic** → `WellKnownTypes` + plan fact |
| Church/Scott `handleCases` encoding | `DataClassGenerator.scala:118-321` | **generic algorithm** → core desugaring |
| `typeMatch` instanceof dispatch structure | `DataClassGenerator.scala:366-423` | **generic structure**, emission JVM |
| Free-variable / capture analysis | `LambdaGenerator.scala:163-175` | **DONE** → `freeVariables` method on IR (W4) |
| N-ary lambda → nested single-arg peel | `LambdaGenerator.scala:43-56` | **generic** → `uncurry` normalization (W5) |
| Erasure (`valueType`) | `CommonPatterns.scala:14-22` | **DONE** → `GroundValue.carrierFQN` (W6); FQN→class JVM |
| Monomorphic-instance dedup by representation | `JvmClassGenerator.scala:233-276` | **generic** → shared key method (W7); pre-dedup optional |
| "body-less def with no native = error" | `JvmClassGenerator.scala:315-317` | **generic guard** → lang check |
| Intrinsic FQN set / width-suffix names | `Intrinsics.scala:35-59` | duplicated stdlib↔backend |
| ASM emission, descriptors, verifier | `asm/*`, `MethodGenerator`, `ClassGenerator` | **JVM — keep** |
| `java.lang.*` / `BigInteger` map, `Jvm*` types | `NativeType.scala:29-129` | **JVM — keep (the platform layer)** |
| JAR / manifest generation | `jargen/*` | **JVM — keep** |

## Work items

Each item lists: what is there now, why it is generic, where it should live, a design sketch, and effort/risk.

---

### W1 — Promote `PatternMatch`/`TypeMatch` to `WellKnownTypes` constants — **DONE**

Implemented: `WellKnownTypes` now carries `patternMatchAbilityName` / `patternMatchHandleCasesName` /
`typeMatchAbilityName` / `typeMatchMethodName` plus the recognition helpers `isPatternMatchImplementation`,
`isPatternMatchHandleCases`, `isTypeMatchImplementation`, `isTypeMatchTypeMatch`. The backend
(`JvmClassGenerator`, `ExpressionCodeGenerator`) and the `lang`-side `MatchNativesProcessor` (which duplicated the
exact same predicates) now reference these instead of literal strings; `MatchNativesProcessor`'s private
`isHandleCasesImpl`/`isTypeMatchImpl`/`isAbilityImpl` were deleted. The generated-bytecode `handleCases` *method*
names (`JvmIdentifier.encode("handleCases")`) were intentionally left as backend emit/call artifacts (no
lang↔backend drift). Whole suite green (`./mill lang.test jvm.test`). Remaining literal `"PatternMatch"`/
`"TypeMatch"` strings live in the *construction* side (`DataDefinitionDesugarer`, `CoreProcessor`,
`TypeMatchDesugarer`, `DataMatchDesugarer`, `ModuleName.defaultSystemModules`); routing those through the
constants is deferred to avoid an object-initialization cycle with `ModuleName` and is not needed for backend
portability.

**Now.** The backend matches ability implementations by raw strings:
`abilityName.value === "PatternMatch"`, `vfqn.name.name === "handleCases"`,
`abilityName.value === "TypeMatch"`, `calledVfqn.name.name === "typeMatch"`, and the singleton naming
`"PatternMatch$" + … + "$impl"` (`ExpressionCodeGenerator.scala:204`). These appear in at least five places
across two files.

**Why generic.** These are language-level protocols defined in `lang/eliot/eliot/lang/PatternMatch.els`
and `TypeMatch.els`. The newer `Coerce`/`Combine` protocols are already named in `WellKnownTypes`; these two
predate that convention and never got the same treatment. Any backend needs to recognise the same names.

**Where.** Add to `WellKnownTypes`:
`patternMatchAbilityName`, `handleCasesFQN` (or at least the local name), `typeMatchAbilityName`, `typeMatchFQN`,
plus a helper to test "is this VFQN a `PatternMatch.handleCases` impl method" / "…a `TypeMatch.typeMatch` impl".

**Effort/risk.** Low / low. Pure constant extraction; no behaviour change. Good first commit — it also makes
W2 cleaner.

---

### W2 — Constructor grouping as generated data, not backend reconstruction — **DONE** (as `ModuleConstructors`)

Implemented as a **new fact + processor** (preference #3), but keyed by **module** rather than by data-type FQN as
the original sketch proposed. The fact is `ModuleConstructors`
(`lang/.../module/fact/ModuleConstructors.scala`), built by `ModuleConstructorsProcessor`
(`lang/.../module/processor/ModuleConstructorsProcessor.scala`, commit `2590fc06`):

```scala
case class ModuleConstructors(
    moduleName: ModuleName,
    byDataType: Map[QualifiedName, Seq[ValueFQN]],  // ctors in source-declaration order; index == position
    typeConstructors: Seq[ValueFQN],
    platform: Platform = Platform.Runtime
)
```

**What it captures vs. the original `DataTypeLayout` sketch.** Same core content — value constructors grouped by
data type in source-declaration order (index is the position in the `Seq`) — plus the module's type constructors,
which the backend also needs. Two deliberate simplifications, both anticipated by the original disposition:
- **Structure only, no field types** — `byDataType` carries `ValueFQN`s, not `ConstructorLayout` records; the
  backend still reads concrete field types/counts from `UncurriedMonomorphicValue.parameters`, exactly as W2's
  "layout carries structure only" note predicted. (The disposition also floated carrying `fieldCount`; that was
  dropped as unnecessary — the count is recoverable downstream.)
- **Module-keyed, not data-type-FQN-keyed** — this sidesteps the original open question about a constructor's
  module always equalling its data type's module: the processor *enumerates the module's own names* and groups
  them, so the invariant is structural, not an assumption. The one-processor scan reads `RoleHint.ValueConstructor`
  off `UnifiedModuleValue`, sorts each data type's constructors by `qualifiedName.range.from`.

**One ordering authority replaces three (the "reuse, not just removal" goal).** All three former re-derivers now
consume `ModuleConstructors`:
- `MatchNativesProcessor.orderedConstructors` reads it (`.of(dataType)`) instead of scanning `RoleHint` itself.
- `DataMatchDesugarer` (the syntactic half of `match`) reads it.
- `JvmClassGenerator` reads `byDataType` for grouping; its `evaluateConstructorDataType` / `stripFunctionLiterals`
  / `stripApplicationTargets` / `stripCurriedReturnType` / `isFunctionTypeApplication` / `extractReturnTypeRef`
  helpers are **deleted**.

Whole suite green. This unblocks W3 (the layout/ordering it needs is now a fact the backend already reads).

---

### W3 — Synthesize `handleCases` / `typeMatch` implementations as core terms

**Now.** `DataClassGenerator.generateSelectorLambdas` / `generateSelectorContinuation` /
`generateSelectorTermination` (`DataClassGenerator.scala:188-321`) build, directly in bytecode, a chain of
selector closures that implement Church/Scott-encoded pattern matching: the i-th selector either carries the
running handler forward or, at the target constructor, applies the chosen case handler to the constructor's
fields. `generateTypeMatch` (`DataClassGenerator.scala:366-423`) similarly emits an `instanceof`-based dispatch.
`matchdesugar` already lowers surface `match` into calls to `handleCases`; what is missing is the *body* of each
data type's `handleCases`, which the backend currently supplies in ASM.

**Why generic.** The selector chain is a Church/Scott encoding of the data eliminator — a pure function of the
constructor count, the target index, and the field list. It is expressible entirely in core Eliot
(`FunctionLiteral` + `apply`). If each data type's `handleCases` (and `typeMatch`) were synthesized as core
expressions in a `lang` phase, **the backend would need zero pattern-match-specific code** — it already compiles
lambdas and applications. This is the single largest reduction in "surface area a new backend must get right,"
because selector-chain bytecode is exactly the intricate, easy-to-get-subtly-wrong code a second backend author
would otherwise reimplement.

**Where.** A desugaring in `lang` that, given the `ModuleConstructors` layout (W2, done), emits the `handleCases`
/`typeMatch` implementation `NamedValue`s as ordinary core terms (nested single-parameter lambdas selecting and
applying a handler to fields). These flow through `monomorphize` → `uncurry` like any other value and reach the
backend as plain `FunctionLiteral` bodies.

**Effort/risk.** High / medium-high. This is the strategic item, and **its prerequisites (W1, W2) are now done** —
the ordering/names are available as facts. Stage it: introduce the synthesis behind the existing backend path and
switch over once it produces equivalent output (the `ExamplesIntegrationTest` match cases are the oracle). Worth
committing to even if delivered incrementally, because it is what most reduces a backend to "literals +
allocations + lambdas + application + a type map."

**Disposition — new desugaring (preference #3), but honestly scoped.** This fits "new, closed functionality."
The claim of "**zero** pattern-match-specific code in the backend" is too strong, though: the synthesis bottoms
out at two irreducible runtime leaves the backend must still emit —
- **field projection** (`GETFIELD` today): applying a handler to a constructor's fields, and
- **constructor-tag test** (`instanceof` today): needed by `typeMatch`'s dispatch. `handleCases` avoids this,
  because dispatch is per-constructor via virtual method, so each constructor's body is just
  `\cases -> cases pickᵢ field₀ … field_k` with no tag test.

Both leaves already belong to the "ideal backend" list (item 2: allocation + field access). **Represent them as
named intrinsic leaves** (like `nativeWiden`), *not* as new IR nodes — that keeps them consistent with the
existing leaf-emitter contract and folds neatly into W8. The genuine, large win remains: the intricate
selector-chain *shape* (handler-forwarding closures) moves from bytecode to `lang`.

**Open questions.**
- **One dispatch authority or two?** Note the *ordering* authority is already shared post-W2: both
  `MatchNativesProcessor` and the backend read constructor order from `ModuleConstructors`. What is *not* yet
  shared is the **selector-chain body**: `MatchNativesProcessor` encodes it as a `VNative`/`churchSelector`
  `SemValue` for NbE, the backend re-encodes it in bytecode. The biggest prize is making the synthesized core body
  *also replace* `MatchNativesProcessor`'s compile-time `VNative`, so NbE and runtime share one definition (best
  cornerstone story, highest risk). The lower-risk alternative keeps `MatchNativesProcessor` for compile time and
  only synthesizes the runtime body. **Decide before starting W3.**
- Field-projection / tag-test as named intrinsic leaves (recommended) vs. new IR nodes — confirm.

---

### W4 — Free-variable / capture analysis as a `lang` concern — **DONE**

Implemented as a method on the IR type (preference #1). `freeVariables` is now an extension on
`UncurriedMonomorphicExpression.Expression`
(`lang/.../uncurry/fact/UncurriedMonomorphicExpression.scala`), the post-uncurry type every backend consumes.
The backend's `LambdaGenerator.collectParameterReferences` was deleted and its one call site now reads
`body.value.expression.freeVariables`. Behaviour is identical (occurrence order preserved, duplicate references
not de-duplicated, nested binders excluded), so no bytecode change — `lang.test` and `jvm.test` (incl.
`ExamplesIntegrationTest`) green. No IR-shape change; denormalizing the capture set onto `FunctionLiteral` was
not warranted (not measured hot).

---

### W5 — Normalise N-ary lambdas to single-parameter form in `uncurry`

**Now.** `LambdaGenerator` peels a multi-parameter `FunctionLiteral` into nested one-argument closures
(`LambdaGenerator.scala:43-56`), mirroring the language's curried `Function` semantics, so that the
single-parameter path handles every nesting level.

**Why generic.** The peel is the same currying transformation `uncurry` already applies to call sites; only the
closure-class *emission* is JVM-specific. Doing it once in `lang` means every backend sees only single-parameter
`FunctionLiteral`s and never reinvents the peel.

**Where.** `MonomorphicUncurryingProcessor` / the `uncurry` fact: normalise lambda literals to single-parameter
form (or expose them already-peeled).

**Effort/risk.** Low-medium / low. Composes naturally with W4 (capture sets per peeled frame).

**Disposition — extend the existing `uncurry` processor (no new processor); denormalize per-frame captures
(preference #2).** This modifies an existing transformation, so it sidesteps the processor question. One
precision the original sketch blurs: `uncurry` *flattens to N-ary* for top-level defs and call sites, whereas
W5 is specifically about **expression-position lambda literals** (closures). Peel only those to single-parameter
form, and use the now-available `freeVariables` (W4, **done**) to attach each peeled frame's capture set — the
maximally backend-ready form.

**Open question (genuine tension).** Single-parameter closures may over-commit to a JVM-ish representation; a
microcontroller backend might prefer N-ary closures and would then have to re-coalesce. Counter-argument:
currying *is* the language's semantics (`a -> b -> c`), so single-parameter is the *canonical* form and N-ary is
the optimization — which argues for peeling in `lang`. This is the one place the plan bakes a representation
choice into shared IR; **decide explicitly.**

---

### W6 — Erasure (`valueType`) as a `lang` method — **DONE**

Implemented as a method on the fact (preference #1), *not* by storing erased types in the IR. The erasure logic
now lives in `lang` as `GroundValue.carrierFQN`
(`lang/.../monomorphize/fact/GroundValue.scala`): function types → `WellKnownTypes.functionCarrierFQN`; a concrete
structure type → its FQN with the qualifier stripped to `Default`; `Type`/erased values →
`WellKnownTypes.anyFQN`. The two carrier FQNs were added to `WellKnownTypes` (`functionCarrierFQN`, `anyFQN`) so
the carrier *vocabulary* is `lang`-owned while the FQN→`java.lang.*` *map* stays the backend's sole erasure
knowledge (`NativeType.types`). The backend's `CommonPatterns.valueType` is now a zero-logic delegate
(`v.carrierFQN`) — kept only as a local convenience name, since rewriting its ~35 call sites to postfix form
bought no safety. A second backend uses `GroundValue.carrierFQN` directly. `lang.test` / `jvm.test` green.

The "store erased types in the IR" alternative was rejected: erasure is lossy (function structure → bare
`Function`), and the method form is lower-risk and gives W7 its dedup key for free.

---

### W7 — Representation-keyed monomorphic instances (eliminate backend dedup) — recommended scope **satisfied by W6**

> **Status.** The disposition's recommended deliverable ("ship the shared-key version") is effectively **in place**:
> `JvmClassGenerator.methodSignatureKey` builds the key from `valueType(...).show`, and `valueType` is now the
> zero-logic delegate to `GroundValue.carrierFQN` (W6, done). So a second backend computes the same dedup key for
> free; the remaining `seen`-set loop is the ~10 trivial lines the disposition said would stay. Only the **optional**
> full pre-dedup (a `lang`-side erased-signature-keyed fact) is outstanding — deferred until the microcontroller
> backend actually lands.

**Now.** `JvmClassGenerator.methodSignatureKey` plus the `seen` / `distinctTypeArgs` loop
(`JvmClassGenerator.scala:233-276`) exist because distinct type-arguments that *lower to the same
representation* (e.g. `Int[0,3]` and `Int[0,5]` both → `Byte`) produce byte-identical methods that must be
emitted once. The dedup key is computed from JVM descriptors.

**Why generic.** Post-lowering, those instances *are* the same instance. The principle is platform-independent;
the descriptor-based key is the only JVM-specific part.

**Where.** If representation-lowering is applied at the IR/key level (W6), then `UsedNames` /
`UncurriedMonomorphicValue` would already be representation-keyed and the collapse would happen before the
backend sees the instances — deleting this loop from every backend. This is also a **safety** item: a backend
that forgets the dedup emits clashing overloads.

**Effort/risk.** Medium / medium. Best done as a consequence of W6 rather than independently.

**Disposition — share the *key* now (via W6's `GroundValue.carrierFQN`); defer full pre-dedup.** Fully removing
the loop requires `lang` to present pre-collapsed instances, but the identity Key cannot be erased (distinct
`typeArguments` must stay distinct through monomorphization), so that needs a *new* fact keyed on the erased
signature — preference #3, justified only with a second backend in hand. The fragile/duplicable part today is
the *key computation* (`methodSignatureKey`, built from `valueType(...).show`); now that W6 has landed
(`GroundValue.carrierFQN`), a new backend computes the key correctly for free and the remaining `seen`-set loop
is ~10 trivial lines. **Ship the shared-key version; treat full pre-dedup as optional.**

**Open question.** Is the structural guarantee (a backend *cannot* forget to dedup) worth a dedicated
erased-signature-keyed fact? Depends on how soon the microcontroller backend lands.

---

### W8 — Generic "every used body-less def must be claimed by a native" check

**Now.** `JvmClassGenerator.createModuleMethod` aborts with `"Function not implemented."` for any used body-less
`def` that is not an intrinsic/native (`JvmClassGenerator.scala:315-317`), and the intrinsic FQN set — including
the width-suffix names `ByteToByte`…`BigIntegerToBigInteger` — is hardcoded in `Intrinsics.scala:35-59`,
duplicating the leaf names declared in `stdlib/eliot/eliot/lang/Int.els`. Nothing prevents the two
lists from drifting.

**Why generic.** "Every used abstract (body-less) definition must be realised by *some* backend native, else it
is a compile error" is a fail-safe invariant every backend should enforce (cf. the project's "gaps must be
fail-safe" rule). Today each backend must remember to enforce it, and the registry is an ad-hoc Scala `Set`.

**Where.** A `lang`-level check parameterised by the active backend's native registry: the backend declares
which FQNs it realises (inline intrinsics + generated natives), and `lang` verifies coverage over the *used*
body-less defs. The leaf-native names should be declared once (the stdlib already declares them) and the backend
should map FQN → emitter rather than re-listing the names.

**Effort/risk.** Low-medium / low. Turns a per-backend convention into a structural guarantee and removes the
stdlib↔backend name duplication.

> **Superseded — see the W8 open question in Sequencing.** The predicate-registry framing below was rejected:
> having `lang` ask the backend "can you realize X?" is a back-edge against the forward fact-DAG. The corrected
> direction is that the check stays a *forward consumer* — either the backend's terminal abort, or a generic check
> over forward-flowing realizer facts. This section is kept for context until W8 is re-specified.

**Disposition — `lang` check (preference #3) + a backend-supplied *predicate* registry.** The check ("every used
body-less def must be claimed by *some* backend native") is closed functionality and belongs in `lang`, computed
over `UsedNames` + the body-less-def facts. The backend supplies *what it realizes* via the existing plugin
`Configuration` mechanism (`JvmPlugin` already threads config).

**Open question (the key design choice).** The `nativeAdd*` / `Subtract*` / `Multiply*` leaves are a *generated
matrix* (operator × width-suffix), not a flat set independent of stdlib — which is exactly why
`Intrinsics.scala:35-59` re-types them. So the registry should be a **predicate `FQN → Boolean`** (or
`FQN → Option[emitter]`), not a `Set`: the check asks the backend "can you realize X?" per used body-less def,
which covers inline intrinsics *and* generated families *and* kills the stdlib↔backend duplication. Confirm a
predicate registry is acceptable over an enumerable set.

## Sequencing

**W1, W2, W4, and W6 are done**, and **W7's recommended shared-key scope is satisfied by W6**. W2's
`ModuleConstructors` fact already unblocks W3 and has deduped `MatchNativesProcessor`/`DataMatchDesugarer`/backend
ordering into one authority. Remaining work:

1. **Small / independent:** **W8** (coverage check) — but its shape is still open (see below); it is *not* a
   `lang` service that interrogates the backend.
2. **IR normalization:** **W5** (extend `uncurry`, uses W4's `freeVariables`). *(W7 dedup: shipped via W6's shared
   key; only the optional pre-dedup fact remains.)*
3. **Strategic, staged:** **W3** (synthesize `handleCases`/`typeMatch`), on top of W2's `ModuleConstructors`
   layout, with the one-vs-two-dispatch-authorities question settled first. This is now the largest remaining win.

**Open questions to settle before/while implementing** (collected):
1. W3 — unify the synthesized body with `MatchNativesProcessor`'s NbE native (one source of truth), or keep two?
2. W3 — field-projection + tag-test as named intrinsic leaves (recommended) or new IR nodes?
3. W5 — is single-parameter-closure the right *canonical* IR form for all backends, or a backend choice?
4. W7 — shared-key version is **shipped** (via W6's `carrierFQN`); the only remaining question is whether the
   optional erased-signature-keyed fact (structural "cannot forget to dedup" guarantee) is worth building — decide
   when the microcontroller backend lands.
5. W8 — the coverage check must stay a *forward* consumer (it cannot interrogate the backend). Open: keep it as
   the backend's terminal abort, or express realizers as forward-flowing facts a generic check consumes? (The
   earlier predicate-registry sketch in W8's disposition is superseded — it was a back-edge against the fact-DAG.)

Net effect once all land: a new backend (the microcontroller target) implements roughly a representation-FQN →
machine-layout map plus the intrinsic leaf emitters (including the field-projection and constructor-tag-test
leaves W3 bottoms out at), and inherits data layout, pattern matching, closure conversion, coercion, dedup, and
the coverage guard from `lang`.

## What stays in the backend (explicitly out of scope)

These are correctly platform-specific and must **not** move:

- ASM bytecode emission, JVM descriptors, verifier constraints (`asm/*`, `MethodGenerator`, `ClassGenerator`).
- The `java.lang.*` / `java.math.BigInteger` mapping and the `Jvm*` representation types
  (`NativeType.scala`) — these *are* the JVM platform layer, by design.
- Concrete instruction choices (`LADD`/`LSUB`/`LMUL`, boxing, `instanceof`), and Unit-as-`Void`.
- JAR / manifest generation (`jargen/*`).

The leaf-native *bodies* (e.g. `nativeAddByteToByte` → one unbox/op/rebox group) also stay backend-specific —
that is the whole point of the layered design — but their *names and the requirement that they exist* become a
shared, checked contract (W8).
