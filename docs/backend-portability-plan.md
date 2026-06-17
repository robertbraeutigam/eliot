# Extracting Generic Mechanisms From the JVM Backend Into `lang`

Status: **Proposal / evaluation.** Nothing here is implemented yet. This is a survey of what currently lives
in the `jvm` module but is platform-independent, and a sequenced plan for moving it into `lang` so that writing
a second backend (the microcontroller target) is easier and safer.

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
   backend type knowledge of integer widths." `docs/jvm-int-representation-plan.md` commits to "no `Int`-specific
   code in the backend." The work below continues that trajectory for the *non-Int* mechanisms.

2. **`WellKnownTypes` is the established home for by-name protocol constants.** It already centralises
   `coerceFQN`, `combinedFQN`, `someFQN`/`noneFQN`, `boolFoldFQN`, `integerLiteralFQN`, and the `BigInteger`
   compile-time natives. But the two *older* dispatch protocols — `PatternMatch` and `TypeMatch` — are **not**
   there; they are hardcoded string literals in the backend (see finding 4).

3. **The IR the backend consumes is already mostly representation-typed.** `UncurriedMonomorphicValue` /
   `UncurriedMonomorphicExpression` carry `GroundValue` types that have passed through `RepresentationLowering`.
   `GroundValue` exposes exactly the generic queries a backend needs (`asFunctionType`, `typeFQN`,
   `deepReturnType`, `functionArity`, `extractParamAndReturnTypes` — `GroundValue.scala:32-69`).

4. **Several backend mechanisms re-derive or re-encode information that is generic.** The four large ones:
   - `JvmClassGenerator` reconstructs constructor → data-type grouping by re-parsing `OperatorResolvedValue`
     signatures (`JvmClassGenerator.scala:383-428`).
   - `JvmClassGenerator` / `ExpressionCodeGenerator` recognise `PatternMatch`/`TypeMatch` by hardcoded strings
     (`JvmClassGenerator.scala:59,79,86`; `ExpressionCodeGenerator.scala:123,135,204`).
   - `DataClassGenerator` hand-builds a Church/Scott encoding of the data eliminator in bytecode
     (`DataClassGenerator.scala:188-321`).
   - `LambdaGenerator` computes a lambda's captured free variables (`LambdaGenerator.scala:163-175`) and peels
     N-ary lambdas into nested single-arg closures (`LambdaGenerator.scala:43-56`).

## Inventory: generic vs. genuinely backend-specific

| Mechanism | Location | Verdict |
|---|---|---|
| Constructor → data-type grouping, return-type extraction | `JvmClassGenerator.scala:48-52,383-428` | **generic** → fact |
| PatternMatch/TypeMatch recognition by name | `JvmClassGenerator.scala:56-99`; `ExpressionCodeGenerator.scala:119-160` | **generic** → `WellKnownTypes` + plan fact |
| Church/Scott `handleCases` encoding | `DataClassGenerator.scala:118-321` | **generic algorithm** → core desugaring |
| `typeMatch` instanceof dispatch structure | `DataClassGenerator.scala:366-423` | **generic structure**, emission JVM |
| Free-variable / capture analysis | `LambdaGenerator.scala:163-175` | **generic** → lang utility / IR field |
| N-ary lambda → nested single-arg peel | `LambdaGenerator.scala:43-56` | **generic** → `uncurry` normalization |
| Erasure (`valueType`) | `CommonPatterns.scala:14-22` | **logic generic**, FQN→class JVM |
| Monomorphic-instance dedup by representation | `JvmClassGenerator.scala:233-276` | **generic principle** → IR keying |
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

**Why generic.** These are language-level protocols defined in `lang/resources/eliot/eliot/lang/PatternMatch.els`
and `TypeMatch.els`. The newer `Coerce`/`Combine` protocols are already named in `WellKnownTypes`; these two
predate that convention and never got the same treatment. Any backend needs to recognise the same names.

**Where.** Add to `WellKnownTypes`:
`patternMatchAbilityName`, `handleCasesFQN` (or at least the local name), `typeMatchAbilityName`, `typeMatchFQN`,
plus a helper to test "is this VFQN a `PatternMatch.handleCases` impl method" / "…a `TypeMatch.typeMatch` impl".

**Effort/risk.** Low / low. Pure constant extraction; no behaviour change. Good first commit — it also makes
W2 cleaner.

---

### W2 — `DataTypeLayout` fact: constructor grouping as generated data, not backend reconstruction

**Now.** `JvmClassGenerator` computes, for each module: every constructor name (`isConstructor`), each
constructor's data type (`evaluateConstructorDataType`, which calls `OperatorResolvedValue` and then strips the
signature with `stripFunctionLiterals` / `stripApplicationTargets` / `stripCurriedReturnType` /
`isFunctionTypeApplication` / `extractReturnTypeRef`, `JvmClassGenerator.scala:383-428`), groups constructors by
data type, sorts them by source position to recover declaration order, and assigns each a constructor index
(`DataClassGenerator.scala:68-69`).

**Why generic.** This is "what is the closed-union layout of this data type": the ordered list of constructors,
each with its field types and its index, and which type constructor they belong to. It is entirely
backend-independent and is exactly what `matchdesugar` already relies on conceptually. Re-deriving it by
re-parsing an `OperatorResolvedValue` signature in the backend is fragile (it pattern-matches on `Function`
type-application shapes) and would have to be reimplemented, identically, in every backend.

**Where.** A new fact in `lang`, e.g. `monomorphize` or a small `layout` package:

```scala
case class DataTypeLayout(
    dataType: ValueFQN,
    constructors: Seq[ConstructorLayout]   // in declaration order; index == position
)
case class ConstructorLayout(
    ctor: ValueFQN,
    index: Int,
    fields: Seq[MonomorphicParameterDefinition]  // ordered, with concrete types
)
```

`DataDefinitionDesugarer` already has the declaration-order and field information when it generates the
constructor / type-constructor functions, so the layout can be produced there (or by a dedicated processor that
reads its output) rather than reconstructed from signatures downstream. The backend then asks for
`DataTypeLayout.Key(dataType)` instead of running the `strip*` helpers.

**Effort/risk.** Medium / medium. The win is large (deletes ~45 lines of fragile signature-stripping from the
backend and makes the data available to every backend), but care is needed to reproduce the exact ordering the
backend currently derives from source positions.

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

**Where.** A desugaring in `lang` that, given `DataTypeLayout` (W2), emits the `handleCases` /`typeMatch`
implementation `NamedValue`s as ordinary core terms (nested single-parameter lambdas selecting and applying a
handler to fields). These flow through `monomorphize` → `uncurry` like any other value and reach the backend as
plain `FunctionLiteral` bodies.

**Effort/risk.** High / medium-high. This is the strategic item. It can be staged: do W1/W2 first so the layout
and names are available, then introduce the synthesis behind the existing backend path and switch over once it
produces equivalent output (the `ExamplesIntegrationTest` match cases are the oracle). Worth committing to even
if delivered incrementally, because it is what most reduces a backend to "literals + allocations + lambdas +
application + a type map."

---

### W4 — Free-variable / capture analysis as a `lang` concern

**Now.** `LambdaGenerator.collectParameterReferences` (`LambdaGenerator.scala:163-175`) walks a lambda body and
returns the parameter names it references; the generator subtracts the bound parameter to get the captured
environment. Confirmed used only in the `jvm` module.

**Why generic.** Computing a lambda's captured free variables is a pure, backend-independent tree analysis that
every closure-converting backend needs.

**Where.** Either a small utility in `lang` (alongside the `UncurriedMonomorphicExpression` definitions), or —
better — precompute the capture list onto the `FunctionLiteral` node in the IR so the backend receives it
ready-made and never re-walks the tree.

**Effort/risk.** Low / low. If done as a utility, it is a near-mechanical move; if baked into the IR, it touches
the `uncurry` fact shape but removes a class of backend bugs (forgetting to exclude a bound name, mishandling
nested binders).

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

---

### W6 — Unify erasure (`valueType`) with `RepresentationLowering`

**Now.** `CommonPatterns.valueType` (`CommonPatterns.scala:14-22`) collapses a bounds-carrying `GroundValue` to
its runtime carrier: function types → `Function`, `Type`-typed things → `Any`, otherwise the type's FQN with the
data-type suffix stripped. Only the final FQN→`java.lang.*` class step is JVM-specific.

**Why generic.** This erasure is the sibling of `RepresentationLowering`, which already generically rewrites
`Int[MIN, MAX]` → `Jvm*`. The collapse logic (function carrier, `Type`→erased, strip bounds/suffix) is
platform-independent; only the machine-class lookup is not.

**Where.** Extend the existing `RepresentationLowering` pass (already in `lang`, already folded into `uncurry`)
to also perform this erasure, yielding an IR in which **every type position is already a representation FQN**.
The backend then needs only an FQN → machine-type map (`NativeType.types`), with no erasure logic of its own.

**Effort/risk.** Medium / medium. Highest-leverage of the "typed IR" items: it makes W7 unnecessary and shrinks
`CommonPatterns` to a thin lookup.

---

### W7 — Representation-keyed monomorphic instances (eliminate backend dedup)

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

---

### W8 — Generic "every used body-less def must be claimed by a native" check

**Now.** `JvmClassGenerator.createModuleMethod` aborts with `"Function not implemented."` for any used body-less
`def` that is not an intrinsic/native (`JvmClassGenerator.scala:315-317`), and the intrinsic FQN set — including
the width-suffix names `ByteToByte`…`BigIntegerToBigInteger` — is hardcoded in `Intrinsics.scala:35-59`,
duplicating the leaf names declared in `stdlib/resources/eliot/eliot/lang/Int.els`. Nothing prevents the two
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

## Sequencing

1. **Quick, safe, independently shippable:** **W1** (well-known names), **W4** (capture analysis), **W8**
   (native-coverage check). Each de-risks a second backend with little churn and no IR changes.
2. **Typed-IR layer (medium, high leverage):** **W2** (`DataTypeLayout` fact), then **W6** (erasure into
   `RepresentationLowering`) which subsumes **W7** (dedup). **W5** rides along with W4.
3. **Strategic:** **W3** (synthesize `handleCases`/`typeMatch` as core terms), staged behind the existing
   backend path and switched over once output matches.

Net effect once all land: a new backend (the microcontroller target) implements roughly a representation-FQN →
machine-layout map plus the intrinsic leaf emitters, and inherits data layout, pattern matching, closure
conversion, coercion, dedup, and the coverage guard from `lang`.

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
