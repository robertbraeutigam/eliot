# Claude Memory

## Project Overview

ELIOT is a functional, generic programming language for microcontrollers, implemented in Scala 3. This repo
contains the whole compiler and the ELIOT standard library. The compiler is plugin-based over a fact-based
compilation system, with multiple backends (currently JVM).

## Build System

**Mill** (1.1.0+), Scala **3.7.4**. Key deps: `cats-effect`, `parsley-cats` (parsers), `ASM 9.9` (jvm module).

```bash
./mill __.compile                  # compile all modules
./mill __.test                     # test all modules
./mill lang.test                   # one module (also jvm.test, eliotc.test, ide.lsp.test)

# Filter test output by class name (the -- flag is rejected by ScalaTest)
./mill lang.test 2>&1 | grep -v DEBUG | grep "ClassName"

# Compile an example to an executable jar (lands in target/HelloWorld.jar), then run it
./mill examples.run jvm exe-jar examples/src/ -m HelloWorld
java -jar target/HelloWorld.jar
```

For a broad change, verify with the fast example sweep + byte-identity comparison rather than `examples.run`
per example — recipes and their traps are in the `reference_verification_harness_recipes` memory.

**Two compiler diagnostics, both opt-in and both off by default** — each observes every processor invocation
or fact read, so an ordinary build must never pay for them (an always-on tracker once cost 73% of a cold
build):

- `--statistics` prints, per processor, how often it ran and its cumulative **self time** — wall time minus
  time blocked waiting for facts, which is CPU time here since nothing runs in parallel (`eliotc/…/statistics/`,
  entirely a `wrapWith` add-on: no processor is special-cased and `CompilerIO` is untouched). Two derived
  lines close the accounting: dispatch (offering every key to every processor) and the engine. Measuring is
  itself per-invocation work and lands in the dispatch line, inflating a build ~20%, so read that line as an
  upper bound and diff a run with and without the flag for the true figure.
- `--visualize-facts <path>` writes the fact-flow graph (`eliotc/…/visualization/`).

### Module Structure

Modules (see `build.mill`):

1. **eliotc** — core compiler infrastructure (plugins, processors, feedback, utilities)
2. **lang** — compiler/language front end (parsing, AST, type system, modules, resolution, monomorphize)
3. **stdlib** — the platform-independent base standard library (abstract `type`/`def` signatures)
4. **jvm** — JVM backend (ASM bytecode, JAR generation)
5. **examples** — example ELIOT programs
6. **apidoc** — doc-comment facts + HTML site generation

### IDE Tooling (`ide/`)

Everything editor/IDE-related lives under **`ide/`**; put new editor integrations there.

- **`ide/lsp/`** — the LSP server. A Mill module nested under `ide`, so its target is **`ide.lsp`**
  (output under `out/ide/lsp/`). Depends on `lang` + `stdlib`; main class `LspMain`.
  - `ide/lsp/package.sh` builds a distribution under `ide/lsp/dist/` (git-ignored). It bundles **code only** —
    layer `.els` are never shipped; base/stdlib/platform layers reach the compiler on the **path** as ordinary
    dependencies ([[project_lsp_layers_from_path_not_bundled]]). It produces **separate per-module jars, never a
    fat assembly jar**: each layer jar carries a same-path `META-INF/services/…CompilerPlugin` file and a fat jar
    collapses those, silently dropping plugin registrations ([[gotcha_assembly_jar_breaks_layers]]). A second
    classpath dir `compiler-lib/` holds **only ASM**, the one backend dep "Run main" needs that `lib/` lacks.
  - Shipped: whole-workspace diagnostics, hover/go-to-def (reverse `PositionIndex`), live-edit VFS overlay,
    completion, concrete-type hover hints (`TypeHintIndex` from `MonomorphicValue` facts — **Id-normalize
    first**), a `▶ Run main` code lens (`MainIndex`, fires the `eliot.runMain` command), and apidoc doc hover
    (`DocIndex` from `ValueDoc` facts; the LSP activates `ApiDocPlugin` as a *non-target* plugin so only its
    processor runs, never HTML generation).
  - The one remaining design item is parser/checker **error recovery** (`docs/ide-type-hints.md`, Layers A/B) —
    it is what makes hints work on in-progress code. Everything else (find-refs, rename, semantic tokens) is
    routine additive work on the existing index.
- **`ide/textmate/`** — TextMate grammar for `.els` highlighting. Static files, not a build module.
- **`ide/intellij/`** — the shipped IntelliJ plugin: highlighting + diagnostics (LSP4IJ) + a native "Eliot
  Application" run configuration. A **self-contained Gradle build**, not part of the Mill build; its
  `prepareSandbox` shells out to `ide/lsp/package.sh`. Build with `cd ide/intellij && ./gradlew runIde|buildPlugin`.
  See `ide/intellij/README.md`. The `▶ Run main` lens dispatches client-side to an `LSPCommandAction` whose
  **IntelliJ action id must equal the command name**; its before-run task invokes the compiler CLI and gates on
  the exit code, so a stale jar is never run.

## Architecture

### Plugin System

ServiceLoader-based: each plugin implements `CompilerPlugin` (in eliotc), may depend on other plugins, and
configures the pipeline. `LangPlugin` provides core compilation; `JvmPlugin` provides bytecode + jar output.

### Compilation Pipeline

Fact-based: **facts** are immutable pieces of compilation data; **processors** compute facts from other facts on
demand; `FactGenerator` orchestrates lazy computation with caching. A fact is identified by its key, usually a
subset of its data.

### Phases (packages in `lang`, roughly in order)

1. **source** — reading source files (multi-mount, platform-scoped; see the Layers cornerstone)
2. **token** — tokenizer
3. **ast** — building the AST
4. **core** — building the core language AST (desugars `data`, effect rows, meta transfers). Two checks ride the
   desugared named values here: `StrictPositivityChecker` and `VisibilityOrderChecker` (a file's public API must be a
   *prefix* — no public declaration may follow a private one, C++'s `private:` section as an ordering rule). The
   visibility check runs post-desugar precisely so `def`/`type`/`data`/`ability`/`implement` need no per-construct
   arms; `ability`/`implement` mint public values and so are covered with no exception, and a `private data` is
   private in *every* name it mints.
5. **module** — from modules to individual values; unifies same-named modules from different paths
6. **resolve** — resolve identifiers to fully qualified names or parameters
7. **matchdesugar** — pattern matches into function applications; exhaustiveness, nested/constructor/wildcard patterns
8. **operator** — infix operators by precedence and associativity, into structured applications
9. **termination** — the recursion gate (see the *Total by Default* cornerstone)
10. **effect** — helpers only, **no phase**: `EffectMachinery` (recognises the `Effect`/`Suspend` machinery
    abilities), `EffectCarriers` (a signature's carrier binders and declared effects), and
    `EffectRowRendering`/`EffectCarrierNaming` (the one inverter rendering a carrier stack back as its row)
11. **row** — the effect phase. `row/processor/RowElaborationProcessor` produces `RowElaboratedValue` between the
    recursion gate and saturation, doing two things **from declarations only**: **elaboration**
    (`row/RowElaborator` desugars direct style into explicit monadic core — writes the carrier and every
    declaration-determined type argument, hoists effectful strict arguments into `flatMap` chains, sequences
    block `val`s, passes suspended arguments unrun, captures at carrier-headed slots, writes `Id`/`runId` at pure
    boundaries) and **verification** (`verifyRow` checks `derived ⊆ declared` per definition). `row/RowChecker`
    holds the derivation rules; `row/RunBoundaryFunctions` is the platform run-boundary config key. See the
    *Effects Are a Channel* cornerstone.
12. **ability** — checks and returns a type-specific ability implementation
13. **monomorphize** — the NbE monomorphic type checker: evaluates data and value definitions into typed
    structures and checks all types at every instantiated usage, with the single evaluator. (It absorbed the
    former standalone `eval` phase.) Because `row` already wrote the carrier, the checker sees effects as
    ordinary types:
    - **elaboration is not here** — no bind, no `pure`, no `Id` is decided by the checker. The one effect rule it
      keeps is `check/EffectLifter.tryPureWrap` (a pure term into a *rigid* carrier-headed expected type).
    - **verification is not here either** — `derived ⊆ declared` is checked twice outside it (phase 11 pre-mono,
      and `channel/EffectAccountingProcessor` post-mono). The checker holds no effect diagnostic.
    - the **compile track** keeps its mid-spine default ladder and deferred slots *by design* (`Track.Compiler`,
      `Checker.resolveDeferredSlot`): an inline guard's carrier is inferred and pinned to `Either[E]` post hoc —
      the sole live reader of the `Unifier`'s higher-kinded-meta record.
    - other non-equality collaborators, each hooked from `TypeStackLoop.runPostDrainResolution`:
      `check/CarrierKindChecker`, `check/GuardDischargeResolver` (W2b effectful-signature guard discharge —
      formerly `CalculatedReturnResolver`, whose calculated-return half was removed with the `auto`/implicit-generics
      feature), `check/AbilityResolver`.
    - riders on `MonomorphicValue`: `channel/RefinementChannelProcessor` (Int ranges) and
      `channel/EffectAccountingProcessor` (effects), both post-mono channels.
14. **used** — collects used value names starting from a `main`
15. **uncurry** — uncurries calls for the backend

### Error Handling

Errors go through the `CompilationIO` monad (lang). `SourcedError` carries source position; `User` and `Logging`
provide user-facing messages and debug logging.

## Testing

- Tests extend `AsyncFlatSpec` with `AsyncIOSpec` with `Matchers`; files live in `<module>/test/src/`.
- A processor test needing more than a couple of leaf phases runs the **whole** pipeline via the shared builder —
  `extends ProcessorTest(LangProcessors()*)` (with `systemModules = …` / `maxNestedRepeats = …` when those differ)
  — never a hand-listed prefix. Computation is demand-driven and `SequentialCompilerProcessors` dispatches each
  key to its one handling processor, so a harness that triggers only an early fact simply never runs later
  phases; carrying the full pipeline is free, and a new phase is then wired in exactly one place. Only true leaf
  tests (tokenizer/AST/core, or manual fact injection) list processors explicitly. The jvm backend test reuses the
  same list and appends its own: `LangProcessors(…) :+ JvmClassGenerator()`.

## Language Cornerstone: Types Are Values (λ\*)

Eliot has **no internal distinction between the type level and the value level**. Type constructors and value
constructors are both ordinary named values, and the type *of* a value is itself just another value. The only
thing that makes a computation "type-level" is *when* it is forced: type-level code happens to be evaluated
before code generation, but that staging is incidental, not a difference in kind. Formally this is a
**non-stratified Pure Type System** — λ\* / "type-in-type" (`Type : Type`) — where the compile-time/runtime split
is pure **phase / erasure** and type checking is **Normalisation by Evaluation** (the `monomorphize` package: one
`Evaluator`, one `SemValue` domain shared by types and values, `VType` as an ordinary value). This is deliberately
*not* "dependent types" bolted on; dependency is merely a consequence of types being values. Accepted trade-off:
`Type : Type` is logically inconsistent (Girard's paradox) — fine for a general-purpose language, with
termination handled separately rather than by a universe hierarchy.

**Sanctioned sugar vs. required discipline.** Familiar surface distinctions are *intentional sugar*, not
violations: the `Qualifier.Type`/`Qualifier.Default` namespaces, `[]` vs `()` call/pattern syntax, and the
restricted `Expression.typeParser` all collapse to "the same `FunctionDefinition` with a different qualifier tag."
What the cornerstone *requires* of every task: exactly **one evaluator and one value domain** (never a second,
weaker "compile-time" interpreter); type equality is **definitional** (force/normalise via that evaluator, then
compare), never a parallel bespoke mechanism; and kind/arity metadata stays out of semantic phases.

Three durable guardrails:

1. **`unify` is pure definitional equality** — never a `refinements` map or an assignability arm. There is **no
   `Int` widening and no `Coerce`**: `Int` is nullary (`type Int {range: Interval[BigInteger]}`) with bounds held
   as meta-information in the separate **refinement channel** (`monomorphize/channel/RefinementChannelProcessor`,
   checked post-mono). So `Int == Int` definitionally, and a narrower range flowing where a wider one is expected
   is definitionally equal — bound legality is the channel's job, not a checker-inserted coercion.
2. **`VPi` is the one primitive Π-former** *on principle* — do not fold `Function` into an ordinary `data`.
3. **Kind/arity metadata (`RoleHint`, esp. `typeParamCount`) must not drive any typing decision.** Its only
   sanctioned read is constructor-shape reconstruction for `match`.

## Language Cornerstone: Platform-Independence via Layers

Eliot targets everything from an ATtiny to the JVM, so the language and its base stdlib commit to **no platform
assumptions** — not even the size of an `Int`. The rule is **no platform *representation***: the base layer never
says how a type is laid out or how a primitive is computed. Everything representation- or platform-dependent is
declared **abstractly** — `type`s without a value constructor, body-less `def` signatures. The base *may* carry
`def` bodies and ability instances when the computation is **genuinely platform-independent**, byte-for-byte the
same on every target (e.g. `fitsIn`, the discharge helpers `catch`/`else`/`runStateToPair`). It must **never**
contain `data` (a chosen representation), a native leaf, or any representation-dependent body.

- `type Int {range: Interval[BigInteger]}` — an abstract type; no value constructor, no chosen width. Its range is
  channel meta-information, not a type parameter. An `Interval`'s **endpoints are each a `Bound[T]`**
  (`data Bound[T] = Unbounded | Bounded(value: T)`, `data Interval[T](start: Bound[T], end: Bound[T])`), so a range
  may be **half-open** — an unbounded endpoint's direction is read from its *position* (`start` = no lower limit,
  `end` = no upper), and there is no signed infinity. That is what lets the base state the platform-independent half
  of a bound (`atLeast(0)` for a size) instead of inventing a platform maximum it may not assume; a platform narrows
  it to `closed(0, platformMax)`. It is also what supplies the domain's stated **top**: `whole` — open at both ends —
  says *"nothing bounds this"*, so an absent meta means only "not computed yet" (`docs/total-meta-transfers.md` §5).
  The slot carried a **second, outer `Bound`** for that top until the endpoints could express it; collapsing it was
  the point of keeping one spelling, since `ReconcileProcessor.metaByPosition` compares verdicts *structurally* and
  two spellings of the same top read as disagreement. There is deliberately **no global top**: the generic
  `Meta[Bound[D]]` join is deleted, `Bound` is an interval endpoint and nothing else, and a future domain with no
  interval structure grows its own top rather than borrowing a wrapper (a wrapper is a second spelling, which is the
  bug above). The only ⊤ left is the channel's untotality (`None` at a companion-free callee, a parameter, a lambda
  interior), removed by arming R2 and by the §P4 interpretation — not by a value.
  Constructors `interval`/`closed`/`atLeast`/`atMost`/`whole` and the `where`-facing predicate
  `rangeWithin[Lo, Hi](i)` are abstract in the base, bodied per platform. `add`/`subtract` stay exact on a
  half-open interval; `multiply` widens to `whole` (its corner products lose the position that signs an infinity).
- `type String {size: Interval[BigInteger]}` — the channel's **second domain** (`docs/string-length-meta.md`),
  the same one-slot shape and the same machinery, which is what proves the channel is not `Int`-shaped. The unit is the
  **code point** — what `String::length` counts on every target — so a `where` over a size means the same thing
  everywhere; the slot is `size` because `length` is taken by the runtime function in that module. A literal seeds it
  (`Runtime::stringLiteral`'s brace, the string twin of `integerLiteral`'s), and `length` is the first leaf to **state**
  a transfer (`{size(s)}`), carrying a size into the `Int` domain. Stating one needed the backend's **result-edge
  re-encode** (S3, landed): a call boundary hands an integer back at the ⊤ bignum, so a node the transfer narrows is
  converted right after the call — `ExpressionCodeGenerator.convertResultFromBoundary`, the mirror of
  `generateArgumentToBignum` on the way in, applied to every non-intrinsic application (an intrinsic already emits at
  the node's own width). **S4 stated the rest of `String.els`** — `combine` exactly, `substring`/`trim` bounded by their
  subject, case conversion **tripling** its upper bound (`ß` ⤳ `SS`), `repeat`/`replace`/`indexOfInternal`, and
  `parseIntInternal` at the domain top `whole` since its honest bound is exponential in its argument's size. A brace
  **spells a number the compiler's way**: an endpoint is a `BigInteger`, a value-position literal is an `Int`, and
  there is no widening — so a brace's own literals are read as compile-time `BigInteger`s, because a brace's `^Meta`
  companion (and a `where`'s `^Where`) is compiler-pool-only code, exactly like a signature (`CoreProcessor`'s
  `isMetaBody` ⤳ `CoreExpressionConverter`'s `compilerTrackContext`). So an endpoint is written `Bounded(0)`, and a
  negative one is an ordinary subtraction the compile-time `Numeric[BigInteger]` reduces (`Bounded(0 - 1)`); S4's
  type-position workaround `boundedAt[V]` is deleted. An *ordinary def body* is still runtime-track, so a helper
  called from a brace cannot name a `BigInteger` constant in value position — which is why `rangeWithin[Lo, Hi]`
  takes its bounds as type parameters. Left for S5: the platform input leaves, and arming R2.
- `def foldLeft[A, B](initial: B, combine: ..., list: List[A]): B` — an abstract function, signature only.
- A `type X = ...` alias and a body-less `type X` differ only by having a body; `data X(...)` is the *concrete*
  form that additionally introduces a value constructor.

For the operational mechanics of placing/moving `.els` files across layers — two-pool resolution, the
abstract↔concrete merge, signature-match gotchas — use the **`eliot-layers`** skill.

**Layers = redefinition, not inheritance.** A platform "implements" an abstract definition by *defining the same
name again*, in its own root path, with a body. There is no `extends`, `override`, or instance mechanism —
co-located definitions of the same qualified name across root paths are **merged**, preferring the concrete one.
The base declares `type List[A]` and a body-less `def foldLeft(...)`; the `jvm` layer re-declares them concretely
over its `java.util.List` native, and the compiler unifies them into one value.

The carrier is **not** such a merge: there is no base `IO` at all. The concrete `data IO[A]` is the
platform-owned `eliot.jvm.IO`, outside the prelude, and user programs never name it. `main` declares an effect row
(`def main: {Console} Unit`); the jvm target's synthesized entry point instantiates `main`'s inferable carrier to
`IO` by ordinary unification and runs the thunk (`SyntheticMainSourceProcessor`).

**Layers *mix*, they do not *stack*; every file must stand on its own.** Name resolution is per-file — a file's
dictionary is its own declarations plus imports, never names declared in a *sibling* file of the same module. So
when one file needs a name a sibling declares (e.g. a carrier-generic instance `implement[F[_] ~ Suspend]
Console[F]`, which must be colocated with its ability and so lives in the ability's module), that file must
**re-declare what it needs**; the merge then **verifies the copies agree** (`signatureEquality`) rather than
letting them drift. Duplication is the sanctioned mechanism — do *not* "fix" a cross-file reference by widening
the resolver to span sibling files.

**How it works mechanically** (the `source` + `module` packages):

- The compiler gets multiple **source mounts** per platform pool (`source/scan/SourceMount.scala`; CLI roots
  become `FilesystemMount`s, plugins may contribute others — the jvm target mounts its synthesized `main.els`,
  the LSP routes overlaid buffers to a `vfs:` namespace). `PathScanner` resolves a module path against *all*
  mounts of the pool and returns *every* match as one `PathScan`; each URI scheme is served by one processor.
- `ModuleNamesProcessor` extracts names per file; `UnifiedModuleNamesProcessor` flattens all files of a module.
- Per name, `UnifiedModuleValueProcessor` collects the `ModuleValue` from every defining file and calls
  `unifyValues`. Abstract vs concrete is carried by `NamedValue.runtime: Option[Sourced[Expression]]` — `None` =
  abstract, `Some` = concrete. Rules: **prefer the implementation** (else keep the abstract one); **reject
  conflicts** — more than one implementation is "Has multiple implementations.", differing signatures are "Has
  multiple different definitions." So a layer may add a body but must **not** change the signature.

`data` desugars to an abstract type-constructor `FunctionDefinition` plus value-constructor functions
(`DataDefinitionDesugarer`), so even concrete types reduce to the same `NamedValue` model.

### The compiler is itself a platform (where compile-time code lives)

The **compiler is its own platform**, peer to jvm. Source resolution is **platform-scoped** (the `platform`
marker — `compiler` / `runtime` — is a key dimension threaded through the front end): the NbE checker resolves
names in the `compiler` platform, codegen (`used → uncurry → backend`) in the `runtime` platform. So one abstract
base name can have a **distinct concrete implementation per platform**, exactly as `add`/`fold`/`Bool` do via
native-binding routing (`ContributedBinding` + `BindingMergerProcessor`: the compile-time reduction wins for
checking, the runtime body for codegen).

**The compiler platform is not a monolithic layer stacked on `stdlib`; it is assembled from each layer's opt-in
compile-time contribution.** A layer that supports it ships, beside its runtime `eliot/` root, a sibling
**`eliot-compiler/`** root plus Scala natives for what no Eliot body can express. The compiler pool scans the
**entire runtime track** *plus* every root's `eliot-compiler/` overlay, and an overlay definition supersedes the
borrowed one for the same name (`PathScan.overrideFiles`; the runtime track carries no override files, so its
merge is unchanged). The compiler therefore **borrows** a runtime body wherever it is compiler-runnable — a pure
base body, a user program's pure helper, any pure `data`/fold — with the native-leaf boundary as the fail-safe: a
body reaching a bytecode leaf stalls **loudly**, never silently wrong.

What a layer may **not** borrow is a *sibling target* (jvm) that might be absent, so a layer's compile-time track
must be **self-sufficient** from the base plus its own `eliot-compiler/`. Roots reach the compiler via a single
repeatable **`--path <root>/eliot`** (no separate compiler/runtime path flags, no `compiler` Mill module);
`LangPlugin.eliotCompilerOverlay` derives each root's sibling. Two roots ship an overlay today:
`stdlib/eliot-compiler/` (the self-sufficient compile-time `Either` and `Option`, the compile-time `AbortCarrier`
that reduces `if..else raise` return guards, and the `Interval` refinement instances) and
`lang/eliot-compiler/eliot/lang/` (the compile-time `Id`). Anything pure and already on the path
(`Pair`, base bodies) is **borrowed, not duplicated**. `CompilerNativesProcessor` reads that pool; the
compile-time intrinsics (`add`, `Bool` `fold`, `true`/`false`, `typeEquals`) are Scala native **leaves** in
`SystemNativesProcessor` (lang) / `StdlibNativesProcessor` (stdlib arithmetic) — the compiler platform's leaf
bottom, mirroring jvm's bytecode leaves.

**Where to put new compiler code.** When a task needs something evaluated at compile time — a carrier, an
intrinsic, an instance used only during checking — and it is expressible in ordinary Eliot, write it as **Eliot in
the owning layer's `eliot-compiler/` root**, keeping the abstract signature in `lang`/`stdlib` and the *runtime*
concrete impl in `jvm`. **First ask whether it can be borrowed**: a pure body already in the base or reachable on
the runtime track needs no overlay copy — that is how duplication is avoided. Add an overlay copy only when the
name must be *self-sufficient* (its only runtime concrete is a sibling target's) or is a checking-only addition.
Do **not** put `data`/bodies in the abstract base, and do **not** reimplement `data`/`match`/instances as Scala
`SemValue`s — the one NbE evaluator already runs them, so a Scala reimplementation is the single-evaluator
anti-pattern. Reserve Scala natives for genuinely primitive **leaves** no Eliot body can express.

## Language Cornerstone: Use-Site Verification (Sound, Not Modular)

Eliot does not prove a definition correct for every instantiation it *could* receive (modular completeness); it
guarantees that every instantiation which *actually manifests* — the whole program is monomorphized from `main` —
is fully type-checked. Soundness is total (**no incorrect program compiles**); only the modular per-definition
certificate is given up.

**Mechanism.** An obligation that cannot be discharged abstractly (binders left neutral) — a bound-dependent
operation, an ability impl, a calculated bound — is **deferred to the concrete use site**, where the one NbE
checker decides it exactly. A modular checker would reject the definition or demand a constraint; Eliot accepts it
and verifies each use — the same monomorphize-from-`main` stance already used for codegen and ability resolution,
extended from compilation to correctness. This applies to the *implicit/calculated layer* (`infer` params,
effectful-signature guards, abilities); explicit parametric defs still get the ordinary abstract check.

**Trade-off** (intentional): more burden on library authors (totality comes from *tests* — generators and probing,
see `docs/ide-type-hints.md` — not a proof); users may meet a type error "not of their making" (a library's latent
partiality surfacing at the use site, which the IDE should surface at the definition first). It is completely
**safe** (every manifest use is checked) and far more **powerful** (full dependent/computed types at near-zero
annotation, no type-level proof obligations, accepting functions a modular system would reject). This is **not a
silent gap**: it is complete verification of the actual program, hard-erroring at the use site
(cf. [[feedback_gaps_must_be_failsafe]]).

Principle: *we prove a definition correct for every input it does take, not every input it could take — and reject
any program in which some input it does take is wrong.*

## Language Cornerstone: Effects Are a Channel (Rows In, the Carrier Written)

The user writes **effect rows** (`def main: {Console} Unit`); the compiler works in **carriers**. These are two
different things on purpose, and keeping them apart is what makes effects free of special cases. Authoritative
design: `docs/effects-as-rows.md` (§1 the four user rules, §2 the two channels, §3 elaboration and its whitelist,
§4 what is deleted / stays). `docs/effects-as-channel.md` is the **superseded** v2 design — history only.

**Four user rules, and the fourth outranks the other three** (§1):

1. **Effects run where they are written.** Strict call-by-value in *every* plain position, a bare generic slot
   included: `choose(readLine, readLine)` runs both reads.
2. **Suspension is declared, and a row is not a carrier.** A parameter that must *not* run its argument declares
   an open row (`whenTrue: {Effect} A`, `if`'s `value: {Abort} T`). A **row** position means "a value or a
   computation" — the empty row is a legal row — so a pure argument fits and is lifted. A **carrier-typed**
   position (`x: G[A]`, `IO[A]`, a pinned stack) means "a computation on this carrier": a plain `A` is a type
   error there, never a lift. Both are `F[A]` after desugaring, so the difference is read from the **row tag**
   (`EffectRow.parameterEffects`), never the shape. `{Effect}` denotes the signature's *own* carrier when it binds
   exactly one `Effect`-constrained one — which is how `else`'s `fallback: {Effect} A` is `G[A]` *and* accepts
   `host else "localhost"`.
3. **Pinned means captured.** `{Throw[E] | G} A` is a reified computation *and* an ordinary type — usable in
   `data` fields, discharger parameters, `List[TestCase]`. Open rows never appear in types; pinned rows are the
   only place a type contains a computation.
4. **An effect passes through a position if and only if that position declares it.** A **plain generic** (`A`,
   `B`, `T`) is a payload, always — a function that transports effects says so (`f: A => {Effect} B`,
   `initial: {Effect} B`). A **rowless** slot may not receive a computation: a hard error naming the slot, never a
   silent re-route. A **carrier-headed** slot captures, however that carrier is named — a pinned row's stack, one
   of the callee's own carrier binders, the concrete `Id`, or a platform run carrier. That is one predicate, not
   four arms, and there is no third kind of slot.

Rule 4 was agreed and then worked around four times, and every stall in this design's history traces to that
erosion (doc §1 table). **It outranks the tree**: where code, a stdlib signature, an example or a test conflicts
with it, the rule wins and the artefact is the defect.

**The elaborator writes the carrier; the checker never solves for one.** `row/RowElaborator` rewrites each
definition into fully explicit monadic core Eliot — `printLine[F]("hi")`, `flatMap[F](k, readLine[F])` — so
monomorphization, ability resolution, `used`/`uncurry` and the backend are unchanged consumers. Because the
ambient carrier is a *syntactic* function of the definition's own minted binder (`EffectSugarDesugarer` mints it
as generic **0**), every carrier position is **rigid**, and the base binds once at the platform entry point, so
both tracks (`IO` on jvm, `Either[String, _]` on the compile track) work without the elaborator knowing which.
Three rules complete it:

- a region's carrier is `Absent` / `Spelled` (writable from the definition's own declaration) / `Unspelled`
  (exists but only a callee can name it — a pinned capture's interior), and **only `Spelled` writes**;
- the elaborator writes **every type argument a declaration determines**, as a leading positional prefix, stopping
  at the first binder nothing determines (the region supplies the carrier; a pinned parameter supplies its row's
  ability arguments, instantiated from the captured argument's own declared row);
- a callee needing more than the ambient provides runs on the **derived discharge stack** of the difference
  (`carrier(call) = stack(callee.declaredRow ∖ ambient.declaredRow) over ambient`), filtered by the universe's own
  pinned rows, since `Suspend`-riding effects (`Console`, `Log`, `Inf`) have no carrier to discharge onto.

**What that deletes, and what must not come back.** With carriers rigid there is nothing to infer, so there is no
carrier metavariable, no join solver, no `Id`-headed uniform judgment, no mode obligation and no post-drain mode
resolver. **Reintroducing carrier *inference*** — a carrier meta, a lattice, an ordering-sensitive slot decision —
is the historical bug class (carrier theft, premature commitment) and is **prohibited**. **The anti-accretion
whitelist (§3.2) binds every future change**: the elaborator may consult only a callee's declared
parameter/return types, its declared row and carrier binders, its pinned metadata, the run-boundary registry, and
one level of type-alias expansion. A rule that inspects a *sibling argument's expression shape* is inference, not
desugaring, and is prohibited; a decision that cannot be made from the whitelist is a gap to close **in the
declarations**.

**One effect rule is left in the checker**, deliberately: a pure term meeting a **rigid** carrier-headed expected
type is `pure`-lifted (`check/EffectLifter.tryPureWrap` — no metas, no ordering, no lattice). The checker holds no
effect diagnostic of its own. The **compile track** keeps its mid-spine default ladder and deferral by design
(§8): an inline guard's carrier is still inferred there, the one live reader of the `Unifier`'s
higher-kinded-meta record.

**`Id` is the value of the empty row, and it is *written*, not manufactured.** A row-polymorphic definition
instantiated at `ρ := {}` is written at `Id` with `runId` beside it, so it is honestly well-typed rather than
well-typed-modulo-normalization. `Id` stays ordinary `data` (`data Id[A](runId: A)` + `implement Effect[Id]`,
deliberately **no** `Suspend[Id]`, so real I/O can never run on it — only the pure control effects
`Abort`/`Throw`/`State`). `channel/IdNormalizer` erases it at the `WovenValue` seam and
`WovenValueProcessor.assertNoIdResidue` is a **hard build error** on any survivor. Recurring tax: **any new
consumer of `MonomorphicValue` or of mid-mono `SemExpression`s must Id-normalize first.** What v2 was faulted for,
and what is gone, is the checker *manufacturing* a carrier head on pure judgments.

**Discharge falls out structurally, with no annotation.** A discharger's consumed effect lands on an *inner
transformer carrier* (`StateCarrier[S, G]`, not the caller's ambient `G`), so it simply drops out of the derived
row — which is why wrapper-reached discharge inside a `{Console}` body just compiles, and why there is nothing to
spell as a negative effect. A discharger must be **called directly** (`runStateToPair(s0, p)`): by rule 4 the
dot's subject is a plain type parameter, which may not carry a computation, so `p.runStateToPair(s0)` is a hard
error naming the fix. The infix dischargers `catch`/`else` resolve to a direct call and are unaffected. A
discharger's **handler may itself perform effects** (`catch`'s is `onError: E => {Effect} A`, a row over the same
carrier `G`), and a **`val`-bound** computation is dischargeable, since a call needing more than the ambient
declares carries its own discharge stack and the `val` binds the reified computation as data. Known limitation,
not a bug: a handler whose effects enter via a **declared carrier-typed parameter** must still return a
carrier-headed type — that carrier is caller-chosen, so no declaration determines it.

**An ability is not an effect by nature — a method performs an effect because it declares one.** An ability method
spells its effects with a row on its return, exactly as any other definition does
(`ability Console[F[_]] { def printLine(s: String): {Console} Unit }`); that row desugars onto the *ability's own*
binder (`EffectSugarDesugarer.abilityMethodCarrier`), so the ordinary declared-carrier and declared-row rules
answer for it and **no phase reads effect-ness off the shape of an ability's signature**. A method declaring **no**
row performs nothing — which is exactly what a **constructor class** is
(`ability Container[F[_]] { def wrap[A](a: A): F[A] }`), and what the old "any higher-kinded binder of an ability
method is a carrier" rule made impossible to express. The one exception is the **machinery** abilities
`Effect`/`Suspend`, whose methods keep spelling `F[A]`: machinery is filtered out of every row by design, and they
are recognized by name (`EffectMachinery.isMachineryAbility`).

**Carrier-ness is recognized by a tag threaded from elaboration — never by name or shape.** A pinned row desugars
to a carrier stack with no residual marker, so the marker is added at the desugar and carried on the fact
(`EffectRow.returnPinnedEffects` / `pinnedParameterIndices`), plus the platform-contributed run boundaries
(`row/RunBoundaryFunctions`) for concrete carriers no row can spell, like the synthetic main's `IO[A]`.
Classifying by the `<Ability>Carrier` naming convention, an LSP reverse table, or "has an `Effect` instance"
**miscompiles in both directions** and is prohibited.

**Pinned rows** (`{Throw[E] | Id} A`, `docs/effect-row-tails.md`): a tail after `|` makes the row a *concrete
type* — the canonical carrier stack over the base, built in core by the `<Ability>Carrier` naming convention,
entries leftmost-outermost = discharge order, no carrier generic minted. Stored (`data`-field) rows **MUST** be
pinned. The stdlib discharger signatures spell their **input** as a pinned row and their **output** as the plain
carrier (`runThrow(obj: {Throw[E] | G} A): G[Either[E, A]]`), so `signatureEquality` holds across the merge.
`Suspend`-riding effects (`Console`) have no canonical carrier and so cannot be pinned (v1).

**Rows are the user surface and the verifier's vocabulary — they never flow back into types.** `EffectRow` is
declaration metadata (like `paramConstraints`), consumed by the desugar and the renderers; verification is a
separate **channel** with exactly **two verifiers speaking one vocabulary**: the pre-mono per-definition
`RowElaborationProcessor.verifyRow` (what declarations alone settle, reported at the definition before anything
downstream runs) and the post-mono `channel/EffectAccountingProcessor` (`derived ⊆ declared` at ground
instantiations, wired as a **codegen precondition** via `getFactOrAbort`, so an undeclared effect blocks code
generation rather than merely warning). Both emit the same diagnostic ("This value performs the effect 'X' but
does not declare it…"). Accounting gates each reference's contribution by the **ride test**: it counts only if it
performs its effect on the value's *own* ambient carrier, compared by exact `GroundValue` equality against the
callee's forwarded `MonomorphicValue.ambientCarriers`. *Forward what is declared, derive what is done* — a
forwarded per-operation verdict would be a checker self-report and is rejected, as is any negative-effect surface.
The pre-mono check is bounded exactly twice by what declarations cannot settle: an unknown callee leaves the
derivation incomplete, and a definition whose declared return could *itself* be the carrier (an applied
`Box[String]`, `IO[Unit]`, a generic head) is the constructor-class shape. It also owns the one diagnostic
accounting cannot voice — "declared pure but performs effects", for a definition whose return cannot host a
carrier — since such a value's mono fails and produces no `MonomorphicValue`.

**Ambient scope.** The whole `eliot.effect` package is auto-imported: `ModuleName.effectSystemModules` joins the
`eliot.lang` prelude in `defaultSystemModules`, in a **weak** tier — an explicitly imported module is deduplicated,
and an ambient name colliding with a local declaration or explicit import is silently dropped (locals always win,
so the prelude can grow without breaking code), while explicit imports keep the strict shadowing errors. The
carrier machinery lives in the **import-required `eliot.carrier` package** so `map`/`flatMap`/`pure`/`suspend`
never enter user scope.

**Cornerstone fidelity**: this is *more* types-are-values-faithful, not less — carriers are ordinary type
constructors, `Id` is ordinary `data`, and effect flow through generics is ordinary instantiation. No side channel
does type-like work behind the type system's back, and no kind or sort is added to the type language: the
value/computation separation lives in the judgment's second channel, exactly like an `Int`'s refinement range.

**User-facing text stays in payload/row vocabulary**: carrier machinery names (`ThrowCarrier`, `AbortCarrier`, …)
and the `Id[X]` payload wrapper are never rendered to users. One inverter does it — `effect/EffectRowRendering`
driven by `EffectCarrierNaming.abilityNameOfCarrier` — used by `monomorphize/fact/GroundValueRenderer` (LSP hover,
ability-demand diagnostics) and `unify/SemValuePrinter` (`Expected:`/`Actual:` lines), so a carrier stack always
reads as the pinned row that spells it (`{Abort | IO} String`). **Recognizing carrier-ness by name is sanctioned
here and nowhere else** — a misrendering is cosmetic, the same guess in the checker miscompiles. Two deliberate
rules: `Id[X]` is erased to `X`, but an `Id` **row base is kept** (`{Throw[E] | Id} A` is legal surface and is
*not* the open row `{Throw[E]} A`). The one demand with a story rather than a name — `Suspend` at `Id` — gets a
purpose-built message. A consumer of a **pre-`WovenValue`** fact must still Id-normalize its input, since
rendering hides the names but not the inserted machinery *nodes*.

## Language Cornerstone: Total by Default (No Recursion; `Inf` is the Opt-Out)

**Eliot user code cannot express recursion or loops — full stop.** There is no `fix`/`letrec`, lambda parameters
are non-recursive, and every cycle is therefore a self/mutual reference among top-level named values, visible in
the resolved value-reference graph. The `termination` package gates this: `RecursionCheckProcessor` (running
`RecursionChecker`), placed after operator resolution and before the `row` phase, rejects any cycle in a value's
**runtime-body** reference graph ("Value 'X' is defined recursively.") — body only, never the signature, so a
covariant `data Tree(left: Tree, right: Tree)` and the monad-transformer lift are not flagged. A rejected value
never produces its `RecursionCheckedValue` fact, so it never reaches saturation or monomorphization (fail-safe by
construction). Every actual loop lives inside a **platform-provided native** (a `fold`, `forever`, the event
loop); the language mandates no recursion primitive of its own.

Three preconditions make "no recursion" mean what it says: an **occurs-check** in `Unifier` (no inferred infinite
type / Y-combinator), a **strict-positivity check** (`core.processor.StrictPositivityChecker`, no
negative-recursive `data`), and **purity** (no mutable cells — Landin's knot; guarded by
`termination/PurityGuardTest`). With a recursion-free typed core, **every program terminates by default**
(System T, not PCF) — modulo the already-accepted `Type:Type`/Girard residual.

The one opt-out is **`Inf`**, modelled as an ordinary effect *ability*
(`ability Inf[F[_]] { def forever(step: F[Unit]): {Inf} Unit }`, ambient like all of `eliot.effect`) rather than a
bespoke termination lattice — there is **no `Terminating` token**; termination is simply `Inf`'s *absence* from
the effect row. Because a recursion-free core cannot itself diverge, `Inf` can **only originate on a native**, and
it propagates to callers for free through the existing `derived ⊆ declared` subset check — a `{Console}`-only
function calling `forever` is rejected. `Inf` is **run, not discharged**: it is the one effect that may
legitimately reach `main` undischarged (the jvm layer's `implement Inf[IO]` realizes `forever` as a `while(true)`
loop), where it denotes a deliberate non-terminating program — a server or firmware super-loop. Higher-order
propagation is automatic: `Inf` rides the shared carrier like any effect (the function-coloring win — one
effect-transparent combinator is `Inf`-iff-its-step-is), with no per-arrow bit. Deferred, needing foundations that
do not yet exist: WCET/resource bounds, optional size-indexing, a *timeout*-based bound on `{Inf}` (needs a time
type), and linearity for in-place mutation.

## Language Overview

Eliot is a functional, strongly-typed language with whole-application compilation and monomorphization. Its main
building blocks:

- **Named values** — a generalization of "functions", since functions are represented fully curried
  (`a -> b -> c`), so a function is a lambda value. They are "named" because they are not anonymous and can be
  referenced from elsewhere.
- **Data** — completely represented as values in the core model, with no representation of its own. A `data`
  declaration defines a value constructor (if not abstract) and a type constructor; both are ordinary values.
- **Abilities and implementations** — typeclasses and instances, with multi-parameter capability. An
  implementation must be defined either in the module where the ability is defined or with the type it is for,
  and must be unique for a given type combination across the whole search space, with no overlap. Ability
  references are fully resolved during monomorphization, never passed around in structures.

## Compiler Change Patterns

- When making a field optional in `resolve.fact.Expression` case classes, update **all** pattern matches across
  the codebase **and** the test files that construct those cases directly.
- There is a **single** evaluator (`monomorphize/eval/Evaluator.scala`, NbE over `SemValue`). Unresolved/stuck
  terms surface as explicit errors at quote time (`PostDrainQuoter`, "Cannot resolve type."), never via a silent
  `Type` fallback. **Do not reintroduce a second compile-time evaluator** (see the Types Are Values cornerstone).

## Development Notes

- The language is in active development (see the TODO file for planned features).
- Current focus is microcontroller targets, though JVM is the implemented backend.
- The type system aims at dependent types and compile-time guarantees about resource usage.
- The standard library (`stdlib/`) is still minimal.
