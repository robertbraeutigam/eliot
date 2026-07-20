# Claude Memory

## Project Overview

ELIOT is a functional, generic programming language for microcontrollers, implemented in Scala 3.

The project contains all parts of the compiler and the ELIOT standard library.

The compiler uses a plugin-based architecture with a fact-based compilation system and supports 
multiple backends (currently JVM).

## Build System

This project uses **Mill** (version 1.1.0+) as its build tool. Scala version: **3.7.4**.

Key dependencies: `cats-effect` (IO monad), `parsley-cats` (parser combinators), `ASM 9.9` (JVM bytecode in jvm module).

### Common Commands

```bash
# Compile all modules
./mill __.compile

# Run tests for all modules
./mill __.test

# Run tests for a specific module
./mill lang.test
./mill jvm.test
./mill eliotc.test

# Filter test output by class name (the -- flag is rejected by ScalaTest)
./mill lang.test 2>&1 | grep -v DEBUG | grep "ClassName"

# Clean build artifacts
./mill clean

# Run the compiler and generate and executable JAR file from the HelloWorld example
# The generate jar file will be under target/HelloWorld.jar
./mill examples.run jvm exe-jar examples/src/ -m HelloWorld

# To run the generated file, use java
java -jar target/HelloWorld.jar
```

### Module Structure

The project is organized into these compiler/runtime modules (defined in `build.mill`):

1. **eliotc** - Core compiler infrastructure (plugins, processors, feedback, utilities)
2. **lang** - Base compiler/language components (parsing, AST, type system, module system, resolution)
3. **stdlib** - The platform-independent base standard library (abstract `type`/`def` signatures only)
4. **jvm** - JVM backend (bytecode generation using ASM, JAR generation)
5. **examples** - Example ELIOT programs

### IDE Tooling (`ide/`)

Everything editor/IDE-related lives under the top-level **`ide/`** directory:

- **`ide/lsp/`** - The LSP language server. This is a Mill module nested under `ide`, so its build
  target is **`ide.lsp`** (e.g. `./mill ide.lsp.compile`, `./mill ide.lsp.test`, `./mill ide.lsp.jar`);
  its build output lands under `out/ide/lsp/`. Depends on `lang` + `stdlib`; main class is `LspMain`.
  - `ide/lsp/package.sh` builds a runnable distribution under `ide/lsp/dist/` (git-ignored). It bundles
    **code only** — the layer `.els` are NOT shipped; the base/stdlib/platform layers reach the compiler on
    the *path* as ordinary dependencies (the client's workspace roots today, downloaded packages once a
    build system exists — see [[project_lsp_layers_from_path_not_bundled]]). It deliberately produces a
    `lib/` of **separate per-module jars**, never a fat assembly jar — each layer jar carries a same-path
    `META-INF/services/…CompilerPlugin` ServiceLoader file and a fat jar collapses those to one, dropping
    plugin registrations. See the script header and [[gotcha_assembly_jar_breaks_layers]]. It also stages a
    second classpath dir `compiler-lib/` holding **only ASM** — the one backend dep the "Run main" feature
    needs to emit bytecode that is not already in `lib/`. That CLI runs `-cp "lib/*:compiler-lib/*"` (jvm
    backend classes from `lib/eliot-jvm.jar` + ASM); ASM is staged apart so the resident server's `lib/`
    classpath stays minimal, and `eliot-jvm.jar` stays in `lib/` (not duplicated into `compiler-lib/`, which
    would make the combined "Run main" classpath carry the backend twice).
  - `ide/lsp/intellij/` - manual *user-defined server* setup for LSP4IJ (zero-build fallback): setup
    guide + importable template. Superseded for normal use by the shipped plugin in `ide/intellij/`.
  - Status: the spine is built & verified — whole-workspace diagnostics, hover/go-to-def (reverse
    `PositionIndex`), live-edit VFS overlay, in-scope-name completion, **concrete-type hover hints**
    (`TypeHintIndex` built from `MonomorphicValue` facts; `LspPlugin` monomorphizes each file's own `main`
    via `UsedNames`, so hover shows `Int[0,255]`/`A -> B` — first step only, no error recovery yet so it is
    empty on a broken buffer, never wrong), **"Run main" code lens** (server advertises `codeLensProvider`;
    `MainIndex` — built per compile from `ResolvedValue`s named `main` — yields a `▶ Run main` lens carrying
    the `eliot.runMain` command with `[sourceRoot, moduleName]`; handled client-side, see `ide/intellij`),
    **apidoc doc hover** (hover renders Markdown = an `eliot`-fenced type/signature header + the identifier's
    documentation; `DocIndex` built from `ValueDoc` facts. The `apidoc` module now participates in the pipeline: its
    `ValueDocProcessor` turns the parser's `/** */` doc comments into per-`ValueFQN` `ValueDoc` facts, `ApiDocPlugin`
    registers that processor and its HTML `run` consumes those same facts — so the site and hover share one source of
    truth. The LSP depends on `apidoc` and activates `ApiDocPlugin` as a *non-target* plugin, so only the processor runs,
    never HTML generation; `LspPlugin` demands a `ValueDoc` per name across every layer so stdlib docs hover too),
    IntelliJ via LSP4IJ. The one remaining design item,
    parser/checker **error recovery** (compiler tolerance for broken code), is tracked in
    `docs/ide-type-hints.md` (Layers A/B) and is what makes type hints work on in-progress code; the rest
    (find-refs, rename, semantic tokens, signature help) are routine additive features on the existing index.
- **`ide/textmate/`** - TextMate grammar for `.els` syntax highlighting (VS Code extension layout;
  consumable by IntelliJ TextMate Bundles and VS Code). Static editor files, *not* a build module.
- **`ide/intellij/`** - the shipped IntelliJ plugin: one install gives `.els` highlighting (bundles
  `ide/textmate`) + diagnostics (launches the `ide/lsp` server via LSP4IJ) + a **native "Eliot Application"
  run configuration**. It is a **self-contained Gradle build** (the IntelliJ Platform Gradle Plugin has no
  Mill equivalent), *not* part of the Mill build; its `prepareSandbox` shells out to `ide/lsp/package.sh`
  for the per-module server jars (never a fat jar — [[gotcha_assembly_jar_breaks_layers]]) and the
  `compiler-lib/` jars, and runs the server out-of-process via the IDE's JBR. Build with
  `cd ide/intellij && ./gradlew runIde|buildPlugin`. See `ide/intellij/README.md`.
  - **Run main** (`src/main/kotlin/.../run/`): the server's `▶ Run main` code lens fires the `eliot.runMain`
    command, which LSP4IJ dispatches client-side to `EliotRunMainCommandAction` (an `LSPCommandAction`
    registered as the IntelliJ action id `eliot.runMain` — the id MUST equal the command). It creates/reuses
    an `EliotRunConfiguration` (`ConfigurationType`/`Factory`/`Options`/`SettingsEditor`) with a before-run
    `EliotBuildBeforeRunTaskProvider` that runs the compiler CLI (`java -cp server/lib+compiler/lib …Main jvm
    exe-jar <root> -m <module> -o <out>`) to build `<out>/<module>.jar`, then the run config does
    `java -jar`. The build step gates on the compiler's exit code — `Main` now returns `ExitCode.Error` when
    `CompilationResult` has errors (`Compiler.runCompiler: IO[Boolean]`), so a stale jar is never run.

When adding new editor integrations, place them under `ide/`.

## Architecture

### Plugin System

The compiler uses a **ServiceLoader-based plugin architecture**:

- Plugins are discovered via Java's `ServiceLoader` mechanism
- Each plugin implements `CompilerPlugin` (in eliotc module)
- Plugins can depend on other plugins and configure the compilation pipeline
- Two main plugins:
  - `LangPlugin` - Provides core language compilation (tokenization → AST → type checking)
  - `JvmPlugin` - Provides JVM bytecode generation and executable JAR output

### Compilation Pipeline

The compiler follows a **fact-based compilation model**:

1. **Facts** are pieces of immutable compilation data (tokens, AST, resolved types, etc.)
2. **Processors** (sometimes referred to as "Generators") compute facts from other facts on demand
3. **FactGenerator** orchestrates lazy fact computation with caching
4. Facts are identified by their keys, which are usually a subset of the fact's data

### Main components of processing pipeline

Each of these is a package in the "lang" module, roughly in order of processing:

1. source: Reading source files from the filesystem
2. token: Tokenizer
3. ast: Building the AST
4. core: Building the core language AST
5. module: Splitting working with modules into working with individual values. It also unifies similarly named modules from different paths.
6. resolve: Resolve all identifiers to fully qualified names or parameters.
7. matchdesugar: Desugar pattern-match expressions into function applications. Checks exhaustiveness and handles nested patterns, multiple cases, constructor patterns, and wildcards.
8. operator: Resolve infix operators with proper precedence and associativity. Transforms flat expressions into correctly structured function applications.
9. effect: A tiny package holding only two helpers — `EffectMachinery` (recognises the `Effect`/`Suspend` machinery
   abilities the user never names) and `EffectCarriers` (finds a signature's carrier binders and their declared
   effects). Effect **verification no longer lives here**: the entire pre-mono `effect/` phase (`EffectCheckProcessor`,
   `DeclaredEffectChecker`, `EffectUsageCollector`, `EffectAccounting`, `CalleeSignatures`,
   `EffectDischargeSummaryProcessor`, the `EffectCheckedValue`/`EffectDischargeSummary` facts) was deleted, and with it
   the `-E` negative-row syntax and the `dischargedEffects` field on the whole fact chain
   (`docs/effect-accounting-in-monomorphize.md`, **COMPLETE**). Verification moved into the **monomorphize checker**
   (`monomorphize/check/EffectResidualChecker`, run from `TypeStackLoop.runPostDrainResolution` for each value mono):
   compute the value's **residual effect set** — the abilities demanded on its *own ambient carrier* — and require
   `residual ⊆ declared`. `Inf` rides the ambient like any effect, so the same subset test propagates it (load-bearing:
   `Inf[IO]` resolves, so without this an undeclared `Inf` would compile). The friendly diagnostic ("performs the
   effect 'X' but does not declare it") is emitted for `Console`/`Log`/`Inf` leaks whose instance resolves at the base
   carrier; `State`/`Throw`/`Abort` leaks fail earlier at `AbilityResolver` (sound, cryptic). **Discharge falls out
   structurally, with no annotation**: a discharger's consumed effect lands on an *inner transformer carrier*
   (`StateCarrier[S, G]`, not the caller's ambient `G`), so it is simply absent from the residual — which is why
   dot-chained and wrapper-reached discharge (`p.runStateToValue(s0)` inside a `{Console}` body) now just compiles, and
   why there is nothing to spell as `-E`. **Discharge-to-a-pure-value works**: the **identity carrier `Id`**
   (`eliot.lang.Id` — abstract `type Id[A]`/`def runId` in the *lang* layer's `eliot/` root, since the checker inserts
   it by fixed FQN; `data Id[A](runId: A)` + `implement Effect[Id]` in jvm and the `lang/eliot-compiler` overlay;
   deliberately NO `Suspend[Id]`, so real I/O can never run on it — only the pure control effects `Abort`/`Throw`/`State`
   can), and the checker's **pure-boundary Id defaulting** (`EffectLifter.tryIdDefault`, consulted from the
   return-boundary ladder and the `let` expectation in `Checker`) solves a fully-discharged body's still-flex residual
   carrier to `Id` and unwraps it with an inserted `runId` — so `def sign(f: Bool): String = if(f, "+") else "-"` (and
   `catch`/`runStateTo…` under pure returns, chains, block `val`s) just works. Remaining boundary cases, **documented
   limitations, not bugs**: a discharger consumes the *carrier*, so it must receive the effectful call as an expression
   (`printLine(x else "d")`), never a `val`-bound binder — a `val x = <effectful>` sequences the carrier away via
   `flatMap`; and a handler whose effects enter via a **declared carrier-typed parameter**
   (`def getOr(x: {Abort} String, d: String)`) must still return a carrier-headed type (`G[A]`), not a bare pure type —
   that carrier is caller-chosen and can never default to `Id`. The "declared pure but performs effects" fail-safe
   (`EffectResidualChecker.checkDeclaredPure`) is discharge- and Id-aware: a genuine undischarged effect under a pure
   nullary return is reported ("performs an effect but is declared pure"), while a fully-discharged residual (which
   Id-defaults with no committed mismatch) is accepted.
   Effect rows may also be **pinned** (`{Throw[E] | Id} A`, `docs/effect-row-tails.md`): a tail after `|` makes the row
   a *concrete type* — the canonical carrier stack over the base, built in core by the `<Ability>Carrier` naming
   convention (`EffectSugarDesugarer`), entries leftmost-outermost = discharge order, no carrier generic minted. Stored
   (`data`-field) rows MUST be pinned (open field rows error; the old carrier-lift lowering survives only as error
   recovery); the stdlib discharger signatures spell their **input** as a pinned row and their **output** as the plain
   carrier (`runThrow(obj: {Throw[E] | G} A): G[Either[E, A]]` — the pinned input desugars in core to the identical
   structure the jvm-generated data-field accessor has, so `signatureEquality` holds across the merge); LSP hover
   renders a concrete carrier stack back as its pinned row (`GroundValueRenderer`). Suspend-riding effects (`Console`)
   have no canonical carrier and so cannot be pinned (v1; the designed extension is an abstract platform base
   `Suspended` — see the doc).
   The whole `eliot.effect` package is **ambient** (auto-imported): `ModuleName.effectSystemModules` joins the
   `eliot.lang` prelude in `defaultSystemModules`, in a *weak* tier (`ModuleValueProcessor`) — an explicitly imported
   module is deduplicated and an ambient name colliding with a local declaration or explicit import is silently
   dropped (locals always win; the prelude can grow without breaking code), while explicit imports keep the strict
   shadowing errors. The carrier machinery moved to the **import-required `eliot.carrier` package**
   (`stdlib/eliot/eliot/carrier/Effect.els`/`Suspend.els`) so `map`/`flatMap`/`pure`/`suspend` never enter user scope;
   `WellKnownTypes.effectModule` points there, `EffectMachinery` matches by bare name and is unaffected.
10. ability: Checks and returns a type-specific ability implementation.
11. monomorphize: Monomorphic type checker. Evaluates data type and value definitions into typed structures and checks all types at their usage with all instantiated values, using the single NbE evaluator. (This phase absorbed the former standalone `eval` phase, which was removed.) Also hosts the **effect auto-lift** (`check/EffectLifter`): the bind/`pure` decision for an effectful term in a pure position is check-mode elaboration per concrete instantiation — undecidable from declared signatures alone — with flex argument slots deferred until later arguments rigidify them (Phase A/B in `Checker.inferSpine`).
12. used: Collects all the used value names starting at a given "main".
13. uncurry: Uncurries function calls, so its easier to generate on the backend.

### Error Handling

- Errors are reported through `CompilationIO` monad (in lang module)
- `SourcedError` tracks errors with source position information
- `User` and `Logging` traits provide user-facing messages and debug logging

## Testing

- Tests extend `AsyncFlatSpec` with `AsyncIOSpec` with `Matchers`
- Test files are in `<module>/test/src/` directories
- A processor test that needs more than a couple of leaf phases runs the **whole** pipeline via the shared
  `plugin.LangProcessors` builder — `extends ProcessorTest(LangProcessors()*)` (or with `systemModules = …` /
  `maxNestedRepeats = …` when those differ) — never a hand-listed prefix of processors. Computation is demand-driven and
  `SequentialCompilerProcessors` dispatches each key to its one handling processor, so a harness triggering only an early
  fact simply never runs the later phases; carrying the full pipeline is free and a new phase is then added in exactly
  one place. Only true leaf tests (tokenizer/AST/core, or a manual-fact-injection harness) list processors explicitly.
  The jvm backend test reuses the same list and appends its backend processor: `LangProcessors(…) :+ JvmClassGenerator()`.

## Language Cornerstone: Types Are Values (λ\*)

Eliot has **no internal distinction between the type level and the value level**. Type
constructors (`Int[..]`) and value constructors (`Int(..)`) are both ordinary named values
("functions"), and the type *of* a value is itself just another value. The only thing that makes
a computation "type-level" is *when* it is forced: type-level code happens to be evaluated by the
compiler before code generation, but that staging is incidental, not a difference in kind.
Formally this is a **non-stratified Pure Type System** — λ\* / "type-in-type" (`Type : Type`) —
where the compile-time/runtime split is a pure **phase / erasure** distinction and type checking is
implemented as **Normalisation by Evaluation** (the `monomorphize` package: one `Evaluator`, one
`SemValue` domain shared by types and values, `VType` as an ordinary value). This is deliberately
*not* "dependent types" as a bolted-on feature; dependency is merely a consequence of types being
values. Accepted trade-off: `Type : Type` is logically inconsistent (Girard's paradox) — fine for a
general-purpose language, with termination/totality handled separately rather than by a universe
hierarchy.

**Sanctioned sugar vs. required discipline.** Familiar surface distinctions are *intentional sugar*,
not violations: the `Qualifier.Type`/`Qualifier.Default` namespaces, the `[]` vs `()` call/pattern
syntax, and the restricted `Expression.typeParser` all collapse to "the same `FunctionDefinition`
with a different qualifier tag." What the cornerstone *requires* of every task: there is exactly one
evaluator and one value domain (never a second, weaker "compile-time" interpreter); type equality is
definitional (force/normalise via that evaluator, then compare) rather than a parallel bespoke
mechanism; and kind/arity metadata (e.g. `RoleHint.TypeConstructor`) stays out of semantic phases.

The cornerstone-fidelity clean-ups are **complete** (the tracking plan has been retired). Durable
guardrails it leaves behind: (1) `unify` is *pure definitional equality* — directional `Int` widening
is a user-defined `Coerce` ability inserted in the checker's check mode (now *implemented*, and as of D4
factored into its own refinement-lattice module:
`monomorphize.refine.RefinementSolver.unifyOrCoerce`/`tryCoerce`/`buildCoercedExpr`), never a `refinements` map / assignability arm in the unifier; (2) `VPi` is the one primitive Π-former
*on principle* — do not fold `Function` into an ordinary `data` declaration; (3) kind/arity metadata
(`RoleHint`, esp. `typeParamCount`) must not drive any typing decision — its only sanctioned reads are
constructor-shape reconstruction for `match`.

## Language Cornerstone: Platform-Independence via Layers

Eliot targets everything from an ATtiny to the JVM, so the language and its base stdlib commit to **no
platform assumptions** — not even the size of an `Int`. The rule is **no platform *representation***: the base
layer never says how a type is laid out or how a primitive is computed. So everything representation- or
platform-dependent is declared **abstractly** — `type`s without a value constructor, body-less `def`
signatures (`Int`, `printLine`, the `+`/`-`/`*` operators whose bodies do width dispatch). It *may*, however,
carry `def` bodies and ability instances when the computation is **genuinely platform-independent** — byte-for-byte
the same on every target (e.g. `fitsIn`, the effect discharge helpers `catch`/`orElse`/`runStateToPair`, the pure
type-level `Combine[Int,Int]` join). What it must never contain is `data` (a chosen representation), a native
leaf, or any representation-dependent body.

- `type Int[MIN: BigInteger, MAX: BigInteger]` — an abstract type; no value constructor, no chosen width.
- `def foldLeft[A, B](initial: B, combine: ..., list: List[A]): B` — an abstract function; signature only, no
  implementation.
- `type Byte = Int[-128, 127]` — a `type` *with* a body is just an alias; still platform-neutral.

A `type X = ...` (alias) and a body-less `type X` differ only by the presence of a body; a `data X(...)`
declaration is the *concrete* form that additionally introduces a value constructor. **The base stdlib
must avoid `data` and any native or representation-dependent body** — those belong to platform layers;
platform-independent bodies (identical on every target) are allowed. For the operational mechanics of
placing/moving `.els` files across layers — the two-pool (compiler vs runtime) resolution, the
abstract↔concrete merge, and its signature-match gotchas — use the **`eliot-layers`** skill.

**Layers = redefinition, not inheritance.** A platform "implements" an abstract definition by simply
*defining the same name again*, in its own root path, with a body. There is no `extends`, `override`
keyword, or instance mechanism — co-located definitions of the same qualified name across root paths are
**merged**, preferring the concrete one. Example: the base declares `type List[A]` and body-less
`def foldLeft(...)` (abstract); the `jvm` layer re-declares the same names concretely (backed by its
`java.util.List` native). The compiler unifies them into a single value. (The carrier itself is NOT such a
merge anymore: there is no base `IO` at all — the concrete `data IO[A](block: ...)` is the platform-owned
`eliot.jvm.IO`, outside the prelude, and user programs never name it. `main` declares an effect row
(`def main: {Console} Unit`); the jvm target's synthesized entry point instantiates `main`'s inferable
carrier to `IO` by ordinary unification and runs the thunk — see `SyntheticMainSourceProcessor`.)

**Layers *mix*, they do not *stack*; every file must stand on its own.** Name resolution is per-file (a file's
dictionary is its own declarations + imports, never names declared in a *sibling* file of the same module). So when one
file needs a name another file of the same module declares — e.g. a carrier-generic ability instance
`implement[F[_] ~ Suspend] Console[F]`, which must be colocated with its ability and therefore lives in the ability's
module — that file must **re-declare (copy) what it needs** (here, `ability Console[F[_]]` itself); the merge then
**verifies the copies agree** (`signatureEquality`) rather than letting them drift. Duplication is the sanctioned
mechanism here — do *not* "fix" a cross-file reference by widening the resolver to span sibling files.

**How it works mechanically** (the `source` + `module` packages):
- The compiler is given multiple **source mounts** per platform pool (`source/scan/SourceMount.scala`; CLI root
  paths become `FilesystemMount`s, plugins may contribute others — the jvm target mounts its synthesized
  `main.els`, the LSP routes overlaid buffers to a `vfs:` namespace). `PathScanner`
  (`source/scan/PathScanner.scala`) resolves a module path against *all* mounts of the pool and returns *every*
  matching URI as one `PathScan`; each URI scheme's `SourceContent` is served by exactly one processor.
- `ModuleNamesProcessor` extracts names per file; `UnifiedModuleNamesProcessor` flattens the name sets of
  all files for a module into one.
- For each name, `UnifiedModuleValueProcessor.scala` collects the `ModuleValue` from every file that
  defines it and calls `unifyValues`. The abstract/concrete distinction is carried by
  `core/fact/NamedValue.scala`'s `runtime: Option[Sourced[Expression]]` — `None` = abstract (no body),
  `Some` = concrete. Unification rules:
  - **Prefer the implementation**: pick the value whose `runtime.isDefined`; if none, keep the abstract one.
  - **Reject conflicts**: more than one implementation → "Has multiple implementations."; differing
    signatures → "Has multiple different definitions." (`signatureEquality` must hold across all layers, so
    a layer may add a body but must not change the signature).

So "implementing the abstract stdlib for a platform" = adding a layer (a root path) that re-declares the
same names with bodies / as `data`. The base stays universal; each platform fills in representation and
behaviour. (`data` desugars to an abstract type-constructor `FunctionDefinition` plus value-constructor
functions via `DataDefinitionDesugarer`, so even concrete types reduce to the same `NamedValue` model.)

### The compiler is itself a platform (where compile-time code lives)

The **compiler is its own platform**, peer to runtime platforms like jvm. Source resolution is **platform-scoped**
(the `platform` marker — `compiler` / `runtime` — is a key dimension threaded through the front-end): the NbE checker
resolves names in the `compiler` platform; codegen (`used → uncurry → backend`) resolves in the `runtime` platform.
So one abstract base name can have a **distinct concrete implementation per platform** — exactly as `add`/`fold`/`Bool`
already do via the native-binding routing (`ContributedBinding` + `BindingMergerProcessor`: the compile-time reduction
wins for checking, the runtime body is used for codegen). **The compiler platform is not a monolithic layer stacked on
`stdlib`; it is assembled from each layer's opt-in compile-time contribution.** A layer that wants to support the compiler
platform ships, beside its runtime `eliot/` root, a sibling **`eliot-compiler/`** root (its checking-time redefinitions /
instances) plus Scala natives for what no Eliot body can express. The compiler pool scans the **entire runtime track**
*plus* every root's `eliot-compiler/` overlay, and an overlay definition supersedes the borrowed platform one for the
same name (the compiler-as-platform override — `PathScan.overrideFiles` + `UnifiedModuleValueProcessor`, order-free
otherwise; the runtime track carries no override files so its merge is unchanged). So the compiler **borrows** a runtime
body where it is compiler-runnable — a pure base body, a user program's pure helper, any pure `data`/fold — with the
native-leaf boundary as the fail-safe (a body reaching a bytecode leaf stalls loudly, never silently wrong). What a layer
may **not** borrow is a *sibling target* (jvm) that might be absent: so a layer's compile-time track must be
**self-sufficient** from the base + its own `eliot-compiler/`. Roots reach the compiler via a **single repeatable
`--path <root>/eliot`** (no separate `--compiler-path`/`--runtime-path`, no `compiler` Mill module); `LangPlugin` derives
each root's `eliot-compiler/` sibling for the compiler pool (`LangPlugin.eliotCompilerOverlay`). Two roots ship an
overlay today: `stdlib/eliot-compiler/eliot/lang/` holds the self-sufficient compile-time `Either`
(concrete `data Either` + `foldEither` + `implement Effect[Either[String]]`/`Throw[String, Either[String]]`), `Option`
(`data Option` + `foldOption`, needed by the guards), and the guard combinator bodies (`Guard.els`) — with abstract
`type Either[E, A]`/`type Option[A]` in the `stdlib` base and the runtime `jvm` `Either`/`Option` structurally identical;
and `lang/eliot-compiler/eliot/lang/` holds the compile-time `Id` (concrete `data Id` + `implement Effect[Id]`, the
identity carrier the checker's pure-boundary defaulting inserts — lang owns `Id`'s abstract declaration, so lang's
overlay carries its compile-time concrete).
Anything the compiler needs that is pure and already on the path (`Pair`, base bodies) is **borrowed**, not duplicated.
`CompilerNativesProcessor` (contributing the `compiler` `ContributedBinding` from the compiler-marker `SaturatedValue`)
reads that pool. The compile-time intrinsics (`add`, `Bool` `fold`, `true`/`false`, `typeEquals`) are Scala native
**leaves** in `SystemNativesProcessor` (lang's own) / `StdlibNativesProcessor` (stdlib's arithmetic) — the compiler
platform's leaf bottom, mirroring jvm's bytecode leaves. The `Effect`/`Throw` instances are compiler-pool-only; the
effectful-signatures discharge (`monomorphize/check/CalculatedReturnResolver.scala`: a `{Throw[String]}` return on the
`Either[String, _]` carrier reads back as `Right(t)` ⤳ the type `t` / `Left(msg)` ⤳ a diagnostic) recognises the
carrier's `Left`/`Right` by FQN and reduces `fold`/`foldEither` via the merged `NativeBinding`.

**Where to put new compiler code.** When a task needs something the compiler must *evaluate at compile time* — a
carrier (e.g. the effectful-signatures `Either`), a compile-time intrinsic, or an ability instance used only during
checking — and it is expressible in ordinary Eliot, write it as **Eliot in the owning layer's `eliot-compiler/` root**
(for a base-owned name, `stdlib/eliot-compiler/…`), keeping the abstract signature in `lang`/`stdlib` and the *runtime*
concrete impl in `jvm`. First ask whether it can be **borrowed** instead: a pure body that already lives in the base or
is reachable on the runtime track needs no `eliot-compiler/` copy (the compiler borrows it — that is how duplication is
avoided, e.g. `Pair`). Add an overlay copy only when the name must be *self-sufficient* (the layer can't depend on a
sibling target for it — e.g. `Either`/`Option`, whose only runtime concrete is jvm's) or is a checking-only addition (an
`Effect`/`Throw` instance with no runtime counterpart). Do **not** put `data`/bodies in the abstract base (it stays
representation-free, per the cornerstone above), and do **not** reimplement `data`/`match`/instances as Scala
`SemValue`s — the one NbE evaluator already runs them, so a Scala reimplementation is the single-evaluator
anti-pattern. Reserve Scala natives (`SystemNativesProcessor`/`StdlibNativesProcessor`) for genuinely primitive
**leaves** no Eliot body can express (arbitrary-precision arithmetic, `Bool` branching) — these are the compiler
platform's leaf bottom, mirroring jvm's bytecode leaves.

## Language Cornerstone: Use-Site Verification (Sound, Not Modular)

Eliot does not prove a definition correct for every instantiation it *could* receive (modular completeness); it
guarantees every instantiation that *actually manifests* — the whole program is monomorphized from `main` — is
fully type-checked. Soundness is total (**no incorrect program compiles**); only the modular per-definition
certificate is given up.

**Mechanism.** An obligation that can't be discharged abstractly (binders left neutral) — a bound-dependent
`Coerce`/operation, an ability impl, a calculated bound — is **deferred to the concrete use site**, where the one
NbE checker decides it exactly. A modular checker would instead reject the definition or demand a constraint; Eliot
accepts it and verifies each use — the same monomorphize-from-`main` stance already used for codegen and ability
resolution, extended from compilation to correctness. Applies to the *implicit/calculated layer* (`infer` params,
calculated returns — the `auto`/implicit-generics feature, shipped — and abilities); explicit parametric defs still
get the ordinary abstract check.

**Trade-off** (intentional): more burden on library authors (totality comes from *tests* — generators + probing,
see `docs/ide-type-hints.md` — not a proof); users may meet a type error "not of their making" (a library's latent partiality surfacing
at the use site, which the IDE should surface at the definition first); completely **safe** (every manifest use is
checked); far more **powerful** (full dependent/computed types — bounds, costs, resources — at near-zero
annotation, zero type-level proof obligations, no inversion, accepting functions a modular system would reject).
This is **not a silent gap**: it is complete verification of the actual program, hard-erroring at the use site,
never silent acceptance of wrong typing (cf. [[feedback_gaps_must_be_failsafe]]).

Principle: *we prove a definition correct for every input it does take, not every input it could take — and reject
any program in which some input it does take is wrong.*

## Language Cornerstone: Total by Default (No Recursion; `Inf` is the Opt-Out)

**Eliot user code cannot express recursion or loops — full stop.** There is no `fix`/`letrec`, lambda parameters
are non-recursive, and every cycle is therefore a self/mutual reference among top-level named values, visible in the
resolved value-reference graph. The `termination` package gates this: `RecursionCheckProcessor` (running
`RecursionChecker`), placed after operator resolution and before the effect checks, rejects any cycle in a value's
**runtime-body** reference graph (`"Value 'X' is defined recursively."`) — body only, never the signature, so a
covariant `data Tree(left: Tree, right: Tree)` and the monad-transformer lift are not flagged. A rejected value never
produces its `RecursionCheckedValue` fact, so it never reaches saturation or monomorphization (fail-safe by
construction). Every actual loop instead lives inside a **platform-provided native** (a `fold`, `forever`, the event
loop); the language mandates no recursion primitive of its own. Three preconditions make "no recursion" mean what it
says: an **occurs-check** in `Unifier` (no inferred infinite type / Y-combinator), a **strict-positivity check**
(`core.processor.StrictPositivityChecker`, no negative-recursive `data`), and **purity** (no mutable cells — Landin's
knot; guarded by `termination/PurityGuardTest`). With a recursion-free typed core, **every program terminates by
default** (System T, not PCF) — modulo the already-accepted `Type:Type`/Girard residual the Cornerstone does not close.

The one opt-out is **`Inf`**, modelled as an ordinary effect *ability* (`stdlib/.../Inf.els`:
`ability Inf[F[_]] { def forever(step: F[Unit]): F[Unit] }`, ambient like all of `eliot.effect`) rather than a bespoke termination
lattice — there is **no `Terminating` token**; termination is simply `Inf`'s *absence* from the effect row. Because a
recursion-free core cannot itself diverge, `Inf` can **only originate on a native** (`forever`), and it propagates to
callers for free through the *existing* `used ⊆ declared` effect subset check — a `{Console}`-only function calling
`forever` is rejected ("performs the effect 'Inf' but does not declare it"). `Inf` is **run, not discharged**: it is
the one effect that may legitimately reach `main` undischarged (the jvm layer's `implement Inf[IO]` realizes `forever`
as a `while(true)` loop), where it denotes a deliberate non-terminating program — a server / firmware super-loop.
Higher-order propagation is automatic: `Inf` rides the shared carrier like any effect (the function-coloring win — one
effect-transparent combinator is `Inf`-iff-its-step-is), with no per-arrow bit. Deferred (needs foundations that do not
yet exist): WCET/resource bounds and optional size-indexing, a *timeout*-based bound on `{Inf}` (needs a time type),
and linearity for in-place mutation. See [[project_recursion_as_effect]].

## Language Overview

Eliot is a functional, strongly-typed language, with whole-application compilation
with monomorphization.

The main building blocks of the language are:
- Named values: These are a generalization of "functions", because functions are represented in their fully 
                curried form as "a -> b -> c", so it's a lambda value. They are "named" because they are not
                anonymous and can be referenced ("called") from elsewhere.
- Data: Data is completely represented as values in the core model and has no own unique representation. 
        Data defines a value constructor "function" (if not abstract), and a type constructor function. Both are
        "normal" values / "functions".
- Abilities and ability implementations: These are typeclasses and typeclass instances, with multi-parameter capability. Ability implementations
        must be defined either in the module where the ability is defined or with the type they are defined for.
        Ability implementations must be unique for a given type combination in the whole search space, with no overlap.
        References to ability values are fully resolved during monomorphization, they are not passed around in
        structures as in some other languages.

## Compiler Change Patterns

- When making a field optional in `resolve.fact.Expression` case classes, update ALL pattern matches across
  the codebase (eval, monomorphize phases) **and** test files that directly construct those cases.
- There is a **single** evaluator (`monomorphize/eval/Evaluator.scala`, NbE over `SemValue`); the
  old separate `eval`/`interpret` compile-time evaluator was removed. Unresolved/stuck terms surface
  as explicit errors at quote time (`PostDrainQuoter`, "Cannot resolve type."), never via a silent
  `Type` fallback. Do not reintroduce a second compile-time evaluator (see the Language Cornerstone).

## Development Notes

- The language is still in active development (see TODO file for planned features)
- Current focus is on microcontroller targets, though JVM is the implemented backend
- The type system aims to support dependent types and compile-time guarantees about resource usage
- The standard library (`stdlib/`) is currently minimal
