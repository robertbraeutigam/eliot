# Compiler architecture review (fact engine + pipeline)

Last performed: **2026-07-04** (tree at `a7b2e11a`).

A whole-compiler architecture review: the fact-based compilation model (`eliotc` engine), the
fact/processor interactions across the `lang` pipeline, the back half (saturate → monomorphize →
used → uncurry → jvm), and the plugin/session/incremental-cache orchestration — compared against
best-in-class query-based compilers (rustc queries, Salsa, Roslyn, GHC, sixty). Companion to
`monomorphize-review.md`, which covers the NbE checker package internals; this document stays at
the architecture level.

## 1. Scope and method (how to repeat)

1. **Read the engine in its entirety** — `eliotc/src/.../processor/` (`CompilerFact`,
   `CompilerFactKey`, `CompilerProcessor`, `CompilationProcess`, `CompilerIO`, the `common/` base
   processors) and `eliotc/src/.../compiler/` (`IncrementalFactGenerator`, `CompilationSession`,
   `CompilationServer`, `Compiler`, the `cache/` subpackage). The engine is small (~37 files);
   the load-bearing invariants live in `IncrementalFactGenerator` + `DependencyTrackingProcess`.
2. **Map every phase's facts and processors**: for each `lang` package, the fact types (key
   granularity, payload), the producing processor, and its `getFact` reads. Build the dependency
   graph and note fan-out/fan-in points and back-edges.
3. **Trace the back half separately**: how instantiation keys (`Seq[GroundValue]`) and the
   platform axis appear in keys; the `Track` seam; the native-leaf boundary; how codegen consumes
   facts and where it breaks out of the model.
4. **Trace the orchestration**: plugin discovery/activation, session lifecycle, cache
   load/persist, and how the four frontends (CLI, LSP, apidoc, tests) drive `compileOnce`.
5. **Verify suspected engine hazards empirically**, not just by reading: this round used a
   two-file test program built twice through `./mill examples.run jvm exe-jar … -o <scratch>`
   with `com.vanillasource.eliot.eliotc.compiler` at debug level (temporary edit of
   `examples/resources/log4j2.xml`, reverted) to observe the incremental engine's
   regenerated/materialised/validated counts and probe cache staleness.

## 2. Verdict

The architecture is a **query-based compiler** — the same family as rustc's query system, Salsa,
Roslyn, and sixty. Facts are memoized query results; processors are providers; the engine dedups
via a `Deferred`-per-key map; incrementality is demand-driven backward validation with an
equality cutoff. Several parts are ahead of what this project size would predict:

- The incremental cache validates non-serializable `SemValue`-bearing facts **structurally**
  through recorded edges without materialising them (`IncrementalFactGenerator.scala:114-119`) —
  Salsa-grade red-green machinery; a no-change run materialises no `SemValue` at all.
- The active request chain is threaded **explicitly** through `DependencyTrackingProcess` (no
  fiber-local/ambient state), so dependency recording and cycle awareness are ordinary values.
- The `Track` seam (5 divergence hooks, checker core shared) keeps the two-platform cornerstone
  out of the checker's control flow.
- The LSP live-edit overlay is leaf-fact substitution — the same shape as Roslyn workspaces.
- Fact gating by non-registration (`registerFactIfClear`) is a consistent, structural fail-safe
  idiom: a rejected value simply never exists downstream.

The weakness is not the model but **what the engine does not enforce**. The invariants the
architecture depends on — one producer per key, acyclic demand, "no fact implies an error was
reported", pull-only fact flow — are conventions, not checks. Findings E1–E4 below convert every
silent failure mode found into a loud one; they are engine-local and semantics-neutral.

## 3. Findings — engine (`eliotc`)

### E1. Dispatch is an untyped broadcast; the key→processor mapping exists nowhere — OPEN

`SequentialCompilerProcessors.generate` offers every requested key to every processor; each
no-ops unless its `ClassTag` matches (`SingleKeyTypeProcessor.scala:16-20`). Consequences:

- **A key nobody produces is silently `None`.** `getFact = None` conflates "producer declined"
  (a gate), "producer errored", and "no producer exists" (missing processor in a harness, plugin
  misconfiguration, refactor drift).
- **A double producer is silently first-wins** — the second `registerFact` is a no-op on the
  completed `Deferred` (`IncrementalFactGenerator.scala:59-60`). The LSP *depends* on this:
  `VirtualFileStatProcessor` must precede `FileStatProcessor` in the plugin fold order and win
  the race (`LspPlugin.scala:49-60`) — a correctness-critical override expressed as list
  position.
- Every fact request scans all ~40 processors (noise, not a bottleneck — but a symptom).

**Fix**: each processor declares its key class(es); the session builds
`Map[Class[Key] → Processor]`, fails startup on undeclared duplicates, supports an explicit
"override" registration (for the VFS), and errors at runtime on an unhandled key type. rustc has
exactly one provider per query, registered centrally, with explicit overrides. This makes
CLAUDE.md's "one handling processor" claim structurally true.

### E2. An unguarded fact cycle hangs the compiler instead of erroring — OPEN

The requester blocks on `deferred.get` (`IncrementalFactGenerator.scala:56`); the safety net only
fires if generation *returns*. Cycle avoidance is voluntary — exactly three sites check
`activeFactKeys` (`BindingClosure.scala:111`, `ReducedBindingClosure.scala:56`,
`CalculatedReturnResolver.scala:191`); everywhere else relies on the key graph being naturally
acyclic. The CLI has no timeout; the LSP only escapes via the next edit's cancel-restart.

**Fix**: the ancestors chain already flows through every `getFact` — check
`chain.contains(dependency)` in `DependencyTrackingProcess.getFact` and fail the fact with a
synthesized "cyclic fact demand: A ← B ← A" diagnostic instead of forwarding the read. The three
domain-specific guards still run first and give better messages; the engine check is the
backstop. Nearly free; eliminates the only hang-forever failure mode.

### E3. Incremental correctness for *pushed* facts is by race, not by construction — OPEN

`ModuleValueProcessor` is a fan-out: asked for one `ModuleValue.Key`, it registers facts for
**every** name in the file (`ModuleValueProcessor.scala:35-49`). A pushed fact never gets a
`directDependencies` entry — not even when later demanded, because the demand finds the
already-completed `Deferred` and skips `regenerate`. `buildCacheData` then marks it
`injected = !deps.contains(key)` (`IncrementalFactGenerator.scala:196`), and injected entries are
**accepted from cache on sight, forever** (`IncrementalFactGenerator.scala:76`). The flag was
designed for backend-injected dynamic sources (`CacheEntry.scala:17-19`); the heuristic cannot
tell those apart from processor pushes.

**Empirical status (2026-07-04)**: a cold build materialised 747 facts with only 711 generations
— ~36 entries entered the cache as `injected`. Two crafted staleness attacks (edit a helper whose
`ModuleValue` was push-only, switch `main` to use it, rebuild incrementally) both produced
*correct* output: ability-implementation scanning demands nearly every name, and on a file change
some sibling's regeneration re-pushes fresh values before the stale entry is read. That is
timing, not an invariant — whether a demanded-after-change pushed fact is served stale or fresh
depends on which fiber wins.

**Fix**: facts registered *during* a generation inherit that generation's dependency set (they
depend on exactly what the generation read — `DependencyTrackingProcess.registerFact` knows the
generating key); reserve `injected` for registrations made outside any generation
(session/`DynamicContent` injection). Cache correctness becomes by-construction.

### E4. "No fact ⇒ an error was reported" is unenforced — OPEN

`abort` with an empty error chain is representable, and a generation producing neither fact nor
error is a legal no-op — so a demanded-but-unproducible target can yield exit code 0 with no
output (`JvmPlugin.run` discards the jar fact's value with `.void`).

**Fix**: when the safety net completes a key with `None` and that generation contributed zero
errors, synthesize an internal diagnostic ("processor for key X produced neither fact nor
error"). Trivial; with E1 it makes "silent nothing" impossible.

## 4. Findings — pipeline (`lang`)

### P1. The value chain re-declares the same 11-field record five times; two facts are identity wrappers — OPEN

`ResolvedValue → BlockDesugaredValue → MatchDesugaredValue → OperatorResolvedValue →
RecursionCheckedValue → EffectCheckedValue → SaturatedValue` all carry
`(vfqn, name, runtime, typeStack, paramConstraints, fixity, precedence, opaque, inferableArity,
roleHint, platform)`. `RecursionCheckedValue` and `EffectCheckedValue` wrap an unchanged
`OperatorResolvedValue` — they exist purely so the gate is structural. **The gating idiom is
good** (you cannot reach saturation without passing the gates — keep it); the payload duplication
is the fixable part: one shared record generic in the expression type (`ValueInfo[E]`) collapses
five near-identical declarations while keeping distinct fact types and gates.

The outlier: `BlockDesugaredValue` reuses `resolve.fact.Expression` verbatim
(`block/fact/BlockDesugaredValue.scala:8`), so the type system cannot distinguish pre- from
post-block-desugar trees — the only phase boundary in the chain with no type-level meaning. Give
it its own expression type or fold the block lowering into an adjacent phase. (GHC's Trees That
Grow is the reference solution for phase-indexed ASTs; heavier than warranted here.)

### P2. The platform axis is encoded four different ways — OPEN

(a) a `platform` key field (`SaturatedValue`, `NativeBinding`, `ResolvedValue`, …); (b) a
distinct *fact type* (`CompilerMonomorphicValue` vs `MonomorphicValue`); (c) implicit-runtime
with no marker (`MonomorphicValue`, `UncurriedMonomorphicValue`, `UsedNames`,
`TransparentBinding`); (d) a *label string* (`ContributedBinding.Key(vfqn, label)` — `user` means
runtime-body, `compiler` means compiler-body). The twin-type choice (b) is defensible — no
`compiler → runtime` edge can even be named. The label-smuggling (d) is what forces the
pool-membership probe to be reimplemented four times (`DeclaringPool.scala:36`,
`CompilerNativesProcessor.scala:55,117`, `CompilerMonomorphicTypeCheckProcessor.scala:77`).

**Fix**: platform-key `ContributedBinding`; route all pool probes through `DeclaringPool`.

### P3. `ability` is not a pipeline phase — it is a subservice of the checker — OPEN (layout)

Its facts are keyed by `GroundValue` (a monomorphize type) and demanded mid-check;
`AbilityMatcher` (in `ability/util/`) operates entirely on `SemValue`s and instantiates its own
`Evaluator`/`Unifier` (`AbilityMatcher.scala:148,218`), while `AbilityResolver` (a checker
collaborator) lives inside `monomorphize/check/`. The code dependency is bidirectional. This
mirrors rustc, where trait solving lives inside type inference — and rustc eventually carved out
an explicit trait-solver interface because the entanglement hurt.

**Fix**: either move `AbilityMatcher` into monomorphize (honest) or define a narrow solver
interface it implements (modular). CLAUDE.md listing `ability` as pipeline phase 10 misdescribes
the topology.

### P4. Smaller pipeline inconsistencies — OPEN

- **Fixity/precedence read from three different phase facts** for the same operator:
  `BlockDesugaringProcessor.scala:137` reads `ResolvedValue`, `OperatorResolverProcessor.scala:88`
  reads `MatchDesugaredValue`, `InfixPrecedenceResolver.scala:70` reads `ResolvedValue`. Pick one
  canonical source (earliest available: `ResolvedValue`).
- **`RepresentationLowering` reaches ~7 phases back** to `OperatorResolvedValue` just to test
  `runtime.isDefined` (`RepresentationLowering.scala:37`) when `TransparentBinding` (a stuck
  `VTopDef` for body-less values) already encodes that.
- **`saturate`/`effect` read callees' raw `OperatorResolvedValue`**, bypassing the callees' own
  gates (`SaturatedValueProcessor.scala:212`, `CalleeSignatures.scala:23`). Sound today — the
  callee's failure surfaces when its own chain is demanded — but the argument is subtle and
  undocumented at the read sites.
- **Known cross-track back-edge bug**: `CalculatedReturnResolver.scala:183-186` re-enters the
  runtime `MonomorphicValue` unconditionally on the compiler track (documented in-code as a
  follow-up; currently fail-safe).

### P5. Reviewed and accepted as designed

- `used` as an imperative DFS in one fact (not a fact fan-out) matches rustc's monomorphization
  collector; the >500-instantiation backstop is the right guard given divergent type-level
  computation is expressible.
- `monomorphize` at ~6.7k lines (`Checker.scala` 911; next-largest phase `ast` at 1.6k) is normal
  elaborator concentration (Lean/Agda/Idris look the same); the `Track` seam already tamed the
  worst axis. Whole-program monomorphization from `main` has precedent in MLton.
- The "single evaluator" cornerstone holds: the ~5 `Evaluator`/`Unifier` instantiation sites are
  projections over one NbE core, not second interpreters.
- The backend's dynamic `main.els` injection (synthesizing source text and re-entering the front
  end, `JvmProgramGenerator.scala:29,85-88`) is a legitimate reuse of the whole front end — keep,
  but it is exactly the use `injected` should be scoped to (E3), and the
  `// TODO … selecting a random path` at `JvmPlugin.scala:50-54` should be resolved.

## 5. Findings — orchestration

### O1. Side effects inside fact generation — OPEN

`GenerateExecutableJar` is a command wearing a fact costume — its generation writes the JAR
(`JvmProgramGenerator.scala:48-64`), retrofitted into incrementality via `OutputFileStat`.
Query-based systems keep effects out of queries: the validation path (`getFactUntyped`
recompute-and-compare) re-*runs* facts, so a side-effecting fact re-executes its effect during
mere validation.

**Fix**: move the JAR write into `JvmPlugin.run`, consuming `GeneratedModule` facts.

### O2. Plugin lifecycle hygiene — OPEN

- `collectActivatedPlugins` never de-duplicates (`Compiler.scala:76-78`) — a future diamond
  dependency double-registers a plugin's entire processor set.
- Target selection is first-match in ServiceLoader classpath order (`Compiler.scala:41`) —
  ambiguity should be an error.
- The LSP hardcodes its plugin list (`EliotCompilationService.scala:144`) instead of using
  `collectActivatedPlugins` — two activation code paths that can drift.

### O3. Fact-visualization tracker is always on and racy — OPEN

Every CLI build pays the tracking overhead and writes a graph; `--visualize-facts` only renames
the output file (`Compiler.scala:52-67`). Classification of a production as fulfilling vs
initiating checks a concurrently-growing set (`FactVisualizationTracker.scala:25-49`) —
nondeterministic edges — and warm incremental runs yield misleadingly sparse graphs. Make it
opt-in.

### O4. Minor cache/config notes — OPEN

- Config fingerprint is `sha256(args.mkString(" "))` (`CacheFingerprint.scala:34`) — raw and
  order-sensitive; reordered args force a needless cold build. Anything a plugin's `configure()`
  derives from external state escapes both fingerprints.
- `FactCache.save` trial-serializes every key/deps/value to probe serializability
  (`FactCache.scala:62-64,79-85`) — O(entries) throwaway serialization on every save.
  Performance smell only.
- `Configuration` is `Map[Key[?], Any]` with `asInstanceOf` reads — convention-typed; a reused
  key name with a different `T` fails only at read time.

### O5. Documentation/naming drift — OPEN

- CLAUDE.md's pipeline list omits three real phases: `block` (blocks → applied lambdas, between
  resolve and matchdesugar), `termination`, and `saturate`.
- Naming mixture without convention: `…Reader`/`…Scanner`/`…Processor`/`…Generator`/`…Resolver`;
  the orchestrator is a "fact generator" while in-`lang` units are "processors"; two unrelated
  `ProcessorTest` classes (lang harness vs eliotc unit fixture).

## 6. Comparison to best-in-class

- **rustc queries / Salsa**: same demand-driven memoized shape. ELIOT's structural-drill
  validation for non-serializable facts is what Salsa does with hashes and rustc with
  fingerprints — value-equality comparison is simpler and right at this scale. What they have
  that ELIOT lacks maps exactly to E1 (provider registry) and E2 (engine-level cycle errors).
  Salsa's "queries are pure; inputs are set from outside" discipline maps to O1/E3.
- **Roslyn**: immutable facts + overlay-leaf LSP design is equivalent in spirit to Roslyn
  workspaces; Roslyn's no-I/O-in-queries discipline maps to O1.
- **GHC**: ELIOT's recompilation granularity is far finer than GHC's module level; Trees That
  Grow is the reference if per-phase ADT duplication (P1) ever justifies machinery.
- **MLton**: the precedent for whole-program monomorphizing compilation. **Lean/Agda/Idris**: the
  precedent for elaborator concentration (P5).
- **sixty / "Query-based compilers" (Fredriksson)**: closest overall shape (Haskell, Rock
  library); its cycle-detection notes parallel E2.

## 7. Prioritized recommendations

1. **E1 — key-class → processor registry** with startup duplicate detection and explicit
   override registration. Highest payoff, moderate effort, no semantic change.
2. **E2 — engine-level cycle detection** via the existing ancestors chain. Very cheap.
3. **E3 — fix `injected` classification**: in-generation registrations inherit the generation's
   dep set. Makes incrementality correct by construction.
4. **E4 — "neither fact nor error" synthesized diagnostic.** Trivial.
5. **P1 — shared generic payload record** for the value chain; give `block` its own expression
   type or fold it.
6. **P2 — platform-key `ContributedBinding`**; centralize pool probes in `DeclaringPool`.
7. **O1 — move the JAR write out of fact generation**; scope `injected` to `DynamicContent`.
8. **O2/O3/O5 — plugin hygiene, opt-in visualization, CLAUDE.md pipeline list.**

Items 1–4 are engine-local (`eliotc`), independent of language semantics, and together convert
every silent failure mode found in this review into a loud one.
