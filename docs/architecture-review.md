# Compiler architecture review (fact engine + pipeline)

Last performed: **2026-07-04** (tree at `a7b2e11a`). Engine findings E1–E4 implemented later the
same day (see the per-finding implementation notes).

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

### E1. Dispatch is an untyped broadcast; the key→processor mapping exists nowhere — IMPLEMENTED 2026-07-04

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

**Registry considered and rejected.** A `Map[Class[Key] → Processor]` with startup duplicate
detection was the first proposal, but the key→processor mapping is essentially static, compiler
internal, and any violation surfaces (once the fixes below land) at first demand in ordinary
pipeline testing — the `ProcessorTest` convention runs the whole pipeline, so an unhandled key
class cannot survive the test suite. What the registry would add over demand-time loudness —
startup-time detection, O(1) dispatch, an introspectable mapping — is not worth its machinery
(dispatch cost is ~40 cheap no-ops per fact; the fact-visualization already shows the observed
mapping). **Decided direction: handle the failures directly instead.**

**Decided fix — make `None` unrepresentable at the processor API, and make double-produce loud.**
The obstacle to "only `getFactOrAbort`" is that `None` today is also a *data* channel — a
producer legitimately declining. Known absence-as-data sites: `NativeBinding` consumers (the
checker's `fetchBinding` reads absence as "ordinary value, evaluate the body"); the runtime-pool
probe in `CompilerMonomorphicTypeCheckProcessor.inRuntimePool` (absence is the *good* case);
`ValueResolver` qualified-ref probes (`ValueResolver.scala:208,294`);
`ModuleValueProcessor.importModuleNames` ("Could not find imported module" as a positioned user
error); `ImplementationMarkerUtils`; `PostDrainQuoter`'s roleHint read; ability resolution's
"no implementation" user diagnostic. Each converts to one of two forms:

- **Total facts** — always register; absence lives in the payload. In-tree precedent:
  `ContributedBinding` already carries `Option[BindingContribution]` and is always produced.
  This is the rustc/Salsa discipline: a query always returns a value; "nothing" is a value.
- **`getFactOrError(key)(sourcedMessage)`** — for the "module not found" class where the caller
  owns a user-facing message; replaces the manual `getFact`-then-switch idiom
  (`.claude/rules/eliot-design.md`) with an API that cannot hand `None` back. The
  Option-returning `getFact` then disappears from the processor-facing `CompilerIO` API,
  surviving only as the engine-internal `CompilationProcess.getFact` (the incremental validation
  path needs it).

**Double-produce loudness needs two carve-outs before it can switch on:**

- *Idempotent re-registration is routine* — the `ModuleValueProcessor` push pattern
  double-completes sibling keys with equal payloads whenever two names of one file generate
  concurrently. So: complain only when the second registration is **not equal** to the completed
  value. Coherent with existing invariants — the incremental cache's equality cutoff
  (`v == current`) already requires fact equality to be meaningful.
- *The VFS override is a deliberate different-value double-produce* and must be restructured
  first, or the LSP screams on every overlaid file. Use the in-tree decorator machinery:
  `CompilerProcessor.wrapWith` (already used by the visualization tracker). `LspPlugin` wraps
  the folded processor tree with an interceptor serving `FileStat`/`FileContent` from the
  overlay and delegating the rest — explicit, order-independent (also retiring the O2 order
  fragility), and the disk processor never runs for overlaid files.

**Landing order** (each step independently landable, behaviour-neutral until step 4):
1. Convert absence-as-data facts to total facts (`NativeBinding` first, on the
   `ContributedBinding` precedent) and introduce `getFactOrError`.
2. Retire processor-facing `getFact`.
3. Restructure the VFS override onto `wrapWith`.
4. Switch on loud-on-different-value double-produce **and** the neither-fact-nor-error
   diagnostic (E4 — which step 1–2 make precise).

**Implementation (2026-07-04) — landed, with two deviations discovered during implementation:**

- *Total facts were NOT adopted* (step 1 as sketched). Reading `PathScanner` showed the codebase
  already has a sanctioned decline idiom — a compiler-platform miss aborts silently by design
  (`PathScanner.scala`, hit for *most* names) — and total-izing would have forced `Option`
  payloads through every `NativeBinding`/`UnifiedModuleValue` consumer while drifting semantics
  ("Could not find imported module" detection). Instead the decline was made **first-class**: the
  engine now distinguishes an explicit `abort` (decline) from running off the end, and
  `SequentialCompilerProcessors` preserves the abort signal of its children
  (`CompilerIO.recoverWithAborted`). The enforced contract is **fact XOR errors XOR
  explicit-abort XOR missing-dependency-read**.
- Processor-facing `getFact` is retired (private in `CompilerIO`). The API is now
  `getFactOrAbort` (default), `getFactOrError(key)(error)` (caller-owned message), and
  `getFactIfProduced` (explicit tolerant read for the ~30 absence-is-expected-and-handled sites:
  skip-broken-callee walks, pool probes, optional enrichment). Every former call site was
  classified one by one; `.claude/rules/eliot-design.md` records the new rule. One prior
  misjudgment caught by tests: `SystemNativesProcessor`'s `UpToDate` touch-read must stay
  tolerant — its own comment said so.
- *VFS override* (step 3): implemented as a `processorWrapper` parameter on
  `CompilationSession.create` applied **after** the full plugin fold
  (`lsp/virtual/VfsOverlayProcessor` wrapping the complete tree), not literally via `wrapWith` at
  the plugin's fold position — a plugin-level wrap would only enclose the accumulator folded *so
  far*, leaving the on-disk readers outside it, i.e. still order-dependent. The two racing overlay
  processors are deleted; for an overlaid file the on-disk readers are never consulted.
- *Double-produce* is loud: re-registering an **equal** value is a no-op (the push pattern);
  registering a different value for a completed key, or after the key concluded empty, is an
  internal error (`IncrementalFactGenerator.registerFact`). The full suite confirmed zero
  legitimate different-value double-produces exist.
- *Missing-producer diagnostic* ("no processor produced a fact, an error, or a decline") is
  gated by `strictAccounting`, **on** for every `CompilationSession` (CLI, LSP, apidoc), **off**
  for directly-constructed generators. Running it globally false-fired 187× in the test suite:
  the harness convention deliberately runs partial bundles (source-phase facts injected, their
  processors absent), so the invariant is a *whole-bundle* property and is enforced exactly where
  whole bundles run. It immediately caught one real incompleteness: two LSP session tests ran
  `LspPlugin` (which demands `ValueDoc`) without `ApiDocPlugin` — fixed to match production.

### E2. An unguarded fact cycle hangs the compiler instead of erroring — IMPLEMENTED 2026-07-04

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

**Implementation (2026-07-04)**: as specified, in `DependencyTrackingProcess.getFact` — an
on-chain read records a "Cyclic fact demand: A <- B <- A" error and returns `None` (counting as a
missing read, so no cascade diagnostics). The refused edge is deliberately *not* recorded as a
dependency, so a cyclic edge can never enter the persisted cache, where it would deadlock
validation. Covered by engine tests (self-cycle and mutual cycle both error instead of hanging).

### E3. Incremental caching is broken for *pushed* facts — racy in one shape, deterministic in another — IMPLEMENTED 2026-07-04

`ModuleValueProcessor` is a fan-out: asked for one `ModuleValue.Key`, it registers facts for
**every** name in the file (`ModuleValueProcessor.scala:35-49`). Pushed facts inherit **nothing**
from the generation that produced them: the `DependencyTrackingProcess` wrapper records reads
only under the *requested* key, and a pushed fact never gets a `directDependencies` entry — not
even when later demanded, because the demand finds the already-completed `Deferred` and skips
`regenerate`. Per (file, platform), only the one name whose demand *triggered* the generation is
properly tracked; which name that is, is a race. `buildCacheData` then marks every untracked
fact `injected = !deps.contains(key)` (`IncrementalFactGenerator.scala:196`). The flag was
designed for backend-injected dynamic sources (`CacheEntry.scala:17-19`); the heuristic cannot
tell those apart from processor pushes. "Pushed facts" and "facts with broken caching" are the
same set, through two distinct paths:

- **Path 1 — resolve path (racy staleness).** A demanded key whose prior entry is `injected` is
  completed from cache on sight, no validation (`IncrementalFactGenerator.scala:76`). Rescue is
  possible: a concurrently regenerating sibling re-pushes fresh values, and if that push
  completes the `Deferred` first, fresh wins. Timing, not an invariant.
- **Path 2 — validation path (deterministic staleness).** `computeUnchanged` on an injected
  entry returns unconditionally *unchanged* without ever reading a value
  (`IncrementalFactGenerator.scala:107-108`). Scenario: `main` uses both `a` and `b` from one
  file; in run 1, `a` won the trigger race (properly tracked), `b` was pushed → injected. Run 2
  edits `b`'s **body**. Validating `b`'s chain: `UnifiedModuleValue(b)`'s deps are `PathScan`
  (unchanged), `ModuleNames` (recomputes *equal* — a body edit changes no names, the equality
  cutoff correctly reports unchanged), and `ModuleValue(b)` — injected → "unchanged". Every link
  validates, so the whole chain above `b` is accepted stale, into codegen. The fresh-push race
  cannot rescue this path: validation short-circuits on the flag and **never touches the pushed
  key's `Deferred`**, so a re-pushed fresh value is never even read. The only nondeterminism is
  run 1's classification; given it, run 2's staleness is deterministic.

**Empirical status (2026-07-04)**: a cold build materialised 747 facts with only 711 generations
— ~36 entries entered the cache as `injected`. Two crafted staleness attacks (edit a helper whose
`ModuleValue` was push-only, switch `main` to use it, rebuild incrementally) both produced
*correct* output — but both attacked **path 1** (newly demanded name), where the sibling
re-push race can rescue, and in those runs it did. The path-2 shape (both names already cached,
edit one) was identified by analysis after the experiments and has no rescue.

**Fix** (covers both paths): facts registered *during* a generation inherit that generation's
dependency set — they depend on exactly what the generation read. Mechanically:
`DependencyTrackingProcess.registerFact` records `pushedKey → generatingKey`, and
`buildCacheData` gives each pushed fact the generating key's final dep set. Reserve `injected`
for registrations made outside any generation (session/`DynamicContent` injection), where
accept-on-sight is genuinely correct. With a real dep set, path 1 validates like any generated
fact and path 2's short-circuit no longer applies. Cache correctness becomes by-construction.

**Implementation (2026-07-04)**: as specified, with one refinement — `injected` is now an
*explicit* channel rather than a residual heuristic: `CompilationProcess.registerInjectedFact`
(+ `registerInjectedFactIfClear` in `CompilerIO`), used by `DynamicContent`. This matters because
the dynamic `main.els` facts are registered *during* a generation that then reads them back —
inheriting that generation's dep set would have created a cyclic cache edge. Out-of-generation
direct `registerFact` (test injection) still classifies as injected, preserving old behaviour.
`CACHE_VERSION` bumped to 3 (entry semantics changed). Verified: engine tests (pushed fact
carries the pusher's dep set, regenerates when it changes, is accepted when unchanged), plus the
end-to-end path-2 probe — both names used, edit one body, incremental rebuild — run 3×, fresh
output every time.

### E4. "No fact ⇒ an error was reported" is unenforced — IMPLEMENTED 2026-07-04 (as part of E1 step 4)

`abort` with an empty error chain is representable, and a generation producing neither fact nor
error is a legal no-op — so a demanded-but-unproducible target can yield exit code 0 with no
output (`JvmPlugin.run` discards the jar fact's value with `.void`).

**Fix**: when the safety net completes a key with `None` and that generation contributed zero
errors, synthesize an internal diagnostic ("processor for key X produced neither fact nor
error").

**Correction (2026-07-04)**: as originally stated this check would false-positive on every
legitimate decline (no `NativeBinding` for a non-native name, pool probes, "module not found"
handled by the consumer) — those produce neither fact nor error *by design* today. It becomes
precise only after E1 steps 1–2 eliminate absence-as-data, at which point the enforceable
contract is: **every generation ends with the requested fact registered XOR ≥1 error recorded.**
Ships as E1 landing-order step 4.

**Implementation (2026-07-04)**: the final contract is **fact XOR errors XOR explicit-abort
(decline) XOR missing-dependency-read** — declines stayed as aborts instead of being converted to
total facts (see E1's implementation note), so the abort side had to be part of the accounting.
Enforced in `IncrementalFactGenerator.regenerate` under `strictAccounting` (on for every
`CompilationSession`; off for the deliberately-partial test harness bundles, where a missing
producer is the harness convention, not a bug). A demanded key with no producer in a complete
bundle now yields "Internal error: no processor produced a fact, an error, or a decline for
&lt;key&gt;" instead of a silent exit 0.

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
  that ELIOT lacks maps exactly to E1 (total queries — a query always returns a value, one
  producer each; reached via E1's decided direction rather than a registry) and E2 (engine-level
  cycle errors).
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

1. **E1 — DONE 2026-07-04** (revised form: abort-as-decline + `getFactOrError`/`getFactIfProduced`
   + `getFact` retired + VFS interceptor + loud double-produce; see E1's implementation note).
2. **E2 — DONE 2026-07-04**: engine-level cycle detection via the existing ancestors chain.
3. **E3 — DONE 2026-07-04**: in-generation registrations inherit the generation's dep set;
   `injected` is an explicit channel; `CACHE_VERSION` 3.
4. **E4 — DONE 2026-07-04** as E1 step 4 (`strictAccounting`, on for sessions).
5. **P1 — shared generic payload record** for the value chain; give `block` its own expression
   type or fold it.
6. **P2 — platform-key `ContributedBinding`**; centralize pool probes in `DeclaringPool`.
7. **O1 — move the JAR write out of fact generation**; scope `injected` to `DynamicContent`.
8. **O2/O3/O5 — plugin hygiene, opt-in visualization, CLAUDE.md pipeline list.**

Items 1–4 landed 2026-07-04 (single commit; full suite green, HelloWorld + incremental probes
verified under the new strict accounting). Together they convert every silent failure mode found
in this review into a loud one. Remaining open work is P1–P5 and O1–O5.
