# TODO

Remaining work and open ideas. Completed or superseded items have been removed — for what is
already built, see the git history, the cornerstones in `.claude/CLAUDE.md`, and the memory
notes.

## Type system & language

- `Coerce` should return `{Throw} To` instead of `Option[To]`, so that it matches the way type
  returns work and can return a *customized* error message to print at the failing use site (rather
  than a bare `none` that the checker has to turn into a generic "no coercion" diagnostic). See
  `lang/eliot/eliot/compiler/Coerce.els`.
- Add generics to function literals.
- Introduce arrays (records / multi-field `data` are already done).
- **Unify `Int` literal handling across the two tracks.** Managing the `BigInteger`/`Int` split
  between the compiler track and the runtime track is a constant source of friction. It cannot be
  solved by simply aliasing `type Int = BigInteger` on the compiler track, because `Int` carries
  meta-information that is itself expressed in terms of `BigInteger` (`type Int {range:
  Interval[BigInteger]}`). One unified handling of numeric literals for both tracks is wanted.
- **A native that produces a meta-carrying type must state its meta-information.** Any type with
  non-`Unit` meta-information that comes out of a native function *and* has no meta-information
  definition at that native should be a compile error — the native has to say what it does to the
  meta-information every time, rather than silently defaulting.
- Are there still "auto" generics? Can they be removed — and should they be?
- **Flow grades: quantitative computation tracking (cycles/WCET, stack, peak memory) on the
  effect row.** Design sketched 2026-07-10 in the bounds-as-refinements discussion; depends on
  the refinement channel (`docs/bounds-as-refinements.md`) landing first — the dependency ladder
  is ranges → sizes → grades (fold cost needs sizes, frame sizes need ranges). Core idea: the
  effect row generalizes from "set of abilities" to "abilities + **named grades**" —
  `def onTick(s: State): {Timer, cycles: ≤800} Unit` — where a grade is *not* an ability
  (nothing is performed, nothing resolved or discharged) but a quantity with an algebra,
  registered per platform (`flow cycles {Interval[BigInteger]}` +
  `implement Grade[D] { seq (+), branch (interval hull → BCET..WCET), zero, within (≤) }`);
  the existing effect row is the powerset special case (seq = branch = union, within = ⊆).
  The §4.2 projection discipline transfers wholesale: a parameter's grade is referenced by slot
  projection (`step.cycles`), never bound; return-position entries are expressions (axiomatic on
  natives, checked contracts on bodied defs) — showcase, with cross-channel composition:
  `def fold[T, A](ls: List[T], init: A, step: F[A]): {cycles: ls.size.end * step.cycles + 7} F[A]`.
  Grade-only rows on pure functions do not force a carrier (precedent: negatives-only discharge
  rows are pure pass-throughs). Leaf grades are stated on native signatures by the platform
  layer (AVR datasheet cycle counts); width-dependent leaf costs are either expressions over the
  operands' value-metas or accounted post-lowering where `Represent`'s layout is known (same as
  stack frames). Accounting = generalize the effect walk (`EffectUsageCollector` + discharge-
  summary DAG) from the union lattice to arbitrary `Grade`s, run over the *residual* program
  (CTFE'd-away code costs zero). Covers: cycles (exact on cache-less simple cores — totality +
  monomorphization deleted the hard WCET subproblems: loop bounds come from sizes, no recursion,
  no indirect calls), max stack depth (sum frames / branch max over the whole-program DAG,
  post-lowering), and peak memory (composes as a monoid on `(net, peak)` pairs); an `Inf`
  super-loop's *step* carries the deadline budget — the real-time contract. Out of scope for the
  mechanism: in-place reuse (linearity is a type discipline, not meta-information) and hardware
  fidelity beyond simple cores. This supersedes the older idea of "WCET as type parameters /
  effects with type parameters" — quantities stay out of the type channel per the
  differentiation rule (doc §3).
- **Important:** after compilation the program should be guaranteed to fit all resources —
  memory, stack, etc. (Mechanism: the flow-grade system above, plus `Represent`-driven layout
  from the refinement channel.)

## Effects & I/O

- Separate the different kinds of I/O (pin output, pin input, timers, …) and let the type
  system infer multiple typeclasses for them.
- **`printLine` can fail, and nothing says so.** Console output can throw on the platform, so it
  needs error handling in the row. Does that failure belong on `Suspend`, or as its own effect
  beside `Console`? (Related to the deliberate decision that console *read* failure stays out of
  the row — see the `Console` EOF work.)
- A plain `if..else` needs an effect carrier — it cannot be pure.

## Syntax sugar & ergonomics

- String formatting as plain syntactic sugar for type-parameters:
  `s"Something $variable"` → `s["Something $variable"]`, where `s[STR: String]: String`.
  Because `STR` is a type-parameter, `s` is optimized away in favour of its result, so the
  parsing can be arbitrarily complex.
- Parse strings and numbers into custom types (string interpolation, regexp parsing, …);
  numeric literals are just a special case of this.
- Unify the display of value constructors and type constructors using `[]` and `()`:
  `Box$DataType(String)` → `Box[String]`; `Box("a")` stays `Box("a")`.
- Introduce a `UserShow[T]` — like `Show[T]`, but intended for end users.
- **Importing a module should bring in the abilities it implements.** When a module implements an
  ability, that ability's functions should become available through the import — e.g. `import
  BigInteger` would also get you `Compare` and `Numeric`.
- `&&` and `||` currently have the same precedence; `||` should bind looser than `&&`.
- `a-1` does not parse as `a - 1`.

## Optimization

- Lists consisting only of constants should not take runtime memory.
- Can compile-time bounds be used to optimize the `Seq` implementation? (e.g. only `head` →
  linked list, only iteration → array.)
- Benchmark goal: complicated functions that generate an LED-light pattern should compile to a
  handful of instructions (uniting switching multiple LEDs into a single instruction), where a
  C compiler would compile everything.

## Compiler architecture & tooling

- **Monomorphization keying — landed as a code-size optimization; demote dropped.** The codegen
  type-dedup fix is in (it grew out of the recursion-as-effect discussion; the original full plan is in
  git history): the `used` codegen driver dedups its `MonomorphicValue` demand on a
  **codegen-relevant projection** of the type args (B1 per-binder relevance analysis in
  `saturate/fact/BinderRoles.scala`; B2 projection in `used/CodegenProjection.scala`) — phantoms
  collapse-erase, width-equivalent bounds collapse to a representation key, dispatch/reified families
  stay specialized — and a **non-convergence backstop** in `used/UsedNamesProcessor.scala` hard-errors
  on divergent type-level computation instead of hanging (it counts repeated `vfqn` frames in the
  `processValue` DFS; the original "reuse `activeFactKeys`" sketch was wrong — siblings keep that chain
  flat). With recursion removed from the language (see the "Total by Default" cornerstone in
  `.claude/CLAUDE.md`), the unbounded
  `f[N]→f[N-1]→…` family that motivated **demote** cannot arise, so demote and its B4 policy are
  **dropped** and the `recursionVariant`/`Demote` machinery deleted. What remains is purely an
  optimization — the projection only ever folds identical code, never load-bearing for termination —
  and the backstop is a fail-safe for the residual `Type:Type`/Girard divergence. Possible future
  tightening: a true phantom (a size index that never reaches representation) currently classifies
  conservatively as representation, so it is not collapse-erased; refining that is a size win, not
  correctness.

- **DONE (2026-07-10, bounds-as-refinements Step 0): Fail-safe hole — leftover postponed unification
  constraints are silently dropped.** The unifier's `postponed` queue
  (`monomorphize/unify/Unifier.scala`) re-attempts constraints on every `drain()`, but a constraint
  that never discharged used to be carried along and forgotten at the end of the check — nothing
  converted survivors into errors, so an unproven equality could pass silently (what let the pre-fix
  applied-associated-type garbage compile — the body-vs-declared unification postponed forever).
  **Fixed:** `Unifier.flushPostponed()` runs as the last step of `TypeStackLoop.runPostDrainPipeline`
  (after `defaultUnsolvedMetas`): a triage re-`drain()` first discharges any constraint the defaulting
  just made verifiable, then every survivor becomes a hard "Type mismatch." with its recorded context.
  Two shapes are triaged benign because a *more precise* fail-safe already owns them (`isBenignPostponement`):
  an applied **abstract associated type** (unsolved `MetaRole.AbstractAssoc` head — postponed by design
  for the assoc reducer; a genuinely-unreduced one is caught by the assoc-reduction loud-fail / strict
  quoter) and a **`$bad-apply` head** (a phantom meta defaulted to `VType`/`VConst` then applied —
  either already diagnosed or a vacuous phantom). The genuine class the flush is the sole backstop for
  — a postponed application whose meta *did* solve to a concrete head that then mismatches — is caught.
  Covered by `PostponedFlushTest`.

- **`getFact` still lives.** The processor-facing API is supposed to expose only `getFactOrAbort`
  / `getFactOrError` / `getFactIfProduced` (see `.claude/rules/eliot-design.md`), but a plain
  `getFact` is still there on `CompilationProcess` and its wrappers. Either rename it to
  `getFactIfProduced` or hide it entirely behind the three intent-carrying reads.
- `OperatorResolvedValue` should carry an `isGuard` flag (the fact currently has none) — check
  what needs it and whether it can be derived instead of stored.
- `namedValues` can recurse — the reflection-driven enumeration is not protected against a value
  that reaches itself.
- Separate the cache graph from the values, so not everything has to be deserialized.
- **Cache serialization is slow.**
- **The cache gets rebuilt** when it should have been reused.
- **Even with a warm cache the compile is not immediate** — and not just because of statistics
  collection. See the "warm build still computes" note in git history for what is expected to be
  recomputed and what is not.
- **A compile error leaves the old artifacts in place**, so a failed build can be followed by a
  successful run of a stale jar.
- **Implement scoped caches**, so the stdlib does not get built again on every compile for the
  same monomorphizations.
- **Incremental cache corrupted by concurrent/out-of-date compilers.** A stale `.eliot-cache`
  made a CLI compile report errors from a *previous version* of an edited file (positions and
  types from old content, underlining unrelated new text); deleting the cache fixed it. Suspected
  trigger: an out-of-date IntelliJ plugin's resident LSP compiler running against the same
  workspace breaks the cache every time it runs. Harden the cache: content-hash (not
  mtime) invalidation, a compiler-version/cache-format stamp so a different compiler build never
  reuses (or silently poisons) another's cache, and ideally per-writer isolation or locking for
  concurrent compilers.
- Default imports should not be hardcoded; all of `lang.*` should be imported.
- Remove the `Show` instances used for printing expression/fact internals.
- Rename processors to generators?
- Make the JVM backend run only when `exec` is set.

## Microcontroller target

- You cannot set a pin `high` without first configuring it as an output — the type system
  should enforce this.
