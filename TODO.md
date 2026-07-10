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

- **Fail-safe hole: leftover postponed unification constraints are silently dropped.** The unifier's
  `postponed` queue (`monomorphize/unify/Unifier.scala`) re-attempts constraints on every `drain()`,
  but a constraint that never discharges is simply carried along and forgotten at the end of the
  check — nothing converts survivors into errors, so an unproven equality can pass silently. This is
  what let the pre-fix applied-associated-type garbage through unnoticed (2026-07-09, see the
  associated-type application work): the body-vs-declared unification postponed forever and the value
  compiled anyway. Fix direction: after the post-drain pipeline's finalizer, treat every remaining
  postponed constraint as a hard "Type mismatch" (with its recorded context) — with a triage pass
  first, since some postponements may currently be benign-by-accident (e.g. constraints whose metas
  get defaulted to `Type` and would then trivially re-verify). Per the fail-safe rule, an unproven
  obligation must be an error, never a silent drop.

- Separate the cache graph from the values, so not everything has to be deserialized.
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
