# TODO

Remaining work and open ideas. Completed or superseded items have been removed — for what is
already built, see the git history, the cornerstones in `.claude/CLAUDE.md`, and the memory
notes.

## Type system & language

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
  effect row.** Design sketched 2026-07-10 in the bounds-as-refinements discussion. The dependency
  ladder is ranges → sizes → grades (fold cost needs sizes, frame sizes need ranges): the
  refinement channel's **ranges** domain has shipped, so what this still waits on is its **second
  domain, `List`/`Array` `size`**. Core idea: the effect row generalizes from "set of abilities" to
  "abilities + **named grades**" —
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

- **A lambda body at a rowless arrow slot does not get its own pure region.** It is elaborated in
  the *enclosing* region, so a discharge written inline there lands on the caller's carrier
  instead of reaching the `Id` boundary:

  ```
  def call(f: Option[String] => String): {Console} Unit = printLine(readLine.f)
  def main: IO[Unit] = call(s -> s.orAbort else "")   // Expected: String, Actual: IO(String)
  ```

  `f`'s codomain is declared rowless, so by §1 rule 4 the lambda's body is a value position and
  its `else` should discharge to `Id` — which is exactly what happens when the same discharge sits
  in a named pure helper (`call(s -> orEmpty(s))` compiles and runs). This is the same principle as
  the block-tail fix in `a669f530` (a value position must reach the boundary), applied one level
  further in: `elaborateLambdaNatural` would take the slot's declared codomain rather than
  inheriting `region`. It is held back because it changes what "a lambda at a plain arrow slot"
  means for *every* call — the existing arm deliberately lets an effectful body become a bind chain
  on the enclosing carrier — so it is a rule decision, not a patch (doc standing rule 2). Pinned by
  `ExamplesIntegrationTest1`, "bind an effectful subject dotted into a function-typed parameter",
  whose handler is spelled around the gap.
- **Rule-4 violations are diagnosed twice, unequally.** A user pipe declaring no row
  (`|>[A, B](a: A, f: A => B): B`) given a computation gets the elaborator's own "This argument is
  a computation, but argument N of '|>' declares no effect row". The stdlib `.` — whose `f`
  declares `{Effect}` — instead hoists the subject and leaves the checker to report an
  unattributable `Type mismatch. Expected: IO(IO(Option(String)))` at the subject. Both are the
  same violation (`readLine.flatMap(f)`); only one names the slot.

## Syntax sugar & ergonomics

- String formatting as plain syntactic sugar for type-parameters:
  `s"Something $variable"` → `s["Something $variable"]`, where `s[STR: String]: String`.
  Because `STR` is a type-parameter, `s` is optimized away in favour of its result, so the
  parsing can be arbitrarily complex.
- Parse strings and numbers into custom types (string interpolation, regexp parsing, …);
  numeric literals are just a special case of this.
- **The two type printers disagree on constructor application.** `monomorphize/fact/GroundValueRenderer`
  already renders a type-constructor application the way the user writes it (`List[String]`), but
  `monomorphize/unify/SemValuePrinter` — which produces the `Expected:`/`Actual:` lines — renders
  *every* application with parens (`List(String)`). Unify them on `[]` for type constructors and
  `()` for value constructors, so `Box("a")` stays `Box("a")` while its type reads `Box[String]`.
- Introduce a `UserShow[T]` — like `Show[T]`, but intended for end users.
- **Importing a module should bring in the abilities it implements.** When a module implements an
  ability, that ability's functions should become available through the import — e.g. `import
  BigInteger` would also get you `Compare` and `Numeric`.

## Optimization

- Lists consisting only of constants should not take runtime memory.
- **Sharpen the codegen phantom classification.** The `used` driver dedups its `MonomorphicValue`
  demand on a codegen-relevant projection of the type arguments (`saturate/fact/BinderRoles.scala`
  + `used/CodegenProjection.scala`), but a *true* phantom — a size index that never reaches
  representation — still classifies conservatively as representation, so identical code is not
  collapse-erased. Refining it is a code-size win, never correctness: the projection only ever
  folds code that is already identical.
- Can compile-time bounds be used to optimize the `Seq` implementation? (e.g. only `head` →
  linked list, only iteration → array.)
- Benchmark goal: complicated functions that generate an LED-light pattern should compile to a
  handful of instructions (uniting switching multiple LEDs into a single instruction), where a
  C compiler would compile everything.

## Compiler architecture & tooling

- **`getFact` still lives.** The processor-facing API is supposed to expose only `getFactOrAbort`
  / `getFactOrError` / `getFactIfProduced` (see `.claude/rules/eliot-design.md`), but a plain
  `getFact` is still there on `CompilationProcess` and its wrappers. Either rename it to
  `getFactIfProduced` or hide it entirely behind the three intent-carrying reads.
- `OperatorResolvedValue` should carry an `isGuard` flag (the fact currently has none) — check
  what needs it and whether it can be derived instead of stored.
- `namedValues` can recurse — the reflection-driven enumeration is not protected against a value
  that reaches itself.
- **The warm build's dominant cost is now plugin/JVM startup**, not the cache. Replacing Java
  serialization with explicit codecs over a content-addressed object store cut a warm build in
  half (load −79%, save −77%) and the cache is down to ~36% of it, behind `compiler engine, plugin
  setup and i/o` at ~43%. See `docs/incremental-compilation.md` — §19 for that profile, and for
  why a lazy index is measured and rejected. Left on the cache itself: caching *declines* (§6 step
  3) and compaction (§13 step 5, no longer urgent since a warm build appends nothing).
- **A compile error leaves the old artifacts in place**, so a failed build can be followed by a
  successful run of a stale jar.
- **Implement scoped caches**, so the stdlib does not get built again on every compile for the
  same monomorphizations.
- **Incremental cache corrupted by concurrent/out-of-date compilers.** A stale cache made a CLI
  compile report errors from a *previous version* of an edited file (positions and types from old
  content, underlining unrelated new text); deleting the cache fixed it. Suspected trigger: an
  out-of-date IntelliJ plugin's resident LSP compiler running against the same workspace breaks
  the cache every time it runs. Of the three asks, only the **compiler-version stamp** is in place
  (`CacheFingerprint.compiler`, plus a magic and a format version in the store header), so a
  different compiler build can no longer reuse another's cache. **Concurrent writers are handled as
  of §21**: a shared/exclusive lock on the cache (never on the build — a failed acquisition costs a
  full compilation and nothing else), a save rebased onto the region's actual end, a region id that
  catches a region replaced rather than extended, and the index published by atomic rename. Still
  open: the world leaf `FileStat` invalidates on **mtime**, not content — a content digest only
  stops propagation one level up, at `SourceTokens`. That half is *live and observed*: a
  `FullIntegrationTest` suite, which rewrites one `Test.els` per test against a resident session,
  intermittently compiles the **previous** test's program — `File.lastModified()` has millisecond
  resolution and two writes can land in one tick. It is the same symptom as the field report above,
  from the other cause.
- Remove the `Show` instances used for printing expression/fact internals.
- Rename processors to generators?

## Microcontroller target

- You cannot set a pin `high` without first configuring it as an output — the type system
  should enforce this.
