# TODO

Remaining work and open ideas. Completed or superseded items have been removed — for what is
already built, see the git history, the cornerstones in `.claude/CLAUDE.md`, and the memory
notes.

## Type system & language

- **Recursion & termination — `Rec[N]` / `Inf` as effects.** Disallow unbounded recursion by
  default; a function recurses only by proving termination (`Rec[N]`, a phantom well-founded measure
  with partial `dec`/`half`/structural descent) or opting out (`Inf`). Detection rides a latent
  `{Terminating, Inf}` effect on the function arrow (no pointer-tracking). Sound only under three
  preconditions: purity (no mutable cells — decided), occurs-check, and a **strict-positivity check
  on `data`** (not yet built — the concrete new obligation). Full design note:
  `docs/recursion-termination.md`.
- Add generics to function literals.
- Introduce arrays (records / multi-field `data` are already done).
- WCET (worst-case execution time) and other real-time properties as type parameters, or as
  effects with type parameters, e.g. `something().strictlyEvery(10 millis)`.
- **Important:** after compilation the program should be guaranteed to fit all resources —
  memory, stack, etc.

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
  flat). With recursion removed from the language (`docs/recursion-termination.md`), the unbounded
  `f[N]→f[N-1]→…` family that motivated **demote** cannot arise, so demote and its B4 policy are
  **dropped** and the `recursionVariant`/`Demote` machinery deleted. What remains is purely an
  optimization — the projection only ever folds identical code, never load-bearing for termination —
  and the backstop is a fail-safe for the residual `Type:Type`/Girard divergence. Possible future
  tightening: a true phantom (a size index that never reaches representation) currently classifies
  conservatively as representation, so it is not collapse-erased; refining that is a size win, not
  correctness.

- Separate the cache graph from the values, so not everything has to be deserialized.
- Default imports should not be hardcoded; all of `lang.*` should be imported.
- Remove the `Show` instances used for printing expression/fact internals.
- Rename processors to generators?
- Make the JVM backend run only when `exec` is set.

## Microcontroller target

- You cannot set a pin `high` without first configuring it as an output — the type system
  should enforce this.
