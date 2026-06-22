# TODO

Remaining work and open ideas. Completed or superseded items have been removed — for what is
already built, see the git history, the cornerstones in `.claude/CLAUDE.md`, and the memory
notes.

## Type system & language

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

- **Monomorphization type-explosion / key on the codegen-relevant form, not the full type.**
  `MonomorphicValue.Key` is `(vfqn, typeArguments: Seq[GroundValue])` — the *full, unreduced*
  ground type, so two instantiations that differ only in a compile-time-only index produce
  *distinct* specializations even when the generated code is identical. This bites: (a) size-indexed
  recursion (`Rec[N]`: `f[N]` → `f[N-1]` → … is O(N) or unbounded specializations — representation
  lowering only dedups in `uncurry`, *after* mono has already paid the cost); (b) any compile-time
  data threaded through type args (a `Map`/complex structure used for resource tracking, analytics,
  proofs) explodes the key the same way even though it never reaches codegen. The evaluator
  (`eval/Evaluator.scala` `applyValue`/`unfoldTopDef`) also has **no** depth/fuel/cycle guard —
  recursion is only stopped by *stuckness* (neutral argument ⇒ residual, no unfold); a recursion
  forced on a concrete arg unfolds without limit. Wanted: a **generic** mechanism — key mono on the
  *erased / codegen-relevant* projection (what survives representation-lowering + dispatch
  resolution + reification), i.e. a relevance / phantom-parameter analysis that collapses the key
  over positions the generated code does not depend on; this makes `Rec[N]`, analytics data, and
  phantom indices all collapse for free (the special case "drop `N` before mono" generalized).
  Plus a **fail-safe backstop** independent of that: reuse the W4 active-fact chain
  (`CompilationProcess.activeFactKeys`) to detect the same FQN recurring with diverging type args
  and hard-error ("monomorphization not converging at <fqn>; arg <x> varies per step — erase or
  bound it") instead of hanging / OOM. Full plan: `docs/monomorphization-keying-plan.md` (grew out
  of the recursion-as-effect / `Rec[N]` discussion).

- Separate the cache graph from the values, so not everything has to be deserialized.
- Default imports should not be hardcoded; all of `lang.*` should be imported.
- Remove the `Show` instances used for printing expression/fact internals.
- Rename processors to generators?
- Make the JVM backend run only when `exec` is set.

## Microcontroller target

- You cannot set a pin `high` without first configuring it as an output — the type system
  should enforce this.
