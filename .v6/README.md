# The staged effects-v6 tree (`docs/effects.md` §10.1 step 8)

This directory is the **prepared tree** for the effects-v6 flag day: the stdlib base layer, the jvm layer, the
compile-track overlays, the examples and `eliot-test`, already written in v6 form. Nothing here is on any build
path — no Mill module reads it and `--path` never points at it — so it compiles nothing and breaks nothing. It
exists so that the flag day (§10.2) is *a compiler change plus a move*, rather than one long day of both.

It is **hidden** (`.v6`, not `v6`) for one concrete reason: the LSP's `SourceRootDiscovery` recovers a workspace's
Eliot roots by convention, and a visible `v6/stdlib/eliot` would be taken for a layer root beside the real one —
every module then declared twice, and the whole workspace red in the editor. A dot-directory is the rule that
already exists for "not part of this project's sources".

The plan says "on a branch". It is a directory instead, on `master`, because that is this repository's working
convention and because a directory does the same job better: it lands in ordinary commits, it is reviewable
next to the tree it will replace, and applying it at F7 is a `git mv` rather than a merge of a branch that has
been diverging for weeks. There is also a gate on it, which a branch would not have had —
`lang/test/…/ast/processor/V6TreeParseTest.scala` tokenizes and parses every file here on every test run, so a
tree nobody compiles cannot rot into unparseable text while the compiler moves underneath it.

## What is here, and what is not

**Only files that change.** A file whose v6 form is identical to its current form is not copied — a duplicate
of `Bool.els` would only be a second place to forget. So this directory is an *overlay*: at F7 it is applied
over the tree, the deletions below are made, and everything not mentioned stays exactly as it is.

Paths mirror the real tree, with `eliot-test/` standing for the sibling repository
`/home/robert/personal/eliot-test`.

## Applying it at F7

1. Copy each file here over its counterpart (`.v6/<path>` → `<path>`; `.v6/eliot-test/<path>` → the eliot-test
   repo's `<path>`).
2. Delete the files listed under **Deletions**.
3. Delete `.v6/` and `V6TreeParseTest.scala`.
4. Rename `examples/src/EffectsFakeCarrier.els` → `EffectsNamedEffect.els` (the v6 file is already staged under
   the new name; the old one is a deletion).

## Deletions

Base (`stdlib/eliot/`):

- `eliot/carrier/Effect.els`, `eliot/carrier/Suspend.els` — the whole `eliot.carrier` package.

Prelude (`lang/eliot/`):

- `eliot/lang/Id.els` — no `Id`, no `runId`. `ModuleName.defaultSystemModules` loses the entry (F2, a compiler
  change, not a tree one).

jvm (`jvm/eliot/`):

- `eliot/lang/Id.els`
- `eliot/jvm/IO.els` — the platform carrier and `runMain`. Under v6 the synthesized entry point installs frames
  for `main`'s row directly (F4), so there is no carrier to instantiate and no run boundary to register;
  `RunBoundaryFunctions` goes with it.

Compile track:

- `lang/eliot-compiler/eliot/lang/Id.els`
- `stdlib/eliot-compiler/eliot/effect/Abort.els` is **replaced** (not deleted): the `AbortCarrier` overlay
  becomes an implementation over the escape intrinsic.

Examples:

- `examples/src/EffectAbilitySet.els` — effect sets are §12 "not now"; the `ability Web[F[_] ~ Console & Log]`
  shape has no carrier binder to hang the requirement on.
- `examples/src/EffectsFakeCarrier.els` — superseded by `EffectsNamedEffect.els`.

Every `*Carrier` type and every `Suspend`/cross-lift instance disappears inside the files that are replaced;
none of them has a file of its own.

## What changed, beyond the obvious

The effect declarations, the named implementations and the `with`s are §9.3 read literally. Four things are
*consequences* that the document does not spell out, and each is a decision this staging made:

1. **A combinator's return row disappears.** `foldLeft`, `map`, `filter`, `foldOption`, `foldEither`, `fold`,
   `.` and the rest used to return `{} B` because the carrier had to be threaded out of them. Under v6 the
   callback's effects are bound at the *caller*, so the combinator itself declares nothing and returns a plain
   `B`. The rows on the *slots* (`f: A => {} B`) stay: they are what D5 reads to decide that a lambda written
   there may reach the enclosing declarations.
2. **`foldLeft`'s seed becomes a payload** (`initial: B`, not `initial: {} B`). It is always evaluated, so
   suspending it says nothing; `fold`'s arms and `else`'s fallback keep their rows because only one of them runs.
3. **Only the dischargers that need a primitive are body-less in the base.** §9.6 lists "runThrow, catch, else,
   runAbort, runState* and written". Of those, only `runThrow`, `runAbort`, `runStateToPair`,
   `runWriterToPair` and `provide` touch a primitive; `catch`, `else`, `runStateToValue`,
   `runStateToFinalState`, `runWriterToValue` and `runWriterToLog` are ordinary platform-independent bodies over
   them and stay in the base, where the base-layer rule says they belong. The doc's `written`/`runState` are
   prose for the existing `runWriterToLog`/`runStateToPair`, whose names §9.6 says are kept.
4. **`runWriterToPair` grows a `~ Combine[W]` constraint.** Its platform body needs `combine` and `empty`; under
   v5 the constraint sat on the `Effect[WriterCarrier]` instance, which no longer exists.

## The jvm primitives: private, and therefore repeated

§9.6 says the three leaves are private. Eliot has no *layer*-private visibility — `private` is module-scoped —
so each jvm module that needs a primitive declares its own copy, exactly as `isNull` is declared four times
today. `eliot.effect.Throw` and `eliot.effect.Abort` each declare `escapeInternal`/`exitInternal`;
`eliot.effect.State`, `Writer` and `Dep` each declare `withCellInternal`/`readCellInternal`/`writeCellInternal`.

That is five copies of two shapes, and it is the right trade: the alternative — one public `eliot.jvm.Cell` —
would put a mutable cell in reach of any jvm program, which is Landin's knot and exactly what
`termination/PurityGuardTest` exists to keep out. Repetition also separates the frames for free: `Abort`'s
escape at `Unit` cannot be confused with a `Throw[Unit]`, because the backend keys a frame class on the name
that installed it.

**The compile track cannot do the same**, and does not: `EffectIntrinsics` matches on the exact FQNs
`eliot.compiler.Escape::escape`/`exit` and `eliot.compiler.Cell::withCell`/`read`/`write`, so those are declared
once, publicly, in `stdlib/eliot-compiler/eliot/compiler/`. They are compile-track only, so no user program can
reach them.

The jvm leaves take **no key**: the instantiation is their own type argument and the backend emits a frame class
per instantiation (§9.6's table — "an exception, one class per instantiation"). The compile-track intrinsics take
the instantiation as a leading **type value**, because the post-mono evaluator erases type arguments (§10.1 step
7). The asymmetry is real and deliberate; if F5 finds the backend cannot key on the instantiation, the jvm leaves
grow a key parameter and only the five discharger bodies change.

## Open items this staging surfaced — for F5, and one for a decision

- **R2 on the compile-track primitives.** `read[S](key: Type): S` and `exit[E, A](key: Type, e: E): A` are
  body-less leaves whose bare generic result gets instantiated at a meta-carrying type, so R2 requires them to
  *state* a transfer, and a brace over a generic result has no spelling today. Step 7 flagged this and left it to
  F5; nothing is invented here. It is fail-safe: a missing brace is a hard error at F5, not a silent wrong bound.
- **The compile track gets `Abort` and `Throw` only, and that is deliberate.** Two things stop `State`, `Writer`
  and `Dep` from being staged there, and both are F5's to answer. First, **frames would collide**: the compile
  track has *one* cell intrinsic keyed by the type value it is handed, so `State[String]`, `Writer[String]` and
  `Dep[String]` would all reach the same cell — where the jvm layer separates them for free by declaring its leaves
  per module. Keeping them apart needs a per-effect key built from an *applied* marker (`StateCell[S]`), and how an
  applied type is written in value position is not settled: the step-7 tests only ever passed a nullary one
  (`String[]`). `Abort` needs only a nullary marker, which is why it has one (`Aborted`, `private`) rather than
  keying on `Unit` and sharing frames with a `Throw[Unit]`. Second, the **nullary-read memoisation trap**: `state`
  and `dependency` are nullary and the evaluator memoises a nullary binding, so they would answer their first read
  forever (step 7's third finding). Nothing is lost meanwhile — a compile-time reduction that reaches an unbodied
  discharger is *stuck*, which is loud, not wrong.
- **`E[]` in value position — ANSWERED at F5: it does not work.** The compile-track dischargers passed a *generic
  binder* as the key (`escape(E[], obj)`), and a binder has no value form: "Value depends on a compile-time parameter
  but does not reduce to a constant." So the compile-track `Throw` is **deleted** rather than rewritten. `Abort`
  stays, keying on its own nullary `Aborted` marker — it is what makes an `if..else` guard reduce, which is the
  control effect the checker actually meets — and a compile-time reduction reaching the now-unbodied `runThrow` is
  *stuck*, which is loud rather than wrong.

- **The jvm leaves grew no key, but they did change shape.** F5 emits them **once per instantiation** instead of once
  erased, so the frame key is the instantiation itself and no key parameter was needed. They did become
  continuation-passing (`escapeInternal(body, onExit, onValue)`, `withCellInternal(initial, body, combine)`) so that
  no native constructs an Eliot `data` — which is the "only the five discharger bodies change" this file predicted,
  arrived at from the other direction.
- **`eliot-test` is not yet moved.** F7 applied everything but the sibling repository; the framework rewrite here
  still waits on it.

- **`eliot.test.pure` has no v6 spelling, and is deleted.** `pure { … }` forbade *all* effects in a test body by
  pinning it to `Id`, which has no `Suspend`. Under v6 a slot's row does not close: §9.4's resolution order says
  an entry the slot does not supply continues the walk into the caller's scope, so a slot cannot say "and
  nothing else". The staged framework drops the word, and the cases that used it are plain `in { … }` — bounded,
  as every case is, by the suite's own declared row. Making that expressible would be a *language* addition (a
  closed row), so it is surfaced here rather than invented: **this one wants a decision before F7.**
- **`eliot-test/docs/mocking.md`** still describes the mock carrier. It is an F9 document, not staged here.

## Notes on the eliot-test rewrite

The mock **carrier** is gone; the doubles are five named implementations bound by the one `with` chain on
`mocked`'s slot type, and the journal lives in `State[Recording]` — a clause row, so it is charged at that slot
and discharged there, and never appears in a test's own row. Three consequences worth stating:

- `Throw[E, Mock]` — the "one instance for every failure channel there will ever be" — is gone with the carrier,
  and with it the rule that a raise arrives already rendered. `raising` is now an ordinary
  `raising[E ~ Show](expectedReport, body: {Throw[E]} Unit)`, so it works for a project's own error type, and it
  is no longer an operation of `Mocking`.
- `mocked` must now discharge `Throw[IoError]`, which the `FileSystem` and `Process` effects declare even though
  no double raises one; it reports it as a failed case.
- `describedAs` loses its `Id` pin. Discharge is a *frame*, not a carrier layer, so a definition can now discharge
  the very effect it declares — the nearest enclosing frame is its own. That closes
  `gotcha_cannot_supply_own_declared_row_entry` for the control effects.

## One compiler change landed with this step

`ImplementBlock`'s pattern is now **optional**, so an anonymous `implement Console { … }` with no brackets parses.
Step 4 landed `effect`, the named `implement` and `with` dark but missed this one shape, which every v6 `effect`
implementation takes. No source in the tree writes a pattern-less `implement`, so it changes nothing until the
flag day; `ASTParserTest`'s pin was inverted to say so.

## The behavioural baseline (`docs/effects.md` §10.1 step 9)

`baseline.txt` beside this file is what the flag-day gate compares against: the recorded behaviour and code
size of every example jar, swept from the tree at the commit named in its header — the parent of the commit
that adds it, since the recording step changed no compiler source and the numbers are therefore still the
tree's. It is produced by
`scripts/example-sweep.sh` (durable — the byte-identity gate of an ordinary change reads the same report's
`jar.md5` lines), and it is deleted with this directory at F7, once it has done its job.

Per module it records the jar's md5, size, class count and total bytecode instruction count, the instruction
counts of the Main-Class stub and of the module's own class, and the program's exit code and standard output
from one run with stdin at `/dev/null`. All 45 examples carrying a `main` compile and exit 0; the other five
(`PluginA`/`B`/`C`, `GreetingTest`, `SettingsTest`) are library modules with no `main` and so produce no jar.
Totals across the 45: **950,823 bytes, 1,741 classes, 26,839 instructions.**

At F8, re-sweep and compare with

```
scripts/example-sweep.sh -o after.txt
diff <(grep -v '^#' .v6/baseline.txt) <(grep -v '^#' after.txt)
```

Every `exit:` and `stdout:` line must be unchanged — that is the gate. The `jar.md5`, `jar.size`,
`jar.classes`, `jar.instructions` and `module.instructions` lines legitimately move, and §8 asks the flag-day
commit to *state* the difference; since specialisation is the mechanism (§9.7) and not a follow-up, the
expectation is no size regression, and a regression is a finding to explain rather than a cost to accept.

Three consecutive sweeps of the unchanged tree produced byte-identical reports, md5s included, so a difference
in a later sweep is a real difference. Two things make that hold and must not be dropped: **stdin is
`/dev/null`**, because four examples read it and a tty changes what they print, and the **fact cache is
deleted between compiles**, or stale facts replay.
