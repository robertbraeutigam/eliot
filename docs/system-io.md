# Process environment and subprocesses (`eliot.system`) — Design

Status: IMPLEMENTED (jvm). Driven by the build system's needs — a CLI needs its arguments, and the
launcher shells out to `git` and spawns the compiler — but designed as the general-purpose process
API of the stdlib. The companion of `docs/file-io.md`, and deliberately shaped like it.

## What shipped

- `stdlib/eliot/eliot/system/Environment.els` (abstract) + `jvm/eliot/eliot/system/Environment.els`:
  the `Environment` ability — `arguments`, `environmentVariable`, `workingDirectory`.
- `stdlib/eliot/eliot/system/Process.els` (abstract) + `jvm/eliot/eliot/system/Process.els`: the
  `Process` ability — `run` (captures both streams) and `runInheritingIo` (hands the child this
  program's streams, yields the exit code) — plus `ProcessResult` and the `command` /
  `withArgument` builders.
- `jvm/src/.../processor/SystemNatives.scala`: the leaves, and the argument stash described below.
- `jvm/test/.../SystemIoIntegrationTest.scala`: end-to-end coverage, spawning this JVM's own `java`.

## The four decisions

**1. Two abilities, not one.** `Environment` cannot fail, so it rides `Suspend` alone, exactly like
`Console`. `Process` can fail to *start* a program, so its instance additionally requires
`Throw[IoError]`. Fusing them would force a failure channel on `environmentVariable`, which has
nothing to report.

**2. `Process` reuses `eliot.file`'s `IoError` rather than introducing its own.** Every program that
runs other programs also reads files — that is what a build tool *is* — and two error types in one
row means two `catch`es, each hand-pinned by its error type, because `runThrow` cannot pick between
two `Throw`s by inference. One shared type collapses that to one discharge. The cost is a dependency
from `eliot.system` to `eliot.file`, which is honest: both are the operating system.

**3. A non-zero exit is data, not a failure.** It is how programs report decisions — `git ls-remote`
on a missing ref, a compiler on a program with errors — and the caller almost always wants to read
the output before deciding what it means. Only *not being able to run the program at all* raises.
Go, Rust and Python all made the opposite choice at some layer and all of them grew a "but don't
raise" variant; starting on the right side of that costs nothing.

**4. Commands are lists, never strings.** Quoting is the one part of process spawning that differs
on every platform and is wrong in every hand-rolled splitter. `command("git").withArgument(…)` builds
the list left to right; nothing ever re-splits a string.

Two deliberate omissions, both matching `eliot.file`'s reduced scope: **no process handle and no
streaming** (both operations start a child and wait for it, so nothing can be left running), and
**no environment control for the child** (it inherits this program's). Interactive children need the
same `Bracket` machinery file handles are waiting on.

## The one mechanism worth explaining: `arguments`

A JVM program receives its arguments as the parameter of `main(String[])` — a stack slot of a method
in a class no leaf native can reach. So the backend stashes them:
`JvmClassGenerator.createApplicationMain` declares a public static `eliot$arguments` field of type
`java.util.List` on the synthesized entry-point class and stores `Arrays.asList(args)` into it before
calling the woven `main`; `Environment.argumentsInternal` is a bare `GETSTATIC` of that field.

The field lives on the **entry-point class** (module `main`, internal name `main`) rather than on
`Environment`'s own class for a specific reason: whole-program compilation only emits classes that
are reachable, so a program that never mentions `Environment` never generates that class — but every
executable jar has an entry point. `Arrays.asList` wraps without copying, and Eliot's `List` is
immutable by contract (`append` builds a fresh list), so handing the wrapper out directly is safe.

`arguments` excludes the program's own name. On every platform that name is absent, ambiguous, or a
re-spelling of something the program already knows, and a verb dispatcher that has to remember to
skip a leading element gets it wrong exactly once.

## Gaps this leaves for the build system

The launcher still needs, and does not have: **SHA-256** and **HTTP GET** (both now reachable by
shelling out, which is why they were not built), and — the one that is not an I/O question — a
**bounded-iteration primitive**. Minimal-version selection computes a transitive closure, which is an
unbounded fixpoint loop that a total language cannot write; the options are a stdlib `iterate` native
(the reason `walk` is a native) or an explicit iteration budget. A budget folded over a list of units
works today and needs nothing from the platform, at the price of a declared ceiling.
