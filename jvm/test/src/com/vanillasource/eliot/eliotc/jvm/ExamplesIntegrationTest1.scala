package com.vanillasource.eliot.eliotc.jvm

/** Part 1 of 4 of the end-to-end example-program suite (see [[FullIntegrationTest]]). The 88 cases are split across
  * four classes so Mill runs them in four parallel test-worker JVMs — each warming its own resident compilation
  * session — instead of one worker compiling all 88 serially. Keep each part self-contained: a case that shares a
  * class-level helper (e.g. `orderingPrelude`) must stay in the same part as that helper. */
class ExamplesIntegrationTest1 extends FullIntegrationTest {

  "hello world" should "print a string" in {
    compileAndRun("""import eliot.effect.Console
def main: IO[Unit] = printLine("Hello World!")""")
      .asserting(_ shouldBe "Hello World!")
  }

  // --- Effects M2: library spine (Effect/Sync/Console) + the Console -> Sync -> IO layering, run end-to-end ---

  // The headline M2 acceptance: a hand-monadic `{Console}` computation reading a line and echoing it. `flatMap` is an
  // `Effect[IO]` op resolved at the concrete use site (the carrier is not in `echo`'s declared effect set); `readLine`
  // and `printLine` resolve through the constrained HKT instance `implement[F[_] ~ Sync] Console[F]` at `F := IO`, which
  // in turn discharges `Sync[IO]`. `main` commits to the concrete runnable carrier `IO[Unit]` (Decision 8).
  "console effect" should "read a line and echo it through the Console -> Sync -> IO layering" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Effect
        |
        |def echo: {Console} Unit = flatMap(s -> printLine(s), readLine)
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "echoed line\n"
    ).asserting(_ shouldBe "echoed line")
  }

  // `printLine` is now the `Console` effect's method, generic over any `Sync` carrier — yet a plain `main : IO[Unit]`
  // still resolves it at `F := IO` (the `Console[IO]` instance rides the base `Sync[IO]`), so the original HelloWorld
  // keeps working unchanged.
  it should "still print a literal via the Console effect at a concrete IO main" in {
    compileAndRun("""import eliot.effect.Console
def main: IO[Unit] = printLine("Hello World!")""")
      .asserting(_ shouldBe "Hello World!")
  }

  // A `{Console}` business function is carrier-polymorphic; pinning it at the call site (`main : IO[Unit] = greet`)
  // infers `F := IO` and resolves both effect operations through the layering.
  it should "run a carrier-polymorphic {Console} function pinned to IO at the call site" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Effect
        |
        |def greet: {Console} Unit = flatMap(ignore -> printLine("b"), printLine("a"))
        |
        |def main: IO[Unit] = greet""".stripMargin
    ).asserting(_ shouldBe "a\nb")
  }

  // The `private` leaf native behind `printLine` is unreachable from application code: naming it across the module
  // boundary is refused by the resolver (the fail-safe that keeps untracked I/O impossible).
  "the private I/O leaf" should "be unreachable from application code" in {
    compileForErrors("""def main: IO[Unit] = IO(_ -> eliot.effect.Console::printLineInternal("x"))""")
      .asserting(_ should include("Name is private."))
  }

  // --- Effects M3: body auto-lift (the headline) — direct-style code, no hand-written flatMap ---

  // THE headline: a direct-style program. `readLine` is effectful (`F[String]`) but flows into `printLine`, which expects
  // a plain `String`; the checker's effect lift binds it, producing `flatMap(x -> printLine(x), readLine)`, with the
  // carrier pinned to `IO` by `main`'s return. No `import eliot.effect.Effect`, no hand-written `flatMap`.
  "effect auto-lift" should "sequence a direct-style printLine(readLine) at a concrete IO main" in {
    compileAndRun("""import eliot.effect.Console
def main: IO[Unit] = printLine(readLine)""", stdin = "echoed line\n")
      .asserting(_ shouldBe "echoed line")
  }

  // The same direct-style body in a carrier-polymorphic `{Console}` business function, pinned to `IO` at the call site.
  it should "sequence a direct-style {Console} business function pinned to IO" in {
    compileAndRun(
      """import eliot.effect.Console
        |def echo: {Console} Unit = printLine(readLine)
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "carrier line\n"
    ).asserting(_ shouldBe "carrier line")
  }

  // Already-monadic code is left untouched (the rewrite is idempotent): the hand-written `flatMap` still compiles and
  // runs exactly as before, proving auto-lift does not double-bind a stored effect action.
  it should "leave already-monadic flatMap code unchanged" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Effect
        |
        |def echo: {Console} Unit = flatMap(s -> printLine(s), readLine)
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "still works\n"
    ).asserting(_ shouldBe "still works")
  }

  // The `.` operator chains an ability method on an abstract effect carrier: `readLine.flatMap(f)` is `.(readLine,
  // flatMap(f))`. The subject flows into `.`'s flex `a: A` slot, which the checker's effect lift defers; `flatMap`
  // rigidifies `A` to its carrier-typed storage slot, so the subject passes through unsequenced (binding it into a bare
  // `a: A` would corrupt the carrier). This is the idiomatic subject-last spelling of the hand-written `flatMap` above.
  it should "chain an ability method on an abstract carrier via the dot operator" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Effect
        |
        |def echo: {Console} Unit = readLine.flatMap(line -> printLine(line))
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "dot chained\n"
    ).asserting(_ shouldBe "dot chained")
  }

  // The dual case: an effectful subject dotted into a *plain-value* function (`readLine.shout`, `shout(s: String)`) must
  // still bind — the inlined `shout(readLine)` sequences `readLine` into `shout`'s `String` slot. Proves the inlining
  // restores the ordinary bind decision rather than blanket-suppressing it for every dotted subject.
  it should "bind an effectful subject dotted into a plain-value function" in {
    compileAndRun(
      """import eliot.effect.Console
        |
        |def shout(s: String): String = s
        |
        |def echo: {Console} Unit = printLine(readLine.shout)
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "loud\n"
    ).asserting(_ shouldBe "loud")
  }

  // --- The checker-side effect lift (docs/effect-lift-in-checker.md, Step 4): shapes no pre-typing heuristic could
  // decide. The bind/pass-through decision is per *instantiation* in the NbE checker, so a generic slot (`.`'s or a
  // user pipe's `a: A`) sequences an effectful subject when its instantiation is a plain value, and passes it through
  // when its instantiation is the carrier itself. ---

  // The regression of the dot-inline era (commit 81485de9): an effectful subject dotted into a *function-typed
  // parameter*. `.`'s flex `a: A` slot defers, `f` rigidifies `A := String`, and the deferred slot bind-lifts.
  it should "bind an effectful subject dotted into a function-typed parameter" in {
    compileAndRun(
      """import eliot.effect.Console
        |
        |def call(f: String => String): {Console} Unit = printLine(readLine.f)
        |
        |def main: IO[Unit] = call(s -> s)""".stripMargin,
      stdin = "through f\n"
    ).asserting(_ shouldBe "through f")
  }

  // A user-defined pipe with `.`'s exact shape, chaining an ability method: the effectful subject passes through the
  // flex slot into `flatMap`'s carrier storage (no bind) — proving the decision is type-directed, not `.`-specific.
  it should "chain an ability method through a user-defined pipe operator" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Effect
        |
        |infix left below apply def |>[A, B](a: A, f: A => B): B = f(a)
        |
        |def echo: {Console} Unit = readLine |> flatMap(line -> printLine(line))
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "piped\n"
    ).asserting(_ shouldBe "piped")
  }

  // The non-infix twin — an ordinary named function with the identical signature — proving the fix is shape-generic,
  // not operator plumbing.
  it should "chain an ability method through a non-infix pipe function" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Effect
        |
        |def pipe[A, B](a: A, f: A => B): B = f(a)
        |
        |def echo: {Console} Unit = pipe(readLine, flatMap(line -> printLine(line)))
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "plainly piped\n"
    ).asserting(_ shouldBe "plainly piped")
  }

  // The bind direction through the same user pipe: the subject flows into a *concrete* `String` slot, so the deferred
  // flex slot rigidifies and the subject is sequenced.
  it should "bind an effectful subject piped into a concrete slot" in {
    compileAndRun(
      """import eliot.effect.Console
        |
        |infix left below apply def |>[A, B](a: A, f: A => B): B = f(a)
        |
        |def shout(s: String): String = s
        |
        |def echo: {Console} Unit = printLine(readLine |> shout)
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "piped loud\n"
    ).asserting(_ shouldBe "piped loud")
  }

  // Author-lifted machinery into a pure slot: the former phase's `isAuthorMachineryCall` left `pure("lifted")` alone
  // and monomorphization rejected it; under the type-directed lift it binds like any carrier-headed argument.
  it should "bind author-written machinery flowing into a pure slot" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Effect
        |
        |def echo: {Console} Unit = printLine(pure("lifted"))
        |
        |def main: IO[Unit] = echo""".stripMargin
    ).asserting(_ shouldBe "lifted")
  }

  // Fail-safe: a value that performs an effect but is declared with a non-carrier (pure) return type is rejected at the
  // effect-check phase, not silently miscompiled.
  "an effectful body under a pure return" should "be rejected" in {
    compileForErrors(
      """import eliot.effect.Console
        |def helper: String = printLine(readLine)
        |
        |def main: IO[Unit] = printLine(helper)""".stripMargin
    ).asserting(_ should include("performs an effect but is declared pure"))
  }

  // --- Effects M4: multi-effect composition + propagation + Dep ---

  // The `Log` effect mirrors `Console` (a fine effect riding the `Sync` base): `log` writes a tagged line. A `{Log}`
  // business function pinned to `IO` at the call site runs through the JVM `Log` instance.
  "log effect" should "emit a tagged diagnostic line through the Log -> Sync -> IO layering" in {
    compileAndRun(
      """import eliot.effect.Log
        |def announce: {Log} Unit = log("starting up")
        |
        |def main: IO[Unit] = announce""".stripMargin
    ).asserting(_ shouldBe "[LOG] starting up")
  }

  // Multiple effects in one signature, carrier-unified across callees: `log` (Log) and `readLine` (Console) share the
  // one carrier `F`, auto-lifted into a single `flatMap` chain.
  "multiple effects in one signature" should "carrier-unify Log and Console in a direct-style body" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Log
        |def echoLog: {Log, Console} Unit = log(readLine)
        |
        |def main: IO[Unit] = echoLog""".stripMargin,
      stdin = "from stdin\n"
    ).asserting(_ shouldBe "[LOG] from stdin")
  }

  // Effect propagation is a plain set-subset check: a body may only perform effects it declares. Calling a `{Log}`
  // function from a `{Console}`-only function leaks `Log`, rejected at the definition with a precise message.
  "an undeclared effect" should "be rejected with a precise propagation error" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.effect.Log
        |def doLog: {Log} Unit = log("hi")
        |
        |def caller: {Console} Unit = doLog
        |
        |def main: IO[Unit] = caller""".stripMargin
    ).asserting(_ should include("performs the effect 'Log' but does not declare it"))
  }

  // The headline M4 program: three effects (`Dep[Database]`, `Log`, `Console`) composed in one direct-style body, run
  // end to end. `dependency` is dispatched by the dependency type and collapses to the injected singleton.
  "a multi-effect Dep/Log/Console program" should "compile and run end to end" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Log
        |import eliot.effect.Dep
        |import eliot.effect.Effect
        |
        |data Database(url: String)
        |
        |implement Dep[Database, IO] {
        |   def dependency: IO[Database] = pure(Database("jdbc://app-db"))
        |}
        |
        |def run: {Dep[Database], Log, Console} Unit = andThen(log(dependency.url), printLine(readLine))
        |
        |def andThen(first: Unit, second: Unit): Unit = second
        |
        |def main: IO[Unit] = run""".stripMargin,
      stdin = "echoed\n"
    ).asserting(_ shouldBe "[LOG] jdbc://app-db\nechoed")
  }

  // Two distinct-typed `Dep`s in one body each resolve `dependency` to their own instance and yield the correct distinct value
  // (the first dependency's url, then the second's name) — proving by-type dispatch does not collapse the two.
  "two distinct-typed Deps" should "each resolve dependency to its own instance in one body" in {
    val program =
      """import eliot.effect.Console
        |import eliot.effect.Dep
        |import eliot.effect.Effect
        |
        |data Database(url: String)
        |data Logger(name: String)
        |
        |implement Dep[Database, IO] { def dependency: IO[Database] = pure(Database("the-db")) }
        |implement Dep[Logger, IO] { def dependency: IO[Logger] = pure(Logger("the-logger")) }
        |
        |def first: {Dep[Database], Dep[Logger]} String = pick(url(dependency), name(dependency))
        |
        |def main: IO[Unit] = printLine(first)""".stripMargin
    compileAndRun(program + "\n\ndef pick(a: String, b: String): String = a")
      .asserting(_ shouldBe "the-db")
  }

  it should "resolve the second distinct Dep to its own value" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Dep
        |import eliot.effect.Effect
        |
        |data Database(url: String)
        |data Logger(name: String)
        |
        |implement Dep[Database, IO] { def dependency: IO[Database] = pure(Database("the-db")) }
        |implement Dep[Logger, IO] { def dependency: IO[Logger] = pure(Logger("the-logger")) }
        |
        |def second: {Dep[Database], Dep[Logger]} String = pick(url(dependency), name(dependency))
        |
        |def pick(a: String, b: String): String = b
        |
        |def main: IO[Unit] = printLine(second)""".stripMargin
    ).asserting(_ shouldBe "the-logger")
  }
}
