package com.vanillasource.eliot.eliotc.jvm

/** Part 1 of 4 of the end-to-end example-program suite (see [[FullIntegrationTest]]). The 88 cases are split across
  * four classes so Mill runs them in four parallel test-worker JVMs — each warming its own resident compilation
  * session — instead of one worker compiling all 88 serially. Keep each part self-contained: a case that shares a
  * class-level helper (e.g. `orderingPrelude`) must stay in the same part as that helper. */
class ExamplesIntegrationTest1 extends FullIntegrationTest {

  "hello world" should "print a string" in {
    compileAndRun("""import eliot.jvm.IO
import eliot.effect.Console
def main: IO[Unit] = printLine("Hello World!")""")
      .asserting(_ shouldBe "Hello World!")
  }

  // --- Effects M2: library spine (Effect/Suspend/Console) + the Console -> Suspend -> IO layering, run end-to-end ---

  // The headline M2 acceptance: a hand-monadic `{Console}` computation reading a line and echoing it. `flatMap` is an
  // `Effect[IO]` op resolved at the concrete use site (the carrier is not in `echo`'s declared effect set); `readLine`
  // and `printLine` resolve through the constrained HKT instance `implement[F[_] ~ Suspend] Console[F]` at `F := IO`, which
  // in turn discharges `Suspend[IO]`. `main` commits to the concrete runnable carrier `IO[Unit]` (Decision 8).
  "console effect" should "read a line and echo it through the Console -> Suspend -> IO layering" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |
        |def orEmpty(o: Option[String]): String = o.orAbort else ""
        |
        |def echo: {Console} Unit = flatMap(s -> printLine(orEmpty(s)), readLine)
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "echoed line\n"
    ).asserting(_ shouldBe "echoed line")
  }

  // `printLine` is now the `Console` effect's method, generic over any `Suspend` carrier — yet a plain `main : IO[Unit]`
  // still resolves it at `F := IO` (the `Console[IO]` instance rides the base `Suspend[IO]`), so the original HelloWorld
  // keeps working unchanged.
  it should "still print a literal via the Console effect at a concrete IO main" in {
    compileAndRun("""import eliot.jvm.IO
import eliot.effect.Console
def main: IO[Unit] = printLine("Hello World!")""")
      .asserting(_ shouldBe "Hello World!")
  }

  // A `{Console}` business function is carrier-polymorphic; pinning it at the call site (`main : IO[Unit] = greet`)
  // infers `F := IO` and resolves both effect operations through the layering.
  it should "run a carrier-polymorphic {Console} function pinned to IO at the call site" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |
        |def greet: {Console} Unit = flatMap(ignore -> printLine("b"), printLine("a"))
        |
        |def main: IO[Unit] = greet""".stripMargin
    ).asserting(_ shouldBe "a\nb")
  }

  // The `private` leaf native behind `printLine` is unreachable from application code: naming it across the module
  // boundary is refused by the resolver (the fail-safe that keeps untracked I/O impossible).
  "the private I/O leaf" should "be unreachable from application code" in {
    compileForErrors("""import eliot.jvm.IO
def main: IO[Unit] = IO(_ -> eliot.effect.Console::printLineInternal("x"))""")
      .asserting(_ should include("Name is private."))
  }

  // --- Effects M3: body auto-lift (the headline) — direct-style code, no hand-written flatMap ---

  // THE headline: a direct-style program. `readLine` is effectful (`F[Option[String]]`) but flows into `orEmpty`, which
  // expects a plain `Option[String]`; the checker's effect lift binds it, producing `flatMap(x -> printLine(orEmpty(x)),
  // readLine)`, with the carrier pinned to `IO` by `main`'s return. No `import eliot.carrier.Effect`, no hand-written
  // `flatMap`. (`orEmpty` discharges `readLine`'s end-of-input `Option`; the effect machinery under test is the same
  // either way, so every program below carries it rather than complicating what it is probing.)
  "effect auto-lift" should "sequence a direct-style printLine(readLine) at a concrete IO main" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |def orEmpty(o: Option[String]): String = o.orAbort else ""
        |
        |def main: IO[Unit] = printLine(orEmpty(readLine))""".stripMargin,
      stdin = "echoed line\n"
    ).asserting(_ shouldBe "echoed line")
  }

  // The same direct-style body in a carrier-polymorphic `{Console}` business function, pinned to `IO` at the call site.
  it should "sequence a direct-style {Console} business function pinned to IO" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |def orEmpty(o: Option[String]): String = o.orAbort else ""
        |
        |def echo: {Console} Unit = printLine(orEmpty(readLine))
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "carrier line\n"
    ).asserting(_ shouldBe "carrier line")
  }

  // Already-monadic code is left untouched (the rewrite is idempotent): the hand-written `flatMap` still compiles and
  // runs exactly as before, proving auto-lift does not double-bind a stored effect action.
  it should "leave already-monadic flatMap code unchanged" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |
        |def orEmpty(o: Option[String]): String = o.orAbort else ""
        |
        |def echo: {Console} Unit = flatMap(s -> printLine(orEmpty(s)), readLine)
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "still works\n"
    ).asserting(_ shouldBe "still works")
  }

  // `readLine.flatMap(f)` is `.(readLine, flatMap(f))`, and `.`'s subject slot is the plain generic `a: A`
  // (`def .[A, B](a: A, f: A => {Effect} B): {Effect} B`). Passing a computation through it is what §1 rule 4
  // forbids — an effect passes through a position only if that position declares one — so this spelling is
  // **rejected**, exactly as `p.runStateToPair(s0)` is. The subject is run where it is written (rule 1) and
  // `flatMap`'s carrier-typed storage slot then has a payload where it wanted the computation. The direct call
  // above (`flatMap(f, readLine)`) is the spelling that works, its `fa: F[A]` slot being carrier-typed.
  //
  // It used to read as green: the compile failed, no jar was written, and the previous test's jar — an echo
  // program, fed this test's own stdin — answered with the expected text.
  it should "reject an ability method chained on an abstract carrier via the dot operator" in {
    compileForErrors(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |
        |def orEmpty(o: Option[String]): String = o.orAbort else ""
        |
        |def echo: {Console} Unit = readLine.flatMap(line -> printLine(orEmpty(line)))
        |
        |def main: IO[Unit] = echo""".stripMargin
    ).asserting(_ should include("Type mismatch."))
  }

  // The dual case: an effectful subject dotted into a *plain-value* function (`readLine.shout`, `shout(s: Option[String])`)
  // must still bind — the inlined `shout(readLine)` sequences `readLine` into `shout`'s plain slot. Proves the inlining
  // restores the ordinary bind decision rather than blanket-suppressing it for every dotted subject.
  it should "bind an effectful subject dotted into a plain-value function" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |
        |def shout(s: Option[String]): String = s.orAbort else ""
        |
        |def echo: {Console} Unit = printLine(readLine.shout)
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "loud\n"
    ).asserting(_ shouldBe "loud")
  }

  // --- Effectful subjects at a generic slot. The decision is read from *declarations* before checking (effects as
  // rows v3), not per instantiation in the checker: a computation at a rowless slot is run where it is written, and
  // may never pass through — the pass-through arm these cases were written for was v2's, and is deleted. ---

  // An effectful subject dotted into a *function-typed parameter*: `readLine` is run where written and `f` receives
  // its payload, `.`'s `A` being instantiated at a plain value.
  // The handler is a named pure function rather than an inline `s -> s.orAbort else ""`: a lambda body at a
  // *rowless* arrow slot is still elaborated in the enclosing region, so an inline discharge there lands on the
  // caller's carrier (`IO[String]`) instead of reaching the `Id` boundary, and no longer matches the slot's declared
  // `String`. That is a live gap, unrelated to the dotted subject this case is about.
  it should "bind an effectful subject dotted into a function-typed parameter" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |
        |def orEmpty(o: Option[String]): String = o.orAbort else ""
        |
        |def call(f: Option[String] => String): {Console} Unit = printLine(readLine.f)
        |
        |def main: IO[Unit] = call(s -> orEmpty(s))""".stripMargin,
      stdin = "through f\n"
    ).asserting(_ shouldBe "through f")
  }

  // A user-defined pipe with `.`'s shape, but declaring no row anywhere (`|>[A, B](a: A, f: A => B): B`): handing it
  // a computation is the same §1 rule-4 violation as the dot above, and here the elaborator names the slot itself
  // rather than leaving a type mismatch downstream. Rule 4 is what makes the two spellings agree — the decision is
  // read from declarations, not from the operator.
  it should "reject an ability method chained through a user-defined pipe operator" in {
    compileForErrors(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |
        |infix left below apply def |>[A, B](a: A, f: A => B): B = f(a)
        |
        |def orEmpty(o: Option[String]): String = o.orAbort else ""
        |
        |def echo: {Console} Unit = readLine |> flatMap(line -> printLine(orEmpty(line)))
        |
        |def main: IO[Unit] = echo""".stripMargin
    ).asserting(_ should include("declares no effect row"))
  }

  // The non-infix twin — an ordinary named function with the identical signature — rejected identically, which is
  // what "read from declarations" means: no operator plumbing takes part in the decision.
  it should "reject an ability method chained through a non-infix pipe function" in {
    compileForErrors(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |
        |def pipe[A, B](a: A, f: A => B): B = f(a)
        |
        |def orEmpty(o: Option[String]): String = o.orAbort else ""
        |
        |def echo: {Console} Unit = pipe(readLine, flatMap(line -> printLine(orEmpty(line))))
        |
        |def main: IO[Unit] = echo""".stripMargin
    ).asserting(_ should include("declares no effect row"))
  }

  // The bind direction through the same user pipe: the subject flows into a *concrete* `String` slot, so the deferred
  // flex slot rigidifies and the subject is sequenced.
  it should "bind an effectful subject piped into a concrete slot" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |
        |infix left below apply def |>[A, B](a: A, f: A => B): B = f(a)
        |
        |def shout(s: Option[String]): String = s.orAbort else ""
        |
        |def echo: {Console} Unit = printLine(readLine |> shout)
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "piped loud\n"
    ).asserting(_ shouldBe "piped loud")
  }

  // Author-written machinery into a pure slot is a §1 rule-4 violation, and has been rejected since A.11.7-X:
  // `printLine`'s parameter is a plain `String`, which declares no effect row, so a computation may not land there.
  // The tree used to accept it — the checker hoisted it into a real `IO.pure` + `IO.flatMap` round trip for a string
  // the caller already held.
  it should "reject author-written machinery flowing into a pure slot" in {
    compileForErrors(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |
        |def echo: {Console} Unit = printLine(pure("lifted"))
        |
        |def main: IO[Unit] = echo""".stripMargin
    ).asserting(_ should include("argument 1 of 'printLine' declares no effect row"))
  }

  // A **constructor class** — an ability over a type constructor that is not an effect. Its shape is exactly an effect
  // ability's (a higher-kinded binder its methods' types mention), and it was read as one: the row derivation charged
  // every use of `unwrap`/`wrap` with the effect `Ctr`, so this program was rejected with "performs the effect 'Ctr'
  // but does not declare it" and there was no spelling that made it compile. Effect-ness is declared per method now,
  // with a row, and `Ctr` declares none.
  "a constructor-class ability" should "perform no effect, and drop into pure code" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |
        |ability Ctr[F[_]] {
        |   def wrap[A](a: A): F[A]
        |   def unwrap[A](fa: F[A]): A
        |}
        |
        |data Bx[A](content: A)
        |
        |implement Ctr[Bx] {
        |   def wrap[A](a: A): Bx[A] = Bx(a)
        |   def unwrap[A](fa: Bx[A]): A = fa.content
        |}
        |
        |def rebox(b: Bx[String]): Bx[String] = wrap(unwrap(b))
        |
        |def unboxed(b: Bx[String]): String = unwrap(b)
        |
        |def main: IO[Unit] = printLine(unboxed(rebox(Bx("boxed"))))""".stripMargin
    ).asserting(_ shouldBe "boxed")
  }

  // Fail-safe: a value that performs an effect but is declared with a non-carrier (pure) return type is rejected by the
  // per-definition row verification, not silently miscompiled.
  "an effectful body under a pure return" should "be rejected" in {
    compileForErrors(
      """import eliot.jvm.IO
import eliot.effect.Console
        |def orEmpty(o: Option[String]): String = o.orAbort else ""
        |
        |def helper: String = printLine(orEmpty(readLine))
        |
        |def main: IO[Unit] = printLine(helper)""".stripMargin
    ).asserting(_ should include("performs the effect 'Console' but does not declare it"))
  }

  // --- Effects M4: multi-effect composition + propagation + Dep ---

  // The `Log` effect mirrors `Console` (a fine effect riding the `Suspend` base): `log` writes a tagged line. A `{Log}`
  // business function pinned to `IO` at the call site runs through the JVM `Log` instance.
  "log effect" should "emit a tagged diagnostic line through the Log -> Suspend -> IO layering" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Log
        |def announce: {Log} Unit = log("starting up")
        |
        |def main: IO[Unit] = announce""".stripMargin
    ).asserting(_ shouldBe "[LOG] starting up")
  }

  // Multiple effects in one signature, carrier-unified across callees: `log` (Log) and `readLine` (Console) share the
  // one carrier `F`, auto-lifted into a single `flatMap` chain.
  "multiple effects in one signature" should "carrier-unify Log and Console in a direct-style body" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.effect.Log
        |def orEmpty(o: Option[String]): String = o.orAbort else ""
        |
        |def echoLog: {Log, Console} Unit = log(orEmpty(readLine))
        |
        |def main: IO[Unit] = echoLog""".stripMargin,
      stdin = "from stdin\n"
    ).asserting(_ shouldBe "[LOG] from stdin")
  }

  // Effect propagation is a plain set-subset check: a body may only perform effects it declares. Calling a `{Log}`
  // function from a `{Console}`-only function leaks `Log`, rejected at the definition with a precise message.
  "an undeclared effect" should "be rejected with a precise propagation error" in {
    compileForErrors(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.effect.Log
        |def doLog: {Log} Unit = log("hi")
        |
        |def caller: {Console} Unit = doLog
        |
        |def main: IO[Unit] = caller""".stripMargin
    ).asserting(_ should include("performs the effect 'Log' but does not declare it"))
  }

  // The headline M4 program: three effects (`Dep[Database]`, `Log`, `Console`) composed in one direct-style body, run
  // end to end. `dependency` reads the environment; the Database is injected at the discharge site by `provide`.
  "a multi-effect Dep/Log/Console program" should "compile and run end to end" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.effect.Log
        |import eliot.effect.Dep
        |import eliot.carrier.Effect
        |
        |data Database(url: String)
        |
        |def orEmpty(o: Option[String]): String = o.orAbort else ""
        |
        |def run: {Dep[Database], Log, Console} Unit = andThen(log(dependency.url), printLine(orEmpty(readLine)))
        |
        |def andThen(first: Unit, second: Unit): Unit = second
        |
        |def main: IO[Unit] = run.provide(Database("jdbc://app-db"))""".stripMargin,
      stdin = "echoed\n"
    ).asserting(_ shouldBe "[LOG] jdbc://app-db\nechoed")
  }

  // Two distinct-typed `Dep`s in one body each read their own environment and yield the correct distinct value (the
  // first dependency's url, then the second's name) — proving by-type dispatch does not collapse the two. Each is
  // supplied by its own chained `.provide` (fully discharged to a pure result — the flex-flex carrier-alias case).
  "two distinct-typed Deps" should "each resolve dependency to its own value in one body" in {
    val program =
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.effect.Dep
        |import eliot.carrier.Effect
        |
        |data Database(url: String)
        |data Logger(name: String)
        |
        |def firstDep: {Dep[Database], Dep[Logger]} String = pick(url(dependency), name(dependency))
        |
        |def main: IO[Unit] = printLine(provide(Logger("the-logger"), provide(Database("the-db"), firstDep)))""".stripMargin
    compileAndRun(program + "\n\ndef pick(a: String, b: String): String = a")
      .asserting(_ shouldBe "the-db")
  }

  it should "resolve the second distinct Dep to its own value" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.effect.Dep
        |import eliot.carrier.Effect
        |
        |data Database(url: String)
        |data Logger(name: String)
        |
        |def secondDep: {Dep[Database], Dep[Logger]} String = pick(url(dependency), name(dependency))
        |
        |def pick(a: String, b: String): String = b
        |
        |def main: IO[Unit] = printLine(provide(Logger("the-logger"), provide(Database("the-db"), secondDep)))""".stripMargin
    ).asserting(_ shouldBe "the-logger")
  }

  // --- Testing strategy: a fake effect implementation supplied by the test, with production code untouched ---

  // The substitution mechanism of `docs/testing-effects.md` §2, end to end: `greet` is ordinary carrier-generic
  // production code declaring `{Terminal}` and nothing else, and the test supplies both the carrier (`Session`, a pure
  // input/transcript pair with no `Suspend` instance, so it cannot perform real I/O) and the `Terminal[Session]`
  // instance that interprets the ability into it. Unification instantiates `greet`'s minted carrier to `Session` at
  // `runSession`'s slot, and monomorphization resolves the fake. Also pins the two shapes §3 flags as accidental: the
  // run sits *inside* `greetSession` (a program cannot be passed to a runner — L3) and `greetSession`'s declared return
  // is an *applied* type (a nullary one trips the pre-mono row verifier — L2).
  "a fake effect carrier" should "let a test interpret an ability without changing the production code" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |
        |ability Terminal[F[_]] {
        |   def write(line: String): {Terminal} Unit
        |   def read: {Terminal} String
        |}
        |
        |def greet: {Terminal} Unit = {
        |   val name = read
        |   write("Hello, " ++ name ++ "!")
        |}
        |
        |data Session[A](runSession: Function[Pair[String, String], Pair[A, Pair[String, String]]])
        |
        |implement Effect[Session] {
        |   def pure[A](a: A): Session[A] = Session(w -> Pair(a, w))
        |   def flatMap[A, B](f: Function[A, Session[B]], fa: Session[A]): Session[B] =
        |      Session(w -> foldPair(a -> w2 -> runSession(f(a))(w2), runSession(fa)(w)))
        |   def map[A, B](f: Function[A, B], fa: Session[A]): Session[B] =
        |      Session(w -> foldPair(a -> w2 -> Pair(f(a), w2), runSession(fa)(w)))
        |}
        |
        |implement Terminal[Session] {
        |   def write(line: String): Session[Unit] =
        |      Session(w -> Pair(unit, Pair(first(w), second(w) ++ line ++ ";")))
        |   def read: Session[String] = Session(w -> Pair(first(w), w))
        |}
        |
        |def greetSession: Pair[Unit, Pair[String, String]] = runSession(greet)(Pair("Bob", ""))
        |
        |def main: IO[Unit] = printLine(second(second(greetSession)))""".stripMargin
    ).asserting(_ shouldBe "Hello, Bob!;")
  }
}
