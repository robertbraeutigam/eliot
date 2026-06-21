package com.vanillasource.eliot.eliotc.jvm

class ExamplesIntegrationTest extends FullIntegrationTest {

  "hello world" should "print a string" in {
    compileAndRun("""def main: IO[Unit] = println("Hello World!")""")
      .asserting(_ shouldBe "Hello World!")
  }

  // --- Effects M2: library spine (Monad/Sync/Console) + the Console -> Sync -> IO layering, run end-to-end ---

  // The headline M2 acceptance: a hand-monadic `{Console}` computation reading a line and echoing it. `flatMap` is a
  // `Monad[IO]` op resolved at the concrete use site (the carrier is not in `echo`'s declared effect set); `readLine`
  // and `println` resolve through the constrained HKT instance `implement[F[_] ~ Sync] Console[F]` at `F := IO`, which
  // in turn discharges `Sync[IO]`. `main` commits to the concrete runnable carrier `IO[Unit]` (Decision 8).
  "console effect" should "read a line and echo it through the Console -> Sync -> IO layering" in {
    compileAndRun(
      """import eliot.lang.Monad
        |
        |def echo: {Console} Unit = flatMap(readLine, s -> println(s))
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "echoed line\n"
    ).asserting(_ shouldBe "echoed line")
  }

  // `println` is now the `Console` effect's method, generic over any `Sync` carrier — yet a plain `main : IO[Unit]`
  // still resolves it at `F := IO` (the `Console[IO]` instance rides the base `Sync[IO]`), so the original HelloWorld
  // keeps working unchanged.
  it should "still print a literal via the Console effect at a concrete IO main" in {
    compileAndRun("""def main: IO[Unit] = println("Hello World!")""")
      .asserting(_ shouldBe "Hello World!")
  }

  // A `{Console}` business function is carrier-polymorphic; pinning it at the call site (`main : IO[Unit] = greet`)
  // infers `F := IO` and resolves both effect operations through the layering.
  it should "run a carrier-polymorphic {Console} function pinned to IO at the call site" in {
    compileAndRun(
      """import eliot.lang.Monad
        |
        |def greet: {Console} Unit = flatMap(println("a"), ignore -> println("b"))
        |
        |def main: IO[Unit] = greet""".stripMargin
    ).asserting(_ shouldBe "a\nb")
  }

  // The `private` leaf native behind `println` is unreachable from application code: naming it across the module
  // boundary is refused by the resolver (the fail-safe that keeps untracked I/O impossible).
  "the private I/O leaf" should "be unreachable from application code" in {
    compileForErrors("""def main: IO[Unit] = IO(_ -> eliot.lang.Console::printlnInternal("x"))""")
      .asserting(_ should include("Name is private."))
  }

  // --- Effects M3: body auto-lift (the headline) — direct-style code, no hand-written flatMap ---

  // THE headline: a direct-style program. `readLine` is effectful (`F[String]`) but flows into `println`, which expects
  // a plain `String`; the effect-desugar phase binds it, producing `flatMap(readLine, x -> println(x))`, with the
  // carrier pinned to `IO` by `main`'s return. No `import eliot.lang.Monad`, no hand-written `flatMap`.
  "effect auto-lift" should "sequence a direct-style println(readLine) at a concrete IO main" in {
    compileAndRun("""def main: IO[Unit] = println(readLine)""", stdin = "echoed line\n")
      .asserting(_ shouldBe "echoed line")
  }

  // The same direct-style body in a carrier-polymorphic `{Console}` business function, pinned to `IO` at the call site.
  it should "sequence a direct-style {Console} business function pinned to IO" in {
    compileAndRun(
      """def echo: {Console} Unit = println(readLine)
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "carrier line\n"
    ).asserting(_ shouldBe "carrier line")
  }

  // Already-monadic code is left untouched (the rewrite is idempotent): the hand-written `flatMap` still compiles and
  // runs exactly as before, proving auto-lift does not double-bind a stored effect action.
  it should "leave already-monadic flatMap code unchanged" in {
    compileAndRun(
      """import eliot.lang.Monad
        |
        |def echo: {Console} Unit = flatMap(readLine, s -> println(s))
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "still works\n"
    ).asserting(_ shouldBe "still works")
  }

  // Fail-safe: a value that performs an effect but is declared with a non-carrier (pure) return type is rejected at the
  // effect-desugar phase, not silently miscompiled.
  "an effectful body under a pure return" should "be rejected" in {
    compileForErrors(
      """def helper: String = println(readLine)
        |
        |def main: IO[Unit] = println(helper)""".stripMargin
    ).asserting(_ should include("performs an effect but is declared pure"))
  }

  // --- Effects M4: multi-effect composition + propagation + Dep ---

  // The `Log` effect mirrors `Console` (a fine effect riding the `Sync` base): `log` writes a tagged line. A `{Log}`
  // business function pinned to `IO` at the call site runs through the JVM `Log` instance.
  "log effect" should "emit a tagged diagnostic line through the Log -> Sync -> IO layering" in {
    compileAndRun(
      """def announce: {Log} Unit = log("starting up")
        |
        |def main: IO[Unit] = announce""".stripMargin
    ).asserting(_ shouldBe "[LOG] starting up")
  }

  // Multiple effects in one signature, carrier-unified across callees: `log` (Log) and `readLine` (Console) share the
  // one carrier `F`, auto-lifted into a single `flatMap` chain.
  "multiple effects in one signature" should "carrier-unify Log and Console in a direct-style body" in {
    compileAndRun(
      """def echoLog: {Log, Console} Unit = log(readLine)
        |
        |def main: IO[Unit] = echoLog""".stripMargin,
      stdin = "from stdin\n"
    ).asserting(_ shouldBe "[LOG] from stdin")
  }

  // Effect propagation is a plain set-subset check: a body may only perform effects it declares. Calling a `{Log}`
  // function from a `{Console}`-only function leaks `Log`, rejected at the definition with a precise message.
  "an undeclared effect" should "be rejected with a precise propagation error" in {
    compileForErrors(
      """def doLog: {Log} Unit = log("hi")
        |
        |def caller: {Console} Unit = doLog
        |
        |def main: IO[Unit] = caller""".stripMargin
    ).asserting(_ should include("performs the effect 'Log' but does not declare it"))
  }

  // The headline M4 program: three effects (`Dep[Database]`, `Log`, `Console`) composed in one direct-style body, run
  // end to end. `get` is dispatched by the dependency type and collapses to the injected singleton.
  "a multi-effect Dep/Log/Console program" should "compile and run end to end" in {
    compileAndRun(
      """import eliot.lang.Monad
        |
        |data Database(url: String)
        |
        |implement Dep[Database, IO] {
        |   def get: IO[Database] = pure(Database("jdbc://app-db"))
        |}
        |
        |def run: {Dep[Database], Log, Console} Unit = andThen(log(url(get)), println(readLine))
        |
        |def andThen(first: Unit, second: Unit): Unit = second
        |
        |def main: IO[Unit] = run""".stripMargin,
      stdin = "echoed\n"
    ).asserting(_ shouldBe "[LOG] jdbc://app-db\nechoed")
  }

  // Two distinct-typed `Dep`s in one body each resolve `get` to their own instance and yield the correct distinct value
  // (the first dependency's url, then the second's name) — proving by-type dispatch does not collapse the two.
  "two distinct-typed Deps" should "each resolve get to its own instance in one body" in {
    val program =
      """import eliot.lang.Monad
        |
        |data Database(url: String)
        |data Logger(name: String)
        |
        |implement Dep[Database, IO] { def get: IO[Database] = pure(Database("the-db")) }
        |implement Dep[Logger, IO] { def get: IO[Logger] = pure(Logger("the-logger")) }
        |
        |def first: {Dep[Database], Dep[Logger]} String = pick(url(get), name(get))
        |
        |def main: IO[Unit] = println(first)""".stripMargin
    compileAndRun(program + "\n\ndef pick(a: String, b: String): String = a")
      .asserting(_ shouldBe "the-db")
  }

  it should "resolve the second distinct Dep to its own value" in {
    compileAndRun(
      """import eliot.lang.Monad
        |
        |data Database(url: String)
        |data Logger(name: String)
        |
        |implement Dep[Database, IO] { def get: IO[Database] = pure(Database("the-db")) }
        |implement Dep[Logger, IO] { def get: IO[Logger] = pure(Logger("the-logger")) }
        |
        |def second: {Dep[Database], Dep[Logger]} String = pick(url(get), name(get))
        |
        |def pick(a: String, b: String): String = b
        |
        |def main: IO[Unit] = println(second)""".stripMargin
    ).asserting(_ shouldBe "the-logger")
  }

  // Coherence: two implementations of `Dep` for the same dependency type overlap and are rejected (at most one `Dep`
  // per type), via the ordinary ability overlap check.
  "two same-type Dep implementations" should "be rejected as overlapping" in {
    compileForErrors(
      """import eliot.lang.Monad
        |
        |data Database(url: String)
        |
        |implement Dep[Database, IO] { def get: IO[Database] = pure(Database("one")) }
        |implement Dep[Database, IO] { def get: IO[Database] = pure(Database("two")) }
        |
        |def useDb: {Dep[Database]} String = url(get)
        |
        |def main: IO[Unit] = println(useDb)""".stripMargin
    ).asserting(_ should include("Overlapping ability implementation"))
  }

  // --- Effects M5: structural-effect discharge — Abort -> Option via the OptionT transformer ---

  // A completed `{Abort}` computation discharges, via `runAbort`, to `Some` — the `Option` is born only here, at the
  // discharge edge, not in the `{Abort} String` signature. `main` pins the residual carrier `G := IO`.
  "the Abort effect" should "discharge a completed computation to Some via runAbort" in {
    compileAndRun(
      """import eliot.lang.Monad
        |import eliot.lang.Abort
        |import eliot.lang.Option
        |import eliot.lang.OptionT
        |
        |def safe: {Abort} String = "config-value"
        |
        |def main: IO[Unit] = flatMap(runAbort(safe), o -> println(foldOption(o, "<absent>", s -> s)))""".stripMargin
    ).asserting(_ shouldBe "config-value")
  }

  // A short-circuiting `{Abort}` computation discharges to `None`. `abort` resolves to `Abort[OptionT[IO]]` after the
  // carrier is refined to `OptionT[G]` by partial-application injectivity at the `runAbort` call.
  it should "discharge an aborted computation to None via runAbort" in {
    compileAndRun(
      """import eliot.lang.Monad
        |import eliot.lang.Abort
        |import eliot.lang.Option
        |import eliot.lang.OptionT
        |
        |def giveUp: {Abort} String = abort
        |
        |def main: IO[Unit] = flatMap(runAbort(giveUp), o -> println(foldOption(o, "gave up!", s -> s)))""".stripMargin
    ).asserting(_ shouldBe "gave up!")
  }

  // The Decision-10 acceptance: a `{Console, Abort}` program. `Console` rides the `OptionT[IO]` stack via the single
  // `Sync[OptionT[G]]` base lift (no per-effect lifting), so the print runs; then `abort` short-circuits the result to
  // `None`. Proves the constrained-HKT instance + base-Sync-lift path end to end.
  "a {Console, Abort} program" should "run Console through the OptionT[IO] stack via the Sync lift, then short-circuit" in {
    compileAndRun(
      """import eliot.lang.Monad
        |import eliot.lang.Abort
        |import eliot.lang.Option
        |import eliot.lang.OptionT
        |
        |def andThen[A](first: Unit, second: A): A = second
        |
        |def loud: {Console, Abort} String = andThen(println("trying"), abort)
        |
        |def main: IO[Unit] = flatMap(runAbort(loud), o -> println(foldOption(o, "stopped", s -> s)))""".stripMargin
    ).asserting(_ shouldBe "trying\nstopped")
  }

  // Throw[E] is the typed-error sibling of Abort, discharging to Either[E, _] via the EitherT transformer — proving the
  // structural-discharge pattern generalises to a two-type-parameter effect and a two-constructor result.
  "the Throw effect" should "discharge a completed computation to Right via runThrow" in {
    compileAndRun(
      """import eliot.lang.Monad
        |import eliot.lang.Throw
        |import eliot.lang.Either
        |import eliot.lang.EitherT
        |
        |def parseOk: {Throw[String]} String = "parsed-value"
        |
        |def main: IO[Unit] = flatMap(runThrow(parseOk), e -> println(foldEither(e, err -> err, v -> v)))""".stripMargin
    ).asserting(_ shouldBe "parsed-value")
  }

  it should "discharge a failed computation to Left, carrying the typed error" in {
    compileAndRun(
      """import eliot.lang.Monad
        |import eliot.lang.Throw
        |import eliot.lang.Either
        |import eliot.lang.EitherT
        |
        |def parseBad: {Throw[String]} String = raise("malformed input")
        |
        |def main: IO[Unit] = flatMap(runThrow(parseBad), e -> println(foldEither(e, err -> err, v -> v)))""".stripMargin
    ).asserting(_ shouldBe "malformed input")
  }

  // Static testability (M5): the SAME carrier-polymorphic {Abort} business logic runs under a pure `Id` test carrier
  // (G := Id), with no production IO — the effect discharges to a plain Option the test inspects. main only does IO to
  // print the already-computed pure results.
  "a carrier-polymorphic {Abort} program" should "run under a pure Id test carrier with no IO and discharge to Option" in {
    compileAndRun(
      """import eliot.lang.Monad
        |import eliot.lang.Applicative
        |import eliot.lang.Abort
        |import eliot.lang.Option
        |import eliot.lang.OptionT
        |
        |data Id[A](runId: A)
        |
        |implement Monad[Id] {
        |   def pure[A](a: A): Id[A] = Id(a)
        |   def flatMap[A, B](fa: Id[A], f: Function[A, Id[B]]): Id[B] = f(runId(fa))
        |}
        |
        |implement Applicative[Id] {
        |   def map[A, B](fa: Id[A], f: Function[A, B]): Id[B] = Id(f(runId(fa)))
        |}
        |
        |def allowed: {Abort} String = "granted"
        |def denied: {Abort} String = abort
        |
        |def testAllowed: Option[String] = runId(runAbort(allowed))
        |def testDenied: Option[String] = runId(runAbort(denied))
        |
        |def main: IO[Unit] = flatMap(
        |   println(foldOption(testAllowed, "DENIED", s -> s)),
        |   ignored -> println(foldOption(testDenied, "DENIED", s -> s)))""".stripMargin
    ).asserting(_ shouldBe "granted\nDENIED")
  }

  // The State effect (M5): a `{State[S]}` computation discharges to a `Pair[A, S]` (result + final state) via the
  // `StateT` transformer, born only at the `runState` edge. `getState`/`putState` resolve to `State[StateT[S, IO]]`
  // after the carrier is refined to `StateT[S, G]` by partial-application injectivity at the `runState` call. `swap`
  // reads the state, installs a new one, and returns the previous value; discharged on IO from initial "before".
  "the State effect" should "thread state through a {State} computation and discharge to a Pair via runState" in {
    compileAndRun(
      """import eliot.lang.Monad
        |import eliot.lang.State
        |import eliot.lang.Pair
        |import eliot.lang.StateT
        |
        |def swap(next: String): {State[String]} String =
        |   flatMap(getState, old -> flatMap(putState(next), ignored -> pure(old)))
        |
        |def prog: IO[Pair[String, String]] = runState(swap("after"), "before")
        |
        |def main: IO[Unit] = flatMap(prog, p -> flatMap(println(first(p)), ignored -> println(second(p))))""".stripMargin
    ).asserting(_ shouldBe "before\nafter")
  }

  // Static testability for State: the SAME carrier-polymorphic {State} logic runs under a pure `Id` carrier (G := Id),
  // discharging to a plain `Pair` with no IO. This is the State→Pair discharge the generic-multi-field-data codegen
  // bug previously blocked (a two-field generic `Pair` at the `Unit`/`String` mix of `getState`/`putState`).
  "a carrier-polymorphic {State} program" should "run under a pure Id carrier with no IO and discharge to a Pair" in {
    compileAndRun(
      """import eliot.lang.Monad
        |import eliot.lang.Applicative
        |import eliot.lang.State
        |import eliot.lang.Pair
        |import eliot.lang.StateT
        |
        |data Id[A](runId: A)
        |
        |implement Monad[Id] {
        |   def pure[A](a: A): Id[A] = Id(a)
        |   def flatMap[A, B](fa: Id[A], f: Function[A, Id[B]]): Id[B] = f(runId(fa))
        |}
        |
        |implement Applicative[Id] {
        |   def map[A, B](fa: Id[A], f: Function[A, B]): Id[B] = Id(f(runId(fa)))
        |}
        |
        |def swap(next: String): {State[String]} String =
        |   flatMap(getState, old -> flatMap(putState(next), ignored -> pure(old)))
        |
        |def demo: Pair[String, String] = runId(runState(swap("second"), "first"))
        |
        |def main: IO[Unit] = flatMap(println(first(demo)), ignored -> println(second(demo)))""".stripMargin
    ).asserting(_ shouldBe "first\nsecond")
  }

  // A {State, Console} program: Console rides the `StateT[S, IO]` stack via the single `Sync[StateT[S, G]]` base lift
  // (the n-not-n×m lifting), so the print runs while the state threads through and discharges to a Pair.
  "a {State, Console} program" should "run Console through the StateT[String, IO] stack via the Sync lift" in {
    compileAndRun(
      """import eliot.lang.Monad
        |import eliot.lang.State
        |import eliot.lang.Pair
        |import eliot.lang.StateT
        |
        |def step: {State[String], Console} String =
        |   flatMap(println("running step"),
        |      ignored -> flatMap(getState, old -> flatMap(putState("done"), ignored2 -> pure(old))))
        |
        |def main: IO[Unit] = flatMap(runState(step, "start"),
        |   p -> flatMap(println(first(p)), ignored -> println(second(p))))""".stripMargin
    ).asserting(_ shouldBe "running step\nstart\ndone")
  }

  "ability" should "dispatch to correct implementation" in {
    compileAndRun(
      """ability Show[A] {
        |   def show(a: A): String
        |}
        |
        |data Hello(name: String)
        |
        |implement Show[Hello] {
        |   def show(a: Hello): String = "Hello World!"
        |}
        |
        |def main: IO[Unit] = println(show(Hello("World")))""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "ability constraint" should "pass ability through generic function" in {
    compileAndRun(
      """ability Show[A] {
        |   def show(a: A): String
        |}
        |
        |data Hello(name: String)
        |
        |implement Show[Hello] {
        |   def show(a: Hello): String = "Hello World!"
        |}
        |
        |def showAnything[A ~ Show](thing: A): String = show(thing)
        |
        |def main: IO[Unit] = println(showAnything(Hello("World")))""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "ability derive" should "derive implementation for generic type" in {
    compileAndRun(
      """ability Show[A] {
        |  def show(a: A): String
        |}
        |
        |implement Show[String] {
        |  def show(str: String): String = str
        |}
        |
        |data Box[A](content: A)
        |
        |implement[A ~ Show] Show[Box[A]] {
        |  def show(box: Box[A]): String = show(content(box))
        |}
        |
        |def main: IO[Unit] = println(show(Box("Hello World!")))""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "ability associated type" should "handle associated types in abilities" in {
    compileAndRun(
      """ability AssociatedType[T] {
        |   type MagicType
        |
        |   def handle(value: T, param: MagicType): String
        |}
        |
        |data Name(name: String)
        |
        |implement AssociatedType[Name] {
        |   type MagicType = String
        |
        |   def handle(value: Name, param: MagicType): String = "Hello"
        |}
        |
        |def main: IO[Unit] = println(handle(Name("Johnny"), "Ni"))""".stripMargin
    ).asserting(_ shouldBe "Hello")
  }

  "generic types" should "support type-level integer parameters" in {
    compileAndRun(
      """def hello[I: BigInteger]: String = "Hello World!"
        |
        |def main: IO[Unit] = println(hello[1])""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "pattern matching" should "match data constructors and extract values" in {
    compileAndRun(
      """data Maybe[A] = Nothing | Just(value: A)
        |
        |def describe(m: Maybe[String]): String = m match {
        |  case Nothing -> "empty"
        |  case Just(v) -> v
        |}
        |
        |def main: IO[Unit] = println(describe(Just("hello")))""".stripMargin
    ).asserting(_ shouldBe "hello")
  }

  "operators" should "evaluate infix operators with correct associativity" in {
    compileAndRun(
      """def main: IO[Unit] = println(content(Bool("Hello") | Bool("World") | Bool("!")))
        |
        |data Bool(content: String)
        |
        |infix left
        |def |(lhs: Bool, rhs: Bool): Bool = rhs""".stripMargin
    ).asserting(_ shouldBe "!")
  }

  "handle with" should "support multiple data types with pattern matching" in {
    compileAndRun(
      """data Something = Else | Other
        |
        |data Greeting = Hello | Goodbye
        |
        |def greet(g: Greeting): String = g match {
        |  case Hello -> "Hello!"
        |  case Goodbye -> "Goodbye!"
        |}
        |
        |def something(s: Something): String = s match {
        |  case Else -> "Else!"
        |  case Other -> "Other!"
        |}
        |
        |infix def or(s1: String, s2: String): String = s1
        |
        |def main: IO[Unit] = println(something(Else) or greet(Goodbye))""".stripMargin
    ).asserting(_ shouldBe "Else!")
  }

  "monomorph check" should "handle dependent type integer arithmetic" in {
    compileAndRun(
      """data Box[I: BigInteger](content: String)
        |
        |def someFunction[I: BigInteger](arg: String): Box[I.inc] = Box[3](arg)
        |
        |def main: IO[Unit] = println(content(someFunction[2]("Hello World!")))""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "function as type" should "use type-level computation for types" in {
    compileAndRun(
      """data Box[A](content: A)
        |
        |def stringBox: Type = Box[String]
        |
        |def stringBoxWithContent: stringBox = Box("Hello World!")
        |
        |def main: IO[Unit] = println(content(stringBoxWithContent))""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "type values" should "match on type-level values" in {
    compileAndRun(
      """data Person[NAME: String](content: String)
        |
        |data Box[A](a: A)
        |
        |def personName(t: Type): String = t match {
        |   case Person[name] -> name
        |   case _            -> "<not a person>"
        |}
        |
        |def main: IO[Unit] = println(personName(Person["John"]))""".stripMargin
    ).asserting(_ shouldBe "John")
  }

  "dot operator" should "support method-style chaining" in {
    compileAndRun(
      """data Box[A](content: A)
        |
        |def filter[A](expr: String, box: Box[A]): Box[A] = box
        |
        |def map[A, B](f: Function[A, B], box: Box[A]): Box[B] = Box(f(content(box)))
        |
        |def flatMap[A, B](f: Function[A, Box[B]], box: Box[A]): Box[B] = f(content(box))
        |
        |def as[A, B](b: B, box: Box[A]): Box[B] = box.map(_ -> b)
        |
        |def logic: Box[String] = Box("Hello").filter("Expr").map(_ -> "Earth!").as("World!")
        |
        |def main: IO[Unit] = println(logic.content)""".stripMargin
    ).asserting(_ shouldBe "World!")
  }

  "unicode" should "support unicode operator names" in {
    compileAndRun(
      """def main: IO[Unit] = println(<===>)
        |
        |def <===>: String = "Hello World!"""".stripMargin
    ).asserting(_ shouldBe "Hello World!")
  }

  "integer addition" should "compute and print a sum at runtime" in {
    compileAndRun(
      """def main: IO[Unit] = println(intToString(3 + 4))""".stripMargin
    ).asserting(_ shouldBe "7")
  }

  "integer subtraction" should "compute and print a difference at runtime" in {
    compileAndRun(
      """def main: IO[Unit] = println(intToString(10 - 4))""".stripMargin
    ).asserting(_ shouldBe "6")
  }

  "integer arithmetic" should "respect operator precedence at runtime" in {
    compileAndRun(
      """def main: IO[Unit] = println(intToString(2 + 3 * 4))""".stripMargin
    ).asserting(_ shouldBe "14")
  }

  it should "compute a negative result at runtime" in {
    compileAndRun(
      """def main: IO[Unit] = println(intToString(3 - 10))""".stripMargin
    ).asserting(_ shouldBe "-7")
  }

  // Byte operands whose sum overflows a byte carry up to a Short result (`nativeAddByteToShort`): 100 and 100 each fit
  // `Byte` ([-128,127]) but 200 does not, so the result is laid out at the wider representation.
  it should "carry a byte-operand sum into a wider result representation at runtime" in {
    compileAndRun(
      """def main: IO[Unit] = println(intToString(100 + 100))""".stripMargin
    ).asserting(_ shouldBe "200")
  }

  // Short operands whose difference fits a byte: 1000 and 999 are `Short`, but the result range [1,1] fits `Byte`, so
  // the surrounding `nativeWiden` narrows the leaf's result back down (additive cancellation).
  it should "narrow a short-operand difference into a byte result at runtime" in {
    compileAndRun(
      """def main: IO[Unit] = println(intToString(1000 - 999))""".stripMargin
    ).asserting(_ shouldBe "1")
  }

  "integer range widening" should "widen a bare literal into a broader declared range at runtime" in {
    compileAndRun(
      """def widened: Int[0, 1000] = 7
        |
        |def main: IO[Unit] = println(intToString(widened))""".stripMargin
    ).asserting(_ shouldBe "7")
  }

  it should "accept a literal assigned through a width alias and print it" in {
    compileAndRun(
      """def small: Byte = 42
        |
        |def main: IO[Unit] = println(intToString(small))""".stripMargin
    ).asserting(_ shouldBe "42")
  }

  it should "construct and access a record with a bare Int field (W2)" in {
    // `Counter`'s bare `Int` field generalizes the type to `Counter[lo, hi]`; the accessor `n` (a match under the hood)
    // recovers the field. Exercises construct + accessor + handleCases end-to-end at runtime.
    compileAndRun(
      """data Counter(n: Int)
        |
        |def main: IO[Unit] = println(intToString(n(Counter(42))))""".stripMargin
    ).asserting(_ shouldBe "42")
  }

  it should "round-trip a record field through an explicit match (W2)" in {
    compileAndRun(
      """data Counter(n: Int)
        |
        |def field(c: Counter[7, 7]): Int[7, 7] = c match { case Counter(x) -> x }
        |
        |def main: IO[Unit] = println(intToString(field(Counter(7))))""".stripMargin
    ).asserting(_ shouldBe "7")
  }

  // W2 follow-up: type-level matching over an auto-bounded record. `Counter`'s bare `Int` field grows the type to
  // `Counter[lo, hi]`, so the `typeMatch` matcher's handler must bind both synthesized bounds (`case Counter[lo, hi]`).
  it should "type-level match over an auto-bounded record (W2 follow-up)" in {
    compileAndRun(
      """data Counter(n: Int)
        |
        |def describe(t: Type): String = t match {
        |   case Counter[lo, hi] -> "counter"
        |   case _               -> "<other>"
        |}
        |
        |def main: IO[Unit] = println(describe(Counter[0, 255]))""".stripMargin
    ).asserting(_ shouldBe "counter")
  }

  it should "widen an arithmetic result into a broader declared range at runtime" in {
    compileAndRun(
      """def total: Int[0, 1000] = 3 + 4
        |
        |def main: IO[Unit] = println(intToString(total))""".stripMargin
    ).asserting(_ shouldBe "7")
  }

  // W3: a bare `Int` return is *calculated* from the body. `double`'s return bounds come from `x + x`, not from the
  // (omitted) source annotation; the caller observes them off `double`'s monomorphized signature and runs it.
  it should "calculate a bare Int return from the body and run it (W3)" in {
    compileAndRun(
      """def double(x: Int): Int = x + x
        |
        |def main: IO[Unit] = println(intToString(double(21)))""".stripMargin
    ).asserting(_ shouldBe "42")
  }

  it should "calculate a bare data return from the body and run it (W3)" in {
    compileAndRun(
      """data Counter(n: Int)
        |
        |def mk(v: Int): Counter = Counter(v)
        |
        |def main: IO[Unit] = println(intToString(n(mk(42))))""".stripMargin
    ).asserting(_ shouldBe "42")
  }

  // Phase 3: representation selection. Each range below lowers to a different JVM wrapper (Short/Integer/Long); a wrong
  // width would overflow and print the wrong number, so these assert the chosen representation is wide enough.
  "integer representation selection" should "carry an Int[0, 70000] value at the 32-bit representation" in {
    compileAndRun(
      """def big: Int[0, 70000] = 70000
        |
        |def main: IO[Unit] = println(intToString(big))""".stripMargin
    ).asserting(_ shouldBe "70000")
  }

  it should "compute a product that overflows 16 bits at the 32-bit representation" in {
    compileAndRun(
      """def product: Int[0, 1000000] = 1000 * 1000
        |
        |def main: IO[Unit] = println(intToString(product))""".stripMargin
    ).asserting(_ shouldBe "1000000")
  }

  it should "carry a value beyond 32 bits at the 64-bit representation" in {
    compileAndRun(
      """def huge: Int[0, 5000000000] = 5000000000
        |
        |def main: IO[Unit] = println(intToString(huge))""".stripMargin
    ).asserting(_ shouldBe "5000000000")
  }

  // Mangling/dedup: a generic function instantiated at two ranges that share a representation (both `Byte`) lowers to a
  // single concrete signature, so the per-range mangled methods collapse correctly and both calls dispatch.
  "generic instantiation" should "reuse one method for two same-representation ranges at runtime" in {
    compileAndRun(
      """def id[Mn: BigInteger, Mx: BigInteger](x: Int[Mn, Mx]): Int[Mn, Mx] = x
        |def a: Int[0, 3] = 3
        |def b: Int[0, 5] = 5
        |
        |def main: IO[Unit] = println(intToString(id(a) + id(b)))""".stripMargin
    ).asserting(_ shouldBe "8")
  }

  // The same generic at two ranges with *different* representations (`Byte` and `Long`) lowers to two distinct concrete
  // signatures — collision-free overloads — and each call dispatches to the one matching its width.
  it should "dispatch distinct methods for two different-representation ranges at runtime" in {
    compileAndRun(
      """def id[Mn: BigInteger, Mx: BigInteger](x: Int[Mn, Mx]): Int[Mn, Mx] = x
        |def a: Int[0, 3] = 3
        |def big: Int[0, 5000000000] = 5000000000
        |
        |def main: IO[Unit] = println(intToString(id(a) + id(big)))""".stripMargin
    ).asserting(_ shouldBe "5000000003")
  }

  // Eliminating a multi-field `data` desugars its `match` into an N-parameter handler (`x -> y -> body`), which the
  // backend lowers into nested single-argument closures. Extracting the *first* field exercises the outer closure.
  "multi-field data" should "extract the first field of a two-field constructor" in {
    compileAndRun(
      """data Pair(a: String, b: String)
        |
        |def first(p: Pair): String = p match {
        |  case Pair(x, y) -> x
        |}
        |
        |def main: IO[Unit] = println(first(Pair("hello", "world")))""".stripMargin
    ).asserting(_ shouldBe "hello")
  }

  // Extracting the *second* field returns the inner closure's parameter, so it checks the captured-variable plumbing
  // across the peeled frames.
  it should "extract the second field of a two-field constructor" in {
    compileAndRun(
      """data Pair(a: String, b: String)
        |
        |def second(p: Pair): String = p match {
        |  case Pair(x, y) -> y
        |}
        |
        |def main: IO[Unit] = println(second(Pair("hello", "world")))""".stripMargin
    ).asserting(_ shouldBe "world")
  }

  // A three-field constructor peels into three nested closures; selecting the middle field confirms more than two
  // levels of nesting resolve their parameters correctly.
  it should "extract the middle field of a three-field constructor" in {
    compileAndRun(
      """data Triple(a: String, b: String, c: String)
        |
        |def middle(t: Triple): String = t match {
        |  case Triple(x, y, z) -> y
        |}
        |
        |def main: IO[Unit] = println(middle(Triple("one", "two", "three")))""".stripMargin
    ).asserting(_ shouldBe "two")
  }

  // A two-field `Int` round-trip: the fields lower to different representations (`Byte`-range and `Long`-range), so
  // matching them out and summing also exercises cross-representation arithmetic over the peeled closures.
  it should "match out two integer fields at different representations and sum them" in {
    compileAndRun(
      """data IntPair(small: Int[0, 255], large: Int[0, 5000000000])
        |
        |def sum(p: IntPair): Int[0, 5000000255] = p match {
        |  case IntPair(s, l) -> s + l
        |}
        |
        |def main: IO[Unit] = println(intToString(sum(IntPair(200, 5000000000))))""".stripMargin
    ).asserting(_ shouldBe "5000000200")
  }

  // A *generic* multi-field constructor is emitted once (one class + factory per data type) but used at two
  // instantiations whose bare type-parameter fields have different JVM carriers (`Pair[Box[String], String]` stores a
  // `Box`, `Pair[String, String]` stores a `String`). Both the factory and every call site must erase the polymorphic
  // fields to `Object`, or the single shared `Pair(..)` descriptor mismatches the call site (a runtime
  // `NoSuchMethodError`). This is the generic-multi-field codegen bug that previously blocked two-field generic data.
  it should "construct and match a generic two-field constructor at two distinct instantiations" in {
    compileAndRun(
      """data Box[A](item: A)
        |
        |data Pair[A, B](first: A, second: B)
        |
        |def secondOf[A, B](p: Pair[A, B]): B = p match {
        |  case Pair(x, y) -> y
        |}
        |
        |def main: IO[Unit] = println(secondOf(Pair(Box("boxed"), secondOf(Pair("c", "plain")))))""".stripMargin
    ).asserting(_ shouldBe "plain")
  }

  // The same erasure, but with a *mixed* constructor: a concrete `tag: String` field beside a polymorphic `value: A`
  // field, used at `Tagged[String]` and `Tagged[Tagged[String]]`. The concrete field must keep its `String` descriptor
  // (only the polymorphic field erases), and the two instantiations must still agree on the shared factory. Exercised
  // through the auto-generated single-constructor field accessor (`value`), i.e. the pattern-match codegen path.
  it should "construct and access a generic constructor mixing a concrete and a polymorphic field" in {
    compileAndRun(
      """data Tagged[A](tag: String, value: A)
        |
        |def main: IO[Unit] = println(value(value(Tagged("outer", Tagged("inner", "deep")))))""".stripMargin
    ).asserting(_ shouldBe "deep")
  }

  // A single-constructor *union* whose constructor name differs from the type name (`data Color = Red`). The data class
  // must be named after the *type* (`Color`), not the constructor (`Red`): the factory's declared return, the match
  // parameter type, and the `PatternMatch$Color$impl` singleton all refer to `Color`, so naming the class `Red` made the
  // factory `Red()` return a `Red` while every call site expected a `Color` (a runtime `NoSuchMethodError`). Records
  // (constructor name = type name) always coincided and were never affected.
  it should "construct and match a single-constructor union whose constructor name differs from the type" in {
    compileAndRun(
      """data Color = Red
        |
        |def name(c: Color): String = c match {
        |  case Red -> "red"
        |}
        |
        |def main: IO[Unit] = println(name(Red))""".stripMargin
    ).asserting(_ shouldBe "red")
  }

  // The same single-constructor-union naming, but with a field and a generic parameter, so it also rides the
  // bare-type-parameter erasure: `Box[A] = Wrap(item: A)` (constructor `Wrap` ≠ type `Box`) accessed via the
  // auto-generated `item` accessor.
  it should "construct and access a generic single-constructor union whose constructor name differs from the type" in {
    compileAndRun(
      """data Box[A] = Wrap(item: A)
        |
        |def main: IO[Unit] = println(item(Wrap("wrapped")))""".stripMargin
    ).asserting(_ shouldBe "wrapped")
  }
}
