package com.vanillasource.eliot.eliotc.jvm

/** Part 2 of 4 of the end-to-end example-program suite (see [[FullIntegrationTest]]). The 88 cases are split across
  * four classes so Mill runs them in four parallel test-worker JVMs — each warming its own resident compilation session
  * — instead of one worker compiling all 88 serially. Keep each part self-contained: a case that shares a class-level
  * helper (e.g. `orderingPrelude`) must stay in the same part as that helper.
  */
class ExamplesIntegrationTest2 extends FullIntegrationTest {

  // Coherence: two implementations of an ability for the same type overlap and are rejected (at most one instance per
  // type combination), via the ordinary ability overlap check.
  "two overlapping ability implementations" should "be rejected as overlapping" in {
    // A generic `Display[A]` and a specific `Display[Database]` are distinct implementations (different `(ability, pattern)`
    // identities) whose patterns unify, so the definition-time overlap lint rejects the pair. (Two *identical*
    // `Display[Database]` would instead be the same identity and collide as a duplicate name.)
    compileForErrors(
      """import eliot.jvm.IO
import eliot.effect.Console
        |
        |ability Display[A] {
        |   def display(a: A): String
        |}
        |
        |data Database(url: String)
        |
        |implement[A] Display[A] { def display(a: A): String = "one" }
        |implement Display[Database] { def display(d: Database): String = "two" }
        |
        |def useDb: String = display(Database("x"))
        |
        |def main: IO[Unit] = printLine(useDb)""".stripMargin
    ).asserting(_ should include("Overlapping ability implementation"))
  }

  // --- Effects M5: structural-effect discharge — Abort -> Option via the AbortCarrier transformer ---

  // A completed `{Abort}` computation discharges, via `runAbort`, to `Some` — the `Option` is born only here, at the
  // discharge edge, not in the `{Abort} String` signature. `main` pins the residual carrier `G := IO`.
  "the Abort effect" should "discharge a completed computation to Some via runAbort" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |import eliot.effect.Abort
        |
        |def safe: {Abort} String = "config-value"
        |
        |def main: IO[Unit] = flatMap(o -> printLine(foldOption(o, "<absent>", s -> s)), runAbort(safe))""".stripMargin
    ).asserting(_ shouldBe "config-value")
  }

  // A short-circuiting `{Abort}` computation discharges to `None`. `abort` resolves to `Abort[AbortCarrier[IO]]` after the
  // carrier is refined to `AbortCarrier[G]` by partial-application injectivity at the `runAbort` call.
  it should "discharge an aborted computation to None via runAbort" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |import eliot.effect.Abort
        |
        |def giveUp: {Abort} String = abort
        |
        |def main: IO[Unit] = flatMap(o -> printLine(foldOption(o, "gave up!", s -> s)), runAbort(giveUp))""".stripMargin
    ).asserting(_ shouldBe "gave up!")
  }

  // The Decision-10 acceptance: a `{Console, Abort}` program. `Console` rides the `AbortCarrier[IO]` stack via the single
  // `Suspend[AbortCarrier[G]]` base lift (no per-effect lifting), so the print runs; then `abort` short-circuits the result to
  // `None`. Proves the constrained-HKT instance + base-Suspend-lift path end to end.
  "a {Console, Abort} program" should "run Console through the AbortCarrier[IO] stack via the Suspend lift, then short-circuit" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |import eliot.effect.Abort
        |
        |def andThen[A](first: Unit, second: A): A = second
        |
        |def loud: {Console, Abort} String = andThen(printLine("trying"), abort)
        |
        |def main: IO[Unit] = flatMap(o -> printLine(foldOption(o, "stopped", s -> s)), runAbort(loud))""".stripMargin
    ).asserting(_ shouldBe "trying\nstopped")
  }

  // Throw[E] is the typed-error sibling of Abort, discharging to Either[E, _] via the ThrowCarrier transformer — proving the
  // structural-discharge pattern generalises to a two-type-parameter effect and a two-constructor result.
  "the Throw effect" should "discharge a completed computation to Right via runThrow" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |import eliot.effect.Throw
        |
        |def parseOk: {Throw[String]} String = "parsed-value"
        |
        |def main: IO[Unit] = flatMap(e -> printLine(foldEither(e, err -> err, v -> v)), runThrow(parseOk))""".stripMargin
    ).asserting(_ shouldBe "parsed-value")
  }

  it should "discharge a failed computation to Left, carrying the typed error" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |import eliot.effect.Throw
        |
        |def parseBad: {Throw[String]} String = raise("malformed input")
        |
        |def main: IO[Unit] = flatMap(e -> printLine(foldEither(e, err -> err, v -> v)), runThrow(parseBad))""".stripMargin
    ).asserting(_ shouldBe "malformed input")
  }

  // The everyday discharge: a SINGLE `import eliot.effect.Throw` brings in `raise` AND the `catch` utility, which
  // discharges `{Throw[E]}` and recovers a raised error to a value of the same type — no `Either`/`ThrowCarrier`/`Effect`
  // import, no transformer named. Written infix with a parenthesized lambda operand (`p catch (e -> …)`), which the
  // adjacency-sensitive call parser keeps separate from a call.
  it should "discharge-and-recover in one step via a single import and infix catch" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.effect.Throw
        |
        |def parseOk: {Throw[String]} String = "parsed-value"
        |def parseBad: {Throw[String]} String = raise("malformed input")
        |
        |def main: IO[Unit] = {
        |   printLine(parseOk catch (err -> err))
        |   printLine(parseBad catch (err -> err))
        |}""".stripMargin
    ).asserting(_ shouldBe "parsed-value\nmalformed input")
  }

  // Ability-implementation guards, the Throw client (ability-guards Stage 4): TWO distinct error types in one effect
  // row force the `ThrowCarrier` to nest, which needs both the native `Throw[E, ThrowCarrier[E, G]]` instance and the
  // lift `Throw[E2, ThrowCarrier[E1, G]] where E1 != E2`. Those structurally overlap on the diagonal `E1 = E2`; the
  // `where E1 != E2` guard (discharged at the concrete use site by reducing `Eq[Type]`) makes them disjoint, so the
  // lift routes the foreign error inward while the native owns its own slot. Here `fetch` raises first, so the
  // outer `NetError` catch recovers to its reason. This is the `examples/src/EffectsTwoThrows.els` probe end to end.
  "two distinct Throw error types in one row" should "compile via the guarded self-lift and catch each by its type" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.effect.Throw
        |
        |data NetError(netReason: String)
        |data ParseError(parseReason: String)
        |
        |def fetch(url: String): {Throw[NetError]} String = raise(NetError("http 503"))
        |def parse(raw: String): {Throw[ParseError]} String = raise(ParseError("unexpected token"))
        |
        |def loadConfig(url: String): {Throw[NetError], Throw[ParseError]} String = parse(fetch(url))
        |
        |def main: IO[Unit] =
        |   printLine(loadConfig("https://cfg") catch ((netErr: NetError) -> netErr.netReason) catch ((parseErr: ParseError) -> parseErr.parseReason))""".stripMargin
    ).asserting(_ shouldBe "http 503")
  }

  it should "recover the inner error type when the outer computation succeeds" in {
    // `fetch` succeeds, `parse` raises: the residual row after the `NetError` catch is `{Throw[ParseError]}`, recovered
    // by the second catch — exercising the lift routing a *foreign* error inward (off the diagonal) and then its own
    // native discharge.
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.effect.Throw
        |
        |data NetError(netReason: String)
        |data ParseError(parseReason: String)
        |
        |def fetch(url: String): {Throw[NetError]} String = url
        |def parse(raw: String): {Throw[ParseError]} String = raise(ParseError("unexpected token"))
        |
        |def loadConfig(url: String): {Throw[NetError], Throw[ParseError]} String = parse(fetch(url))
        |
        |def main: IO[Unit] =
        |   printLine(loadConfig("https://cfg") catch ((netErr: NetError) -> netErr.netReason) catch ((parseErr: ParseError) -> parseErr.parseReason))""".stripMargin
    ).asserting(_ shouldBe "unexpected token")
  }

  // The degenerate diagonal: the *same* error type appears in both throwing functions, so the row collapses to one
  // `{Throw[String]}` on a single carrier. The guarded lift declines on the diagonal (`where String != String` = false),
  // so the native `Throw[String, ThrowCarrier[String, IO]]` wins deterministically — no "Multiple ability
  // implementations" ambiguity — and the single `catch` recovers the raised error.
  "two same-typed throws composed on one carrier" should "resolve to the native instance (the lift declines)" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.effect.Throw
        |
        |def raiseFirst: {Throw[String]} String = raise("first failed")
        |def keepSecond(prev: String): {Throw[String]} String = prev
        |
        |def combined: {Throw[String]} String = keepSecond(raiseFirst)
        |
        |def main: IO[Unit] = printLine(combined catch (err -> err))""".stripMargin
    ).asserting(_ shouldBe "first failed")
  }

  // The Abort analogue: a single `import eliot.effect.Abort` brings in `abort` AND the infix `else` utility, which
  // discharges `{Abort}` and supplies a fallback on short-circuit — no `Option`/`AbortCarrier` named.
  "the Abort effect's else" should "discharge-and-default in one step via a single import and infix else" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.effect.Abort
        |
        |def safe: {Abort} String = "config-value"
        |def giveUp: {Abort} String = abort
        |
        |def main: IO[Unit] = {
        |   printLine(safe else "<fallback>")
        |   printLine(giveUp else "<fallback>")
        |}""".stripMargin
    ).asserting(_ shouldBe "config-value\n<fallback>")
  }

  // Regression for the carrier-depth mangling collision. `grade`'s `if..else if..else` chain monomorphizes `if` at the
  // nested carrier `AbortCarrier[AbortCarrier[IO]]`, while the plain `if(..) else ..` in `main` uses `AbortCarrier[IO]`.
  // Both once mangled to a single `if$AbortCarrier$String` method (head-only type-argument suffix), so the backend's
  // signature-dedup treated the two different bodies as byte-identical and dropped one; the surviving wrong-carrier body
  // then produced a value one nesting level off, crashing at runtime with a ClassCastException in the `runAbort`
  // accessor. Recursive type-argument mangling keeps the two depths distinct.
  "if..else used at two effect-carrier nesting depths" should "not collapse into one mangled JVM method" in {
    compileAndRun(
      """
        |import eliot.jvm.IO
        |import eliot.effect.Console
        |import eliot.effect.Abort
        |
        |def grade(s: String): {Abort} String =
        |   if(s == "A", "excellent") else if(s == "B", "good") else "fail"
        |
        |def main: IO[Unit] = {
        |   printLine(grade("A") else "?")
        |   printLine(if(true, "taken") else "skipped")
        |}""".stripMargin
    ).asserting(_ shouldBe "excellent\ntaken")
  }

  // `if`'s branch is a carrier value `F[T]`, so a branch may itself be effectful. Only the SELECTED branch's effect
  // runs — both branches are built, the unchosen one is never executed — matching an imperative `if`.
  "if..else with effectful branches" should "run only the selected branch's effect" in {
    compileAndRun(
      """
        |import eliot.jvm.IO
        |import eliot.effect.Console
        |import eliot.effect.Abort
        |
        |def main: IO[Unit] = {
        |   if(true, printLine("then")) else printLine("else")
        |   if(false, printLine("then")) else printLine("else")
        |}""".stripMargin
    ).asserting(_ shouldBe "then\nelse")
  }

  // Pure-boundary Id defaulting (the identity carrier): a fully-discharged `if..else` meets a PURE declared return
  // directly — the residual carrier defaults to the built-in `Id` (the lang layer's identity carrier) and the checker unwraps it with `runId`
  // (`EffectLifter.tryIdDefault`), so branching needs no carrier in the signature. Exercises the direct return, an
  // `else if` chain, a block `val` holding the discharged branch, and a genuinely runtime condition (from stdin).
  "if..else in a pure function" should "discharge to the Id carrier and unwrap automatically" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.effect.Abort
        |
        |def sign(flag: Bool): String = if(flag, "+") else "-"
        |
        |def chain(a: Bool, b: Bool): String = if(a, "first") else if(b, "second") else "third"
        |
        |def viaBlock(flag: Bool): String = {
        |   val label = if(flag, "yes") else "no"
        |   label
        |}
        |
        |def main: IO[Unit] = {
        |   printLine(sign(true))
        |   printLine(sign(false))
        |   printLine(chain(false, true))
        |   val runtimeFlag = readLine == "y"
        |   printLine(sign(runtimeFlag))
        |   printLine(viaBlock(runtimeFlag))
        |}""".stripMargin,
      stdin = "y\n"
    ).asserting(_ shouldBe "+\n-\nsecond\n+\nyes")
  }

  // The other pure control effects discharge to a pure return the same way: `catch` fully discharges `{Throw}` and
  // `runStateToPair` fully discharges `{State}`, so both results land in bare pure types via the Id defaulting. No
  // `Suspend[Id]` instance exists, so a genuinely side-effecting body under a pure return still fails to resolve —
  // the defaulting can never smuggle real I/O.
  "catch and runStateToPair in a pure function" should "discharge to pure values via the Id carrier" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.effect.Throw
        |import eliot.effect.State
        |
        |def parsed(raw: String): {Throw[String]} String = raise("unparseable")
        |
        |def recovered: String = parsed("x") catch (err -> err)
        |
        |def counted: Pair[String, String] = runStateToPair("initial", state)
        |
        |def main: IO[Unit] = {
        |   printLine(recovered)
        |   printLine(counted.first)
        |}""".stripMargin
    ).asserting(_ shouldBe "unparseable\ninitial")
  }

  // A bare pure value supplied to a generic effect-carrier parameter `F[A]` auto-lifts via `pure` — previously it
  // degenerately unified `F[A] := String` and miscompiled to a runtime VerifyError.
  "a pure value into a generic effect-carrier parameter" should "auto-lift via pure" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |
        |def echo[F[_] ~ Effect, A](value: F[A]): F[A] = value
        |
        |def main: IO[Unit] = printLine(echo("hello"))""".stripMargin
    ).asserting(_ shouldBe "hello")
  }

  // Static testability (M5): the SAME carrier-polymorphic {Abort} business logic runs under a pure `Id` test carrier
  // (G := Id), with no production IO — the effect discharges to a plain Option the test inspects. main only does IO to
  // print the already-computed pure results.
  "a carrier-polymorphic {Abort} program" should "run under a pure Id test carrier with no IO and discharge to Option" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |import eliot.effect.Abort
        |
        |data Id[A](runId: A)
        |
        |implement Effect[Id] {
        |   def pure[A](a: A): Id[A] = Id(a)
        |   def flatMap[A, B](f: Function[A, Id[B]], fa: Id[A]): Id[B] = f(runId(fa))
        |   def map[A, B](f: Function[A, B], fa: Id[A]): Id[B] = Id(f(runId(fa)))
        |}
        |
        |def allowed: {Abort} String = "granted"
        |def denied: {Abort} String = abort
        |
        |def testAllowed: Option[String] = runId(runAbort(allowed))
        |def testDenied: Option[String] = runId(runAbort(denied))
        |
        |def main: IO[Unit] = flatMap(
        |   ignored -> printLine(foldOption(testDenied, "DENIED", s -> s)),
        |   printLine(foldOption(testAllowed, "DENIED", s -> s)))""".stripMargin
    ).asserting(_ shouldBe "granted\nDENIED")
  }

  // The State effect (M5): a `{State[S]}` computation discharges to a `Pair[A, S]` (result + final state) via the
  // `StateCarrier` transformer, born only at the `runStateToPair` edge. `state`/`putState` resolve to `State[StateCarrier[S, IO]]`
  // after the carrier is refined to `StateCarrier[S, G]` by partial-application injectivity at the `runStateToPair` call. `swap`
  // reads the state, installs a new one, and returns the previous value; discharged on IO from initial "before".
  "the State effect" should "thread state through a {State} computation and discharge to a Pair via runStateToPair" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |import eliot.effect.State
        |
        |def swap(next: String): {State[String]} String =
        |   flatMap(old -> flatMap(ignored -> pure(old), putState(next)), state)
        |
        |def prog: IO[Pair[String, String]] = runStateToPair("before", swap("after"))
        |
        |def main: IO[Unit] = flatMap(p -> flatMap(ignored -> printLine(second(p)), printLine(first(p))), prog)""".stripMargin
    ).asserting(_ shouldBe "before\nafter")
  }

  // Static testability for State: the SAME carrier-polymorphic {State} logic runs under a pure `Id` carrier (G := Id),
  // discharging to a plain `Pair` with no IO. This is the State→Pair discharge the generic-multi-field-data codegen
  // bug previously blocked (a two-field generic `Pair` at the `Unit`/`String` mix of `state`/`putState`).
  "a carrier-polymorphic {State} program" should "run under a pure Id carrier with no IO and discharge to a Pair" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |import eliot.effect.State
        |
        |data Id[A](runId: A)
        |
        |implement Effect[Id] {
        |   def pure[A](a: A): Id[A] = Id(a)
        |   def flatMap[A, B](f: Function[A, Id[B]], fa: Id[A]): Id[B] = f(runId(fa))
        |   def map[A, B](f: Function[A, B], fa: Id[A]): Id[B] = Id(f(runId(fa)))
        |}
        |
        |def swap(next: String): {State[String]} String =
        |   flatMap(old -> flatMap(ignored -> pure(old), putState(next)), state)
        |
        |def demo: Pair[String, String] = runId(runStateToPair("first", swap("second")))
        |
        |def main: IO[Unit] = flatMap(ignored -> printLine(second(demo)), printLine(first(demo)))""".stripMargin
    ).asserting(_ shouldBe "first\nsecond")
  }

  // The two projecting discharges: `runStateToValue` keeps only the result (dropping the final state) and
  // `runStateToFinalState` only the final state (dropping the result). Both `map` over `runStateToPair`, so they need a
  // `G ~ Effect`. `swap` returns the previous value and installs `next`; from "before" the value is "before" and the
  // final state is "after".
  "the projecting State discharges" should "keep only the value, or only the final state" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |import eliot.effect.State
        |
        |data Id[A](runId: A)
        |
        |implement Effect[Id] {
        |   def pure[A](a: A): Id[A] = Id(a)
        |   def flatMap[A, B](f: Function[A, Id[B]], fa: Id[A]): Id[B] = f(runId(fa))
        |   def map[A, B](f: Function[A, B], fa: Id[A]): Id[B] = Id(f(runId(fa)))
        |}
        |
        |def swap(next: String): {State[String]} String =
        |   flatMap(old -> flatMap(ignored -> pure(old), putState(next)), state)
        |
        |def onlyValue: String = runId(runStateToValue("before", swap("after")))
        |def onlyState: String = runId(runStateToFinalState("before", swap("after")))
        |
        |def main: IO[Unit] = flatMap(ignored -> printLine(onlyState), printLine(onlyValue))""".stripMargin
    ).asserting(_ shouldBe "before\nafter")
  }

  // Effect-accounting migration: a `{State}` computation discharged
  // via a DOT-chained `runStateToValue` inside a `{Console}`-declaring body compiles and runs. The pre-mono accounting
  // rejected this (a dot-chained discharger was not credited — "performs the effect 'State'"); the monomorphize-phase
  // residual check verifies exactly, so the discharged `State` (its ability method on the inner `StateCarrier`, not the
  // ambient carrier) is absent from `show`'s `{Console}` residual.
  "a dot-chained State discharge inside a {Console} body" should "compile and run, the State absent from the residual" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.effect.State
        |
        |def counter: {State[String]} String = {
        |  putState("done")
        |  state
        |}
        |
        |def show: {Console} Unit = printLine(counter.runStateToValue("init"))
        |
        |def main: IO[Unit] = show""".stripMargin
    ).asserting(_ shouldBe "done")
  }

  // The derived `updateState(f)` = `putState(f(state))`: it reads the current state, applies `f`, and writes it back,
  // all via the effect auto-lift (no explicit flatMap). `flip` genuinely reads the current state (it matches on it), so
  // from `Off` the final state is `On`.
  "the derived updateState" should "read the state, apply the function, and write the result back" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |import eliot.effect.State
        |
        |data Id[A](runId: A)
        |
        |implement Effect[Id] {
        |   def pure[A](a: A): Id[A] = Id(a)
        |   def flatMap[A, B](f: Function[A, Id[B]], fa: Id[A]): Id[B] = f(runId(fa))
        |   def map[A, B](f: Function[A, B], fa: Id[A]): Id[B] = Id(f(runId(fa)))
        |}
        |
        |data Toggle = Off | On
        |
        |def flip(t: Toggle): Toggle = t match {
        |   case Off -> On
        |   case On -> Off
        |}
        |
        |def describe(t: Toggle): String = t match {
        |   case Off -> "off"
        |   case On -> "on"
        |}
        |
        |def switch: {State[Toggle]} Unit = updateState(t -> flip(t))
        |def result: Toggle = runId(runStateToFinalState(Off, switch))
        |
        |def main: IO[Unit] = printLine(describe(result))""".stripMargin
    ).asserting(_ shouldBe "on")
  }

  // A {State, Console} program: Console rides the `StateCarrier[S, IO]` stack via the single `Suspend[StateCarrier[S, G]]` base lift
  // (the n-not-n×m lifting), so the print runs while the state threads through and discharges to a Pair.
  "a {State, Console} program" should "run Console through the StateCarrier[String, IO] stack via the Suspend lift" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |import eliot.effect.State
        |
        |def step: {State[String], Console} String =
        |   flatMap(
        |      ignored -> flatMap(old -> flatMap(ignored2 -> pure(old), putState("done")), state),
        |      printLine("running step"))
        |
        |def main: IO[Unit] = flatMap(
        |   p -> flatMap(ignored -> printLine(second(p)), printLine(first(p))),
        |   runStateToPair("start", step))""".stripMargin
    ).asserting(_ shouldBe "running step\nstart\ndone")
  }

  // Ordering-at-the-edge (M5, the last open item): ONE carrier-polymorphic `{State[String], Abort}` program —
  // install a new state, then `abort` — discharged in the TWO possible orders, giving two genuinely different
  // results (and result *types*). The interaction "does the abort roll back the state?" is left open in the flat
  // effect set and decided only by the order the `run*` calls are nested, via the n² cross-lifting instances
  // `State[AbortCarrier[G]]` (in `AbortCarrier`) and `Abort[StateCarrier[S, G]]` (in `StateCarrier`). Run on a pure `Id` carrier.
  private val orderingPrelude =
    """import eliot.carrier.Effect
      |import eliot.effect.State
      |import eliot.effect.Abort
      |
      |data Id[A](runId: A)
      |
      |implement Effect[Id] {
      |   def pure[A](a: A): Id[A] = Id(a)
      |   def flatMap[A, B](f: Function[A, Id[B]], fa: Id[A]): Id[B] = f(runId(fa))
      |   def map[A, B](f: Function[A, B], fa: Id[A]): Id[B] = Id(f(runId(fa)))
      |}
      |
      |def modifyThenAbort: {State[String], Abort} String =
      |   flatMap(ignored -> abort, putState("modified"))
      |
      |""".stripMargin

  // Discharge Abort first (inner), State second (outer) ⟹ `Pair[Option[A], S]`: the state SURVIVES the abort, so the
  // final state is the installed "modified" even though the value aborted to None. State rides through `AbortCarrier` via
  // the `State[AbortCarrier[G]]` cross-lift.
  "ordering at the discharge edge" should "let state survive an abort when State is discharged outermost" in {
    compileAndRun(
      orderingPrelude +
        """import eliot.jvm.IO
import eliot.effect.Console
          |def stateSurvives: Pair[Option[String], String] =
          |   runId(runStateToPair("initial", runAbort(modifyThenAbort)))
          |
          |def main: IO[Unit] = flatMap(
          |   ignored -> printLine(second(stateSurvives)),
          |   printLine(foldOption(first(stateSurvives), "<no value>", s -> s)))""".stripMargin
    ).asserting(_ shouldBe "<no value>\nmodified")
  }

  // Discharge State first (inner), Abort second (outer) ⟹ `Option[Pair[A, S]]`: the abort DISCARDS the state — the
  // whole pair is torn down to None, so there is no surviving state at all. Abort rides through `StateCarrier` via the
  // `Abort[StateCarrier[S, G]]` cross-lift. The opposite result from the same program: ordering decides interaction.
  it should "discard state on an abort when Abort is discharged outermost" in {
    compileAndRun(
      orderingPrelude +
        """import eliot.jvm.IO
import eliot.effect.Console
          |def stateDiscarded: Option[Pair[String, String]] =
          |   runId(runAbort(runStateToPair("initial", modifyThenAbort)))
          |
          |def main: IO[Unit] = printLine(foldOption(stateDiscarded, "<no state>", p -> second(p)))""".stripMargin
    ).asserting(_ shouldBe "<no state>")
  }

  // --- Block syntax: `val` bindings and statement sequencing ---

  // The headline: a multi-step body written as a block instead of a hand-nested flatMap. Each bare statement is a `val`
  // with a discarded binder, so the steps are sequenced through the carrier automatically, in order.
  "a block of statements" should "sequence effectful steps in order" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |def main: IO[Unit] = {
        |  printLine("first")
        |  printLine("second")
        |  printLine("third")
        |}""".stripMargin
    ).asserting(_ shouldBe "first\nsecond\nthird")
  }

  // A `val` binds the *carried* result of an effectful step (here `readLine`), so the body sees the plain value; the
  // block lowers to `flatMap(line -> printLine(line), readLine)`.
  "a val binding an effectful result" should "bind the carried value and use it" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |def echo: {Console} Unit = {
        |  val line = readLine
        |  printLine(line)
        |}
        |
        |def main: IO[Unit] = echo""".stripMargin,
      stdin = "typed line\n"
    ).asserting(_ shouldBe "typed line")
  }

  // A non-effectful `val` binds a plain value (no carrier), used twice in the body. This is the immediately-applied
  // lambda `let` form: the block lowers to `(msg -> …)(greeting)`, with `greeting` a pure value, not a carried action.
  "a non-effectful val binding" should "bind a plain value usable multiple times" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |def greeting: String = "Hi"
        |
        |def main: IO[Unit] = {
        |  val msg = greeting
        |  printLine(msg)
        |  printLine(msg)
        |}""".stripMargin
    ).asserting(_ shouldBe "Hi\nHi")
  }

  // A pure `val` and an effectful `val` interleaved in one block: the pure binding inlines as a plain `let`, the
  // effectful one threads through `flatMap` — both in the same lowered tower.
  it should "interleave a pure binding with an effectful one" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |def main: IO[Unit] = {
        |  val label = "echo:"
        |  val line = readLine
        |  printLine(label)
        |  printLine(line)
        |}""".stripMargin,
      stdin = "hello\n"
    ).asserting(_ shouldBe "echo:\nhello")
  }

  // The docs' headline equivalence: `swap` written as a block produces the same result as the hand-written flatMap nest
  // (cf. the "State effect" test above). `val old = state` binds the carried state; `putState(next)` is a bare
  // effectful statement; `old` is the result expression.
  "a {State} computation in block form" should "thread state exactly like the hand-written flatMap nest" in {
    compileAndRun(
      """import eliot.jvm.IO
import eliot.effect.Console
        |import eliot.carrier.Effect
        |import eliot.effect.State
        |
        |def swap(next: String): {State[String]} String = {
        |  val old = state
        |  putState(next)
        |  old
        |}
        |
        |def main: IO[Unit] = flatMap(
        |   p -> flatMap(ignored -> printLine(second(p)), printLine(first(p))),
        |   runStateToPair("before", swap("after")))""".stripMargin
    ).asserting(_ shouldBe "before\nafter")
  }
}
