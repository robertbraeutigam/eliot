package com.vanillasource.eliot.eliotc.jvm

/** End-to-end coverage of the termination preconditions and the no-recursion rule as they reach a real compile.
  *
  * M0 (preconditions): the strict-positivity check runs in `CoreProcessor` for every loaded file; the occurs-check half
  * is exercised at the unit level (`OccursCheckTest`). Here we confirm the declared negative-recursive-datatype route is
  * rejected through the full pipeline, while covariant self-reference (structural recursion in data) still compiles.
  *
  * M1 (the no-recursion rule): a self or mutual cycle in the body-level value-reference graph is rejected (the
  * `RecursionCheckProcessor`), while a non-recursive helper chain — however deep — compiles and runs, and the
  * monad-transformer lifting pattern (an impl method calling the same-named abstract method on an inner carrier) is not
  * mistaken for recursion.
  *
  * M2 (higher-order propagation, the function-coloring piece): because `Inf` is an ordinary effect riding the carrier
  * (M1's design choice), a single effect-transparent higher-order combinator is `Inf`-iff-its-step-is — terminating
  * over a terminating step, looping over an `Inf` step — with no separate termination lattice and no change to the
  * combinator. The step's own capability effects union with `Inf` through the shared carrier, an `Inf` action survives
  * a round-trip through a data field, and the same subset check governs propagation through a higher-order driver.
  */
class TerminationIntegrationTest extends FullIntegrationTest {

  "a data type referencing itself left of an arrow" should "be rejected as not strictly positive" in {
    compileForErrors(
      """import eliot.effect.Console
        |data Loop(f: Function[Loop, String])
        |
        |def main: IO[Unit] = println("unreachable")""".stripMargin
    ).asserting(_ should include("contravariant position"))
  }

  "a data type referencing itself covariantly (structural recursion)" should "compile and run" in {
    compileAndRun(
      """import eliot.effect.Console
        |data Tree(left: Tree, right: Tree)
        |
        |def main: IO[Unit] = println("ok")""".stripMargin
    ).asserting(_ shouldBe "ok")
  }

  "a directly self-recursive value" should "be rejected as recursion" in {
    compileForErrors(
      """import eliot.effect.Console
        |def loop(x: String): String = loop(x)
        |
        |def main: IO[Unit] = println(loop("unreachable"))""".stripMargin
    ).asserting(_ should include("recursively"))
  }

  "a mutually-recursive pair of values" should "be rejected as recursion" in {
    compileForErrors(
      """import eliot.effect.Console
        |def ping(x: String): String = pong(x)
        |def pong(x: String): String = ping(x)
        |
        |def main: IO[Unit] = println(ping("unreachable"))""".stripMargin
    ).asserting(_ should include("recursively"))
  }

  "a deep non-recursive helper chain" should "compile and run" in {
    compileAndRun(
      """import eliot.effect.Console
        |def a(x: String): String = b(x)
        |def b(x: String): String = c(x)
        |def c(x: String): String = x
        |
        |def main: IO[Unit] = println(a("ok"))""".stripMargin
    ).asserting(_ shouldBe "ok")
  }

  // --- M1 (the `Inf` effect): declare, propagate, run ---

  // Propagation: calling `forever` performs the `Inf` effect, so a carrier-polymorphic value that uses it must declare
  // `{Inf}`. A `{Console}`-only value that loops is rejected by the same subset check that governs every effect — `Inf`
  // rides the ordinary effect pipeline, it is not a special termination lattice.
  "a {Console} value that calls forever without declaring Inf" should "be rejected (Inf propagation)" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.effect.Inf
        |
        |def bad: {Console} Unit = forever(println("x"))
        |
        |def main: IO[Unit] = bad""".stripMargin
    ).asserting(_ should include("performs the effect 'Inf'"))
  }

  // Run, don't discharge: an `{Inf}` program is realised on the `IO` carrier and run forever. `main : IO[Unit]` drives
  // `forever`, whose JVM instance loops the step's deferred block endlessly — the loop never returns, so the test bounds
  // it and confirms the step ran many times (not just once).
  "an IO main built from forever over a terminating step" should "run the step endlessly" in {
    compileAndRunBounded(
      """import eliot.effect.Console
        |import eliot.effect.Inf
        |
        |def main: IO[Unit] = forever(println("tick"))""".stripMargin,
      timeoutMillis = 400
    ).asserting(_.linesIterator.count(_ == "tick") should be > 5)
  }

  // The same loop reached through a carrier-polymorphic `{Inf, Console}` value pinned to `IO` at `main`: the declared
  // effect set resolves to the concrete `IO` carrier (the `Inf[IO]` and `Console[IO]` instances) and runs end-to-end.
  "a carrier-polymorphic {Inf, Console} super-loop pinned to IO at main" should "run endlessly" in {
    compileAndRunBounded(
      """import eliot.effect.Console
        |import eliot.effect.Inf
        |
        |def serve: {Inf, Console} Unit = forever(println("serving"))
        |
        |def main: IO[Unit] = serve""".stripMargin,
      timeoutMillis = 400
    ).asserting(_.linesIterator.count(_ == "serving") should be > 5)
  }

  // --- M2 (higher-order propagation: the function-coloring piece) ---

  // Function-coloring, terminating side. The effect-transparent combinator `runStep` declares no effect of its own; its
  // result effect is exactly its step's. Over a terminating step it terminates with finite output. The combinator's
  // definition is byte-for-byte the one used in the `Inf` case below — only the supplied step differs.
  "a higher-order combinator over a terminating step" should "itself terminate" in {
    compileAndRun(
      """import eliot.effect.Console
        |def runStep[F[_]](step: Function[Unit, F[Unit]]): F[Unit] = step(unit)
        |
        |def main: IO[Unit] = runStep(_ -> println("done"))""".stripMargin
    ).asserting(_ shouldBe "done")
  }

  // Function-coloring, Inf side. The *same* `runStep` definition over an `Inf` step loops endlessly: the step's `Inf`
  // reaches the result through the shared carrier with no change to the combinator — one combinator serves both colours
  // (Nystrom's function-coloring win), because `Inf` is a carrier effect, not a separate lattice slot.
  "the same higher-order combinator over an Inf step" should "loop endlessly" in {
    compileAndRunBounded(
      """import eliot.effect.Console
        |import eliot.effect.Inf
        |
        |def runStep[F[_]](step: Function[Unit, F[Unit]]): F[Unit] = step(unit)
        |
        |def main: IO[Unit] = runStep(_ -> forever(println("loop")))""".stripMargin,
      timeoutMillis = 400
    ).asserting(_.linesIterator.count(_ == "loop") should be > 5)
  }

  // An `Inf` action stored in a data structure, pulled back out through its field accessor and run, carries its `Inf`
  // to the caller: the effect rides the carrier `F` of `Box[F]`, so the stored action loops when run — data is only a
  // courier for the carrier-typed value, it does not launder the effect.
  "an Inf action stored in data then run through its accessor" should "loop endlessly" in {
    compileAndRunBounded(
      """import eliot.effect.Console
        |import eliot.effect.Inf
        |
        |data Box[F[_]](action: F[Unit])
        |
        |def runBox[F[_]](b: Box[F]): F[Unit] = action(b)
        |
        |def main: IO[Unit] = runBox(Box(forever(println("boxed"))))""".stripMargin,
      timeoutMillis = 400
    ).asserting(_.linesIterator.count(_ == "boxed") should be > 5)
  }

  // The step's own capability effect rides the same carrier as the driver's `Inf`: a `{Console}` step run by an
  // `{Inf, Console}` driver unions both effects (the `{e}`-on-the-step polymorphism the M1 deviation deferred to M2)
  // and runs end-to-end.
  "an {Inf, Console} driver over a {Console} step" should "union both effects and loop endlessly" in {
    compileAndRunBounded(
      """import eliot.effect.Console
        |import eliot.effect.Inf
        |
        |def driver(step: {Console} Unit): {Inf, Console} Unit = forever(step)
        |
        |def main: IO[Unit] = driver(println("tick"))""".stripMargin,
      timeoutMillis = 400
    ).asserting(_.linesIterator.count(_ == "tick") should be > 5)
  }

  // Propagation is the same used-subset-of-declared check through a higher-order driver: the driver above declaring only
  // `{Console}` (omitting `Inf`) is rejected — calling `forever` performs `Inf`, which must be declared.
  "a driver that calls forever while declaring only {Console}" should "be rejected (Inf not declared)" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.effect.Inf
        |
        |def driver(step: {Console} Unit): {Console} Unit = forever(step)
        |
        |def main: IO[Unit] = driver(println("tick"))""".stripMargin
    ).asserting(_ should include("performs the effect 'Inf'"))
  }
}
