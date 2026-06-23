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
  */
class TerminationIntegrationTest extends FullIntegrationTest {

  "a data type referencing itself left of an arrow" should "be rejected as not strictly positive" in {
    compileForErrors(
      """data Loop(f: Function[Loop, String])
        |
        |def main: IO[Unit] = println("unreachable")""".stripMargin
    ).asserting(_ should include("contravariant position"))
  }

  "a data type referencing itself covariantly (structural recursion)" should "compile and run" in {
    compileAndRun(
      """data Tree(left: Tree, right: Tree)
        |
        |def main: IO[Unit] = println("ok")""".stripMargin
    ).asserting(_ shouldBe "ok")
  }

  "a directly self-recursive value" should "be rejected as recursion" in {
    compileForErrors(
      """def loop(x: String): String = loop(x)
        |
        |def main: IO[Unit] = println(loop("unreachable"))""".stripMargin
    ).asserting(_ should include("recursively"))
  }

  "a mutually-recursive pair of values" should "be rejected as recursion" in {
    compileForErrors(
      """def ping(x: String): String = pong(x)
        |def pong(x: String): String = ping(x)
        |
        |def main: IO[Unit] = println(ping("unreachable"))""".stripMargin
    ).asserting(_ should include("recursively"))
  }

  "a deep non-recursive helper chain" should "compile and run" in {
    compileAndRun(
      """def a(x: String): String = b(x)
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
      """import eliot.lang.Inf
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
      """import eliot.lang.Inf
        |
        |def main: IO[Unit] = forever(println("tick"))""".stripMargin,
      timeoutMillis = 400
    ).asserting(_.linesIterator.count(_ == "tick") should be > 5)
  }

  // The same loop reached through a carrier-polymorphic `{Inf, Console}` value pinned to `IO` at `main`: the declared
  // effect set resolves to the concrete `IO` carrier (the `Inf[IO]` and `Console[IO]` instances) and runs end-to-end.
  "a carrier-polymorphic {Inf, Console} super-loop pinned to IO at main" should "run endlessly" in {
    compileAndRunBounded(
      """import eliot.lang.Inf
        |
        |def serve: {Inf, Console} Unit = forever(println("serving"))
        |
        |def main: IO[Unit] = serve""".stripMargin,
      timeoutMillis = 400
    ).asserting(_.linesIterator.count(_ == "serving") should be > 5)
  }
}
