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
}
