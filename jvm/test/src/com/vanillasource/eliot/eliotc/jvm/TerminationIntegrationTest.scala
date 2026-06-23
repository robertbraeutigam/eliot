package com.vanillasource.eliot.eliotc.jvm

/** End-to-end coverage of the M0 termination preconditions as they reach a real compile (the strict-positivity check
  * runs in `CoreProcessor` for every loaded file). The occurs-check half of M0 is exercised directly at the unit level
  * (`OccursCheckTest`); here we confirm the declared negative-recursive-datatype route is rejected through the full
  * pipeline, while covariant self-reference (structural recursion in data) still compiles and runs.
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
}
