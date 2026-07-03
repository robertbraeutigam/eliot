package com.vanillasource.eliot.eliotc.jvm

/** End-to-end regression for the refinement reconciliation
  * ([[com.vanillasource.eliot.eliotc.monomorphize.refine.RefinementSolver.reconcileRefinements]]): a coercion that a
  * post-drain resolution — a `Combine` join or a deferred upper bound — only *verified* used to reach the backend
  * without its `nativeWiden` payload, so the value arrived at the contributor's own machine representation and the
  * generated class failed verification (`VerifyError`: a boxed `Short` on the stack where the join's `Integer` is
  * expected). These programs must not only compile but run.
  */
class RefinementReconciliationIntegrationTest extends FullIntegrationTest {

  "a Combine-joined generic slot" should "run with each contributor widened to the join" in {
    compileAndRun(
      """import eliot.effect.Console
        |
        |def pick[A](a: A, b: A): A = a
        |
        |def main: IO[Unit] = printLine(intToString(pick(3, 700)))""".stripMargin
    ).asserting(_ shouldBe "3")
  }

  "a deferred upper bound wider than the join" should "run with the join widened to the declared range" in {
    compileAndRun(
      """import eliot.effect.Console
        |
        |def pick[A](a: A, b: A): A = b
        |
        |def wide: Int[0, 100000] = pick(3, 700)
        |
        |def main: IO[Unit] = printLine(intToString(wide))""".stripMargin
    ).asserting(_ shouldBe "700")
  }

  "an integer literal as an effect-discharge fallback" should "run widened into the discharged slot" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.State
        |import eliot.effect.Abort
        |import eliot.lang.Pair
        |
        |def parsePort(raw: String): {Abort} UnsignedShort = abort
        |
        |def nextPort: {Console, State[String], Abort} UnsignedShort = {
        |  val raw = readLine
        |  putState(raw)
        |  parsePort(raw)
        |}
        |
        |def main: IO[Unit] = {
        |  val result = runStateToPair(nextPort orElse 8080, "<none>")
        |  printLine(intToString(result.first))
        |}""".stripMargin,
      stdin = "not-a-number\n"
    ).asserting(_ shouldBe "8080")
  }
}
