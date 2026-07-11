package com.vanillasource.eliot.eliotc.jvm

/** Regression for the `integerLiteral` evaluator reduction: a value-position literal (desugared to `integerLiteral[V]`)
  * consumed by a compile-time computation — here a `where` guard bound — must reduce to its `BigInteger` value `V` so
  * the guard's `fitsIn`/`&&` can fire. Before the fix `integerLiteral` had no evaluator reduction (a quote-time rewrite
  * only), so `myLo`/`myHi` stayed stuck and the guard errored with "Cannot quote stuck native application Bool::&&".
  * Self-contained: the constants are user-defined, so this exercises the fix without any layer edit. */
class GuardLiteralReductionTest extends FullIntegrationTest {
  "a user constant from a value-position literal in a where guard" should "reduce and discharge the guard" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Bool
        |def myLo: BigInteger = 0
        |def myHi: BigInteger = 100
        |ability InRange[N: BigInteger] { def keep(x: Int): Int }
        |implement[N: BigInteger] InRange[N] where fitsIn[myLo, myHi, N, N] { def keep(x: Int): Int = x }
        |def use: Int = keep[42](7)
        |def main: IO[Unit] = printLine(intToString(use))""".stripMargin
    ).asserting(_ shouldBe "7")
  }
}
