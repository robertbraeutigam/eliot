package com.vanillasource.eliot.eliotc.jvm

/** End-to-end coverage of effectful-signatures **G1**: a guarded return type written with the standard guard combinator
  * vocabulary (`eliot.lang.Guard` — `error`/`when`/`orError`) over the carrier, compiled through the real pipeline with
  * the shipped `compiler`-layer `Either`/`Option` carriers + combinator bodies.
  *
  * The combinators carry the `{Throw[String]}` **effect sugar** signature (`error`/`orError` return `{Throw[String]} A`):
  * their bodies express rejection as `raise` and acceptance as `pure`, which the compiler backend reduces to `Left(msg)`
  * / `Right(t)`. The guard is then an ordinary application the NbE checker reduces to `Right(t)` / `Left(msg)`, and the
  * discharge accepts the type or rejects with the author's message. So a satisfied guard compiles and runs as if the
  * return were the bare type, and an unsatisfied one fails the build with the author message.
  *
  * The effect sugar *does* auto-lift the combinator bodies, but the lift is correct here because `foldOption`'s
  * `ifNone: B` is an eliminator *branch* position (its type is the callee's return type), which the direct-style
  * desugarer passes through instead of sequencing — see `CalleeInfo.isBranchPosition`. (Sequencing it would collapse
  * every guard to an unconditional `Left`; that is the regression this suite guards against.) This is also the first
  * end-to-end exercise of the compile-time `Option` carrier (`compiler/.../Option.els`), since `when`/`orError` reduce
  * through its `Some`/`None`/`foldOption`.
  */
class GuardSignatureIntegrationTest extends FullIntegrationTest {

  "a satisfied `orError(when(...))` guard" should "type as its payload and run as the bare type" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Guard
        |import eliot.lang.Bool
        |
        |def greeting[COND: Bool]: orError(when(String[], COND), "greeting unavailable") = "hello"
        |
        |def main: IO[Unit] = println(greeting[true])""".stripMargin
    ).asserting(_ shouldBe "hello")
  }

  "an unsatisfied `orError(when(...))` guard" should "fail the build with the author message" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.lang.Guard
        |import eliot.lang.Bool
        |
        |def greeting[COND: Bool]: orError(when(String[], COND), "greeting unavailable") = "hello"
        |
        |def main: IO[Unit] = println(greeting[false])""".stripMargin
    ).asserting(_ should include("greeting unavailable"))
  }

  "a bare `error(msg)` guard" should "fail the build with the author message" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.lang.Guard
        |
        |def unavailable: error("not available") = "x"
        |
        |def main: IO[Unit] = println(unavailable)""".stripMargin
    ).asserting(_ should include("not available"))
  }

  // --- G2: the infix guard surface + the compile-time `>` comparison ---
  //
  // G2 lets the same guard read with the designed infix syntax — `A when (cond) orError "…"` — instead of the
  // application form above. The return-type parser admits a flat infix expression and the operator phase lowers it to
  // `orError(when(A, cond), "…")`, so it discharges identically. The `>` operand exercises the compile-time integer
  // comparison (`eliot.lang.BigInteger.>`, reduced through the existing `inc`/`lessThanOrEqual` natives). (G2)

  "a satisfied infix `when … orError` guard" should "type as its payload and run as the bare type" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Guard
        |import eliot.lang.Bool
        |
        |def greeting[COND: Bool]: String[] when (COND) orError "greeting unavailable" = "hello"
        |
        |def main: IO[Unit] = println(greeting[true])""".stripMargin
    ).asserting(_ shouldBe "hello")
  }

  "an unsatisfied infix `when … orError` guard" should "fail the build with the author message" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.lang.Guard
        |import eliot.lang.Bool
        |
        |def greeting[COND: Bool]: String[] when (COND) orError "greeting unavailable" = "hello"
        |
        |def main: IO[Unit] = println(greeting[false])""".stripMargin
    ).asserting(_ should include("greeting unavailable"))
  }

  // `BigInteger` (and hence its `>` operator) is ambiently imported into every module, so no explicit import is needed.
  "a satisfied `MIN > 0` infix guard" should "type as its payload and run as the bare type" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Guard
        |
        |def positive[MIN: BigInteger]: String[] when (MIN > 0) orError "must be positive" = "ok"
        |
        |def main: IO[Unit] = println(positive[5])""".stripMargin
    ).asserting(_ shouldBe "ok")
  }

  "an unsatisfied `MIN > 0` infix guard" should "fail the build with the author message" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.lang.Guard
        |
        |def positive[MIN: BigInteger]: String[] when (MIN > 0) orError "must be positive" = "ok"
        |
        |def main: IO[Unit] = println(positive[0])""".stripMargin
    ).asserting(_ should include("must be positive"))
  }

  "a satisfied `N < 10` infix guard" should "type as its payload and run as the bare type" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Guard
        |
        |def small[N: BigInteger]: String[] when (N < 10) orError "too big" = "ok"
        |
        |def main: IO[Unit] = println(small[3])""".stripMargin
    ).asserting(_ shouldBe "ok")
  }
}
