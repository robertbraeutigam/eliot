package com.vanillasource.eliot.eliotc.jvm

/** End-to-end coverage of effectful-signatures **G1**: a guarded return type written with the standard guard combinator
  * vocabulary (`eliot.lang.Guard` — `error`/`when`/`orError`) over the carrier, compiled through the real pipeline with
  * the shipped `compiler`-layer `Either`/`Option` carriers + combinator bodies.
  *
  * The guard is an ordinary application the NbE checker reduces to `Right(t)` / `Left(msg)`; the discharge then accepts
  * the type or rejects with the author's message. So a satisfied guard compiles and runs as if the return were the bare
  * type, and an unsatisfied one fails the build with the author message — no auto-lift, no special guard syntax. This
  * is also the first end-to-end exercise of the compile-time `Option` carrier (`compiler/.../Option.els`), since
  * `when`/`orError` reduce through its `Some`/`None`/`foldOption`. See `docs/effectful-signatures.md` (G1).
  */
class GuardSignatureIntegrationTest extends FullIntegrationTest {

  "a satisfied `orError(when(...))` guard" should "type as its payload and run as the bare type" in {
    compileAndRun(
      """import eliot.lang.Guard
        |import eliot.lang.Bool
        |
        |def greeting[COND: Bool]: orError(when(String[], COND), "greeting unavailable") = "hello"
        |
        |def main: IO[Unit] = println(greeting[true])""".stripMargin
    ).asserting(_ shouldBe "hello")
  }

  "an unsatisfied `orError(when(...))` guard" should "fail the build with the author message" in {
    compileForErrors(
      """import eliot.lang.Guard
        |import eliot.lang.Bool
        |
        |def greeting[COND: Bool]: orError(when(String[], COND), "greeting unavailable") = "hello"
        |
        |def main: IO[Unit] = println(greeting[false])""".stripMargin
    ).asserting(_ should include("greeting unavailable"))
  }

  "a bare `error(msg)` guard" should "fail the build with the author message" in {
    compileForErrors(
      """import eliot.lang.Guard
        |
        |def unavailable: error("not available") = "x"
        |
        |def main: IO[Unit] = println(unavailable)""".stripMargin
    ).asserting(_ should include("not available"))
  }
}
