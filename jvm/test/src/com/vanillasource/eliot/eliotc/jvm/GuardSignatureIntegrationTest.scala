package com.vanillasource.eliot.eliotc.jvm

/** End-to-end coverage of effectful-signatures **G1**: a guarded return type written with the standard guard combinator
  * vocabulary (`eliot.lang.Guard` — `when`/`orError`, plus a bare `raise` for an unconditional rejection) over the
  * carrier, compiled through the real pipeline with the shipped `compiler`-layer `Either`/`Option` carriers + combinator
  * bodies.
  *
  * The combinators carry the `{Throw[String]}` **effect sugar** signature (`orError` returns `{Throw[String]} A`):
  * their bodies express rejection as `raise` and acceptance as `pure`, which the compiler backend reduces to `Left(msg)`
  * / `Right(t)`. The guard is then an ordinary application the NbE checker reduces to `Right(t)` / `Left(msg)`, and the
  * discharge accepts the type or rejects with the author's message. So a satisfied guard compiles and runs as if the
  * return were the bare type, and an unsatisfied one fails the build with the author message.
  *
  * The checker's effect lift *does* elaborate the combinator bodies, but the lift is correct here because
  * `foldOption`'s `ifNone: B` is a flex eliminator slot the lift defers and then passes through (the effectful branch
  * value rides the eliminator's own return; see docs/effect-lift-in-checker.md). (Sequencing it would collapse
  * every guard to an unconditional `Left`; that is the regression this suite guards against.) This is also the first
  * end-to-end exercise of the compile-time `Option` carrier (`stdlib/eliot-compiler/.../Option.els`), since `when`/`orError` reduce
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
        |def main: IO[Unit] = printLine(greeting[true])""".stripMargin
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
        |def main: IO[Unit] = printLine(greeting[false])""".stripMargin
    ).asserting(_ should include("greeting unavailable"))
  }

  "a bare `raise(msg)` guard" should "fail the build with the author message" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.effect.Throw
        |
        |def unavailable: raise("not available") = "x"
        |
        |def main: IO[Unit] = printLine(unavailable)""".stripMargin
    ).asserting(_ should include("not available"))
  }

  // The *inline* `if(cond) T else raise(msg)` guard (signature split, Step 7): unlike `orError` (a precomputed nullary
  // native), the guard is spelled with the carrier-generic `if`/`else` over a **stacked** carrier (`AbortCarrier` over
  // the `Throw[String]` base `Either[String]`). The signature twin's compiler mono reduces it — resolving each layer's
  // abilities recursively and reducing the `match` in the deep body pipeline — to the same `Right(t)`/`Left(msg)`
  // verdict the combinator form produces, which the consumer's twin read discharges.

  "a satisfied inline `if..else..raise` guard" should "type as its payload and run as the bare type" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Throw
        |import eliot.effect.Abort
        |import eliot.lang.Bool
        |
        |def greeting[COND: Bool]: if(COND, String[]) else raise("greeting unavailable") = "hello"
        |
        |def main: IO[Unit] = printLine(greeting[true])""".stripMargin
    ).asserting(_ shouldBe "hello")
  }

  "an unsatisfied inline `if..else..raise` guard" should "fail the build with the author message" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.effect.Throw
        |import eliot.effect.Abort
        |import eliot.lang.Bool
        |
        |def greeting[COND: Bool]: if(COND, String[]) else raise("greeting unavailable") = "hello"
        |
        |def main: IO[Unit] = printLine(greeting[false])""".stripMargin
    ).asserting(_ should include("greeting unavailable"))
  }

  // A compile-time guard written through a *user-defined* pipe and a user guard combinator (effect-lift Step 5): the
  // pipe and `guardOr` are ordinary user source, compiled on the compiler track like any other value; the checker's
  // effect lift elaborates their `{Throw[String]}` path per instantiation and the signature still reduces to
  // `Right`/`Left` for the discharge. Proves the compile-time reduction is not special-cased to the stdlib combinators.
  private val pipedGuard: String =
    """import eliot.effect.Console
      |import eliot.lang.Guard
      |import eliot.lang.Bool
      |import eliot.lang.Option
      |import eliot.effect.Throw
      |
      |infix left below apply def |>[A, B](a: A, f: A => B): B = f(a)
      |
      |def guardOr[A](msg: String, o: Option[A]): {Throw[String]} A = orError(o, msg)
      |
      |def greeting[COND: Bool]: when(String[], COND) |> guardOr("greeting unavailable") = "hello"
      |""".stripMargin

  "a satisfied guard written through a user pipe" should "type as its payload and run as the bare type" in {
    compileAndRun(pipedGuard + "\ndef main: IO[Unit] = printLine(greeting[true])")
      .asserting(_ shouldBe "hello")
  }

  "an unsatisfied guard written through a user pipe" should "fail the build with the author message" in {
    compileForErrors(pipedGuard + "\ndef main: IO[Unit] = printLine(greeting[false])")
      .asserting(_ should include("greeting unavailable"))
  }

  // --- G2: the infix guard surface + the compile-time `>` comparison ---
  //
  // G2 lets the same guard read with the designed infix syntax — `A when (cond) orError "…"` — instead of the
  // application form above. The return-type parser admits a flat infix expression and the operator phase lowers it to
  // `orError(when(A, cond), "…")`, so it discharges identically. The `>` operand exercises the compile-time integer
  // comparison (`eliot.lang.BigInteger.>`, reduced through the existing `lessThanOrEqual` native). (G2)

  "a satisfied infix `when … orError` guard" should "type as its payload and run as the bare type" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Guard
        |import eliot.lang.Bool
        |
        |def greeting[COND: Bool]: String[] when (COND) orError "greeting unavailable" = "hello"
        |
        |def main: IO[Unit] = printLine(greeting[true])""".stripMargin
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
        |def main: IO[Unit] = printLine(greeting[false])""".stripMargin
    ).asserting(_ should include("greeting unavailable"))
  }

  // `>`/`<` are `Compare` comparison operators (`eliot.lang.Compare`), so each guard module imports `Compare`. `BigInteger`
  // itself stays ambiently imported; only the ordering surface moved to the ability.
  "a satisfied `MIN > 0` infix guard" should "type as its payload and run as the bare type" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Guard
        |import eliot.lang.Compare
        |
        |def positive[MIN: BigInteger]: String[] when (MIN > 0) orError "must be positive" = "ok"
        |
        |def main: IO[Unit] = printLine(positive[5])""".stripMargin
    ).asserting(_ shouldBe "ok")
  }

  "an unsatisfied `MIN > 0` infix guard" should "fail the build with the author message" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.lang.Guard
        |import eliot.lang.Compare
        |
        |def positive[MIN: BigInteger]: String[] when (MIN > 0) orError "must be positive" = "ok"
        |
        |def main: IO[Unit] = printLine(positive[0])""".stripMargin
    ).asserting(_ should include("must be positive"))
  }

  "a satisfied `N < 10` infix guard" should "type as its payload and run as the bare type" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.lang.Guard
        |import eliot.lang.Compare
        |
        |def small[N: BigInteger]: String[] when (N < 10) orError "too big" = "ok"
        |
        |def main: IO[Unit] = printLine(small[3])""".stripMargin
    ).asserting(_ shouldBe "ok")
  }
}
