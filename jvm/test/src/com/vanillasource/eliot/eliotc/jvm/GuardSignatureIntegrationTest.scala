package com.vanillasource.eliot.eliotc.jvm

/** End-to-end coverage of effectful-signatures **G1**: a guarded return type — a return whose *type* is a compile-time
  * computation that may reject — written with the **inline** guard vocabulary `if(cond, T) else raise(msg)` (plus a bare
  * `raise(msg)` for an unconditional rejection) over the effect carrier, compiled through the real pipeline with the
  * shipped `compiler`-layer `Either`/`Option`/`Abort` carriers.
  *
  * The guard rides the `{Throw[String]}`/`{Abort}` effect: `raise` expresses rejection and the pure arm expresses
  * acceptance, which the signature twin's compiler monomorphization reduces to `Left(msg)` / `Right(t)`. The consumer's
  * twin read then discharges — accepting the payload type or rejecting with the author's message. So a satisfied guard
  * compiles and runs as if the return were the bare type, and an unsatisfied one fails the build with the author
  * message. (This replaces the retired `eliot.lang.Guard` `when`/`orError` combinator surface — the signature split
  * Step 10; the inline form reduces through the same `if`/`else`/`fold`/`raise` machinery.)
  *
  * The checker's effect lift *does* elaborate the guard's branches, but the pure arm rides `if`'s own `{Abort}` return
  * and the `else` discharger consumes it, so a satisfied guard collapses to `Right(t)` rather than an unconditional
  * `Left`; that regression is what this suite guards against.
  */
class GuardSignatureIntegrationTest extends FullIntegrationTest {

  "a satisfied inline `if..else..raise` guard" should "type as its payload and run as the bare type" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Throw
        |import eliot.effect.Abort
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
        |
        |def greeting[COND: Bool]: if(COND, String[]) else raise("greeting unavailable") = "hello"
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

  // A compile-time guard written through a *user-defined* pipe and a user guard combinator (effect-lift Step 5): the
  // pipe and `guardOr` are ordinary user source, compiled on the compiler track like any other value; the checker's
  // effect lift elaborates their `{Throw[String]}` path per instantiation and the signature still reduces to
  // `Right`/`Left` for the discharge. Proves the compile-time reduction is not special-cased to the `if`/`else` shape.
  private val pipedGuard: String =
    """import eliot.effect.Console
      |import eliot.effect.Throw
      |import eliot.effect.Abort
      |
      |infix left below apply def |>[A, B](a: A, f: A => B): B = f(a)
      |
      |def guardOr[A](cond: Bool, value: A): {Throw[String]} A = if(cond, value) else raise("greeting unavailable")
      |
      |def greeting[COND: Bool]: String[] |> guardOr(COND) = "hello"
      |""".stripMargin

  "a satisfied guard written through a user pipe" should "type as its payload and run as the bare type" in {
    compileAndRun(pipedGuard + "\ndef main: IO[Unit] = printLine(greeting[true])")
      .asserting(_ shouldBe "hello")
  }

  "an unsatisfied guard written through a user pipe" should "fail the build with the author message" in {
    compileForErrors(pipedGuard + "\ndef main: IO[Unit] = printLine(greeting[false])")
      .asserting(_ should include("greeting unavailable"))
  }

  // --- G2: the compile-time integer comparison inside an inline guard ---
  //
  // A guard whose condition compares a `BigInteger` type parameter against a *literal* (`MIN > 0`, `N < 10`) exercises
  // both the compile-time comparison (`eliot.lang.Compare`, reduced through the existing `lessThanOrEqual` native) and
  // the signature-literal handling: an integer literal in a signature is a compile-time `BigInteger`, so it unifies
  // with `MIN`/`N` rather than defaulting to a runtime `Int` (the signature split Step 10 fix in
  // `CoreExpressionConverter`). `BigInteger` stays ambiently imported; only the ordering surface needs `Compare`.

  "a satisfied `MIN > 0` inline guard" should "type as its payload and run as the bare type" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Throw
        |import eliot.effect.Abort
        |
        |def positive[MIN: BigInteger]: if(MIN > 0, String[]) else raise("must be positive") = "ok"
        |
        |def main: IO[Unit] = printLine(positive[5])""".stripMargin
    ).asserting(_ shouldBe "ok")
  }

  "an unsatisfied `MIN > 0` inline guard" should "fail the build with the author message" in {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.effect.Throw
        |import eliot.effect.Abort
        |
        |def positive[MIN: BigInteger]: if(MIN > 0, String[]) else raise("must be positive") = "ok"
        |
        |def main: IO[Unit] = printLine(positive[0])""".stripMargin
    ).asserting(_ should include("must be positive"))
  }

  "a satisfied `N < 10` inline guard" should "type as its payload and run as the bare type" in {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Throw
        |import eliot.effect.Abort
        |
        |def small[N: BigInteger]: if(N < 10, String[]) else raise("too big") = "ok"
        |
        |def main: IO[Unit] = printLine(small[3])""".stripMargin
    ).asserting(_ shouldBe "ok")
  }
}
