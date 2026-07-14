package com.vanillasource.eliot.eliotc.jvm

/** Goal fixtures for the **type-levels-as-values** plan (docs/type-levels-as-values.md), Step 0.
  *
  * These pin the intended end state of Step B: an effectful *return type* written directly as `if(COND, T) else
  * raise(msg)` (or a bare `raise(msg)`, or a by-name helper that reduces to one) is a **type-level named value** — a
  * level-1 body compiled on the compiler track by the same pipeline as any runtime value. A satisfied guard types as
  * its payload and the program runs; an unsatisfied one fails the build with the author's message.
  *
  * Every case is `ignore`d until Step B lands the fact-mode read of the level-1 value (§2.1 "fact" access mode). They
  * are committed now — *before* the mechanism exists — because every prior attempt (docs/return-position-unification.md
  * §4) stayed nominally "green" while drifting precisely because the goal was never encoded as a test. Un-`ignore` these
  * as Step B flips them.
  *
  * The suite deliberately supersedes the combinator-vocabulary [[GuardSignatureIntegrationTest]] (`when`/`orError`),
  * which the Cleanup step deletes: the same guarantees, expressed in the `if..else..raise` surface that needs no
  * `eliot.lang.Guard` module.
  */
class TypeLevelReturnIntegrationTest extends FullIntegrationTest {

  // --- The two-arm `if(COND, T) else raise(msg)` return guard ---

  "a satisfied `if(COND, T) else raise` return guard" should "type as its payload and run as the bare type" ignore {
    compileAndRun(
      """import eliot.effect.Console
        |import eliot.effect.Abort
        |import eliot.effect.Throw
        |import eliot.lang.Bool
        |
        |def greeting[COND: Bool]: if(COND, String[]) else raise("greeting unavailable") = "hello"
        |
        |def main: IO[Unit] = printLine(greeting[true])""".stripMargin
    ).asserting(_ shouldBe "hello")
  }

  "an unsatisfied `if(COND, T) else raise` return guard" should "fail the build with the author message" ignore {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.effect.Abort
        |import eliot.effect.Throw
        |import eliot.lang.Bool
        |
        |def greeting[COND: Bool]: if(COND, String[]) else raise("greeting unavailable") = "hello"
        |
        |def main: IO[Unit] = printLine(greeting[false])""".stripMargin
    ).asserting(_ should include("greeting unavailable"))
  }

  // --- The bare unconditional `raise(msg)` rejection ---

  "a bare `raise(msg)` return guard" should "fail the build with the author message" ignore {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.effect.Throw
        |
        |def unavailable: raise("not available") = "x"
        |
        |def main: IO[Unit] = printLine(unavailable)""".stripMargin
    ).asserting(_ should include("not available"))
  }

  // --- Helper-guard-by-name: a user `def` returning `{Throw[String]} Type`, referenced *by name* in a return slot.
  // The reference `nonEmpty[A, N]` is syntactically indistinguishable from a plain type application — this kills any
  // temptation to recognise the guard by scanning syntax; it must be resolved as an ordinary level-1 named value.

  private val byNameHelper: String =
    """import eliot.effect.Console
      |import eliot.effect.Abort
      |import eliot.effect.Throw
      |import eliot.lang.Bool
      |import eliot.lang.Compare
      |
      |def nonEmpty[A: Type, N: BigInteger]: {Throw[String]} Type = if(N > 0, A) else raise("must be non-empty")
      |
      |def valueOf[A: Type, N: BigInteger](a: A): nonEmpty[A, N] = a
      |""".stripMargin

  "a satisfied by-name helper guard in a return slot" should "type as its payload and run" ignore {
    compileAndRun(byNameHelper + "\ndef main: IO[Unit] = printLine(valueOf[String[], 3](\"present\"))")
      .asserting(_ shouldBe "present")
  }

  "an unsatisfied by-name helper guard in a return slot" should "fail the build with the author message" ignore {
    compileForErrors(byNameHelper + "\ndef main: IO[Unit] = printLine(valueOf[String[], 0](\"present\"))")
      .asserting(_ should include("must be non-empty"))
  }

  // --- The §2.1 soundness pin: body authority and payload authority are OPPOSITE and must never merge. A body whose
  // type (`Int`, from `5`) disagrees with the guard's accepted payload (`String`) MUST fail the build. The first guard
  // attempt merged the two access modes and silently typed `= 5` as `Int`; this fixture makes that regression loud.

  "a return guard whose body disagrees with the accepted payload" should "fail the build (never silently accept)" ignore {
    compileForErrors(
      """import eliot.effect.Console
        |import eliot.effect.Abort
        |import eliot.effect.Throw
        |import eliot.lang.Bool
        |
        |def g[COND: Bool]: if(COND, String[]) else raise("unavailable") = 5
        |
        |def main: IO[Unit] = printLine(g[true])""".stripMargin
    ).asserting(_ should not be empty)
  }
}
