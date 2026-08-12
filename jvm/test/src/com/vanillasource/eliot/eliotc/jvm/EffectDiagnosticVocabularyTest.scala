package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.compiler.Compiler
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** The effects-as-channel §9 gate on the *diagnostic* side: an error a user reads speaks payload and effect-row
  * vocabulary, never carrier machinery. Carriers (`AbortCarrier`, `ThrowCarrier`, …) and the identity carrier `Id` are
  * compiler-internal — `Throw.els` says so in as many words ("Plumbing only — application code never names it") — so a
  * message naming them tells the user about a type no surface syntax even spells.
  *
  * Both programs here are *expected to fail*: what is asserted is how the failure reads. They are compiled over the
  * real base layer (`lang` + `stdlib` + `jvm`), because the shapes only arise once the carriers are the concrete
  * platform ones.
  */
class EffectDiagnosticVocabularyTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  /** An effect performed under a return that cannot host a carrier — the everyday "I forgot the effect set" mistake.
    * The per-definition row verification decides it from declarations and reports it at `helper`, in effect
    * vocabulary; before A.11.6 it surfaced only after a failed monomorphization, as "performs an effect but is
    * declared pure", without naming the effect.
    */
  private val pureReturnLeak =
    """def orEmpty(o: Option[String]): String = o.orAbort else ""
      |
      |def helper: String = printLine(orEmpty(readLine))
      |
      |def main: {Console} Unit = printLine(helper)
      |""".stripMargin

  /** A pure value where a pinned computation is expected (`resume`'s declared parameter is the reified
    * `{Abort | IO} String`) — a legitimate type mismatch whose `Expected:` line must render the carrier stack as its
    * pinned row, never as `AbortCarrier(...)`.
    */
  private val pinnedMismatch =
    """import eliot.jvm.IO
      |
      |def resume(c: {Abort | IO} String): {Console} Unit = printLine(c else "localhost")
      |
      |def main: {Console} Unit = resume("plain")
      |""".stripMargin

  /** A side effect reaching a computation pinned to the pure base: the `TestCase` field's row is pinned to `Id`, which
    * has no `Suspend` instance *by design*, so `printLine` cannot run there. The demand that fails is the user's own
    * `Console` at that row — the jvm instance declines for want of `Suspend` (constraint-aware declination,
    * docs/testing-effects.md L1) — and the base of the row is what earns the effect-vocabulary wording instead of
    * "No ability implementation found for ability 'Console' with type arguments [{Throw[String] | Id}]".
    */
  private val sideEffectOnPureBase =
    """import eliot.lang.Id
      |
      |data TestCase(name: String, body: {Throw[String] | Id} Unit)
      |
      |def bad: TestCase = TestCase("x", printLine("hi"))
      |
      |def main: {Console} Unit = printLine(name(bad))
      |""".stripMargin

  /** A genuinely missing cross-lift (`State` over a `Throw` layer has no instance), which is the everyday way a
    * *carrier stack* reaches an ability-demand message. Written without dot-chaining on purpose, so the failure is the
    * missing instance rather than anything the chain does to it.
    */
  private val missingCrossLift =
    """def counted: {State[String], Throw[String]} String = {
      |   putState("seen")
      |   raise("boom")
      |}
      |
      |def main: {Console} Unit =
      |   printLine(foldEither(e -> e, s -> s, runStateToValue("i", runThrow(counted))))
      |""".stripMargin

  "an ability-demand diagnostic" should "render a carrier-stack argument as one pinned row" in {
    compileErrors(missingCrossLift).asserting(_.mkString should include("{Throw[String], State[String] | IO}"))
  }

  // Regression: an ability's carrier argument is an `F[_]`, so its last argument is the base, not a payload. Read as a
  // payload-applied type it split one slot off and printed `{Throw | String} {State | String} IO` — wrong, and
  // confidently so. `GroundValue.valueType` cannot tell the two apart; only the reading context can.
  it should "not mistake the ability argument of a carrier for its base" in {
    compileErrors(missingCrossLift).asserting(_.mkString should not include "{Throw | String}")
  }

  "a type-mismatch diagnostic" should "render a carrier-headed expectation as its pinned effect row" in {
    compileErrors(pinnedMismatch).asserting(_.mkString should include("{Abort | IO} String"))
  }

  it should "name no carrier machinery" in {
    compileErrors(pinnedMismatch).asserting(_.mkString should not include "Carrier")
  }

  "an undeclared effect under a pure return" should "read as an effect leak at the definition, in row vocabulary" in {
    compileErrors(pureReturnLeak).asserting(
      _.mkString should include("performs the effect 'Console' but does not declare it")
    )
  }

  it should "name no carrier machinery either" in {
    compileErrors(pureReturnLeak).asserting(_.mkString should not include "Carrier")
  }

  "a side effect on the pure identity base" should "be explained in effect vocabulary, not as a missing instance" in {
    compileErrors(sideEffectOnPureBase).asserting(
      _.mkString should include("cannot run here, because the computation it runs in is pure")
    )
  }

  it should "not report it as an unimplemented machinery ability" in {
    compileErrors(sideEffectOnPureBase).asserting(_.mkString should not include "ability 'Suspend'")
  }

  /** Compile the program (module `Test`) over the base layer roots and return everything the user is shown for each
    * error — its message *and* its description lines, which is where the `Expected:` / `Actual:` types live. Never
    * raises on the errors themselves, since every program here is expected to fail.
    */
  private def compileErrors(source: String): IO[Seq[String]] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("eliot-diag-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("eliot-diag-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), source))
      args        = List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test") ++ layerPathArgs
      sessionOpt <- Compiler.createSession(args)
      session    <- IO.fromOption(sessionOpt)(new IllegalStateException("Could not create the compilation session."))
      result     <- session.compileOnce()
      _          <- IO.raiseWhen(result.errors.isEmpty)(new IllegalStateException("Expected the program not to compile."))
    } yield result.errors.flatMap(error => error.message +: error.description)

  private def layerPathArgs: List[String] = {
    val repoRoot             =
      Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
    def root(module: String) = repoRoot.resolve(module).resolve("eliot").toString
    List("--path", root("lang"), "--path", root("stdlib"), "--path", root("jvm"))
  }
}
