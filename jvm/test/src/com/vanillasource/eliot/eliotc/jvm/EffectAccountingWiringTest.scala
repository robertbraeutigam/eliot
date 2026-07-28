package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.compiler.Compiler
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** Effects-as-channel U4-c-1 (docs/effects-as-channel.md §5/§10): effect accounting is wired as a **codegen precondition**
  * — `WovenValueProcessor` demands `EffectAccounting` via `getFactOrAbort`, so a value with an undeclared effect fails
  * accounting and its abort blocks codegen. Accounting is the sole, unconditional verifier (U4-c-2), so this must:
  *   - **not block a valid effectful program** — accounting must not over-count, or it would redden valid code (the parity
  *     direction; the whole base a program pulls in is exercised, and a spurious over-count anywhere surfaces as an error
  *     here);
  *   - **reject a leak** — an undeclared effect must redden, both the ordinary Console case and the `Inf` concrete-carrier
  *     impl case (`forever` from `implement Inf[IO]`, whose carrier is counted via its forwarded ambient).
  *
  * Codegen for valid programs is byte-identical whether or not accounting runs — covered by
  * `EffectShapeCompileTest`.
  */
class EffectAccountingWiringTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  // A valid effectful program: `catch` discharges `{Throw[String]}` so the captured `parseBad`/`parseOk` do not leak
  // Throw into `main`, which performs only Console. Accounting must accept it.
  private val validSource =
    """def parseOk: {Throw[String]} String = "ok"
      |def parseBad: {Throw[String]} String = raise("bad")
      |def main: {Console} Unit = {
      |   printLine(parseOk catch (err -> err))
      |   printLine(parseBad catch (err -> err))
      |}
      |""".stripMargin

  // `main` performs Inf (via `forever`) and Console (via `printLine`) but declares only Console ⇒ undeclared Inf.
  private val undeclaredInfSource =
    """def main: {Console} Unit = forever(printLine("tick"))
      |""".stripMargin

  // A `{Inf}`-declared value performs Console (via `printLine`) without declaring it ⇒ undeclared Console.
  private val undeclaredConsoleSource =
    """def spin: {Inf} Unit = forever(printLine("tick"))
      |def main: {Inf} Unit = spin
      |""".stripMargin

  "the wired effect accounting" should "not block a valid effectful program (parity)" in {
    compileErrors(validSource).asserting(_ shouldBe empty)
  }

  it should "reject a value that performs an undeclared Console effect" in {
    compileErrors(undeclaredConsoleSource).asserting(_.exists(_.contains("performs the effect 'Console' but does not declare")) shouldBe true)
  }

  it should "reject an undeclared Inf reaching a Console-only value (the concrete-carrier impl arm)" in {
    compileErrors(undeclaredInfSource).asserting(_.exists(_.contains("performs the effect 'Inf' but does not declare")) shouldBe true)
  }

  /** Compile the program (module `Test`) over the base layer and return the diagnostic messages. */
  private def compileErrors(source: String): IO[Seq[String]] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("acct-wire-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("acct-wire-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), source))
      args        =
        List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test") ++ layerPathArgs
      sessionOpt <- Compiler.createSession(args)
      session    <- IO.fromOption(sessionOpt)(new IllegalStateException("Could not create the compilation session."))
      result     <- session.compileOnce()
    } yield result.errors.map(_.message)

  private def layerPathArgs: List[String] = {
    val repoRoot             =
      Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
    def root(module: String) = repoRoot.resolve(module).resolve("eliot").toString
    List("--path", root("lang"), "--path", root("stdlib"), "--path", root("jvm"))
  }
}
