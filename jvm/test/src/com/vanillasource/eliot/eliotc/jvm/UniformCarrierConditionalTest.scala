package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.compiler.Compiler
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** Effects-as-channel (docs/effects-as-channel.md §3): a **compile-succeeds** gate for the uniform-carrier
  * *non-overlap* improvements — programs the pre-uniform path **rejected** but the uniform-carrier ladder accepts
  * (carrierhood is positional, so a carrier meta is never stolen). The pre-uniform path has been removed (U4-e close-out
  * slice 2), so the historical "rejected under `--legacy-carrier`" contrast is gone; the durable assertion is that these
  * shapes **compile under the (now sole) uniform checker**. Two shapes:
  *
  *   - **conditional `CarrierSlot` arm** — `if(c, None) else Some(x)` mixes a pure `None : Option[?E]` arm and a
  *     `Some(x) : Option[Int]` arm. The uniform `CarrierSlot` arm pure-wraps `None` into the carrier's payload slot
  *     first (`?G` kept a meta the `else` discharge solves), so the `Some(x)` sibling decides the element type and it
  *     compiles (the pre-uniform equal-arity unify stole the carrier whole, `?G := Option`, then failed).
  *   - **effectful value into a data slot** (compound-state) — an effectful `items : {Console} List[String]` passed to
  *     `foldLeft`'s `list : List[A]`. The uniform payload slot **binds**: the carrier is split off, the `List[String]`
  *     payload fills `List[A]` (`A := String`), and the effectful list is sequenced at the call site (the pre-uniform
  *     equal-arity unify stole the carrier, `?F := List`, then `Effect[List]` had no instance — the `val` workaround was
  *     required).
  */
class UniformCarrierConditionalTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  private val conditionalSource =
    """def choose(c: Bool, x: Int): Option[Int] = if(c, None) else Some(x)
      |
      |def main: {Console} Unit = printLine(foldOption("none", v -> "some", choose(true, 5)))
      |""".stripMargin

  private val compoundStateSource =
    """import eliot.collection.List
      |
      |def items: {Console} List[String] = {
      |   printLine("loading...")
      |   empty
      |}
      |
      |def summary: {Console} String = foldLeft("start", x -> acc -> acc, items)
      |
      |def main: {Console} Unit = printLine(summary)
      |""".stripMargin

  // Effects-as-channel §6/§10 (U4-e prerequisite): a post-mono MonomorphicValue consumer must see through the uniform
  // path's pervasive `Id`. `useByte(1000)`'s argument range `[1000,1000]` sits inside `pure@Effect[Id]( 1000 )`; before
  // the refinement channel normalized `Id` away it read "value range is not known here" (a wrong diagnostic) instead of
  // the bound violation. This pins that the range survives the `Id` wrapper under the uniform-carrier checker.
  private val refinementSource =
    """import eliot.jvm.IO
      |import eliot.effect.Console
      |
      |def byteMin: BigInteger = -128
      |def byteMax: BigInteger = 127
      |def withinByte(i: Interval[BigInteger]): Bool = lessThanOrEqual(byteMin, start(i)) && lessThanOrEqual(end(i), byteMax)
      |def useByte(x: Int): Int where withinByte(range(x)) = x
      |
      |def main: IO[Unit] = printLine(show(useByte(1000)))
      |""".stripMargin

  "a where-precondition over a uniform-carriered argument" should "see the argument's range through the Id wrapper" in {
    compileErrors(refinementSource)
      .asserting(_.map(_.message).mkString should include("is not satisfied"))
  }

  "if(c, None) else Some(x)" should "compile (the CarrierSlot arm pure-wraps instead of stealing the carrier)" in {
    compileErrors(conditionalSource).asserting(_ shouldBe empty)
  }

  "an effectful list into foldLeft's List[A]" should "compile (the payload binds, the carrier is split off first)" in {
    compileErrors(compoundStateSource).asserting(_ shouldBe empty)
  }

  /** Compile the program (module `Test`) over the base layer roots, returning the compilation diagnostics
    * (empty = success).
    */
  private def compileErrors(source: String): IO[Seq[CompilerError]] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("eliot-cond-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("eliot-cond-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), source))
      args        = List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test") ++ layerPathArgs
      sessionOpt <- Compiler.createSession(args)
      session    <- IO.fromOption(sessionOpt)(new IllegalStateException("Could not create the compilation session."))
      result     <- session.compileOnce()
    } yield result.errors

  /** The base-layer `eliot/` source roots handed to the compiler as filesystem roots, exactly as
    * [[UniformCarrierCompileTest]] does — the repo root reaches the forked test JVM via `ELIOT_REPO_ROOT`.
    */
  private def layerPathArgs: List[String] = {
    val repoRoot             =
      Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
    def root(module: String) = repoRoot.resolve(module).resolve("eliot").toString
    List("--path", root("lang"), "--path", root("stdlib"), "--path", root("jvm"))
  }
}
