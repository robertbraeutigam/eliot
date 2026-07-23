package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.compiler.Compiler
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** Effects-as-channel U3a-2b(ii), the conditional-arm slice (docs/effects-as-channel.md §3): a **compile-succeeds**
  * gate for the uniform-carrier *non-overlap* improvements — cases the **default** path rejects but the uniform-carrier
  * ladder accepts (carrierhood is positional, so a carrier meta is never stolen). Unlike
  * [[UniformCarrierByteIdenticalTest]] (which compares bytes where behavior overlaps) this asserts the *outcome
  * differs*: rejected off, accepted on. Two shapes:
  *
  *   - **conditional `CarrierSlot` arm** — `if(c, None) else Some(x)` mixes a pure `None : Option[?E]` arm and a
  *     `Some(x) : Option[Int]` arm. On the default path the pure `None` arm meets `if`'s carrier slot `?G[?T]` at
  *     *equal arity*, so `tryUnifyCommitting` **steals** the carrier whole (`?G := Option`) before the pure-wrap arm
  *     (which only pre-fires on a strictly *under*-applied actual) can run; the stolen `Option` then fails to unify with
  *     the discharger's `AbortCarrier`, a `Type mismatch`. Under `--uniform-carrier` the `CarrierSlot` arm pure-wraps
  *     `None` into the carrier's payload slot first (`?G` kept a meta the `else` discharge solves), so the `Some(x)`
  *     sibling decides the element type and it compiles.
  *   - **effectful value into a data slot** (compound-state) — an effectful `items : {Console} List[String]` passed to
  *     `foldLeft`'s `list : List[A]`. On the default path the equal-arity unify **steals** the carrier (`?F := List`),
  *     then `Effect[List]` has no instance — a hard rejection (the `val` workaround was required). Under
  *     `--uniform-carrier` the payload slot **binds**: the carrier is split off, the `List[String]` payload fills
  *     `List[A]` (`A := String`), and the effectful list is sequenced at the call site.
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

  "if(c, None) else Some(x)" should "be REJECTED by the default path (the pure arm steals the if carrier)" in {
    compileErrors(conditionalSource, uniformCarrier = false).asserting(_ should not be empty)
  }

  it should "COMPILE under --uniform-carrier (the CarrierSlot arm pure-wraps instead of stealing)" in {
    compileErrors(conditionalSource, uniformCarrier = true).asserting(_ shouldBe empty)
  }

  "an effectful list into foldLeft's List[A]" should "be REJECTED by the default path (the carrier is stolen to List)" in {
    compileErrors(compoundStateSource, uniformCarrier = false).asserting(_ should not be empty)
  }

  it should "COMPILE under --uniform-carrier (the payload binds, the carrier is split off first)" in {
    compileErrors(compoundStateSource, uniformCarrier = true).asserting(_ shouldBe empty)
  }

  /** Compile the program (module `Test`) over the base layer roots, optionally under `--uniform-carrier`, returning the
    * compilation diagnostics (empty = success). A fresh session per call keeps the two runs independent.
    */
  private def compileErrors(source: String, uniformCarrier: Boolean): IO[Seq[CompilerError]] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("eliot-cond-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("eliot-cond-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), source))
      flag        = if (uniformCarrier) List("--uniform-carrier") else Nil
      args        = List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test") ++
                      layerPathArgs ++ flag
      sessionOpt <- Compiler.createSession(args)
      session    <- IO.fromOption(sessionOpt)(new IllegalStateException("Could not create the compilation session."))
      result     <- session.compileOnce()
    } yield result.errors

  /** The base-layer `eliot/` source roots handed to the compiler as filesystem roots, exactly as
    * [[UniformCarrierByteIdenticalTest]] does — the repo root reaches the forked test JVM via `ELIOT_REPO_ROOT`.
    */
  private def layerPathArgs: List[String] = {
    val repoRoot             =
      Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
    def root(module: String) = repoRoot.resolve(module).resolve("eliot").toString
    List("--path", root("lang"), "--path", root("stdlib"), "--path", root("jvm"))
  }
}
