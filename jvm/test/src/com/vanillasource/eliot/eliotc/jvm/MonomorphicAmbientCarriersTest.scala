package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.compiler.Compiler
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.typeFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** Effects-as-channel U4-c-0a (docs/effects-as-channel.md §5, §10): the single writer of
  * [[MonomorphicValue.ambientCarriers]] must stamp the value's own ambient effect carriers as **full ground values** —
  * the authoritative "ambient" input the effect-accounting verifier's ride test derives against, forwarded here rather
  * than reconstructed from the mono key ↔ signature-binder alignment. This compiles a small effectful program over the
  * whole base layer (`lang` + `stdlib` + `jvm`) and asserts both carrier spellings are forwarded correctly:
  *   - an *effect-polymorphic* value (`greet : {Console} Unit`), monomorphized at the concrete `IO` carrier the
  *     synthetic entry binds, carries `{IO}` — its open effect row's carrier binder, solved to `IO`;
  *   - a *pure* value (`label`) carries the empty set (a nullary `String` return is not carrier-headed).
  *
  * Nothing consumes the field yet (the derivation rewire is U4-c-0d), so this pins the writer directly — an under-count
  * here (an empty set where `{IO}` is due) is the fail-safe-violating direction the later ride test would silently
  * inherit.
  */
class MonomorphicAmbientCarriersTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  private val source =
    """def label(line: String): String = line
      |
      |def greet: {Console} Unit = printLine(label("hi"))
      |
      |def main: {Console} Unit = greet
      |""".stripMargin

  "the MonomorphicValue.ambientCarriers writer" should "forward {IO} for an effect-polymorphic value bound at IO" in {
    monosNamed("greet").asserting { greets =>
      greets.flatMap(_.ambientCarriers).flatMap(_.typeFQN).map(_.name.name).toSet shouldBe Set("IO")
    }
  }

  it should "forward the empty set for a pure value" in {
    monosNamed("label").asserting(labels => labels.map(_.ambientCarriers) should contain only Set.empty)
  }

  /** Every runtime [[MonomorphicValue]] whose original value has the given simple name. */
  private def monosNamed(name: String): IO[Seq[MonomorphicValue]] =
    compileFacts(source).map(facts =>
      facts.values.collect { case mv: MonomorphicValue if mv.vfqn.name.name == name => mv }.toSeq
    )

  /** Compile the program (module `Test`) over the base-layer roots and return the produced fact map. */
  private def compileFacts(source: String): IO[Map[CompilerFactKey[?], CompilerFact]] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("eliot-ac-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("eliot-ac-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), source))
      args        = List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test") ++ layerPathArgs
      sessionOpt <- Compiler.createSession(args)
      session    <- IO.fromOption(sessionOpt)(new IllegalStateException("Could not create the compilation session."))
      result     <- session.compileOnce()
      _          <- IO.raiseWhen(result.errors.nonEmpty)(
                      new IllegalStateException(s"Compilation errors: ${result.errors.map(_.message).mkString(", ")}")
                    )
      facts      <- result.generator.currentFacts()
    } yield facts

  /** The base-layer `eliot/` source roots handed to the compiler as filesystem roots, exactly as
    * [[EffectShapeCompileTest]] does — the repo root reaches the forked test JVM via `ELIOT_REPO_ROOT`.
    */
  private def layerPathArgs: List[String] = {
    val repoRoot             =
      Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
    def root(module: String) = repoRoot.resolve(module).resolve("eliot").toString
    List("--path", root("lang"), "--path", root("stdlib"), "--path", root("jvm"))
  }
}
