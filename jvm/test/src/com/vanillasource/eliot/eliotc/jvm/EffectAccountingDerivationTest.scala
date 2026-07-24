package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.Compiler
import com.vanillasource.eliot.eliotc.monomorphize.channel.EffectAccounting
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** Effects-as-channel U4-c-0d (docs/effects-as-channel.md §5): the post-mono derivation
  * ([[com.vanillasource.eliot.eliotc.monomorphize.channel.EffectAccountingProcessor]]) must produce each value's derived
  * effect row by gating every reference through the ride test against the value's own ambient carriers. This drives the
  * derivation directly by demanding `EffectAccounting` for **every** monomorphic value of a valid program and asserts
  * (a) no accounting error is raised — a spurious over-count on valid code aborts and would surface here — and (b) the
  * derived rows are exactly right: an effect performed on the ambient is counted, a discharged / captured computation is
  * not, and the synthetic entry (empty ambient) counts nothing. (The `--effect-channel` argument is vestigial since
  * U4-c-2 made accounting unconditional; kept harmlessly until the flag is removed at U4-e.)
  *
  * It is the valid-program half of the U4-c-1 parity gate, exercised one slice early to pin the derivation. The
  * undeclared-effect **rejection** direction lands with the wiring at U4-c-1.
  */
class EffectAccountingDerivationTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  // `catch` discharges {Throw[String]}; the discharged/captured `parseOk`/`parseBad` must NOT propagate Throw to `main`,
  // which performs only `Console`. Both a user `main` ({Console}) and the jvm synthetic entry (empty ambient) exist.
  private val throwSource =
    """def parseOk: {Throw[String]} String = "ok"
      |def parseBad: {Throw[String]} String = raise("bad")
      |def main: {Console} Unit = {
      |   printLine(parseOk catch (err -> err))
      |   printLine(parseBad catch (err -> err))
      |}
      |""".stripMargin

  // `forever` (`implement Inf[IO]`, a binder-less concrete-carrier impl) must be counted as `Inf` via its forwarded
  // ambient, and propagate with `Console` through the ordinary callee `loopForever`.
  private val infSource =
    """def loopForever: {Inf, Console} Unit = forever(printLine("tick"))
      |def main: {Inf, Console} Unit = loopForever
      |""".stripMargin

  "the effect-accounting derivation" should "count Console and exclude the discharged Throw of captured computations" in {
    derive(throwSource).asserting { case (rowsByName, errors) =>
      errors shouldBe empty
      rowsByName("main") shouldBe Set(Set("Console"), Set.empty)
      rowsByName("parseBad") shouldBe Set(Set.empty)
    }
  }

  it should "count Inf (a concrete-carrier impl) and propagate it with Console through a callee" in {
    derive(infSource).asserting { case (rowsByName, errors) =>
      errors shouldBe empty
      rowsByName("main") shouldBe Set(Set("Inf", "Console"), Set.empty)
      rowsByName("loopForever") shouldBe Set(Set("Inf", "Console"))
    }
  }

  /** Compile the program under `--effect-channel`, demand `EffectAccounting` for every monomorphic value, and return the
    * derived effect-ability names grouped by simple value name, plus every accounting error raised.
    */
  private def derive(source: String): IO[(String => Set[Set[String]], Seq[String])] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("acct-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("acct-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), source))
      args        =
        List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test", "--effect-channel") ++ layerPathArgs
      sessionOpt <- Compiler.createSession(args)
      session    <- IO.fromOption(sessionOpt)(new IllegalStateException("Could not create the compilation session."))
      result     <- session.compileOnce()
      _          <- IO.raiseWhen(result.errors.nonEmpty)(
                      new IllegalStateException(s"Compilation errors: ${result.errors.map(_.message).mkString(", ")}")
                    )
      facts      <- result.generator.currentFacts()
      mvs         = facts.values.collect { case mv: MonomorphicValue => mv }.toSeq
      accts      <- mvs.traverseFilter(mv =>
                      result.generator.getFact(EffectAccounting.Key(mv.vfqn, mv.typeArguments)).map(_.map(mv -> _))
                    )
      errors     <- result.generator.currentErrors()
    } yield {
      val rowsByName: String => Set[Set[String]] = name =>
        accts.collect { case (mv, a) if mv.vfqn.name.name == name => a.derivedRow.map(_.abilityName) }.toSet
      (rowsByName, errors.map(_.message))
    }

  private def layerPathArgs: List[String] = {
    val repoRoot             =
      Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
    def root(module: String) = repoRoot.resolve(module).resolve("eliot").toString
    List("--path", root("lang"), "--path", root("stdlib"), "--path", root("jvm"))
  }
}
