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

/** What the **post-mono** effect accounting
  * ([[com.vanillasource.eliot.eliotc.monomorphize.channel.EffectAccountingProcessor]]) actually derives under effects
  * v6, pinned exactly — because it turns out to be considerably less than its v5 predecessor, and that is the evidence
  * D7 (`docs/effects.md` §11, "can the post-mono accounting verifier retire?") asks for.
  *
  * Under names, "performs X" is "a reference forwards one of the implementations *this* value received, to a callee
  * that declares X as a row entry". That reads effect-ness in one place — the callee's declared row — and it is what
  * catches **propagation through a declaring callee**: `main` calling `{Inf, Console} loopForever` at its own bindings
  * forwards both.
  *
  * What it cannot see is a **direct operation call**. By the time a body is monomorphic, `AbilityResolver` has already
  * rewritten `printLine` into the *implementation* method, and an implementation method declares no row — the row is on
  * the ability's member, which is no longer what the body names. So `loopForever` itself, whose whole body is
  * `forever(printLine(…))`, derives nothing at all, and so does a `main` that performs `Console` directly.
  *
  * That is not a hole: the **pre-mono scope check** in `BindingWriter` reports an uncovered effect at the reference,
  * for an operation and a declaring callee alike, and it is complete before monomorphization because nothing about it
  * is instantiation-dependent. It does mean the post-mono verifier's remaining coverage is a subset of the scope
  * check's, which is the whole of D7's question. This suite is the measurement, not the verdict — it asserts what is,
  * so that retiring the verifier (or restoring its reach) is a decision made against evidence.
  *
  * It drives the derivation directly, demanding `EffectAccounting` for **every** monomorphic value of a valid program,
  * and also asserts that no accounting error is raised — a spurious over-count on valid code aborts and would surface
  * here.
  */
class EffectAccountingDerivationTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  // `catch` supplies and discharges `{Throw[String]}` at its own slot, so the captured `parseOk`/`parseBad` must NOT
  // propagate `Throw` to `main`. Both a user `main` and the synthetic entry exist; neither forwards anything, because
  // `main` performs `Console` through a direct operation call and `catch` declares no row of its own.
  private val throwSource =
    """def parseOk: {Throw[String]} String = "ok"
      |def parseBad: {Throw[String]} String = raise("bad")
      |def main: {Console} Unit = {
      |   printLine(parseOk catch (err -> err))
      |   printLine(parseBad catch (err -> err))
      |}
      |""".stripMargin

  // Propagation through a declaring callee, which is what the derivation *can* see: `main` forwards its own `Inf` and
  // `Console` bindings to `loopForever`, which declares both.
  private val infSource =
    """def loopForever: {Inf, Console} Unit = forever(printLine("tick"))
      |def main: {Inf, Console} Unit = loopForever
      |""".stripMargin

  "the effect-accounting derivation" should "exclude the supplied Throw of a discharged computation" in {
    derive(throwSource).asserting { case (rowsByName, errors) =>
      errors shouldBe empty
      rowsByName("parseBad") shouldBe Set(Set.empty)
    }
  }

  // The measurement, stated as an assertion so it cannot drift silently: a direct operation call forwards nothing,
  // because the reference the monomorphic body holds is the *implementation* method and that declares no row.
  it should "derive nothing for an effect performed by a direct operation call" in {
    derive(throwSource).asserting { case (rowsByName, errors) =>
      errors shouldBe empty
      rowsByName("main") shouldBe Set(Set.empty)
    }
  }

  it should "forward Inf and Console through a callee that declares them" in {
    derive(infSource).asserting { case (rowsByName, errors) =>
      errors shouldBe empty
      rowsByName("main") shouldBe Set(Set("Inf", "Console"), Set.empty)
    }
  }

  // The same measurement from the other side: `loopForever`'s own body is nothing but operation calls, so it forwards
  // nothing — even though it declares, and genuinely performs, both effects.
  it should "derive nothing for a value whose body is only operation calls" in {
    derive(infSource).asserting { case (rowsByName, errors) =>
      errors shouldBe empty
      rowsByName("loopForever") shouldBe Set(Set.empty)
    }
  }

  /** Compile the program, demand `EffectAccounting` for every monomorphic value, and return the
    * derived effect-ability names grouped by simple value name, plus every accounting error raised.
    */
  private def derive(source: String): IO[(String => Set[Set[String]], Seq[String])] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("acct-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("acct-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), source))
      args        =
        List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test") ++ layerPathArgs
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
