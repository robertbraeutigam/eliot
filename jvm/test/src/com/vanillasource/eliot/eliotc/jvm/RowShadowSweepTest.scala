package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.Compiler
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.RunBoundaryFunction
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.row.RowChecker
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** The effects-as-rows **R3 shadow sweep** (docs/effects-as-rows.md §8): compile a representative real program through
  * the live v2 pipeline over the full `lang`/`stdlib`/`jvm` layers, then row-check **every** body-carrying
  * `OperatorResolvedValue` the compile demanded — user program, stdlib dischargers, jvm ability implementations and
  * the synthetic entry alike — with the standalone [[RowChecker]].
  *
  * The corpus is v2-green by construction (the program compiles), so v2's verdict on every checked definition is
  * "accepted"; a [[RowChecker.RowResult.leak]] is therefore a *disagreement* to triage — either a v2 blind spot or a
  * v3 rule gap. The assertions pin the current state: zero leaks, over a non-vacuous number of checked definitions.
  * `unknownCallees` is deliberately not asserted empty — the universe only contains what the compile demanded — but
  * leaks are computed from what *is* known, so a leak report is always actionable.
  *
  * Each sweep uses its **own cold session** (not [[FullIntegrationTest]]'s shared resident one): an incremental
  * recompile of a replaced source can report stale cross-program diagnostics and retains the previous program's
  * facts, either of which would pollute the swept universe.
  */
class RowShadowSweepTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  import EffectCorpus.{combinedProgram, infProgram}

  "the row shadow sweep" should "agree with v2 on the combined effects corpus (Console, Throw, State, Abort, discharge-to-pure)" in {
    sweepForLeaks(combinedProgram)
      .asserting { case (leaks, checked) => (leaks, checked > 30) shouldBe (Seq.empty, true) }
  }

  it should "agree with v2 on an Inf super-loop program" in {
    sweepForLeaks(infProgram)
      .asserting { case (leaks, checked) => (leaks, checked > 10) shouldBe (Seq.empty, true) }
  }

  /** Compile in a fresh session, then row-check every checkable definition of the run's fact universe. Returns the
    * rendered leaks (empty = full agreement with v2 on this corpus) and how many definitions were checked.
    */
  private def sweepForLeaks(source: String): IO[(Seq[(String, Set[String])], Int)] =
    compileForFacts(source).map { case (facts, runBoundaries) =>
      val values  = facts.values.collect {
        case orv: OperatorResolvedValue if orv.platform == Platform.Runtime => orv.vfqn -> orv
      }.toMap
      val results = RowChecker.checkAll(RowChecker.Universe(values, runBoundaries))
      val leaks         = results.filter(_.leak.nonEmpty).map(r => (r.vfqn.toString, r.leak.map(_.abilityName)))
      (leaks, results.size)
    }

  /** Compile `source` (expected clean) in a fresh, cold compilation session over the real layer roots and return the
    * run's full fact universe together with the platform's run boundaries. Mirrors
    * [[FullIntegrationTest.SharedSession]]'s configuration, without the residency.
    *
    * The run boundaries come from the session's own configuration — the same place `LangPlugin` reads them — rather
    * than from the fact universe: the `RunBoundaryFunction` *fact* is produced on demand, and since the v2 bridge was
    * deleted nothing in the pipeline demands it, so collecting it out of the facts would silently yield the empty set
    * and cost every nominal-run capture in the sweep.
    */
  private def compileForFacts(source: String): IO[(Map[CompilerFactKey[?], CompilerFact], Set[ValueFQN])] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("eliot-row-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("eliot-row-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), source))
      args        = List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test") ++ layerPathArgs
      sessionOpt <- Compiler.createSession(args)
      session    <- IO.fromOption(sessionOpt)(new IllegalStateException("Could not create the compilation session."))
      result     <- session.compileOnce()
      _          <- IO
                      .raiseError(new Exception(s"Compilation errors: ${result.errors.map(_.message).mkString(", ")}"))
                      .whenA(result.errors.nonEmpty)
      facts      <- result.generator.currentFacts()
    } yield (facts, session.effectiveConfiguration.getOrElse(RunBoundaryFunction.configKey, Set.empty))

  private def layerPathArgs: List[String] = {
    val repoRoot             = Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
    def root(module: String) = repoRoot.resolve(module).resolve("eliot").toString
    List("--path", root("lang"), "--path", root("stdlib"), "--path", root("jvm"))
  }
}
