package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.Compiler
import com.vanillasource.eliot.eliotc.monomorphize.fact.RunBoundaryFunction
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import com.vanillasource.eliot.eliotc.row.{RowChecker, RowElaborator}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** The effects-as-rows **R4 end-to-end shadow compile** (docs/effects-as-rows.md §8): prove the [[RowElaborator]]'s
  * output *compiles and runs* through the real pipeline, not merely that it matches hand-written twins structurally.
  *
  * Mechanism — the fact-injection seam (`CompilationSession.compileOnce(seedFacts = …)`): compile the representative
  * corpus normally (run A), elaborate **every** body-carrying runtime [[OperatorResolvedValue]] the compile demanded —
  * user program, stdlib dischargers, jvm ability implementations and the synthetic entry alike — then compile the
  * same sources again (run B) with the elaborated values pre-registered. A registered fact preempts its processor, so
  * run B's checker, monomorphization and codegen all consume the elaborated bodies; everything downstream regenerates
  * from them. The oracle is **behavioral identity**: run B must compile clean and its executable must print exactly
  * run A's output. (Byte-identity of the jars is deliberately not the gate — inserted `$row$N` binders legitimately
  * rename lambda classes; see the byte-identity-is-an-oracle rule.)
  *
  * The elaborated corpus is also asserted to differ from the original on a non-trivial number of definitions, so the
  * experiment cannot silently degrade to "identity elaboration compiled fine".
  */
class RowElaborationShadowCompileTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  "the elaboration shadow compile" should "compile and run the combined corpus identically from pre-elaborated facts" in {
    (for {
      sourceDir <- IO.blocking(Files.createTempDirectory("eliot-elab-src"))
      _         <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), EffectCorpus.combinedProgram))

      // Run A: the ordinary compile; its fact universe is the elaboration input, its output the behavioral oracle.
      a                          <- compileRun(sourceDir, Seq.empty)
      (aFacts, aJar)              = a
      universe                    = universeOf(aFacts)
      elaborated                  = universe.values.values.toSeq
                                      .filter(RowChecker.checkable)
                                      .flatMap(orv => RowElaborator.elaborate(orv, universe).map(e => orv.copy(runtime = Some(e))))
      changed                     = elaborated.count(orv => universe.values(orv.vfqn).runtime != orv.runtime)
      outputA                    <- FullIntegrationTest.runJar(aJar, "")

      // Run B: same sources, fresh session, the elaborated values seeded — downstream consumes them.
      b                          <- compileRun(sourceDir, elaborated)
      (_, bJar)                   = b
      outputB                    <- FullIntegrationTest.runJar(bJar, "")
    // The changed-count tripwire only guards against elaboration silently degrading to identity; under the A.8.6
    // deferral discipline the desugar legitimately rewrites fewer definitions (instantiation-decided shapes pass
    // through untouched for the checker), so any positive count is the meaningful bound.
    } yield (outputB, changed > 0, outputA) shouldBe (outputA, true, outputA)).recover { case e =>
      fail(e.getMessage)
    }
  }

  private def universeOf(facts: Map[?, CompilerFact]): RowChecker.Universe = {
    val values        = facts.values.collect {
      case orv: OperatorResolvedValue if orv.platform == Platform.Runtime => orv.vfqn -> orv
    }.toMap
    val runBoundaries = facts.values.collect { case rb: RunBoundaryFunction => rb.vfqn }.toSet
    RowChecker.Universe(values, runBoundaries)
  }

  /** One cold compile of `sourceDir` (module `Test`) over the real layer roots with the given seed facts, returning
    * the run's fact universe and the produced executable jar.
    */
  private def compileRun(sourceDir: Path, seedFacts: Seq[CompilerFact]): IO[(Map[?, CompilerFact], Path)] =
    for {
      targetDir  <- IO.blocking(Files.createTempDirectory("eliot-elab-target"))
      args        = List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test") ++ layerPathArgs
      sessionOpt <- Compiler.createSession(args)
      session    <- IO.fromOption(sessionOpt)(new IllegalStateException("Could not create the compilation session."))
      result     <- session.compileOnce(seedFacts = seedFacts)
      _          <- IO
                      .raiseError(
                        new Exception(
                          s"Compilation errors: ${result.errors
                              .map(e =>
                                s"${e.contentSource}:${e.sourceRange.from.line}:${e.sourceRange.from.col}: ${e.message} ${e.description.mkString(" | ")}"
                              )
                              .mkString("\n")}"
                        )
                      )
                      .whenA(result.errors.nonEmpty)
      facts      <- result.generator.currentFacts()
    } yield (facts, targetDir.resolve("Test.jar"))

  private def layerPathArgs: List[String] = {
    val repoRoot             = Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
    def root(module: String) = repoRoot.resolve(module).resolve("eliot").toString
    List("--path", root("lang"), "--path", root("stdlib"), "--path", root("jvm"))
  }
}
