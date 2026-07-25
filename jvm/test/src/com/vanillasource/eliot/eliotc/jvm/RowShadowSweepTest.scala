package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.Compiler
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

  /** One combined program covering the design's worked shapes: direct-style Console blocks, discharge-to-pure
    * (`catch` + `runStateToPair` under pure returns), an effectful catch handler, a carrier-polymorphic Abort program
    * under a local pure Id carrier (with a hand-written `Effect` instance), the State effect at a concrete IO carrier,
    * and a run-carrier-headed `main` sequencing them all (so demand-driven compilation reaches every definition).
    */
  private val combinedProgram =
    """import eliot.jvm.IO
      |import eliot.effect.Console
      |import eliot.carrier.Effect
      |import eliot.effect.Throw
      |import eliot.effect.State
      |import eliot.effect.Abort
      |
      |data Id[A](runId: A)
      |
      |implement Effect[Id] {
      |   def pure[A](a: A): Id[A] = Id(a)
      |   def flatMap[A, B](f: Function[A, Id[B]], fa: Id[A]): Id[B] = f(runId(fa))
      |   def map[A, B](f: Function[A, B], fa: Id[A]): Id[B] = Id(f(runId(fa)))
      |}
      |
      |def shout(s: String): {Console} Unit = printLine(s)
      |
      |def greet: {Console} Unit = {
      |   shout("a")
      |   printLine("b")
      |}
      |
      |def parsed(raw: String): {Throw[String]} String = raise("unparseable")
      |
      |def recovered: String = parsed("x") catch (err -> err)
      |
      |def counted: Pair[String, String] = runStateToPair("initial", state)
      |
      |def failUnit: {Throw[String]} Unit = raise("boom")
      |
      |def caught: {Console} Unit = failUnit catch (err -> printLine(err))
      |
      |def allowed: {Abort} String = "granted"
      |def denied: {Abort} String = abort
      |
      |def testAllowed: Option[String] = runId(runAbort(allowed))
      |def testDenied: Option[String] = runId(runAbort(denied))
      |
      |def swap(next: String): {State[String]} String =
      |   flatMap(old -> flatMap(ignored -> pure(old), putState(next)), state)
      |
      |def prog: IO[Pair[String, String]] = runStateToPair("before", swap("after"))
      |
      |def main: IO[Unit] = {
      |   greet
      |   printLine(recovered)
      |   printLine(counted.first)
      |   caught
      |   printLine(foldOption("DENIED", s -> s, testAllowed))
      |   printLine(foldOption("DENIED", s -> s, testDenied))
      |   flatMap(pair -> printLine(pair.first), prog)
      |}""".stripMargin

  /** A deliberately non-terminating program: `Inf` is an ordinary row entry riding the same union. Compile-only. */
  private val infProgram =
    """import eliot.jvm.IO
      |import eliot.effect.Console
      |import eliot.effect.Inf
      |
      |def main: {Inf, Console} Unit = forever(printLine("tick"))""".stripMargin

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
    compileForFacts(source).map { facts =>
      val values        = facts.values.collect {
        case orv: OperatorResolvedValue if orv.platform == Platform.Runtime => orv.vfqn -> orv
      }.toMap
      val runBoundaries = facts.values.collect { case rb: RunBoundaryFunction => rb.vfqn }.toSet
      val results       = RowChecker.checkAll(RowChecker.Universe(values, runBoundaries))
      val leaks         = results.filter(_.leak.nonEmpty).map(r => (r.vfqn.toString, r.leak.map(_.abilityName)))
      (leaks, results.size)
    }

  /** Compile `source` (expected clean) in a fresh, cold compilation session over the real layer roots and return the
    * run's full fact universe. Mirrors [[FullIntegrationTest.SharedSession]]'s configuration, without the residency.
    */
  private def compileForFacts(source: String): IO[Map[CompilerFactKey[?], CompilerFact]] =
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
    } yield facts

  private def layerPathArgs: List[String] = {
    val repoRoot             = Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
    def root(module: String) = repoRoot.resolve(module).resolve("eliot").toString
    List("--path", root("lang"), "--path", root("stdlib"), "--path", root("jvm"))
  }
}
