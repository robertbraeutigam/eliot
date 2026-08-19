package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.Compiler
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, GroundValueRenderer, MonomorphicValue}
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.collection.concurrent.TrieMap

/** The measurement behind the **effects-v4 P0 spike** (`docs/effects-v4-p0-spike.md`, `docs/effects-as-channel-v4.md`
  * §11 P0 / risk R1): at the `WovenValue` codegen seam, is the carrier of every definition *ground* — and is it a
  * function of the seam's key?
  *
  * v4 proposes to take the carrier out of the type language and write it in one post-monomorphization lowering. The
  * question that decides whether that is possible is not "is the carrier known" but "**is it known from what the seam
  * is keyed by**": under v4 a mono key holds only *payload* type arguments, because a carrier never appears in a type.
  * This suite pins the three shapes P0 names, over the real `lang`/`stdlib`/`jvm` layers:
  *
  *   - **S1, a `{Console}` block** — the callee's mono key is *nothing but* the carrier, so its payload key is empty
  *     and its single instance's carrier is the ambient of the region that calls it;
  *   - **S2, a `catch` discharge** — one definition (`emit`) is monomorphized twice, at `IO` and at
  *     `{Throw[String] | IO}`, with an **identical** payload key. This is the load-bearing measurement: a payload-only
  *     key does *not* determine the carrier, so the seam's key must carry the carrier stack itself (which is what
  *     `WovenValue`'s own "weave key = mono key × stack" note anticipates);
  *   - **S3, a stored computation** — a definition reached *only* through a `data` field (`assertEquals`, held in
  *     `TestCase.body` and put in a `List[TestCase]`) still has a ground carrier, but it takes it from the field's
  *     *type* — the one place v4 removes it, which is why the spike note records a canonical-stack obligation there.
  *
  * Every instance's carriers are also asserted to be fully ground (no [[GroundValue.Param]] residue), which is the
  * plain form of R1's question: nothing at this seam is still waiting to be solved.
  */
class EffectsV4SeamGroundnessTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  import EffectsV4SeamGroundnessTest.*

  "a {Console} block (S1)" should "monomorphize its callee once, at the carrier of the calling region" in {
    stacksOf(consoleBlock, "echo").asserting(_ shouldBe Set(Set("IO")))
  }

  it should "key that callee by nothing but its carrier — its payload key is empty" in {
    payloadKeysOf(consoleBlock, "echo").asserting(_ shouldBe Set(Seq.empty))
  }

  "a catch discharge (S2)" should "monomorphize the same definition at both the ambient and the discharge stack" in {
    stacksOf(catchDischarge, "emit").asserting(_ shouldBe Set(Set("IO"), Set("{Throw[String] | IO}")))
  }

  // The measurement v4 turns on: both instances of `emit` have the *same* payload key, so a payload-only seam key
  // would collapse two different weaves into one. The seam must be keyed by the carrier stack as well.
  it should "give those two instances the same payload key" in {
    payloadKeysOf(catchDischarge, "emit").asserting(_ shouldBe Set(Seq.empty))
  }

  it should "resolve the ability method separately per stack" in {
    stacksOf(catchDischarge, "printLine").asserting(_ shouldBe Set(Set("IO"), Set("{Throw[String] | IO}")))
  }

  "a stored computation (S3)" should "give the stored definition a ground carrier" in {
    stacksOf(storedComputation, "assertEquals").asserting(_ shouldBe Set(Set("{Throw[String] | Id}")))
  }

  it should "key that definition by nothing but its carrier — its payload key is empty" in {
    payloadKeysOf(storedComputation, "assertEquals").asserting(_ shouldBe Set(Seq.empty))
  }

  // The field accessor takes the stored stack from the *type* of the field, not from a type argument of its own — the
  // information v4 removes from the type, and so the one that has to come from a canonical row ⤳ stack rule instead.
  it should "read the stored stack off the data field, with no type argument of its own" in {
    typeArgumentsOf(storedComputation, "body").asserting(_ shouldBe Set(Seq.empty))
  }

  it should "read that stack as the field's declared row over the pure base" in {
    stacksOf(storedComputation, "body").asserting(_ shouldBe Set(Set("{Throw[String] | Id}")))
  }

  it should "keep the list holding those computations carrier-free" in {
    stacksOf(storedComputation, "cases").asserting(_ shouldBe Set(Set.empty))
  }

  "every instance at the seam" should "carry only fully ground carriers, in all three shapes" in {
    Seq(consoleBlock, catchDischarge, storedComputation)
      .traverse(shape => monos(shape).map(_.flatMap(_.ambientCarriers).filter(hasParam)))
      .asserting(_.flatten shouldBe Seq.empty)
  }

  /** The rendered carrier stacks of every monomorphic instance of the value with this simple name — one set per
    * instance, so two instances of the same definition at different stacks stay distinguishable.
    */
  private def stacksOf(source: String, name: String): IO[Set[Set[String]]] =
    instances(source, name).map(_.map(_.ambientCarriers.map(GroundValueRenderer.renderConstructor)).toSet)

  /** The **payload key** of every instance: its mono type arguments minus the ones that are the value's own carrier —
    * the key a v4 mono would be left with, since a carrier would no longer appear in any type.
    */
  private def payloadKeysOf(source: String, name: String): IO[Set[Seq[String]]] =
    instances(source, name).map(
      _.map(mv =>
        mv.typeArguments.filterNot(mv.ambientCarriers.contains).map(GroundValueRenderer.renderConstructor)
      ).toSet
    )

  private def typeArgumentsOf(source: String, name: String): IO[Set[Seq[String]]] =
    instances(source, name).map(_.map(_.typeArguments.map(GroundValueRenderer.renderConstructor)).toSet)

  private def instances(source: String, name: String): IO[Seq[MonomorphicValue]] =
    monos(source).map(_.filter(_.vfqn.name.name === name))

  private def monos(source: String): IO[Seq[MonomorphicValue]] =
    compileFacts(source).map(_.values.collect { case mv: MonomorphicValue => mv }.toSeq)

  private def hasParam(value: GroundValue): Boolean = value match {
    case GroundValue.Param(_, _, _)     => true
    case GroundValue.Structure(_, as, _) => as.exists(hasParam)
    case _                               => false
  }

  /** Compile the program (as module `Test`) over the real base layers and return its fact map, caching per source so
    * the several assertions about one shape share a single compilation.
    */
  private def compileFacts(source: String): IO[Map[CompilerFactKey[?], CompilerFact]] =
    cache.get(source) match {
      case Some(facts) => IO.pure(facts)
      case None        => compile(source).flatTap(facts => IO.delay(cache.put(source, facts)).void)
    }

  private def compile(source: String): IO[Map[CompilerFactKey[?], CompilerFact]] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("eliot-v4-spike-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("eliot-v4-spike-target"))
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
    * [[MonomorphicAmbientCarriersTest]] does — the repo root reaches the forked test JVM via `ELIOT_REPO_ROOT`.
    */
  private def layerPathArgs: List[String] = {
    val repoRoot             =
      Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
    def root(module: String) = repoRoot.resolve(module).resolve("eliot").toString
    List("--path", root("lang"), "--path", root("stdlib"), "--path", root("jvm"))
  }
}

object EffectsV4SeamGroundnessTest {

  /** One compilation per shape, shared by that shape's assertions. */
  private val cache = TrieMap[String, Map[CompilerFactKey[?], CompilerFact]]()

  /** S1 — a `{Console}` block: `echo` names no carrier, so its whole mono key is the one the region binds. */
  private val consoleBlock =
    """def echo: {Console} Unit = {
      |   printLine("a")
      |   printLine("b")
      |}
      |
      |def main: {Console} Unit = echo
      |""".stripMargin

  /** S2 — a `catch` discharge, with one definition (`emit`) called both inside and outside the discharged region, so
    * the same payload key manifests at two different stacks.
    */
  private val catchDischarge =
    """def emit(s: String): {Console} Unit = printLine(s)
      |
      |def parseBad: {Throw[String], Console} String = {
      |   emit("parsing")
      |   raise("malformed input")
      |}
      |
      |def main: {Console} Unit = {
      |   emit("start")
      |   emit(parseBad catch (err -> err))
      |}
      |""".stripMargin

  /** S3 — a computation stored in a `data` field and collected into a `List`, the nearest v3-expressible form of v4's
    * `List[TestCase]` shape (a list element is a payload today, so the row must be pinned into the field's type).
    */
  private val storedComputation =
    """import eliot.collection.List
      |import eliot.lang.Id
      |import eliot.effect.Throw
      |
      |data TestCase(name: String, body: {Throw[String] | Id} Unit)
      |
      |def assertEquals(expected: String, actual: String): {Throw[String]} Unit =
      |   if(expected == actual, unit) else raise("expected '" ++ expected ++ "' but was '" ++ actual ++ "'")
      |
      |def cases: List[TestCase] =
      |   append(append(empty, TestCase("ok", assertEquals("a", "a"))), TestCase("bad", assertEquals("a", "b")))
      |
      |def outcome(test: TestCase): String =
      |   foldEither(e -> "FAIL " ++ e, _ -> "PASS", runId(runThrow(body(test))))
      |
      |def runCase(test: TestCase): {Console} Unit = printLine(name(test) ++ ": " ++ outcome(test))
      |
      |def main: {Console} Unit = cases.foreach(runCase)
      |""".stripMargin
}
