package com.vanillasource.eliot.eliotc.jvm.codec

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.compiler.cache.FactSerialization
import com.vanillasource.eliot.eliotc.compiler.{CompilationSession, Compiler}
import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec
import com.vanillasource.eliot.eliotc.processor.CompilerFact
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** The conformance measurement for `docs/incremental-compilation.md` §13's first step: are explicit codecs a viable
  * replacement for Java serialization in the incremental cache?
  *
  * Coverage is proven by the *compiler* — [[FactCodecs]] names one instance per type reachable from a fact, so a type
  * with no encoding cannot compile rather than silently encoding badly. What compilation cannot decide is answered
  * here, against the facts of a real build:
  *
  *   - **the law** (§2): `read(write(v))` must equal the value. Round-trip inequality is what catches a field compared
  *     by reference (`ClassFile`'s `Array[Byte]`); the other half of §2's defect class — a value that round-trips
  *     equal but *recomputes* different, i.e. `SemValue.VNative`'s lambda — cannot reach here at all, because no
  *     `FactCodec` exists for a function.
  *   - **the size**, which is what §13's premise rests on. The comparison is deliberately unfair to the codecs: their
  *     total is the sum of **independent per-fact frames**, while Java's is **one shared stream** with back-references
  *     and a single class-descriptor table — the very structure sharing §5 measured a 7.5× penalty for giving up. If
  *     independent codec frames still come out smaller, the §5/§12 blowup was class descriptors and framing overhead,
  *     not information, and content addressing (§13) has room to work.
  *   - **completeness**: every fact type a real build materialises must be either covered or explicitly declined.
  */
class FactCodecConformanceTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  private val program =
    """def greet(name: String): String = "hello " ++ name
      |
      |def main: {Console} Unit = printLine(greet("world"))""".stripMargin

  "the explicit codecs" should "cover every fact type a real build materialises" in {
    withFacts(facts => IO.pure(facts.map(_.getClass).toSet.filterNot(covered).map(_.getName) shouldBe declinedNames))
  }

  it should "round-trip every fact it covers back to an equal value" in {
    withFacts(facts => IO.pure(facts.filter(covered.compose(_.getClass)).filterNot(roundTrips).map(named) shouldBe Nil))
  }

  it should "encode a build's facts smaller than Java serialization, once sub-values are shared" in {
    withFacts { facts =>
      val encodable = facts.filter(fact => covered(fact.getClass) && FactSerialization.canSerialize(fact))
      val frames    = encodable.map(encodedSize).sum
      val shared    = FactCodec.toSharedBytes[Any](encodable)(using anyCodec(encodable)).length
      val java      = FactSerialization.toBytes(encodable).length

      report(encodable, frames, shared, java).as(shared should be < java)
    }
  }

  /** One codec over a heterogeneous fact list: a leading varint picks the per-type codec. The tag costs a byte per
    * fact, which is the honest overhead of putting differently-typed facts in one stream.
    */
  private def anyCodec(facts: Seq[CompilerFact]): FactCodec[Any] = {
    val types = facts.map(_.getClass).distinct
    FactCodec.instance[Any](
      (out, value) => {
        val index = types.indexOf(value.getClass)
        FactCodec[Int].write(out, index)
        FactCodecs.registry(types(index)).write(out, value)
      },
      in => FactCodecs.registry(types(FactCodec[Int].read(in))).read(in)
    )
  }

  private def covered(factClass: Class[?]): Boolean = FactCodecs.registry.contains(factClass)

  private def declinedNames: Set[String] = FactCodecs.declined.keySet

  private def named(fact: CompilerFact): String = fact.getClass.getName

  private def encodedSize(fact: CompilerFact): Int = FactCodec.toBytes[Any](fact)(using FactCodecs.registry(fact.getClass)).length

  private def roundTrips(fact: CompilerFact): Boolean =
    try {
      val codec = FactCodecs.registry(fact.getClass)
      FactCodec.fromBytes[Any](FactCodec.toBytes[Any](fact)(using codec))(using codec) == fact
    } catch { case _: Exception => false }

  /** Print the per-type breakdown the measurement is for; the assertions above are the part that must not regress. */
  private def report(facts: Seq[CompilerFact], frames: Int, shared: Int, java: Int): IO[Unit] = IO.delay {
    info(f"facts encoded: ${facts.size}%,d")
    info(f"java serialization, one shared graph:   ${java}%,12d bytes  (baseline)")
    info(f"explicit codecs, independent frames:    ${frames}%,12d bytes  ${frames.toDouble / java}%.2f×")
    info(f"explicit codecs, shared sub-values:     ${shared}%,12d bytes  ${shared.toDouble / java}%.2f×")
    facts
      .groupBy(named)
      .view
      .mapValues(group => (group.size, group.map(encodedSize).sum))
      .toSeq
      .sortBy { case (_, (_, bytes)) => -bytes }
      .take(8)
      .foreach { case (name, (count, bytes)) => info(f"  $bytes%,9d B  $count%,5d ×  ${name.split('.').last}") }
  }

  /** Compile a small program in a dedicated session and hand every materialised fact to `use`. */
  private def withFacts(use: Seq[CompilerFact] => IO[Assertion]): IO[Assertion] =
    for {
      sourceDir  <- IO.blocking(Files.createTempDirectory("eliot-codec-src"))
      targetDir  <- IO.blocking(Files.createTempDirectory("eliot-codec-target"))
      _          <- IO.blocking(Files.writeString(sourceDir.resolve("Test.els"), program))
      args        = List("jvm", "exe-jar", sourceDir.toString, "-o", targetDir.toString, "-m", "Test") ++ layerPathArgs
      sessionOpt <- Compiler.createSession(args)
      session    <- IO.fromOption(sessionOpt)(new IllegalStateException("Could not create the compilation session."))
      result     <- session.compileOnce()
      cache      <- result.generator.buildCacheData()
      assertion  <- use(cache.entries.values.flatMap(_.value).toSeq)
    } yield assertion

  private def layerPathArgs: List[String] = {
    val repoRoot             =
      Path.of(Option(System.getenv("ELIOT_REPO_ROOT")).getOrElse(System.getProperty("user.dir")))
    def root(module: String) = repoRoot.resolve(module).resolve("eliot").toString
    List("--path", root("lang"), "--path", root("stdlib"), "--path", root("jvm"))
  }
}
