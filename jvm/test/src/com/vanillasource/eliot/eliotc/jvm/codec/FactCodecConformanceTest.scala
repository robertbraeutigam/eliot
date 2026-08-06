package com.vanillasource.eliot.eliotc.jvm.codec

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.compiler.cache.FactSerialization
import com.vanillasource.eliot.eliotc.codec.LangFactCodecs
import com.vanillasource.eliot.eliotc.compiler.cache.codec.{
  ContentAddressedInput,
  ContentAddressedOutput,
  CoreFactCodecs,
  FactCodec,
  FactKeyCodecs
}
import com.vanillasource.eliot.eliotc.compiler.Compiler
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

/** The conformance measurement for `docs/incremental-compilation.md` §13: are explicit codecs a viable replacement for
  * Java serialization in the incremental cache?
  *
  * Coverage is proven by the *compiler*, twice over, so nothing here has to check it. `CompilerFactKey.valueCodec` is
  * abstract with no default, so a fact type that states no persistence decision does not compile; and the structural
  * instances (`LangFactCodecs` and its layer peers) are named per type, so a field whose type has no encoding does not
  * compile either. What compilation cannot decide is answered here, against the facts of a real build:
  *
  *   - **the law** (§2): `read(write(v))` must equal the value. Round-trip inequality is what catches a field compared
  *     by reference (`ClassFile`'s `Array[Byte]`, §14); the other half of §2's defect class — a value that round-trips
  *     equal but *recomputes* different, i.e. `SemValue.VNative`'s lambda — cannot reach here at all, because no
  *     `FactCodec` exists for a function. That is the property the whole approach turns on: a reflective walker would
  *     encode both silently.
  *   - **the size**, which is what §13's premise rests on. Both encodings are compared against Java's *one shared
  *     graph*: the codecs' independent per-fact frames (the §5 layout) and the same codecs writing into one stream
  *     that deduplicates equal sub-values (the byte-level model of the content-addressed store). The gap between those
  *     two is the finding — structure sharing, not class descriptors, is what the size depends on.
  *   - **the declines**: exactly the fact types that cannot be equality-stable, and no others, may decline.
  */
class FactCodecConformanceTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  private val program =
    """def greet(name: String): String = "hello " ++ name
      |
      |def main: {Console} Unit = printLine(greet("world"))""".stripMargin

  private val expectedDeclines = Set("ContributedBinding", "NativeBinding")

  "the fact model" should "decline to persist only the values that cannot be equality-stable" in {
    withFacts(facts => IO.pure(facts.filter(_.key().valueCodec.isEmpty).map(simpleName).toSet shouldBe expectedDeclines))
  }

  "the explicit codecs" should "round-trip every fact they cover back to an equal value" in {
    withFacts(facts => IO.pure(facts.filterNot(roundTrips).map(simpleName).distinct shouldBe Nil))
  }

  it should "encode a build's facts smaller than Java serialization, once sub-values are shared" in {
    withFacts { facts =>
      val encodable = facts.filter(fact => fact.key().valueCodec.isDefined && FactSerialization.canSerialize(fact))
      val frames    = encodable.map(encodedSize).sum
      val shared    = FactCodec.toSharedBytes(encodable)(using taggedCodec(encodable)).length
      val java      = FactSerialization.toBytes(encodable).length

      report(encodable, frames, shared, java).as(shared should be < java)
    }
  }

  /** The guard §16 asked for. `CompilerFactKey.valueCodec` is compile-time-complete for a fact's *value*; a key's own
    * codec cannot be stated that way, because decoding a dependency has no key to read it from. The tag table is
    * therefore a runtime map, and this is what makes an omission loud: every key type a real build materialises must
    * be registered. Only that direction is checkable here — a build exercises a subset of the fact model, so a
    * registration with no live key type is not evidence of anything.
    */
  "the key tag table" should "cover every fact key type a build materialises" in {
    withFacts(facts => IO.pure(facts.map(fact => FactKeyCodecs.nameOf(fact.key())).toSet -- keyCodecs.keySet shouldBe Set.empty))
  }

  it should "read back every key it registers" in {
    withFacts(facts => IO.pure(facts.map(_.key()).distinct.filterNot(roundTripsAsKey) shouldBe Nil))
  }

  private val keyCodecs: FactKeyCodecs.Registry = CoreFactCodecs.keyCodecs ++ LangFactCodecs.keyCodecs ++ JvmFactCodecs.keyCodecs

  private def roundTripsAsKey(key: CompilerFactKey[?]): Boolean = {
    val codec = keyCodecs(FactKeyCodecs.nameOf(key))

    FactCodec.fromBytes(FactCodec.toBytes(key)(using codec))(using codec) == key
  }

  /** The store the codecs are actually for (§17). Both properties are checked here because they are one design: the
    * objects are content-addressed so that equal subtrees collapse, and a fact is read back by following byte offsets
    * so that nothing is decoded unless something asks for it.
    *
    * The size is measured against the *shared stream*, not against Java, because §14 already established that as the
    * bar: it is the encoding with in-stream back-references, which is the most a single-stream format can share. A
    * store that beats it does so by deduplicating across the whole build rather than within one write.
    */
  it should "round-trip a build's facts through a content-addressed object store" in {
    withFacts { facts =>
      val encodable = facts.filter(fact => fact.key().valueCodec.isDefined && FactSerialization.canSerialize(fact))
      val restored  = roundTripThroughStore(encodable)

      IO.pure(restored.zip(encodable).filterNot(pair => pair._1 == pair._2).map(pair => simpleName(pair._2)) shouldBe Nil)
    }
  }

  it should "store a build's facts in less space than one shared stream" in {
    withFacts { facts =>
      val encodable = facts.filter(fact => fact.key().valueCodec.isDefined && FactSerialization.canSerialize(fact))
      val shared    = FactCodec.toSharedBytes(encodable)(using taggedCodec(encodable)).length
      val store     = storeOf(encodable)

      reportStore(shared, store).as(store.appendedBytes.length should be < shared)
    }
  }

  private def storeOf(facts: Seq[CompilerFact]): ContentAddressedOutput = {
    val store = ContentAddressedOutput.empty

    facts.foreach(fact => store.write(fact)(using codecOf(fact)))
    store
  }

  private def roundTripThroughStore(facts: Seq[CompilerFact]): Seq[CompilerFact] = {
    val store   = ContentAddressedOutput.empty
    val offsets = facts.map(fact => store.write(fact)(using codecOf(fact)))
    val input   = new ContentAddressedInput(store.appendedBytes)

    offsets.zip(facts).map((offset, fact) => input.read(offset)(using codecOf(fact)))
  }

  private def reportStore(shared: Int, store: ContentAddressedOutput): IO[Unit] = IO.delay {
    val stored = store.appendedBytes.length

    info(f"explicit codecs, shared sub-values:     ${shared}%,12d bytes  (baseline)")
    info(f"content-addressed object store:         ${stored}%,12d bytes  ${stored.toDouble / shared}%.2f×")
    info(f"distinct objects:                       ${store.objectOffsets.size}%,12d")
  }

  /** One write-only codec over a heterogeneous fact list, so the whole build goes into a single sharing stream. The
    * leading tag costs about a byte per fact — the honest overhead of mixing fact types in one stream. Reading needs a
    * *stable* tag rather than this run's ordering, which is the store's job (§13), not the measurement's.
    */
  private def taggedCodec(facts: Seq[CompilerFact]): FactCodec[CompilerFact] = {
    val types = facts.map(_.getClass).distinct

    FactCodec.instance[CompilerFact](
      (out, value) => {
        FactCodec[Int].write(out, types.indexOf(value.getClass))
        codecOf(value).write(out, value)
      },
      _ => throw new UnsupportedOperationException("The tagged codec is write-only.")
    )
  }

  /** A fact's own codec, as its key states it. Widened because the harness holds facts erased to [[CompilerFact]]. */
  private def codecOf(fact: CompilerFact): FactCodec[CompilerFact] =
    fact
      .key()
      .valueCodec
      .getOrElse(fail(s"${simpleName(fact)} declines to persist"))
      .asInstanceOf[FactCodec[CompilerFact]]

  private def simpleName(fact: CompilerFact): String = fact.getClass.getSimpleName

  private def encodedSize(fact: CompilerFact): Int = FactCodec.toBytes(fact)(using codecOf(fact)).length

  private def roundTrips(fact: CompilerFact): Boolean =
    fact.key().valueCodec.isEmpty || {
      val codec = codecOf(fact)
      FactCodec.fromBytes(FactCodec.toBytes(fact)(using codec))(using codec) == fact
    }

  /** Print the per-type breakdown the measurement is for; the assertions above are the part that must not regress. */
  private def report(facts: Seq[CompilerFact], frames: Int, shared: Int, java: Int): IO[Unit] = IO.delay {
    info(f"facts encoded: ${facts.size}%,d")
    info(f"java serialization, one shared graph:   ${java}%,12d bytes  (baseline)")
    info(f"explicit codecs, independent frames:    ${frames}%,12d bytes  ${frames.toDouble / java}%.2f×")
    info(f"explicit codecs, shared sub-values:     ${shared}%,12d bytes  ${shared.toDouble / java}%.2f×")
    facts
      .groupBy(simpleName)
      .view
      .mapValues(group => (group.size, group.map(encodedSize).sum))
      .toSeq
      .sortBy { case (_, (_, bytes)) => -bytes }
      .take(8)
      .foreach { case (name, (count, bytes)) => info(f"  $bytes%,9d B  $count%,5d ×  $name") }
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
