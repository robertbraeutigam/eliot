package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.CompilerMonomorphicValue
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.net.URI
import java.nio.file.Path

/** Compiler-as-platform Increment C, step (c): the **native-leaf boundary**. Reducing compile-time code must bottom out
  * in a compiler-platform leaf. A name that is **runtime-concrete but compiler-abstract** — its only realization a
  * runtime (jvm) body or a runtime native — is a *runtime-only value*, and reaching it while checking a compiler-track
  * value is a hard error, not a silent stuck fallback (which is correct only for the runtime track, whose backend emits
  * the stuck call). This closes the fail-safe gap the cornerstone forbids: a stuck runtime native at compile time would
  * otherwise silently corrupt the type it was meant to compute.
  *
  * The trigger is precise: a name body-less on *both* platforms (an abstract `type` constructor, an unresolved
  * obligation) is a legitimate compile-time normal form and is NOT flagged; only *runtime-concrete, compiler-abstract*
  * is.
  */
class CompilerNativeLeafBoundaryTest extends ProcessorTest(LangProcessors(systemModules = Seq.empty)*) {

  /** Register a module's source into exactly one platform pool. The URI embeds the platform so the same module path can
    * carry a different body per pool (the abstract-base / concrete-jvm split, in miniature).
    */
  private def scan(platform: Platform, pkg: Seq[String], name: String, content: String): Seq[SourceContent | PathScan] = {
    val path = (pkg :+ s"$name.els").foldLeft(Path.of(""))(_ `resolve` _)
    val uri  = URI.create((platform.toString.toLowerCase +: (pkg :+ s"$name.els")).mkString("/"))
    Seq(
      PathScan(path, Seq(uri), platform),
      SourceContent(uri, Sourced(uri, PositionRange.zero, content))
    )
  }

  private val facts: Seq[com.vanillasource.eliot.eliotc.processor.CompilerFact] =
    // `String` is a shared abstract base type — on both pools.
    scan(Platform.Compiler, Seq("eliot", "lang"), "String", "type String") ++
      scan(Platform.Runtime, Seq("eliot", "lang"), "String", "type String") ++
      // `runtimeOnly`: abstract (body-less) on the compiler platform, concrete (a runtime body) on the runtime platform.
      // This is the shape of a jvm-only leaf — realized only where code actually runs.
      scan(Platform.Compiler, Seq("test"), "Native", "import eliot.lang.String\ndef runtimeOnly: String") ++
      scan(Platform.Runtime, Seq("test"), "Native", "import eliot.lang.String\ndef runtimeOnly: String = \"rt\"") ++
      // `compilerLocal`: concrete on the compiler platform — a legitimate compile-time leaf.
      scan(Platform.Compiler, Seq("test"), "Local", "import eliot.lang.String\ndef compilerLocal: String = \"ct\"") ++
      // The compiler-track values under test: one reaches the runtime-only leaf, one a compiler-concrete value.
      scan(
        Platform.Compiler,
        Seq("test"),
        "M",
        """import eliot.lang.String
          |import test.Native
          |import test.Local
          |
          |def usesRuntimeOnly: String = runtimeOnly
          |def usesCompilerLocal: String = compilerLocal
          |""".stripMargin
      )

  private def fqn(name: String) = ValueFQN(ModuleName(Seq("test"), "M"), QualifiedName(name, Qualifier.Default))

  private def errorsOf(name: String): IO[Seq[TestError]] =
    runGeneratorWithFacts(facts, CompilerMonomorphicValue.Key(fqn(name), Seq.empty)).map { case (_, e) => toTestErrors(e) }

  private def resultOf(name: String): IO[Option[CompilerMonomorphicValue]] =
    runGeneratorWithFacts(facts, CompilerMonomorphicValue.Key(fqn(name), Seq.empty)).map(_._1)

  "the native-leaf boundary (Increment C)" should "reject a compiler-track value reaching a runtime-only leaf" in {
    errorsOf("usesRuntimeOnly").asserting(
      _ shouldBe Seq("Cannot use runtime-only value 'runtimeOnly' at compile time." at "usesRuntimeOnly")
    )
  }

  it should "produce no compiler-monomorphic value for the rejected value (fail-safe by construction)" in {
    resultOf("usesRuntimeOnly").asserting(_ shouldBe None)
  }

  it should "accept a compiler-track value reaching a compiler-concrete leaf" in {
    errorsOf("usesCompilerLocal").asserting(_ shouldBe Seq.empty)
  }
}
