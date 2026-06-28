package com.vanillasource.eliot.eliotc.module.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.net.URI
import java.nio.file.Path

/** CP1 leaf test: the `platform` marker scopes source unification, so the *same* qualified name can have a distinct
  * concrete definer per platform (the compiler platform and a runtime layer like `jvm`) without the two colliding in one
  * pool. Both pools are supplied as injected [[PathScan]]s, one per marker; only `baseM` is shared (it appears in both
  * pools, modelling the abstract base).
  */
class PlatformScopedUnificationTest extends ProcessorTest(LangProcessors(systemModules = Seq.empty)*) {
  private val moduleName  = ModuleName(Seq.empty, "M")
  private val modulePath  = Path.of("M.els")
  private val compilerUri = URI.create("compilerM.els") // the "compiler platform" file: concrete `foo`
  private val runtimeUri   = URI.create("runtimeM.els")  // the "jvm" file: a *different* concrete `foo` at the same FQN
  private val baseUri      = URI.create("baseM.els")     // the abstract base file, present in *both* pools

  private def fooFqn = ValueFQN(moduleName, QualifiedName("foo", Qualifier.Default))
  private def barFqn = ValueFQN(moduleName, QualifiedName("bar", Qualifier.Default))

  private def source(uri: URI, content: String): SourceContent =
    SourceContent(uri, Sourced(uri, PositionRange.zero, content))

  /** Two distinct pools for module `M`: compiler = {compilerM, baseM}, runtime = {runtimeM, baseM}. */
  private val twoPoolFacts = Seq(
    PathScan(modulePath, Seq(compilerUri, baseUri), Platform.Compiler),
    PathScan(modulePath, Seq(runtimeUri, baseUri), Platform.Runtime),
    source(compilerUri, "def foo: A = b"),
    source(runtimeUri, "def foo: A = b"),
    source(baseUri, "def bar: A")
  )

  private def unify(vfqn: ValueFQN, platform: Platform): IO[(Option[UnifiedModuleValue], Seq[TestError])] =
    runGeneratorWithFacts(twoPoolFacts, UnifiedModuleValue.Key(vfqn, platform))
      .map { case (result, errors) => (result, toTestErrors(errors)) }

  "platform-scoped unification" should "resolve a name concrete in both pools with no error under the compiler marker" in {
    unify(fooFqn, Platform.Compiler).asserting(_._2 shouldBe Seq.empty)
  }

  it should "resolve a name concrete in both pools with no error under the runtime marker" in {
    unify(fooFqn, Platform.Runtime).asserting(_._2 shouldBe Seq.empty)
  }

  it should "pick the compiler platform's concrete definer under the compiler marker" in {
    unify(fooFqn, Platform.Compiler).asserting(_._1.flatMap(_.namedValue.runtime).isDefined shouldBe true)
  }

  it should "carry the requested marker on the unified value" in {
    unify(fooFqn, Platform.Compiler).asserting(_._1.map(_.platform) shouldBe Some(Platform.Compiler))
  }

  it should "resolve a shared base name identically under both markers" in {
    (unify(barFqn, Platform.Compiler), unify(barFqn, Platform.Runtime)).mapN { (compiler, runtime) =>
      compiler._1.map(_.namedValue) shouldBe runtime._1.map(_.namedValue)
    }
  }

  it should "still reject two concrete definers that land in the same pool" in {
    val onePoolFacts = Seq(
      PathScan(modulePath, Seq(compilerUri, runtimeUri), Platform.Runtime),
      source(compilerUri, "def foo: A = b"),
      source(runtimeUri, "def foo: A = b")
    )
    runGeneratorWithFacts(onePoolFacts, UnifiedModuleValue.Key(fooFqn, Platform.Runtime))
      .asserting(_._2.map(_.message) shouldBe Seq("Has multiple implementations."))
  }
}
