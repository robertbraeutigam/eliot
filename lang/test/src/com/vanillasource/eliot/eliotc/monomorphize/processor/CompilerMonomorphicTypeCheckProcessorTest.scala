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

/** The compiler track (Increment A): [[CompilerMonomorphicTypeCheckProcessor]] runs the shared checker over a value's
  * `Platform.Compiler` [[com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue]] and produces a
  * [[CompilerMonomorphicValue]], the compiler-platform analogue of
  * [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue]]. `foo` is defined in *both* pools with a
  * distinct body; `d` exists only in the runtime pool — so the compiler track is shown to read the compiler pool
  * specifically (it produces `foo` but not the runtime-only `d`).
  */
class CompilerMonomorphicTypeCheckProcessorTest extends ProcessorTest(LangProcessors(systemModules = Seq.empty)*) {
  private val moduleName  = ModuleName(Seq.empty, "M")
  private val modulePath  = Path.of("M.els")
  private val compilerUri = URI.create("compilerM.els")
  private val runtimeUri  = URI.create("runtimeM.els")

  private def fqn(name: String) = ValueFQN(moduleName, QualifiedName(name, Qualifier.Default))

  private def source(uri: URI, content: String): SourceContent =
    SourceContent(uri, Sourced(uri, PositionRange.zero, content))

  private val twoPoolFacts = Seq(
    PathScan(modulePath, Seq(compilerUri), Platform.Compiler),
    PathScan(modulePath, Seq(runtimeUri), Platform.Runtime),
    source(compilerUri, "type A\ndef b: A\ndef foo: A = b"),
    source(runtimeUri, "type A\ndef c: A\ndef d: A\ndef foo: A = c")
  )

  private def compilerMono(name: String): IO[(Option[CompilerMonomorphicValue], Seq[TestError])] =
    runGeneratorWithFacts(twoPoolFacts, CompilerMonomorphicValue.Key(fqn(name), Seq.empty)).map {
      case (result, errors) => (result, toTestErrors(errors))
    }

  "the compiler track" should "produce a CompilerMonomorphicValue for a value defined in the compiler pool" in {
    compilerMono("foo").asserting(_._1.map(_.vfqn) shouldBe Some(fqn("foo")))
  }

  it should "report no errors checking a compiler-pool value" in {
    compilerMono("foo").asserting(_._2 shouldBe Seq.empty)
  }

  it should "not produce one for a name absent from the compiler pool (runtime-only)" in {
    compilerMono("d").asserting(_._1 shouldBe None)
  }
}
