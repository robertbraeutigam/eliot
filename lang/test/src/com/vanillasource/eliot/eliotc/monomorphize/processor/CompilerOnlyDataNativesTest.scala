package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.{Spine, VTopDef}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{BindingContribution, ContributedBinding}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.net.URI
import java.nio.file.Path

/** Item 2 (two-pool membership guard): the leaf native contributors read a *platform-keyed* source fact through
  * [[DeclaringPool]]'s quiet name-set probe rather than an unconditional runtime-pool request. So a body-less
  * `Type`-qualified name (the shape [[DataTypeNativesProcessor]] fires on — an abstract `type`, or a `data` type's
  * constructor) declared **only** in the compiler pool still gets its data-type native (via the compiler-pool
  * fallback), and requesting it raises no spurious `UnifiedModuleValueProcessor` "Could not find" error for the runtime
  * pool it is absent from. (Every such name that exists today lives in the runtime pool, so this fallback is latent; the
  * test exercises it directly. The `match` native path uses the same [[DeclaringPool]] helper and is covered end-to-end
  * for the runtime pool by the rest of the suite.)
  */
class CompilerOnlyDataNativesTest extends ProcessorTest(LangProcessors(systemModules = Seq.empty)*) {
  private val moduleName  = ModuleName(Seq.empty, "M")
  private val modulePath  = Path.of("M.els")
  private val compilerUri = URI.create("compilerM.els")

  private val boxType: ValueFQN = ValueFQN(moduleName, QualifiedName("Box", Qualifier.Type))

  private def source(uri: URI, content: String): SourceContent =
    SourceContent(uri, Sourced(uri, PositionRange.zero, content))

  // A body-less `Type`-qualified name (an abstract `type`, the shape `DataTypeNativesProcessor` fires on) declared only
  // in the compiler pool — module M has no runtime PathScan at all.
  private val compilerOnlyFacts = Seq(
    PathScan(modulePath, Seq(compilerUri), Platform.Compiler),
    source(compilerUri, "type Box")
  )

  private def fact[K <: com.vanillasource.eliot.eliotc.processor.CompilerFact](
      trigger: com.vanillasource.eliot.eliotc.processor.CompilerFactKey[K]
  ): IO[(Option[K], Seq[TestError])] =
    runGeneratorWithFacts(compilerOnlyFacts, trigger).map { case (result, errors) => (result, toTestErrors(errors)) }

  "the datatype native contributor" should "contribute the type VTopDef for a compiler-pool-only body-less type" in {
    fact(ContributedBinding.Key(boxType, ContributedBinding.dataTypeLabel))
      .asserting(_._1.flatMap(_.contributed) shouldBe Some(BindingContribution.Leaf(VTopDef(boxType, None, Spine.SNil))))
  }

  it should "raise no spurious 'Could not find' error for the runtime pool the type is absent from" in {
    fact(ContributedBinding.Key(boxType, ContributedBinding.dataTypeLabel)).asserting(_._2 shouldBe Seq.empty)
  }
}
