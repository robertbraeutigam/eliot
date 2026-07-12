package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Pins the channel's one recognition point: a refinement operation is a callee with a `^Meta` companion, found by
  * `metaCompanionFqn` uniformly — an arithmetic transfer (`nativeAdd` ⤳ `nativeAdd^Meta`) and a branch merge (`fold`
  * ⤳ `fold^Meta`) go through the same lookup, with no leaf FQN list. The end-to-end transfer/merge is exercised by the
  * full-layer integration suite (`ExamplesIntegrationTest`).
  */
class RefinementChannelProcessorTest extends AnyFlatSpec with Matchers {
  import RefinementChannelProcessor.*

  "metaCompanionFqn" should "name a merge callee's ^Meta companion in its own module and the Meta namespace" in {
    val fold = ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "Bool"), QualifiedName("fold", Qualifier.Default))
    metaCompanionFqn(fold) shouldBe
      ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "Bool"), QualifiedName("fold", Qualifier.Meta))
  }

  it should "name an arithmetic transfer callee's ^Meta companion the same way (no leaf special-case)" in {
    val nativeAdd = ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "Int"), QualifiedName("nativeAdd", Qualifier.Default))
    metaCompanionFqn(nativeAdd) shouldBe
      ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "Int"), QualifiedName("nativeAdd", Qualifier.Meta))
  }
}
