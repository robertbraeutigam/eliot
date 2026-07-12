package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Pins the fragile part of the refinement channel: the FQN-based recognition of the platform arithmetic leaves and
  * their `^Meta` transfer companions. The end-to-end transfer is exercised by the full-layer integration suite
  * (`ExamplesIntegrationTest`), which is the agreement harness the design doc calls for; these unit checks are the
  * tripwire for a leaf name drift.
  */
class RefinementChannelProcessorTest extends AnyFlatSpec with Matchers {
  import RefinementChannelProcessor.*

  "isArithmeticLeaf" should "recognise exactly the three native arithmetic leaves" in {
    Seq(nativeAddFqn, nativeSubtractFqn, nativeMultiplyFqn).map(isArithmeticLeaf) shouldBe Seq(true, true, true)
  }

  it should "not recognise an unrelated Int native" in {
    isArithmeticLeaf(ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "Int"), QualifiedName("nativeWiden", Qualifier.Default))) shouldBe false
  }

  "metaTransferCompanion" should "map each leaf to its ^Meta transfer companion" in {
    Seq(nativeAddFqn, nativeSubtractFqn, nativeMultiplyFqn).map(metaTransferCompanion(_).name) shouldBe
      Seq(
        QualifiedName("rangeAdd", Qualifier.Meta),
        QualifiedName("rangeSubtract", Qualifier.Meta),
        QualifiedName("rangeMultiply", Qualifier.Meta)
      )
  }

  it should "resolve the companion in the eliot.lang.Int module" in {
    metaTransferCompanion(nativeAddFqn).moduleName shouldBe ModuleName(ModuleName.defaultSystemPackage, "Int")
  }

  "metaCompanionFqn" should "name a callee's ^Meta companion in its own module and the Meta namespace" in {
    val fold = ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "Bool"), QualifiedName("fold", Qualifier.Default))
    metaCompanionFqn(fold) shouldBe
      ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "Bool"), QualifiedName("fold", Qualifier.Meta))
  }
}
