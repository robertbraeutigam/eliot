package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Pins the fragile FQN-based recognition of the representation half of the refinement channel (Step 3): the
  * `Represent::layout` ability-method FQN the channel resolves on the `Interval` domain, and the `Int`-shape guard that
  * routes a type into the channel. The end-to-end layout computation + shadow assertion against the `opaque` body is
  * exercised by the full-layer integration suite (`ExamplesIntegrationTest`'s integer-representation cases), which is the
  * agreement harness; these unit checks are the tripwire for an FQN or `Int`-shape drift.
  */
class RefinementRepresentationTest extends AnyFlatSpec with Matchers {
  import RefinementRepresentation.*

  private def intType(min: BigInt, max: BigInt): GroundValue =
    GroundValue.Structure(
      RefinementChannelProcessor.intTypeFqn,
      Seq(
        GroundValue.Direct(min, bigIntGround),
        GroundValue.Direct(max, bigIntGround)
      ),
      GroundValue.Type
    )

  private val bigIntGround: GroundValue =
    GroundValue.Structure(WellKnownTypes.bigIntFQN, Seq.empty, GroundValue.Type)

  "representLayoutFqn" should "name the Represent ability's layout method in the compiler package" in {
    representLayoutFqn shouldBe ValueFQN(
      ModuleName(ModuleName.compilerPackage, "Represent"),
      QualifiedName("layout", Qualifier.Ability("Represent"))
    )
  }

  "isTrackedIntType" should "recognise the tracked Int type constructor" in {
    isTrackedIntType(intType(0, 255)) shouldBe true
  }

  it should "not recognise a non-Int type" in {
    isTrackedIntType(bigIntGround) shouldBe false
  }

  it should "not recognise Type" in {
    isTrackedIntType(GroundValue.Type) shouldBe false
  }
}
