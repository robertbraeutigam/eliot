package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntRepresentationTest extends AnyFlatSpec with Matchers {

  private def jvm(name: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Int"), QualifiedName(name, Qualifier.Default))

  private val bigIntType: GroundValue =
    GroundValue.Structure(
      ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("BigInteger", Qualifier.Type)),
      Seq.empty,
      GroundValue.Type
    )

  /** The meta the channel produces for an `Int` node: `Int$Meta(Interval(min, max))` — the `$Meta` wrapper the backend
    * unwraps. (`intervalBounds`/`representationFor` decode this whole shape, not a bare `Interval`.)
    */
  private def interval(min: BigInt, max: BigInt): GroundValue =
    GroundValue.Structure(
      ValueFQN(ModuleName(defaultSystemPackage, "Int"), QualifiedName("Int$Meta", Qualifier.Default)),
      Seq(
        GroundValue.Structure(
          ValueFQN(ModuleName(defaultSystemPackage, "Interval"), QualifiedName("Interval", Qualifier.Default)),
          Seq(GroundValue.Direct(min, bigIntType), GroundValue.Direct(max, bigIntType)),
          GroundValue.Type
        )
      ),
      GroundValue.Type
    )

  "widthOf" should "pick JvmByte for a range within the signed byte bounds" in {
    IntRepresentation.widthOf(0, 127) shouldBe jvm("JvmByte")
  }

  it should "pick JvmByte for the full signed byte range" in {
    IntRepresentation.widthOf(-128, 127) shouldBe jvm("JvmByte")
  }

  it should "step up to JvmShort just past the byte max" in {
    IntRepresentation.widthOf(0, 128) shouldBe jvm("JvmShort")
  }

  it should "step up to JvmShort just past the byte min" in {
    IntRepresentation.widthOf(-129, 0) shouldBe jvm("JvmShort")
  }

  it should "pick JvmShort at the signed short bounds" in {
    IntRepresentation.widthOf(-32768, 32767) shouldBe jvm("JvmShort")
  }

  it should "step up to JvmInt just past the short max" in {
    IntRepresentation.widthOf(0, 32768) shouldBe jvm("JvmInt")
  }

  it should "pick JvmInt at the signed 32-bit bounds" in {
    IntRepresentation.widthOf(-2147483648L, 2147483647L) shouldBe jvm("JvmInt")
  }

  it should "step up to JvmLong just past the 32-bit max" in {
    IntRepresentation.widthOf(0, 2147483648L) shouldBe jvm("JvmLong")
  }

  it should "pick JvmLong at the signed 64-bit bounds" in {
    IntRepresentation.widthOf(Long.MinValue, Long.MaxValue) shouldBe jvm("JvmLong")
  }

  it should "fall back to JvmBigInteger just past the 64-bit max" in {
    IntRepresentation.widthOf(0, BigInt(Long.MaxValue) + 1) shouldBe jvm("JvmBigInteger")
  }

  it should "fall back to JvmBigInteger just past the 64-bit min" in {
    IntRepresentation.widthOf(BigInt(Long.MinValue) - 1, 0) shouldBe jvm("JvmBigInteger")
  }

  "representationFor" should "decode a present interval meta" in {
    IntRepresentation.representationFor(Some(interval(0, 200))) shouldBe jvm("JvmShort")
  }

  it should "be JvmBigInteger for an absent (top) meta" in {
    IntRepresentation.representationFor(None) shouldBe jvm("JvmBigInteger")
  }

  "intervalBounds" should "read the endpoints of an Interval meta value" in {
    IntRepresentation.intervalBounds(interval(3, 9)) shouldBe Some((BigInt(3), BigInt(9)))
  }

  it should "be None for a non-interval structure" in {
    IntRepresentation.intervalBounds(GroundValue.Type) shouldBe None
  }
}
