package com.vanillasource.eliot.eliotc.monomorphize.check

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Reading a phantom binder's value back from a reference's ground type arguments (effects v6, `docs/effects.md`
  * §10.1 step 6): the `Default` sentinel, an implementation-headed structure, and the positional split that takes the
  * binding off the end of the ability-level arguments and leaves everything else as the pattern.
  */
class ImplementationBindingTest extends AnyFlatSpec with Matchers {

  private val testModule = ModuleName(Seq("eliot", "test"), "Impls")

  private def tpe(name: String): GroundValue =
    GroundValue.Structure(
      ValueFQN(ModuleName(Seq("eliot", "lang"), name), QualifiedName(name, Qualifier.Type)),
      Seq.empty,
      GroundValue.Type
    )

  private val string = tpe("String")
  private val int    = tpe("Int")

  private val namedQualifier = Qualifier.AbilityImplementation("Throw", "E", Some("loggingThrow"))
  private val marker         = ValueFQN(testModule, QualifiedName("Throw", namedQualifier))
  private val implementation = GroundValue.Structure(marker, Seq(string), GroundValue.Type)

  "the Default sentinel" should "read back as Default" in {
    ImplementationBinding.fromGround(ImplementationBinding.defaultGround) shouldBe Some(ImplementationBinding.Default)
  }

  "an implementation-headed structure" should "read back as that implementation at its own type arguments" in {
    ImplementationBinding.fromGround(implementation) shouldBe
      Some(ImplementationBinding.Implementation(marker, Seq(string)))
  }

  "an ordinary type argument" should "not read as a binding" in {
    ImplementationBinding.fromGround(string) shouldBe None
  }

  it should "not read as a binding when it is an associated type of an implementation (same namespace, not the marker)" in {
    val associated = ValueFQN(testModule, QualifiedName("AddResult", namedQualifier))
    ImplementationBinding.fromGround(GroundValue.Structure(associated, Seq.empty, GroundValue.Type)) shouldBe None
  }

  it should "not read as a binding when it is Type itself" in {
    ImplementationBinding.fromGround(GroundValue.Type) shouldBe None
  }

  "split" should "take a trailing Default off the pattern arguments" in {
    ImplementationBinding.split(Seq(string, ImplementationBinding.defaultGround)) shouldBe
      ((Seq(string), Some(ImplementationBinding.Default)))
  }

  it should "take a trailing implementation off the pattern arguments" in {
    ImplementationBinding.split(Seq(string, int, implementation)) shouldBe
      ((Seq(string, int), Some(ImplementationBinding.Implementation(marker, Seq(string)))))
  }

  it should "leave an argument list with no binding whole (the tree as it stands)" in {
    ImplementationBinding.split(Seq(string, int)) shouldBe ((Seq(string, int), None))
  }

  it should "leave an empty argument list whole" in {
    ImplementationBinding.split(Seq.empty) shouldBe ((Seq.empty, None))
  }

  "an implementation's method" should "be that name in the implementation's own module and namespace" in {
    ImplementationBinding.Implementation(marker, Seq.empty).method("raise") shouldBe
      ValueFQN(testModule, QualifiedName("raise", namedQualifier))
  }
}
