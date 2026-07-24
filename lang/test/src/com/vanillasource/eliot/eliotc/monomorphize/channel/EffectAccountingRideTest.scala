package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pure ride-test core of the effect accounting (docs/effects-as-channel.md §5, U4-c-0c):
  * [[EffectAccountingProcessor.ridesAmbient]] decides, from a reference's `typeArguments`, its callee's carrier-binder
  * positions, and the value's ambient carrier set, whether the reference performs its effect on the value's *own*
  * ambient carrier — by **exact `GroundValue` equality**. This matrix pins the one rule that makes run, discharge,
  * capture, and the concrete-carrier impl all fall out without exemptions; the impure extraction of its inputs (the
  * fact-fetched positions, the concrete-impl return head) is the wiring at U4-c-0d.
  */
class EffectAccountingRideTest extends AnyFlatSpec with Matchers {

  private val mod                                       = ModuleName(Seq("test"), "M")
  private def tycon(name: String): ValueFQN             = ValueFQN(mod, QualifiedName(name, Qualifier.Default))
  private def con(name: String, args: GroundValue*)     = GroundValue.Structure(tycon(name), args.toSeq, GroundValue.Type)

  private val io                                        = con("IO")
  private val e1                                        = con("E1")
  private val e2                                        = con("E2")
  private val s                                         = con("S")
  private def throwC(e: GroundValue, g: GroundValue)    = con("ThrowCarrier", e, g)
  private def stateC(g: GroundValue)                    = con("StateCarrier", s, g)

  import EffectAccountingProcessor.{referenceCarriers, ridesAmbient}

  "the ride test" should "count an effect op whose carrier binder is the ambient (run)" in {
    ridesAmbient(Seq(io), Set(0), None, Set(io)) shouldBe true
  }

  it should "not count a discharged op on an inner transformer carrier (discharge is structural)" in {
    ridesAmbient(Seq(throwC(e1, io)), Set(0), None, Set(io)) shouldBe false
  }

  it should "not count a captured computation whose concrete stack is not the ambient" in {
    ridesAmbient(Seq(stateC(io)), Set(0), None, Set(io)) shouldBe false
  }

  it should "never count anything against an empty ambient set (no synthetic-entry exemption)" in {
    ridesAmbient(Seq(io), Set(0), None, Set.empty) shouldBe false
  }

  it should "count a binder-less concrete-carrier impl whose fixed carrier is the ambient (Inf[IO])" in {
    ridesAmbient(Seq.empty, Set.empty, Some(io), Set(io)) shouldBe true
  }

  it should "not count a concrete-carrier impl whose fixed carrier is not the ambient" in {
    ridesAmbient(Seq.empty, Set.empty, Some(io), Set(stateC(io))) shouldBe false
  }

  it should "select only the higher-kinded carrier position, never a plain error binder (lifting impl, lifted)" in {
    ridesAmbient(Seq(e1, stateC(io)), Set(1), None, Set(io)) shouldBe false
  }

  it should "count a lifting impl monomorphized at its own transformer stack" in {
    ridesAmbient(Seq(e1, stateC(io)), Set(1), None, Set(stateC(io))) shouldBe true
  }

  it should "count an op riding a pinned/concrete-carrier stack ambient" in {
    ridesAmbient(Seq(stateC(io)), Set(0), None, Set(stateC(io))) shouldBe true
  }

  it should "separate nested same-transformer stacks by exact equality (not a head-level test)" in {
    ridesAmbient(Seq(throwC(e2, throwC(e1, io))), Set(0), None, Set(throwC(e1, io))) shouldBe false
  }

  "referenceCarriers" should "read a binder-less impl's fixed carrier from the concrete-impl arm" in {
    referenceCarriers(Seq.empty, Set.empty, Some(io)) shouldBe Set(io)
  }

  it should "pick only the carrier-binder positions of the type arguments, ignoring the impl arm" in {
    referenceCarriers(Seq(e1, stateC(io)), Set(1), Some(io)) shouldBe Set(stateC(io))
  }
}
