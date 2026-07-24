package com.vanillasource.eliot.eliotc.monomorphize.channel

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pure ride-test core of the effect accounting (docs/effects-as-channel.md §5, U4-c-0c):
  * [[EffectAccountingProcessor.ridesAmbient]] decides whether a reference performs its effect on the value's *own*
  * ambient carrier — its own carriers (the callee's forwarded `MonomorphicValue.ambientCarriers` at the reference's mono
  * key) intersect the value's ambient set, by **exact `GroundValue` equality**. This matrix pins the one rule that makes
  * run, discharge, capture, and the concrete-carrier impl all fall out without exemptions; obtaining the inputs (the
  * caller's ambient off `MonomorphicValue`, each reference's carriers off the callee's `MonomorphicValue`) is the wiring
  * at U4-c-0d.
  */
class EffectAccountingRideTest extends AnyFlatSpec with Matchers {

  private val mod                                   = ModuleName(Seq("test"), "M")
  private def tycon(name: String): ValueFQN         = ValueFQN(mod, QualifiedName(name, Qualifier.Default))
  private def con(name: String, args: GroundValue*) = GroundValue.Structure(tycon(name), args.toSeq, GroundValue.Type)

  private val io                                    = con("IO")
  private val e1                                    = con("E1")
  private val e2                                    = con("E2")
  private val s                                     = con("S")
  private def throwC(e: GroundValue, g: GroundValue) = con("ThrowCarrier", e, g)
  private def stateC(g: GroundValue)                = con("StateCarrier", s, g)

  import EffectAccountingProcessor.ridesAmbient

  "the ride test" should "count a reference whose carrier is the ambient (run)" in {
    ridesAmbient(Set(io), Set(io)) shouldBe true
  }

  it should "not count a captured/discharged reference on an inner transformer carrier" in {
    ridesAmbient(Set(throwC(e1, io)), Set(io)) shouldBe false
  }

  it should "never count anything against an empty ambient set (no synthetic-entry exemption)" in {
    ridesAmbient(Set(io), Set.empty) shouldBe false
  }

  it should "not count a reference with no carrier (a pure / non-effect callee)" in {
    ridesAmbient(Set.empty, Set(io)) shouldBe false
  }

  it should "count when any of the value's several ambient carriers matches" in {
    ridesAmbient(Set(io), Set(io, stateC(io))) shouldBe true
  }

  it should "not count when the reference and ambient carriers are disjoint" in {
    ridesAmbient(Set(stateC(io)), Set(io)) shouldBe false
  }

  it should "count a reference riding a pinned/concrete-carrier stack ambient" in {
    ridesAmbient(Set(stateC(io)), Set(stateC(io))) shouldBe true
  }

  it should "separate nested same-transformer stacks by exact equality (not a head-level test)" in {
    ridesAmbient(Set(throwC(e2, throwC(e1, io))), Set(throwC(e1, io))) shouldBe false
  }
}
