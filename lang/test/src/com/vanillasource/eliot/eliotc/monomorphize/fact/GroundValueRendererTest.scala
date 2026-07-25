package com.vanillasource.eliot.eliotc.monomorphize.fact

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** User-facing rendering of ground types ([[GroundValueRenderer]]) — in particular the effects-as-channel §9 invariant
  * that **no carrier machinery name and no `Id` payload wrapper is ever shown to a user**: a carrier stack renders as
  * the pinned effect row that spells it, and `Id[X]` renders as `X`.
  *
  * The one deliberate exception is an `Id` *row base*: `{Throw[String] | Id} String` is the legal surface a user writes
  * for a stack pinned to the pure base, and it is a different type from the open row `{Throw[String]} String`, so the
  * base is kept rather than suppressed.
  */
class GroundValueRendererTest extends AnyFlatSpec with Matchers {

  private def typeFQN(packages: Seq[String], module: String, name: String): ValueFQN =
    ValueFQN(ModuleName(packages, module), QualifiedName(name, Qualifier.Type))

  private def con(fqn: ValueFQN, args: GroundValue*): GroundValue =
    GroundValue.Structure(fqn, args, GroundValue.Type)

  /** A carrier applied to its ability arguments and base but *not* to a payload — the shape a carrier takes in an
    * `F[_]` slot. Its kind is an arrow, which is exactly how the renderer tells the two apart.
    */
  private def partial(fqn: ValueFQN, args: GroundValue*): GroundValue =
    GroundValue.Structure(fqn, args, con(WellKnownTypes.functionDataTypeFQN, GroundValue.Type, GroundValue.Type))

  private val throwCarrier  = typeFQN(Seq("eliot", "effect"), "Throw", "ThrowCarrier")
  private val stateCarrier  = typeFQN(Seq("eliot", "effect"), "State", "StateCarrier")
  private val abortCarrier  = typeFQN(Seq("eliot", "effect"), "Abort", "AbortCarrier")
  private val writerCarrier = typeFQN(Seq("eliot", "effect"), "Writer", "WriterCarrier")
  private val listFQN       = typeFQN(Seq("eliot", "collection"), "List", "List")
  private val io            = con(typeFQN(Seq("eliot", "jvm"), "IO", "IO"))
  private val id            = con(WellKnownTypes.idFQN)
  private val string        = con(WellKnownTypes.stringFQN)
  private val unit          = con(typeFQN(Seq("eliot", "lang"), "Unit", "Unit"))

  private def render(value: GroundValue): String = GroundValueRenderer.render(value)

  "ground value renderer" should "render a plain type constructor application" in {
    render(con(listFQN, string)) shouldBe "List[String]"
  }

  it should "render a function type as a right-associative arrow" in {
    render(con(WellKnownTypes.functionDataTypeFQN, string, con(listFQN, string))) shouldBe "String -> List[String]"
  }

  it should "erase the identity carrier's payload wrapper" in {
    render(con(WellKnownTypes.idFQN, string)) shouldBe "String"
  }

  it should "erase a nested identity carrier wrapper inside another type" in {
    render(con(listFQN, con(WellKnownTypes.idFQN, string))) shouldBe "List[String]"
  }

  it should "erase the identity carrier wrapper on both sides of an arrow" in {
    val fn = con(WellKnownTypes.functionDataTypeFQN, con(WellKnownTypes.idFQN, string), con(WellKnownTypes.idFQN, unit))
    render(fn) shouldBe "String -> Unit"
  }

  it should "render a single-layer carrier stack as its pinned row" in {
    render(con(throwCarrier, string, io, string)) shouldBe "{Throw[String] | IO} String"
  }

  it should "render an ability-argument-less carrier as a bare row entry" in {
    render(con(abortCarrier, io, string)) shouldBe "{Abort | IO} String"
  }

  it should "keep an Id row base, which is legal surface and not the same type as an open row" in {
    render(con(throwCarrier, string, id, string)) shouldBe "{Throw[String] | Id} String"
  }

  it should "flatten a nested carrier stack leftmost-outermost" in {
    val stack = con(throwCarrier, string, partial(stateCarrier, string, id), unit)
    render(stack) shouldBe "{Throw[String], State[String] | Id} Unit"
  }

  it should "render a payload-unapplied carrier as a row without a payload" in {
    render(partial(abortCarrier, io)) shouldBe "{Abort | IO}"
  }

  it should "recognize any carrier following the naming convention, not a fixed table" in {
    render(con(writerCarrier, con(listFQN, string), io, unit)) shouldBe "{Writer[List[String]] | IO} Unit"
  }

  it should "leave a non-carrier type whose name merely ends in Carrier alone" in {
    render(con(typeFQN(Seq("app"), "Freight", "TruckCarrier"), string)) shouldBe "TruckCarrier[String]"
  }
}
