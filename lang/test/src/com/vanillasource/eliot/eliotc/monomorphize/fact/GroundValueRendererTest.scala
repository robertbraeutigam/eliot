package com.vanillasource.eliot.eliotc.monomorphize.fact

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** User-facing rendering of ground types ([[GroundValueRenderer]]): a type prints the way the user writes it.
  *
  * The carrier-inversion cases this suite used to carry — a canonical stack rendered as the pinned row that spells it,
  * `Id[X]` rendered as `X` — went with the carrier (effects v6, F3). There is no machinery in a type to hide.
  */
class GroundValueRendererTest extends AnyFlatSpec with Matchers {

  private def typeFQN(packages: Seq[String], module: String, name: String): ValueFQN =
    ValueFQN(ModuleName(packages, module), QualifiedName(name, Qualifier.Type))

  private def con(fqn: ValueFQN, args: GroundValue*): GroundValue =
    GroundValue.Structure(fqn, args, GroundValue.Type)

  /** A carrier applied to its ability arguments and base but *not* to a payload — the shape a carrier takes in an
    * `F[_]` slot. Structurally it is an ordinary application: quoted ground values carry `valueType = Type` for
    * *every* structure (a nullary `IO` included), so nothing about the value itself distinguishes it from a fully
    * applied one. Only the reading context does, which is why it is rendered through `renderConstructor`.
    */
  private def constructor(fqn: ValueFQN, args: GroundValue*): GroundValue = con(fqn, args*)

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

  it should "render a value constructor application with parentheses, not brackets" in {
    val boxValue = ValueFQN(ModuleName(Seq("app"), "Boxes"), QualifiedName("Box", Qualifier.Default))
    render(GroundValue.Structure(boxValue, Seq(string), GroundValue.Type)) shouldBe "Box(String)"
  }

  it should "render a function type as a right-associative arrow" in {
    render(con(WellKnownTypes.functionDataTypeFQN, string, con(listFQN, string))) shouldBe "String -> List[String]"
  }

}
