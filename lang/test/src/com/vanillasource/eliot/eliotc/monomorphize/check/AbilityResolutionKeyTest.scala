package com.vanillasource.eliot.eliotc.monomorphize.check

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

/** The identity of a resolved ability reference ([[CheckState.AbilityResolutionKey]] and
  * [[CheckState.lookupAbilityResolution]]).
  *
  * A source position is *not* a node identity: compiler-synthesized effect machinery inherits the span of the
  * expression it wraps (`RowElaborator`'s `pureWrap`/`bindNodes` build their reference with `value.as(...)`), so one
  * source expression wrapped at two carriers yields two `Effect::pure` references with identical position and FQN and
  * different carriers. Keyed by position alone they collapse to one entry and the second silently inherits the first
  * one's impl — a `pure@Effect[Id]` emitted as `pure@Effect[IO]`, which `IdNormalizer` cannot then erase, shipping an
  * `IO` value into a slot typed `Id[Unit]` (a `ClassCastException` at runtime, no diagnostic anywhere). The
  * ability-level type arguments are therefore part of the key.
  */
class AbilityResolutionKeyTest extends AnyFlatSpec with Matchers {

  private val uri = URI.create("Test.els")

  /** Both machinery references sit at the same span — this is the whole point. */
  private def sameSpan[A](a: A): Sourced[A] = Sourced(uri, PositionRange.zero, a)

  private def carrier(module: String, name: String): GroundValue =
    GroundValue.Structure(
      ValueFQN(ModuleName(Seq("eliot", module), name), QualifiedName(name, Qualifier.Type)),
      Seq.empty,
      GroundValue.Type
    )

  private val idCarrier: GroundValue = carrier("lang", "Id")
  private val ioCarrier: GroundValue = carrier("jvm", "IO")

  private def impl(name: String): (ValueFQN, Seq[GroundValue]) =
    (ValueFQN(ModuleName(Seq("eliot", "test"), "Impls"), QualifiedName(name, Qualifier.Default)), Seq.empty)

  private val pureRef  = sameSpan(WellKnownTypes.effectPureFQN)
  private val idImpl   = impl("pureId")
  private val ioImpl   = impl("pureIO")
  private val collided = Map(
    (pureRef, Option(Seq(idCarrier))) -> idImpl,
    (pureRef, Option(Seq(ioCarrier))) -> ioImpl
  )

  private def lookup(
      resolutions: Map[CheckState.AbilityResolutionKey, (ValueFQN, Seq[GroundValue])],
      args: Option[Seq[GroundValue]]
  ) = CheckState.lookupAbilityResolution(resolutions, pureRef, args)

  "two machinery references at one span" should "each resolve to the impl of their own carrier" in {
    (lookup(collided, Some(Seq(idCarrier))), lookup(collided, Some(Seq(ioCarrier)))) shouldBe
      (Some(idImpl), Some(ioImpl))
  }

  it should "decline rather than guess when the reader cannot derive the carrier" in {
    lookup(collided, None) shouldBe None
  }

  "a reference resolved from its own ground arguments" should "be found by those arguments" in {
    lookup(Map((pureRef, Option(Seq(ioCarrier))) -> ioImpl), Some(Seq(ioCarrier))) shouldBe Some(ioImpl)
  }

  it should "still be found when the reader cannot derive the arguments, being the only entry at that span" in {
    lookup(Map((pureRef, Option(Seq(ioCarrier))) -> ioImpl), None) shouldBe Some(ioImpl)
  }

  "a constraint-covered resolution" should "be found whatever arguments the reader derives" in {
    val constraintCovered = Map[CheckState.AbilityResolutionKey, (ValueFQN, Seq[GroundValue])]((pureRef, None) -> ioImpl)
    (lookup(constraintCovered, Some(Seq(idCarrier))), lookup(constraintCovered, None)) shouldBe
      (Some(ioImpl), Some(ioImpl))
  }

  "an unresolved reference" should "be absent" in {
    lookup(Map.empty, Some(Seq(ioCarrier))) shouldBe None
  }
}
