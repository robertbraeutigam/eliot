package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.domain.{MetaRole, SemValue}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, NativeBinding}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.plugin.LangProcessors
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue

/** The effect-carrier bookkeeping of the effect-lift plan (docs/effect-lift-in-checker.md, Step 2): the checker
  * records the value-under-check's own *ambient* effect-carrier heads ([[CheckState.ambientCarriers]]) and flags
  * ability-constrained higher-kinded instantiation metas as effect carriers
  * ([[com.vanillasource.eliot.eliotc.monomorphize.domain.MetaRole.Instantiation.effectCarrier]]). Neither is visible on
  * the ordinary `MonomorphicValue` output, so a probe processor runs the same [[TypeStackLoop]] through its
  * [[TypeStackLoop.processWithState]] test seam and captures the final [[CheckState]].
  */
class CarrierBookkeepingTest
    extends ProcessorTest((LangProcessors() :+ CarrierBookkeepingTest.CarrierProbeProcessor())*) {
  import CarrierBookkeepingTest.*

  private val ioFQN               =
    ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "IO"), QualifiedName("IO", Qualifier.Type))
  private val ioType: GroundValue = GroundValue.Structure(ioFQN, Seq.empty, GroundValue.Type)

  private val consoleValue = "import eliot.effect.Console\ndef echo: {Console} Unit = printLine(\"x\")"
  private val bareHktValue =
    "type Box[A]\ndef weird[C[_]](c: C[Unit]): C[Unit]\ndef use(b: Box[Unit]): Box[Unit] = weird(b)"

  "ambient carrier bookkeeping" should "record the concrete IO head for a {Console} value instantiated at IO" in {
    probe(consoleValue, "echo", Seq(ioType))
      .asserting(_.map(_.ambientCarriers) shouldBe Some(Set(CheckState.CarrierHead.TopDef(ioFQN))))
  }

  it should "record no ambient carrier when no binder is ability-constrained" in {
    probe(bareHktValue, "use").asserting(_.map(_.ambientCarriers) shouldBe Some(Set.empty))
  }

  "effect-carrier meta flagging" should "flag the Console ability method's carrier instantiation meta" in {
    probe(consoleValue, "echo", Seq(ioType)).asserting(_.map(_.effectCarrierMetas) shouldBe Some(1))
  }

  it should "flag a bare higher-kinded binder's instantiation meta (the unfiltered callee-side carrier notion)" in {
    // A callee result rides *any* of its own HKT binders — including a deliberately unconstrained `G[_]` like
    // `runState`'s (the effect-transparent discharge combinators); the constraint filter is ambient-only.
    probe(bareHktValue, "use").asserting(_.map(_.effectCarrierMetas) shouldBe Some(1))
  }

  private def probe(source: String, name: String, typeArgs: Seq[GroundValue] = Seq.empty): IO[Option[CarrierProbe]] =
    runGenerator(
      source,
      CarrierProbe.Key(ValueFQN(testModuleName, default(name)), typeArgs),
      ambientStubsWith("IO" -> "type IO[A]")
    ).map(_._2.values.collectFirst { case p: CarrierProbe => p })
}

object CarrierBookkeepingTest {

  /** What the probe captures from the final [[CheckState]]: the ambient effect-carrier heads and the number of
    * instantiation metas flagged as effect carriers.
    */
  case class CarrierProbe(
      vfqn: ValueFQN,
      typeArguments: Seq[GroundValue],
      ambientCarriers: Set[CheckState.CarrierHead],
      effectCarrierMetas: Int
  ) extends CompilerFact {
    override def key(): CompilerFactKey[CarrierProbe] = CarrierProbe.Key(vfqn, typeArguments)
  }

  object CarrierProbe {
    case class Key(vfqn: ValueFQN, typeArguments: Seq[GroundValue]) extends CompilerFactKey[CarrierProbe]
  }

  /** A [[com.vanillasource.eliot.eliotc.monomorphize.processor.MonomorphicTypeCheckProcessor]] twin that runs the same
    * [[TypeStackLoop]] through [[TypeStackLoop.processWithState]] and registers the checker bookkeeping the ordinary
    * output fact does not carry.
    */
  class CarrierProbeProcessor
      extends TransformationProcessor[SaturatedValue.Key, CarrierProbe.Key](key => SaturatedValue.Key(key.vfqn)) {

    private def fetchBinding(vfqn: ValueFQN): CompilerIO[Option[SemValue]] =
      getFact(NativeBinding.Key(vfqn, Platform.Runtime)).map(_.map(_.semValue))

    private def resolveAbilityImpl(
        vfqn: ValueFQN,
        typeArgs: Seq[GroundValue]
    ): CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] =
      getFact(AbilityImplementation.Key(vfqn, typeArgs, Platform.Runtime)).map(
        _.map(impl => (impl.implementationFQN, impl.implementationTypeArgs))
      )

    override protected def generateFromKeyAndFact(
        key: CarrierProbe.Key,
        saturatedValue: SaturatedValue
    ): CompilerIO[CarrierProbe] =
      new TypeStackLoop(fetchBinding, resolveAbilityImpl, Track.Runtime)
        .processWithState(key.typeArguments, saturatedValue.value)
        .map { case (state, _) =>
          CarrierProbe(
            key.vfqn,
            key.typeArguments,
            state.ambientCarriers,
            state.unifier.metaRoles.values.count {
              case i: MetaRole.Instantiation => i.effectCarrier
              case _                         => false
            }
          )
        }
  }
}
