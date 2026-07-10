package com.vanillasource.eliot.eliotc.monomorphize.channel

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.domain.MetaStore
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.monomorphize.processor.ReducedBindingClosure
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

/** The representation half of the refinement channel — Step 3 of `docs/bounds-as-refinements.md`: derive an `Int`'s
  * machine layout from its value-range refinement instead of from the hidden `opaque type Int` body.
  *
  * Given a ground `Int[min, max]` type it computes the layout by resolving the compiler-pool `Represent[Interval[…]]`
  * instance (the platform's representation policy, written in Eliot as the same `fitsByte/…` fold the `opaque` body
  * performs) and evaluating its `layout` body on the interval **value** `Interval(min, max)` through the one NbE
  * evaluator — the `RefinementChannelProcessor` transfer/join pattern re-pointed at `Represent.layout`. The result is a
  * representation type (`JvmByte`/…), exactly what the `opaque` unfold produces.
  *
  * [[com.vanillasource.eliot.eliotc.monomorphize.lowering.RepresentationLowering]] drives the Step-3 agreement check:
  * it computes the layout this way *and* by unfolding the `opaque` body, asserts they agree, and consumes this
  * channel-derived one. When no platform `Represent` instance is on the path (e.g. a bare representation-bearing `Int`
  * stub in a unit test with no `eliot-compiler/` overlay) [[channelLayout]] returns [[None]] and the lowering falls back
  * to the `opaque` unfold — reduced coverage, never a false accept.
  */
object RefinementRepresentation {

  /** `eliot.compiler.Represent::layout` — the ability method resolved on the `Interval` domain (§4.4). */
  private[channel] val representLayoutFqn: ValueFQN =
    ValueFQN(ModuleName(ModuleName.compilerPackage, "Represent"), QualifiedName("layout", Qualifier.Ability("Represent")))

  /** Whether `gv` is the tracked `Int` type constructor — the one type the channel derives a representation for. Its
    * bounds need not be extractable here; [[channelLayout]] returns [[None]] (falling back to the `opaque` unfold) if
    * they are not.
    */
  def isTrackedIntType(gv: GroundValue): Boolean = gv match {
    case GroundValue.Structure(fqn, _, _) => fqn == RefinementChannelProcessor.intTypeFqn
    case _                                => false
  }

  /** Compute the machine layout of a ground `Int[min, max]` type via the platform's `Represent[Interval[…]]` instance,
    * or [[None]] when the type is not an extractable `Int`, no `Represent` instance is on the compiler path, or the
    * instance body does not reduce to a representation type.
    *
    * The returned [[GroundValue]] is the raw quoted layout (a representation type such as `JvmByte`); the caller lowers
    * it structurally the same way the `opaque` unfold path does before comparing the two.
    */
  def channelLayout(intGroundType: GroundValue): CompilerIO[Option[GroundValue]] =
    RefinementChannelProcessor.intIntervalOf(intGroundType) match {
      case None         => none[GroundValue].pure[CompilerIO]
      case Some(bounds) =>
        for {
          resolved <- getFactIfProduced(
                        AbilityImplementation.Key(
                          representLayoutFqn,
                          Seq(RefinementChannelProcessor.intervalType),
                          Platform.Compiler
                        )
                      ).map(_.flatMap(_.resolution.resolved))
          result   <- resolved match {
                        case None                          => none[GroundValue].pure[CompilerIO]
                        case Some((implFqn, implTypeArgs)) =>
                          ReducedBindingClosure.reduceInstance(implFqn, implTypeArgs).map {
                            case None       => None
                            case Some(body) =>
                              val applied =
                                Evaluator.applyValue(
                                  body,
                                  Evaluator.groundToSem(RefinementChannelProcessor.intervalValue(bounds))
                                )
                              val forced  = Evaluator.force(applied, MetaStore.empty)
                              Quoter.quote(0, forced, MetaStore.empty).toOption
                          }
                      }
        } yield result
    }
}
