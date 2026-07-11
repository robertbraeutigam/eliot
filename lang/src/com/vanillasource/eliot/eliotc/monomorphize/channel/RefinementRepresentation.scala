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

/** The representation half of the refinement channel (`docs/bounds-as-refinements.md`): derive an `Int`'s machine
  * layout from its value-range refinement. This is the sole source of an `Int`'s representation now — the `opaque type
  * Int` body it succeeded was deleted with the whole opaque track (Step 7d).
  *
  * Given an interval it computes the layout by resolving the compiler-pool `Represent[Interval[…]]` instance (the
  * platform's representation policy, written in Eliot as a `fitsByte/…` fold) and evaluating its `layout` body on the
  * interval **value** `Interval(min, max)` through the one NbE evaluator — the `RefinementChannelProcessor` transfer/join
  * pattern re-pointed at `Represent.layout`. The result is a representation type (`JvmByte`/…).
  *
  * [[com.vanillasource.eliot.eliotc.monomorphize.lowering.RepresentationLowering]] uses the channel's per-node interval
  * when one is known and [[topLayout]] (the `Represent` policy on the ⊤ interval, → a bignum) otherwise. When no
  * platform `Represent` instance is on the path (a bare `Int` stub in a unit test with no `eliot-compiler/` overlay,
  * which does no codegen) [[channelLayout]]/[[topLayout]] return [[None]] and the `Int` is kept verbatim — reduced
  * coverage, never a false layout.
  */
object RefinementRepresentation {

  /** `eliot.compiler.Represent::layout` — the ability method resolved on the `Interval` domain (§4.4). */
  private[channel] val representLayoutFqn: ValueFQN =
    ValueFQN(ModuleName(ModuleName.compilerPackage, "Represent"), QualifiedName("layout", Qualifier.Ability("Represent")))

  /** Whether `gv` is the tracked `Int` type constructor — the one type the channel derives a representation for. */
  def isTrackedIntType(gv: GroundValue): Boolean = gv match {
    case GroundValue.Structure(fqn, _, _) => fqn == RefinementChannelProcessor.intTypeFqn
    case _                                => false
  }

  /** Compute the machine layout for an `Int` node from `nodeInterval` via the platform's `Represent[Interval[…]]`
    * instance, or [[None]] when there is no interval, no `Represent` instance is on the compiler path, or the instance
    * body does not reduce to a representation type.
    *
    * The `nodeInterval` is the per-node interval the refinement channel recorded for *this* node (from
    * [[com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementTable]], keyed by source position) — the sole
    * interval source, since post-flag-day `Int` is nullary and carries no range in its type. [[None]] `nodeInterval`
    * (a ⊤/unknown range) yields [[None]] here; the caller then uses [[topLayout]].
    *
    * The returned [[GroundValue]] is the raw quoted layout (a representation type such as `JvmByte`); the caller lowers
    * it structurally.
    */
  def channelLayout(nodeInterval: Option[(BigInt, BigInt)]): CompilerIO[Option[GroundValue]] =
    nodeInterval match {
      case None         => none[GroundValue].pure[CompilerIO]
      case Some(bounds) => channelLayoutForInterval(bounds)
    }

  /** Compute the machine layout for a concrete `[min, max]` interval value directly — resolve the platform's
    * `Represent[Interval[…]]` instance and evaluate its `layout` body on `Interval(min, max)` through the one NbE
    * evaluator. [[None]] when no `Represent` instance is on the compiler path or the body does not reduce to a
    * representation type. This is the interval-keyed core of [[channelLayout]], usable when the interval is already in
    * hand (from the channel table) rather than embedded in an `Int[min, max]` type.
    */
  /** The ⊤ ("know nothing") interval: a range too wide for any narrow machine width, so the platform's
    * `Represent[Interval]` policy maps it to its widest layout (a bignum). Used as the fallback when the channel has no
    * per-node interval for an `Int` (every `Int` whose range flow analysis cannot pin), keeping the ⊤ representation
    * *policy* in the platform's Eliot `Represent` instance rather than hardcoding a machine type in `lang`.
    */
  private[channel] val topInterval: (BigInt, BigInt) = {
    val bound = BigInt(2).pow(80)
    (-bound, bound)
  }

  /** Compute the ⊤/unknown-range layout — the platform's `Represent[Interval]` policy on the [[topInterval]] (→ a
    * bignum). [[None]] only when no `Represent` instance is on the compiler path (an overlay-less unit-test stub that
    * does no codegen).
    */
  def topLayout: CompilerIO[Option[GroundValue]] = channelLayoutForInterval(topInterval)

  def channelLayoutForInterval(bounds: (BigInt, BigInt)): CompilerIO[Option[GroundValue]] =
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
