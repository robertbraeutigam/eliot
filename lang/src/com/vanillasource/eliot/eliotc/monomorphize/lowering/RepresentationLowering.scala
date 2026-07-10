package com.vanillasource.eliot.eliotc.monomorphize.lowering

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementRepresentation
import com.vanillasource.eliot.eliotc.monomorphize.domain.MetaStore
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, TransparentBinding}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerAbort, compilerError}

/** Phase 3: the generic "unfold to representation" pass. Re-reveals the machine representation that `opaque` hid from
  * the type checker, running *after* checking on already-monomorphized [[GroundValue]]s.
  *
  * The rule is general and never names `Int` for a *generic* opaque type: to lay out a value, unfold `opaque`-bodied
  * type definitions until the head is a type with no further body (a representation type like `JvmByte`, or any
  * body-less platform/data type). The width *policy* lives entirely in the Eliot `opaque type Int` body; this pass only
  * evaluates-and-substitutes it.
  *
  * Example: `Int[0, 255]` ⟶ (unfold opaque `Int` body with `MIN=0, MAX=255`) ⟶ `JvmByte`, which the JVM backend then
  * maps to `java.lang.Byte`.
  *
  * **Step 3 of the bounds-as-refinements migration** (`docs/bounds-as-refinements.md`): for the tracked `Int` type the
  * layout is *also* computed from its value-range refinement — the platform's `Represent[Interval[…]]` instance run
  * through the one NbE evaluator ([[RefinementRepresentation.channelLayout]]) — and the two are asserted equal before the
  * backend consumes the channel-derived one (the `opaque` unfold demoted to a shadow check on the way to its deletion,
  * §5.1/Step 7d). When no platform `Represent` instance is on the path the channel yields nothing and this pass falls
  * back to the `opaque` unfold, so the representation stays correct with reduced cross-check coverage.
  */
object RepresentationLowering {

  /** Rewrite a ground value to its machine representation, recursing structurally. The `context` carries a source
    * position used only to report a (should-not-happen) failure to reduce an opaque body.
    *
    * `nodeInterval` is the refinement channel's per-node interval for *this* node (from the position-keyed
    * [[com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementTable]]), supplied by the uncurry caller for a body
    * node whose own type is a tracked `Int`. It is used only when `gv` is directly that tracked `Int` — any structural
    * recursion into children resets it to [[None]] (a nested `Int` is a different node with its own, type-derived,
    * interval). This is the Step-6 staging move (`docs/bounds-as-refinements.md`, "Staged R2"): the `Int`'s layout is
    * sourced from the channel table, with the `Int[min, max]` type kept only as the shadow cross-check (see
    * [[representInt]]).
    */
  def representationOf(
      gv: GroundValue,
      context: Sourced[?],
      nodeInterval: Option[(BigInt, BigInt)] = None
  ): CompilerIO[GroundValue] =
    gv match {
      case GroundValue.Type              =>
        (GroundValue.Type: GroundValue).pure[CompilerIO]
      case GroundValue.Direct(value, vt) =>
        representationOf(vt, context).map(GroundValue.Direct(value, _))
      case s: GroundValue.Structure      =>
        // Function types are never unfolded (`Function` has no opaque body); recurse into domain/codomain instead.
        if (s.asFunctionType.isDefined) lowerLeaf(s, context)
        else
          getFactIfProduced(OperatorResolvedValue.Key(s.typeName)).flatMap {
            case Some(orv) if orv.runtime.isDefined =>
              if (RefinementRepresentation.isTrackedIntType(s)) representInt(s, context, nodeInterval)
              else unfold(s, context)
            case _                                  => lowerLeaf(s, context)
          }
    }

  /** Lower the tracked `Int` type (Step 3): compute its layout via the refinement channel *and* by unfolding the
    * `opaque` body, assert the two agree, and return the channel-derived one (the backend's source of truth from now on).
    * A divergence is a hard error — the shadow-mode invariant while both representations of the width policy coexist.
    * When the channel cannot compute the layout — no platform `Represent` instance on the path — fall back to the
    * `opaque` unfold; the representation is still correct, only the cross-check is skipped.
    */
  private def representInt(
      s: GroundValue.Structure,
      context: Sourced[?],
      nodeInterval: Option[(BigInt, BigInt)]
  ): CompilerIO[GroundValue] =
    for {
      opaqueRepr <- unfold(s, context)
      channelRaw <- RefinementRepresentation.channelLayout(s, nodeInterval)
      result     <- channelRaw match {
                      case None      => opaqueRepr.pure[CompilerIO]
                      case Some(raw) =>
                        representationOf(raw, context).flatMap { channelRepr =>
                          if (channelRepr == opaqueRepr) channelRepr.pure[CompilerIO]
                          else
                            compilerError(
                              context.as(
                                s"Refinement channel disagrees with the type at an integer representation: " +
                                  s"channel chose ${channelRepr.typeFQN.map(_.show).getOrElse(channelRepr.toString)} " +
                                  s"but the opaque body chose ${opaqueRepr.typeFQN.map(_.show).getOrElse(opaqueRepr.toString)}."
                              ),
                              Seq(
                                "This is an internal shadow-mode invariant of the bounds-as-refinements migration; the " +
                                  "platform's Represent instance and the opaque Int representation body must agree on every " +
                                  "program."
                              )
                            ).as(opaqueRepr)
                        }
                    }
    } yield result

  /** No unfolding for this head: keep it, but lower its arguments (and the value-type) so nested `Int`s reduce too. */
  private def lowerLeaf(s: GroundValue.Structure, context: Sourced[?]): CompilerIO[GroundValue] =
    for {
      loweredArgs <- s.args.traverse(representationOf(_, context))
      loweredType <- representationOf(s.valueType, context)
    } yield GroundValue.Structure(s.typeName, loweredArgs, loweredType)

  /** Unfold this head's (opaque) body applied to its arguments, then recurse on the result (an opaque type may unfold to
    * another opaque type before bottoming out at a representation type).
    */
  private def unfold(s: GroundValue.Structure, context: Sourced[?]): CompilerIO[GroundValue] =
    for {
      binding <- getFactOrAbort(TransparentBinding.Key(s.typeName))
      applied  = s.args.map(Evaluator.groundToSem).foldLeft(binding.semValue)(Evaluator.applyValue)
      forced   = Evaluator.force(applied, MetaStore.empty)
      result  <- Quoter.quote(0, forced, MetaStore.empty) match {
                   case Right(unfolded) if unfolded.typeFQN.contains(s.typeName) =>
                     compilerAbort(
                       context.as("Could not reduce opaque type to its machine representation."),
                       Seq(s"The opaque body of ${s.typeName.show} did not reduce past itself.")
                     )
                   case Right(unfolded)                                          =>
                     representationOf(unfolded, context)
                   case Left(error)                                              =>
                     compilerAbort(
                       context.as("Could not reduce opaque type to its machine representation."),
                       Seq(s"While lowering ${s.typeName.show}: $error")
                     )
                 }
    } yield result
}
