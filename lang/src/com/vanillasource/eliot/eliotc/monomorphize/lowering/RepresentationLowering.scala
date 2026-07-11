package com.vanillasource.eliot.eliotc.monomorphize.lowering

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementRepresentation
import com.vanillasource.eliot.eliotc.monomorphize.domain.MetaStore
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, TransparentBinding}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Phase 3: the generic "unfold to representation" pass. Re-reveals the machine representation that `opaque` hid from
  * the type checker, running *after* checking on already-monomorphized [[GroundValue]]s.
  *
  * The rule is general and never names `Int` for a *generic* opaque type: to lay out a value, unfold `opaque`-bodied
  * type definitions until the head is a type with no further body (a representation type like `JvmByte`, or any
  * body-less platform/data type). The representation *policy* lives entirely in the Eliot `opaque type Int` body; this
  * pass only evaluates-and-substitutes it.
  *
  * **Bounds-as-refinements Step 6-ii** (`docs/bounds-as-refinements.md`, "uniform bignum"): `Int` has lost its type
  * parameters, so it no longer carries a value range in the type; the range is meta-information in the refinement
  * channel. For a tracked `Int` node the layout is taken from the channel's per-node interval when one is known
  * (the platform's `Represent[Interval[…]]` instance run through the one NbE evaluator,
  * [[RefinementRepresentation.channelLayout]]), else from the platform's `opaque type Int` body — which is now
  * `= JvmBigInteger`. Until the channel's flow analysis lands (a later step), no interval is known, so every integer
  * lays out as a bignum: uniform and sound, with narrow layouts returning once the channel computes ranges from flow.
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

  /** Lower the tracked `Int` type. Post-flag-day (`docs/bounds-as-refinements.md` Step 6-ii, "uniform bignum"): the
    * layout comes from the refinement channel's per-node interval when one is known ([[RefinementRepresentation.channelLayout]]),
    * else falls back to the platform's `opaque type Int` body — which is now `= JvmBigInteger`, so an `Int` whose range
    * the channel cannot yet compute (all of them, until the channel's flow analysis lands) lays out as a bignum:
    * uniform, sound, never needing per-node reconciliation. The Step-3 shadow assertion (channel layout == opaque
    * layout) is retired — the type no longer carries the range, so there is nothing to cross-check against; the channel
    * is authoritative when it computes a layout, and the platform body is the ⊤ fallback.
    */
  private def representInt(
      s: GroundValue.Structure,
      context: Sourced[?],
      nodeInterval: Option[(BigInt, BigInt)]
  ): CompilerIO[GroundValue] =
    RefinementRepresentation.channelLayout(s, nodeInterval).flatMap {
      case Some(raw) => representationOf(raw, context)
      case None      => unfold(s, context)
    }

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
