package com.vanillasource.eliot.eliotc.monomorphize.lowering

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementRepresentation
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Phase 3: the "map to machine representation" pass, running *after* checking on already-monomorphized [[GroundValue]]s.
  *
  * The only type that carries a machine layout is the tracked `Int`: its representation is *derived in Eliot from its
  * value-range refinement* by the platform's `Represent[Interval]` instance (`docs/bounds-as-refinements.md` — the
  * successor to the deleted `opaque type Int` body, Step 7d). Every other head is body-less (a `data` type, a
  * representation type like `JvmByte`, `Function`) and is kept verbatim, recursing into its arguments so nested `Int`s
  * lower too.
  *
  * For a tracked `Int` node the layout comes from the channel's per-node interval when one is known
  * ([[RefinementRepresentation.channelLayout]]); otherwise (a ⊤/unknown range — a parameter, a value crossing a
  * boundary) it falls back to the platform's `Represent` policy on the ⊤ interval ([[RefinementRepresentation.topLayout]],
  * → a bignum), so the ⊤ representation stays platform-supplied in Eliot rather than hardcoded here.
  */
object RepresentationLowering {

  /** Rewrite a ground value to its machine representation, recursing structurally.
    *
    * `nodeInterval` is the refinement channel's per-node interval for *this* node (from the position-keyed
    * [[com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementTable]]), supplied by the uncurry caller for a body
    * node whose own type is a tracked `Int`. It is used only when `gv` is directly that tracked `Int` — any structural
    * recursion into children resets it to [[None]] (a nested `Int` is a different node with its own interval).
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
        // Function types are never a representation head; recurse into domain/codomain via `lowerLeaf`.
        if (s.asFunctionType.isDefined) lowerLeaf(s, context)
        else if (RefinementRepresentation.isTrackedIntType(s)) representInt(s, context, nodeInterval)
        else lowerLeaf(s, context)
    }

  /** Lower the tracked `Int` type to a machine layout via the platform's `Represent[Interval]` instance: the channel's
    * per-node interval when known ([[RefinementRepresentation.channelLayout]]), else the ⊤ policy
    * ([[RefinementRepresentation.topLayout]], → a bignum). When no `Represent` instance is on the compiler path (a bare
    * repr-`Int` stub in a unit test with no `eliot-compiler/` overlay, which does no codegen), the `Int` is kept verbatim
    * — reduced coverage, never a false layout.
    */
  private def representInt(
      s: GroundValue.Structure,
      context: Sourced[?],
      nodeInterval: Option[(BigInt, BigInt)]
  ): CompilerIO[GroundValue] =
    RefinementRepresentation.channelLayout(nodeInterval).flatMap {
      case Some(raw) => representationOf(raw, context)
      case None      =>
        RefinementRepresentation.topLayout.flatMap {
          case Some(raw) => representationOf(raw, context)
          case None      => lowerLeaf(s, context)
        }
    }

  /** No layout for this head: keep it, but lower its arguments (and the value-type) so nested `Int`s reduce too. */
  private def lowerLeaf(s: GroundValue.Structure, context: Sourced[?]): CompilerIO[GroundValue] =
    for {
      loweredArgs <- s.args.traverse(representationOf(_, context))
      loweredType <- representationOf(s.valueType, context)
    } yield GroundValue.Structure(s.typeName, loweredArgs, loweredType)
}
