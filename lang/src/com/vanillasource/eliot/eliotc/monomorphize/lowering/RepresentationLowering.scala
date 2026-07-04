package com.vanillasource.eliot.eliotc.monomorphize.lowering

import cats.syntax.all.*
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
  * The rule is general and never names `Int`: to lay out a value, unfold `opaque`-bodied type definitions until the head
  * is a type with no further body (a representation type like `JvmByte`, or any body-less platform/data type). The width
  * *policy* lives entirely in the Eliot `opaque type Int` body; this pass only evaluates-and-substitutes it.
  *
  * Example: `Int[0, 255]` ⟶ (unfold opaque `Int` body with `MIN=0, MAX=255`) ⟶ `JvmByte`, which the JVM backend then
  * maps to `java.lang.Byte`.
  */
object RepresentationLowering {

  /** Rewrite a ground value to its machine representation, recursing structurally. The `context` carries a source
    * position used only to report a (should-not-happen) failure to reduce an opaque body.
    */
  def representationOf(gv: GroundValue, context: Sourced[?]): CompilerIO[GroundValue] =
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
            case Some(orv) if orv.runtime.isDefined => unfold(s, context)
            case _                                  => lowerLeaf(s, context)
          }
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
