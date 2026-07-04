package com.vanillasource.eliot.eliotc.used

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.monomorphize.lowering.RepresentationLowering.representationOf
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.saturate.fact.{BinderRoles, SaturatedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The codegen-relevant projection of a monomorphic instance's type arguments — the monomorphization-keying plan's
  * **B2**. It maps the *full*, type-checking-exact type arguments of a `(vfqn,
  * typeArguments)` instance down to the subset (and form) that actually distinguishes generated code, using the
  * per-binder [[BinderRoles.Disposition]] computed by B1 and carried on [[SaturatedValue]].
  *
  * Two instances of the same `vfqn` whose projections are equal are guaranteed to generate identical code, so the
  * `used` codegen driver dedups its [[com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicValue]] demand on
  * this projection: it materialises (type-checks) only one representative per projection class instead of one per exact
  * bound. The checker / `MonomorphicValue` keys themselves stay full and exact — only this codegen-side dedup is
  * projected.
  *
  * Per-binder disposition (soundness rule: never merge two instances whose generated code differs):
  *
  *   - '''CollapseErase''' — a phantom binder (in no scanned position): dropped from the key entirely.
  *   - '''CollapseToRepresentation''' — a representation-determining binder: replaced by a '''head-preserving'''
  *     representation key. Width-equivalent bounds (`Int[0, 100]`/`Int[0, 50]`, both a byte) fold together, while the
  *     nominal head is kept so two distinct opaque types sharing a representation (a `Money` and an `Int` both lowering
  *     to `JvmByte`) are *not* merged — they generate differently-named methods (soundness checkpoint 2).
  *   - '''Specialize''' — kept verbatim (a distinct ability selection or a bounded reified family).
  *
  * A value with no [[SaturatedValue]] (and hence no role information) keeps all its type arguments verbatim — the most
  * conservative behaviour, identical to no projection at all.
  */
object CodegenProjection {

  def codegenProject(sourcedVfqn: Sourced[ValueFQN], typeArguments: Seq[GroundValue]): CompilerIO[Seq[GroundValue]] =
    if (typeArguments.isEmpty) typeArguments.pure[CompilerIO]
    else
      getFactIfProduced(SaturatedValue.Key(sourcedVfqn.value)).flatMap {
        case None     => typeArguments.pure[CompilerIO]
        case Some(sv) =>
          val dispositions = sv.binderRoles.roles.map(_.disposition)
          typeArguments.zipWithIndex.flatTraverse { (arg, index) =>
            dispositions.lift(index) match {
              case Some(BinderRoles.Disposition.CollapseErase)            =>
                Seq.empty[GroundValue].pure[CompilerIO]
              case Some(BinderRoles.Disposition.CollapseToRepresentation) =>
                representationOf(arg, sourcedVfqn).map(rep => Seq(headPreservingRepresentation(arg, rep)))
              case _                                                      =>
                // Specialize, or a position past the classified binders.
                Seq(arg).pure[CompilerIO]
            }
          }
      }

  /** A dedup-key element that is equal iff both the nominal head ([[GroundValue.carrierFQN]] — what the backend mangles
    * the method name with) and the machine representation ([[RepresentationLowering.representationOf]] — what becomes
    * the JVM descriptor) match. Keying on the representation alone would unsoundly merge two distinct opaque types that
    * share a representation; keying on the bound alone would forgo the width collapse. This synthetic structure is used
    * only as a `visited`-set identity, never to fetch a fact.
    */
  private def headPreservingRepresentation(arg: GroundValue, representation: GroundValue): GroundValue =
    GroundValue.Structure(arg.carrierFQN, Seq(representation), GroundValue.Type)
}
