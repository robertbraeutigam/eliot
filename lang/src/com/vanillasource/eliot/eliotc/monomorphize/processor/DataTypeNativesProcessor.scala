package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.Qualifier
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.{functionDataTypeFQN, typeFQN}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.{BindingContribution, ContributedBinding}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** The `datatype` native contributor: emits the total [[ContributedBinding]] under [[ContributedBinding.dataTypeLabel]]
  * for data-type constructors (body-less values with the `Type` qualifier) — and `None` for every other name.
  *
  * For a data type like `data Box[A]`, binds the constructor as a [[VTopDef]] with no cached body and an empty spine.
  * Every subsequent [[Evaluator.applyValue]] extends the spine with the [[SemValue]] argument as-is — no eager
  * conversion to [[GroundValue]]. When the result is finally quoted post-drain (e.g. via [[com.vanillasource.eliot.
  * eliotc.monomorphize.eval.Quoter]]), the spine entries are turned into `$0`, `$1`, ... fields. Keeping the args as
  * SemValues means unresolved metavariables are preserved — solved later by unification — rather than silently
  * collapsing to `GroundValue.Type`.
  *
  * To keep native suppliers disjoint (the [[BindingMergerProcessor]] requires it), this excludes the two `Type`-qualified
  * names owned by `SystemNativesProcessor`: `Function` (the primitive Π-former) and `Type` itself. A `Type`-qualified
  * name *with* a body (a type alias) is not a constructor either — it contributes `None`, leaving the user supplier to
  * bind it.
  */
class DataTypeNativesProcessor extends SingleFactProcessor[ContributedBinding.Key] {

  override def generateSingleFact(key: ContributedBinding.Key): CompilerIO[ContributedBinding] =
    if (key.label =!= ContributedBinding.dataTypeLabel) abort
    else dataTypeReduction(key.vfqn).map(red => ContributedBinding(key.vfqn, key.label, red.map(BindingContribution.Leaf(_))))

  /** A body-less `Type`-qualified constructor's inert `VTopDef` reduction, or `None` (totality) for anything else: a
    * non-`Type` name, the system-owned `Function`/`Type`, a type alias (has a body), or a name with no resolved value.
    *
    * The `OperatorResolvedValue` read is **pool-guarded** ([[DeclaringPool]]): the value is requested only on the pool
    * that actually declares `vfqn`, so a name present in one pool but absent from the other never trips the
    * `UnifiedModuleValueProcessor` "Could not find" build error. Today every `data` type lives in the runtime pool, so
    * this always selects `Platform.Runtime` and is byte-identical to the former unconditional runtime read; the compiler
    * fallback future-proofs a compiler-pool-only `data` type.
    */
  private def dataTypeReduction(vfqn: ValueFQN): CompilerIO[Option[SemValue]] =
    if (vfqn.name.qualifier === Qualifier.Type && vfqn =!= functionDataTypeFQN && vfqn =!= typeFQN)
      DeclaringPool.of(vfqn).flatMap {
        case Some(platform) =>
          getFact(OperatorResolvedValue.Key(vfqn, platform)).map {
            case Some(value) if value.runtime.isEmpty => (VTopDef(vfqn, None, Spine.SNil): SemValue).some
            case _                                    => none
          }
        case None           => none[SemValue].pure[CompilerIO]
      }
    else none[SemValue].pure[CompilerIO]
}
