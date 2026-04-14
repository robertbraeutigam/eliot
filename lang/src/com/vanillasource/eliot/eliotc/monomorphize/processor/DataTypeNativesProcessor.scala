package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.Qualifier
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.functionDataTypeFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.NativeBinding
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor

/** Emits NativeBinding facts for data type constructors (values with Type qualifier).
  *
  * For data types like `data Box[A]`, binds the constructor as a [[VTopDef]] with no cached body and an empty spine.
  * Every subsequent [[Evaluator.applyValue]] extends the spine with the [[SemValue]] argument as-is — no eager
  * conversion to [[GroundValue]]. When the result is finally quoted post-drain (e.g. via [[com.vanillasource.eliot.
  * eliotc.monomorphize.eval.Quoter]]) or via [[Evaluator.semToGround]], the spine entries are turned into `$0`, `$1`,
  * ... fields. Keeping the args as SemValues means unresolved metavariables are preserved — solved later by
  * unification — rather than silently collapsing to `GroundValue.Type`.
  */
class DataTypeNativesProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, NativeBinding.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    ) {

  override protected def generateFact(key: NativeBinding.Key): CompilerIO[Unit] =
    if (key.vfqn.name.qualifier === Qualifier.Type && key.vfqn =!= functionDataTypeFQN)
      super.generateFact(key)
    else
      abort

  override protected def generateFromKeyAndFact(key: NativeBinding.Key, fact: InputFact): CompilerIO[OutputFact] =
    if (fact.runtime.isEmpty) {
      NativeBinding(key.vfqn, VTopDef(key.vfqn, None, Spine.SNil): SemValue).pure[CompilerIO]
    } else {
      abort
    }
}
