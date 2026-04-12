package com.vanillasource.eliot.eliotc.monomorphize3.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize3.domain.*
import com.vanillasource.eliot.eliotc.monomorphize3.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize3.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize3.fact.NativeBinding
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor

/** Emits NativeBinding facts for user-defined values. Each value is represented as a VTopDef with a lazy thunk that
  * evaluates the value's body on demand.
  *
  * Simple Set[ValueFQN] recursion guard prevents infinite loops during thunk evaluation — values that are currently
  * being evaluated are represented as VTopDef with an unevaluated thunk.
  */
class UserValueNativesProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, NativeBinding.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    ) {

  override protected def generateFromKeyAndFact(key: NativeBinding.Key, fact: InputFact): CompilerIO[OutputFact] = {
    val semValue = VTopDef(
      key.vfqn,
      Lazy {
        fact.runtime match {
          case Some(body) =>
            val evaluator = new Evaluator(_ => None, Map.empty)
            evaluator.eval(Env.empty, body.value)
          case None       => VType // opaque/abstract — no body to evaluate
        }
      },
      Spine.SNil
    )
    NativeBinding(key.vfqn, semValue).pure[CompilerIO]
  }
}
