package com.vanillasource.eliot.eliotc.eval.processor

import com.vanillasource.eliot.eliotc.eval.fact.NamedEvaluable
import com.vanillasource.eliot.eliotc.eval.util.Evaluator.evaluate
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, abort}
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.ResolvedValue

class ExistingNamedValueEvaluator
    extends TransformationProcessor[ResolvedValue.Key, NamedEvaluable.Key](key => ResolvedValue.Key(key.vfqn)) {
  override protected def generateFromKeyAndFact(key: NamedEvaluable.Key, fact: InputFact): CompilerIO[OutputFact] =
    fact.value.value.runtime match {
      case Some(runtimeExpression) => evaluate(runtimeExpression).map(NamedEvaluable(key.vfqn, _))
      case None                    => abort
    }
}
