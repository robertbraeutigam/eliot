package com.vanillasource.eliot.eliotc.eval.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.NamedEvaluable
import com.vanillasource.eliot.eliotc.eval.fact.Types.{functionDataTypeFQN, typeFQN}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator.evaluate
import com.vanillasource.eliot.eliotc.operator.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, abort}
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

class ExistingNamedValueEvaluator
    extends TransformationProcessor[OperatorResolvedValue.Key, NamedEvaluable.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    ) {

  override protected def generateFact(key: NamedEvaluable.Key): CompilerIO[Unit] =
    if (key.vfqn === typeFQN || key.vfqn === functionDataTypeFQN)
      abort
    else
      super.generateFact(key)

  override protected def generateFromKeyAndFact(key: NamedEvaluable.Key, fact: InputFact): CompilerIO[OutputFact] =
    fact.runtime match {
      case Some(runtimeExpression) =>
        evaluate(runtimeExpression, Set(key.vfqn)).map(NamedEvaluable(key.vfqn, _))
      case None                    => abort
    }
}
