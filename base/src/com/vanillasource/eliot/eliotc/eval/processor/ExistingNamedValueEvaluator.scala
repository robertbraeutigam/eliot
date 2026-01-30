package com.vanillasource.eliot.eliotc.eval.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.NamedEvaluable
import com.vanillasource.eliot.eliotc.eval.util.Evaluator.evaluate
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.ResolvedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

class ExistingNamedValueEvaluator
    extends TransformationProcessor[ResolvedValue.Key, NamedEvaluable.Key](key => ResolvedValue.Key(key.vfqn)) {
  override protected def generateFromKeyAndFact(key: NamedEvaluable.Key, fact: InputFact): CompilerIO[OutputFact] =
    fact.value.value.runtime match {
      case Some(runtimeExpression) =>
        evaluate(fact.value.as(runtimeExpression), Set(key.vfqn)).map(NamedEvaluable(key.vfqn, _))
      case None                    =>
        compilerAbort(fact.name.as("Value has no runtime expression."))
    }
}
