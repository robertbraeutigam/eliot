package com.vanillasource.eliot.eliotc.stdlib.eval

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.{ConcreteValue, NativeFunction}
import com.vanillasource.eliot.eliotc.eval.fact.NamedEvaluable
import com.vanillasource.eliot.eliotc.eval.fact.Types.{fullyQualifiedNameType, functionDataTypeFQN, typeFQN}
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure, Type}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Evaluating stdlib functions.
  */
class StdlibValueEvaluator extends SingleFactProcessor[NamedEvaluable.Key] {
  private val stdlibFunctions: Map[ValueFQN, NamedEvaluable] = Map.empty

  override def generateSingleFact(key: NamedEvaluable.Key): CompilerIO[NamedEvaluable] =
    stdlibFunctions.get(key.vfqn).fold(abort)(_.pure[CompilerIO])
}
