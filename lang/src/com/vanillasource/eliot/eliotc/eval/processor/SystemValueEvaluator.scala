package com.vanillasource.eliot.eliotc.eval.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.{ConcreteValue, NativeFunction}
import com.vanillasource.eliot.eliotc.eval.fact.NamedEvaluable
import com.vanillasource.eliot.eliotc.eval.fact.Types.{fullyQualifiedNameType, functionDataTypeFQN, typeFQN}
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure, Type}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Evaluating built-in system values. Even things that are not in source code form can be here, so this does not always
  * get downstream facts for a reason.
  */
class SystemValueEvaluator extends SingleFactProcessor[NamedEvaluable.Key] {
  override def generateSingleFact(key: NamedEvaluable.Key): CompilerIO[NamedEvaluable] =
    if (key.vfqn === functionDataTypeFQN) {
      createFunctionDataTypeEvaluable().pure[CompilerIO]
    } else if (key.vfqn === typeFQN) {
      NamedEvaluable(typeFQN, ConcreteValue(Type)).pure[CompilerIO]
    } else {
      abort
    }

  private def createFunctionDataTypeEvaluable(): NamedEvaluable = {
    val nativeFunction =
      NativeFunction(
        Type,
        paramA =>
          NativeFunction(
            Type,
            paramB =>
              ConcreteValue(
                Structure(
                  Map(
                    "$typeName" -> Direct(functionDataTypeFQN, fullyQualifiedNameType),
                    "A"         -> paramA,
                    "B"         -> paramB
                  ),
                  Type
                )
              )
          )
      )
    NamedEvaluable(functionDataTypeFQN, nativeFunction)
  }
}
