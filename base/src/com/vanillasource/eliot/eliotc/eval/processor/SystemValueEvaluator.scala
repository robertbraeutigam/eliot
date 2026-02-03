package com.vanillasource.eliot.eliotc.eval.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.{ConcreteValue, NativeFunction}
import com.vanillasource.eliot.eliotc.eval.fact.NamedEvaluable
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure, Type}
import com.vanillasource.eliot.eliotc.eval.fact.Types.{fullyQualifiedNameType, functionDataTypeFQN, typeFQN}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.ResolvedValue

/** Evaluating built-in system values.
  */
class SystemValueEvaluator
    extends TransformationProcessor[ResolvedValue.Key, NamedEvaluable.Key](key => ResolvedValue.Key(key.vfqn)) {

  override protected def generateFromKeyAndFact(key: NamedEvaluable.Key, fact: InputFact): CompilerIO[OutputFact] =
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
