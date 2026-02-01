package com.vanillasource.eliot.eliotc.eval.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.{ConcreteValue, NativeFunction}
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure, Type}
import com.vanillasource.eliot.eliotc.eval.util.Types.functionDataTypeFQN
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.ResolvedValue

/** Dedicated processor for Function$DataType which is foundational to the type system.
  *
  * Function$DataType must be available before other data types can be evaluated, since their signatures may use
  * Function types. This processor hardcodes the structure to avoid circular dependencies.
  */
class FunctionDataTypeEvaluator
    extends TransformationProcessor[ResolvedValue.Key, NamedEvaluable.Key](key => ResolvedValue.Key(key.vfqn)) {

  override protected def generateFromKeyAndFact(key: NamedEvaluable.Key, fact: InputFact): CompilerIO[OutputFact] =
    if (key.vfqn === functionDataTypeFQN && fact.runtime.isEmpty) {
      createFunctionDataTypeEvaluable().pure[CompilerIO]
    } else {
      abort
    }

  private def createFunctionDataTypeEvaluable(): NamedEvaluable = {
    val nativeFunction = NativeFunction(
      Type,
      paramA =>
        NativeFunction(
          Type,
          paramB =>
            ConcreteValue(
              Structure(
                Map(
                  "$typeName" -> Direct(functionDataTypeFQN, Type),
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
