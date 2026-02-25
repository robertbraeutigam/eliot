package com.vanillasource.eliot.eliotc.eval.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.Qualifier
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.{
  ConcreteValue,
  FunctionLiteral,
  InitialExpressionValue,
  NativeFunction
}
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure, Type}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.eval.fact.Types.{fullyQualifiedNameType, functionDataTypeFQN}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.OperatorResolvedValue

/** Processor that provides NamedEvaluable facts for data type constructors (values with Type qualifier).
  *
  * For data types like `data Box[A]`, this creates a NativeFunction chain that builds Value.Structure representing the
  * type when all type parameters are applied.
  *
  * Note: Function^Type is handled by FunctionDataTypeEvaluator since it's foundational.
  */
class DataTypeEvaluator
    extends TransformationProcessor[OperatorResolvedValue.Key, NamedEvaluable.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    ) {

  override protected def generateFact(key: NamedEvaluable.Key): CompilerIO[Unit] =
    if (key.vfqn.name.qualifier === Qualifier.Type && key.vfqn =!= functionDataTypeFQN)
      super.generateFact(key)
    else
      abort

  override protected def generateFromKeyAndFact(key: NamedEvaluable.Key, fact: InputFact): CompilerIO[OutputFact] =
    if (fact.runtime.isEmpty) {
      for {
        evaluated <- Evaluator.evaluate(fact.typeStack.map(_.signature))
        typeParams = ExpressionValue.extractLeadingLambdaParams(evaluated)
        result     = NamedEvaluable(key.vfqn, createDataTypeEvaluable(key.vfqn, typeParams))
      } yield result
    } else {
      abort
    }

  /** Creates an ExpressionValue for a data type. For types with no parameters, returns a ConcreteValue directly. For
    * types with parameters, returns a chain of NativeFunctions that collect arguments.
    */
  private def createDataTypeEvaluable(
      vfqn: ValueFQN,
      remainingParams: Seq[(String, Value)],
      collectedArgs: Map[String, Value] = Map.empty
  ): InitialExpressionValue =
    remainingParams match {
      case (name, paramType) +: tail =>
        NativeFunction(paramType, arg => createDataTypeEvaluable(vfqn, tail, collectedArgs + (name -> arg)))
      case _                         =>
        ConcreteValue(
          Structure(
            Map("$typeName" -> Direct(vfqn, fullyQualifiedNameType)) ++ collectedArgs,
            Type
          )
        )
    }

}
