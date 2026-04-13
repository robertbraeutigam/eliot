package com.vanillasource.eliot.eliotc.monomorphize3.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.Qualifier
import com.vanillasource.eliot.eliotc.eval.fact.Types.{functionDataTypeFQN, fullyQualifiedNameType}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator as EvalEvaluator
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize3.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize3.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize3.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize3.fact.{GroundValue, NativeBinding}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor

/** Emits NativeBinding facts for data type constructors (values with Type qualifier). Mirrors the pattern of
  * eval's DataTypeEvaluator but produces SemValue-based NativeBindings.
  *
  * For data types like `data Box[A]`, creates a VNative chain that collects type arguments and builds
  * VConst(Structure(...)) when all parameters are applied.
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
      for {
        evaluated <- EvalEvaluator.evaluate(fact.typeStack.map(_.signature))
        typeParams = ExpressionValue.extractLeadingLambdaParams(evaluated) match {
                       case Seq() =>
                         extractFunctionTypeParams(evaluated)
                           .zipWithIndex
                           .map((v, i) => (s"$$$i", v))
                       case params => params
                     }
      } yield NativeBinding(key.vfqn, createDataTypeNative(key.vfqn, typeParams))
    } else {
      abort
    }

  private def extractFunctionTypeParams(expr: ExpressionValue): Seq[Value] =
    ExpressionValue.extractFunctionParamAndReturn(expr) match {
      case Some((paramExpr, returnExpr)) =>
        ExpressionValue.concreteValueOf(paramExpr) match {
          case Some(paramValue) => paramValue +: extractFunctionTypeParams(returnExpr)
          case None             => Seq.empty
        }
      case None                          => Seq.empty
    }

  /** Creates a SemValue for a data type constructor. For types with no parameters, returns VConst directly. For types
    * with parameters, returns a chain of VNatives that collect arguments.
    */
  private def createDataTypeNative(
      vfqn: ValueFQN,
      remainingParams: Seq[(String, Value)],
      collectedArgs: Map[String, GroundValue] = Map.empty
  ): SemValue =
    remainingParams match {
      case (name, _) +: tail =>
        VNative(
          VType,
          argSem => createDataTypeNative(vfqn, tail, collectedArgs + (name -> Evaluator.semToGround(argSem)))
        )
      case _                 =>
        VConst(
          GroundValue.Structure(
            Map("$typeName" -> GroundValue.Direct(vfqn, GroundValue.Type)) ++ collectedArgs,
            GroundValue.Type
          )
        )
    }
}
