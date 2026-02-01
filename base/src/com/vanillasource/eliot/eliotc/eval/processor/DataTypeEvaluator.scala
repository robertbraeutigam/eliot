package com.vanillasource.eliot.eliotc.eval.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.{ConcreteValue, FunctionLiteral, NativeFunction}
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure, Type}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.eval.util.Types.functionDataTypeFQN
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.ResolvedValue

/** Processor that provides NamedEvaluable facts for data type constructors (values ending with $DataType).
  *
  * For data types like `data Box[A]`, this creates a NativeFunction chain that builds Value.Structure representing the
  * type when all type parameters are applied.
  *
  * Note: Function$DataType is handled by FunctionDataTypeEvaluator since it's foundational.
  */
class DataTypeEvaluator
    extends TransformationProcessor[ResolvedValue.Key, NamedEvaluable.Key](key => ResolvedValue.Key(key.vfqn)) {

  override protected def generateFromKeyAndFact(key: NamedEvaluable.Key, fact: InputFact): CompilerIO[OutputFact] =
    if (key.vfqn.name.endsWith("$DataType") && fact.runtime.isEmpty && key.vfqn =!= functionDataTypeFQN) {
      for {
        evaluated  <- Evaluator.evaluate(fact.typeStack.map(_.signature))
        typeParams  = extractParameters(evaluated)
        result      = createDataTypeEvaluable(key.vfqn, typeParams)
      } yield result
    } else {
      abort
    }

  /** Extracts parameters from an evaluated signature. Walks the FunctionLiteral chain collecting (name, type) pairs
    * until reaching a ConcreteValue (typically Type).
    */
  private def extractParameters(evaluated: ExpressionValue.InitialExpressionValue): Seq[(String, Value)] =
    evaluated match {
      case FunctionLiteral(name, paramType, body) =>
        (name, paramType) +: extractParametersFromBody(body)
      case _                                      =>
        Seq.empty
    }

  private def extractParametersFromBody(body: ExpressionValue): Seq[(String, Value)] =
    body match {
      case FunctionLiteral(name, paramType, innerBody) =>
        (name, paramType) +: extractParametersFromBody(innerBody)
      case _                                           =>
        Seq.empty
    }

  /** Creates a NamedEvaluable for a data type. For types with no parameters, returns a ConcreteValue directly. For
    * types with parameters, returns a chain of NativeFunctions that collect arguments.
    */
  private def createDataTypeEvaluable(vfqn: ValueFQN, typeParams: Seq[(String, Value)]): NamedEvaluable =
    if (typeParams.isEmpty) {
      val structure = createTypeStructure(vfqn, Map.empty)
      NamedEvaluable(vfqn, ConcreteValue(structure))
    } else {
      val nativeFunction = createNativeFunctionChain(vfqn, typeParams, Map.empty)
      NamedEvaluable(vfqn, nativeFunction)
    }

  /** Creates a chain of NativeFunctions that collect type arguments and finally produce a Structure.
    */
  private def createNativeFunctionChain(
      vfqn: ValueFQN,
      remainingParams: Seq[(String, Value)],
      collectedArgs: Map[String, Value]
  ): NativeFunction =
    remainingParams match {
      case (name, paramType) +: tail if tail.isEmpty =>
        NativeFunction(
          paramType,
          arg => {
            val finalArgs = collectedArgs + (name -> arg)
            ConcreteValue(createTypeStructure(vfqn, finalArgs))
          }
        )
      case (name, paramType) +: tail                 =>
        NativeFunction(
          paramType,
          arg => {
            val newCollectedArgs = collectedArgs + (name -> arg)
            createNativeFunctionChain(vfqn, tail, newCollectedArgs)
          }
        )
      case _                                         =>
        NativeFunction(Type, _ => ConcreteValue(createTypeStructure(vfqn, collectedArgs)))
    }

  /** Creates a Value.Structure representing a data type with its type arguments.
    */
  private def createTypeStructure(vfqn: ValueFQN, typeArgs: Map[String, Value]): Value =
    Structure(
      Map("$typeName" -> Direct(vfqn, Type)) ++ typeArgs,
      Type
    )
}
