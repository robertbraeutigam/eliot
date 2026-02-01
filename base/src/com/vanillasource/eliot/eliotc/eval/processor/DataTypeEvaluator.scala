package com.vanillasource.eliot.eliotc.eval.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.{ConcreteValue, NativeFunction}
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure, Type}
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ResolvedValue}

/** Processor that provides NamedEvaluable facts for data type constructors (values ending with $DataType).
  *
  * For data types like `data Function[A, B]`, this creates a NativeFunction chain that builds Value.Structure
  * representing the type when all type parameters are applied.
  */
class DataTypeEvaluator extends SingleKeyTypeProcessor[NamedEvaluable.Key] {

  override protected def generateFact(key: NamedEvaluable.Key): CompilerIO[Unit] =
    if (key.vfqn.name.endsWith("$DataType")) {
      getFact(ResolvedValue.Key(key.vfqn)).flatMap {
        case Some(resolvedValue) =>
          val typeParams = extractTypeParams(resolvedValue.typeStack.value)
          val evaluable  = createDataTypeEvaluable(key.vfqn, typeParams)
          registerFactIfClear(evaluable)
        case None                =>
          ().pure[CompilerIO]
      }
    } else {
      ().pure[CompilerIO]
    }

  /** Extracts type parameter names from the resolved type stack. Type parameters are represented as FunctionLiterals with
    * empty parameter types at the signature level.
    */
  private def extractTypeParams(typeStack: TypeStack[Expression]): Seq[String] =
    typeStack.signature.toSeq.flatMap(collectTypeParamsFromExpr)

  private def collectTypeParamsFromExpr(expr: Expression): Seq[String] =
    expr match {
      case Expression.FunctionLiteral(paramName, paramType, body) if paramType.value.levels.isEmpty =>
        paramName.value +: body.value.signature.toSeq.flatMap(collectTypeParamsFromExpr)
      case Expression.FunctionApplication(target, _)                                               =>
        target.value.signature.toSeq.flatMap(collectTypeParamsFromExpr)
      case _                                                                                       =>
        Seq.empty
    }

  /** Creates a NamedEvaluable for a data type. For types with no parameters, returns a ConcreteValue directly. For types
    * with parameters, returns a chain of NativeFunctions that collect arguments.
    */
  private def createDataTypeEvaluable(vfqn: ValueFQN, typeParams: Seq[String]): NamedEvaluable =
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
      remainingParams: Seq[String],
      collectedArgs: Map[String, Value]
  ): NativeFunction =
    remainingParams match {
      case head +: tail if tail.isEmpty =>
        NativeFunction(
          Type,
          arg => {
            val finalArgs = collectedArgs + (head -> arg)
            ConcreteValue(createTypeStructure(vfqn, finalArgs))
          }
        )
      case head +: tail                 =>
        NativeFunction(
          Type,
          arg => {
            val newCollectedArgs = collectedArgs + (head -> arg)
            createNativeFunctionChain(vfqn, tail, newCollectedArgs)
          }
        )
      case _                            =>
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
