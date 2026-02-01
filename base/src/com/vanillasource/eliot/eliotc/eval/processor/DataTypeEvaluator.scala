package com.vanillasource.eliot.eliotc.eval.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.{ConcreteValue, NativeFunction}
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure, Type}
import com.vanillasource.eliot.eliotc.eval.util.Types.typeFQN
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ResolvedValue}

/** Processor that provides NamedEvaluable facts for data type constructors (values ending with $DataType).
  *
  * For data types like `data Function[A, B]`, this creates a NativeFunction chain that builds Value.Structure
  * representing the type when all type parameters are applied.
  */
class DataTypeEvaluator
    extends TransformationProcessor[ResolvedValue.Key, NamedEvaluable.Key](key => ResolvedValue.Key(key.vfqn)) {

  override protected def generateFromKeyAndFact(key: NamedEvaluable.Key, fact: InputFact): CompilerIO[OutputFact] =
    if (key.vfqn.name.endsWith("$DataType") && fact.runtime.isEmpty) {
      val typeParams = extractTypeParams(fact.typeStack.value)
      createDataTypeEvaluable(key.vfqn, typeParams).pure[CompilerIO]
    } else {
      abort
    }

  private val functionVfqn = ValueFQN(ModuleName(Seq("eliot", "lang"), "Function"), "Function$DataType")

  /** Check if a type stack represents a kind annotation (for universal introductions). A kind annotation is:
    *   - Type (for simple type parameters)
    *   - Function(Type, Type) (for type constructors of arity 1)
    *   - Function(Type, Function(Type, Type)) (for type constructors of arity 2)
    *   - etc.
    */
  private def isKindAnnotation(stack: TypeStack[Expression]): Boolean =
    stack.levels.length == 1 && isKindExpression(stack.signature)

  private def isKindExpression(expr: Expression): Boolean =
    expr match {
      case Expression.ValueReference(vfqn) =>
        vfqn.value === typeFQN
      case Expression.FunctionApplication(targetStack, argStack) =>
        // Check if this is Function(Type, <kind>) where <kind> is Type or another kind
        targetStack.value.signature match {
          case Expression.FunctionApplication(fnStack, argTypeStack) =>
            isFunctionReference(fnStack.value.signature) &&
            isTypeReference(argTypeStack.value.signature) &&
            isKindExpression(argStack.value.signature)
          case _ => false
        }
      case _ => false
    }

  private def isFunctionReference(expr: Expression): Boolean =
    expr match {
      case Expression.ValueReference(vfqn) => vfqn.value === functionVfqn
      case _                               => false
    }

  private def isTypeReference(expr: Expression): Boolean =
    expr match {
      case Expression.ValueReference(vfqn) => vfqn.value === typeFQN
      case _                               => false
    }

  /** Extracts type parameter names from the resolved type stack. Type parameters are represented as FunctionLiterals
    * with a kind annotation as the parameter type.
    */
  private def extractTypeParams(typeStack: TypeStack[Expression]): Seq[String] =
    collectTypeParamsFromExpr(typeStack.signature)

  private def collectTypeParamsFromExpr(expr: Expression): Seq[String] =
    expr match {
      case Expression.FunctionLiteral(paramName, paramType, body) if isKindAnnotation(paramType.value) =>
        paramName.value +: collectTypeParamsFromExpr(body.value.signature)
      case Expression.FunctionApplication(target, _)                                                   =>
        collectTypeParamsFromExpr(target.value.signature)
      case _                                                                                           =>
        Seq.empty
    }

  /** Creates a NamedEvaluable for a data type. For types with no parameters, returns a ConcreteValue directly. For
    * types with parameters, returns a chain of NativeFunctions that collect arguments.
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
