package com.vanillasource.eliot.eliotc.eval.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.{ConcreteValue, FunctionLiteral, NativeFunction}
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Value.{Direct, Structure, Type}
import com.vanillasource.eliot.eliotc.eval.util.Types.{bigIntType, stringType}
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Processor that provides NamedEvaluable facts for data type constructors (values ending with $DataType).
  *
  * For data types like `data Function[A, B]`, this creates a NativeFunction chain that builds Value.Structure
  * representing the type when all type parameters are applied.
  */
class DataTypeEvaluator
    extends TransformationProcessor[ResolvedValue.Key, NamedEvaluable.Key](key => ResolvedValue.Key(key.vfqn)) {

  override protected def generateFromKeyAndFact(key: NamedEvaluable.Key, fact: InputFact): CompilerIO[OutputFact] =
    if (key.vfqn.name.endsWith("$DataType") && fact.runtime.isEmpty) {
      for {
        evaluated  <- evaluateSignature(fact.typeStack.value.signature, key.vfqn, fact.typeStack)
        reduced    <- reduce(evaluated, fact.typeStack)
        typeParams  = extractParameters(reduced)
        result      = createDataTypeEvaluable(key.vfqn, typeParams)
      } yield result
    } else {
      abort
    }

  /** Placeholder value used when the signature references the data type being defined. This breaks the evaluation
    * cycle.
    */
  private val selfReferencePlaceholder: Value =
    Structure(Map("$selfReference" -> Direct("placeholder", Type)), Type)

  /** Evaluates a signature expression, treating references to the data type itself as placeholders. This allows us to
    * properly evaluate the signature and extract parameter types without circular dependency.
    */
  private def evaluateSignature(
      expr: Expression,
      selfVfqn: ValueFQN,
      sourced: Sourced[?]
  ): CompilerIO[ExpressionValue] =
    evaluateSignatureWithContext(expr, selfVfqn, Map.empty, sourced)

  private def evaluateSignatureWithContext(
      expr: Expression,
      selfVfqn: ValueFQN,
      paramContext: Map[String, Value],
      sourced: Sourced[?]
  ): CompilerIO[ExpressionValue] = expr match {
    case Expression.IntegerLiteral(s)                           =>
      ConcreteValue(Value.Direct(s.value, bigIntType)).pure[CompilerIO]
    case Expression.StringLiteral(s)                            =>
      ConcreteValue(Value.Direct(s.value, stringType)).pure[CompilerIO]
    case Expression.ParameterReference(s)                       =>
      val name = s.value
      paramContext.get(name) match {
        case Some(paramType) => ExpressionValue.ParameterReference(name, paramType).pure[CompilerIO]
        case None            => compilerAbort(s.as(s"Unknown parameter: $name"))
      }
    case Expression.ValueReference(s)                           =>
      val vfqn = s.value
      if (vfqn === selfVfqn) {
        ConcreteValue(selfReferencePlaceholder).pure[CompilerIO]
      } else {
        getFactOrAbort(NamedEvaluable.Key(vfqn)).map(_.value)
      }
    case Expression.FunctionLiteral(paramName, paramType, body) =>
      for {
        evaluatedParamType <- evaluateTypeToValue(paramType.value, selfVfqn, paramContext, paramType)
        newContext          = paramContext + (paramName.value -> evaluatedParamType)
        evaluatedBody      <- evaluateSignatureWithContext(body.value.signature, selfVfqn, newContext, body)
      } yield FunctionLiteral(paramName.value, evaluatedParamType, evaluatedBody)
    case Expression.FunctionApplication(target, argument)       =>
      for {
        targetValue <- evaluateSignatureWithContext(target.value.signature, selfVfqn, paramContext, target)
        argValue    <- evaluateSignatureWithContext(argument.value.signature, selfVfqn, paramContext, argument)
      } yield ExpressionValue.FunctionApplication(targetValue, argValue)
  }

  private def evaluateTypeToValue(
      typeStack: TypeStack[Expression],
      selfVfqn: ValueFQN,
      paramContext: Map[String, Value],
      sourced: Sourced[?]
  ): CompilerIO[Value] =
    for {
      evaluated <- evaluateSignatureWithContext(typeStack.signature, selfVfqn, paramContext, sourced)
      reduced   <- reduce(evaluated, sourced)
      result    <- reduced match {
                     case ConcreteValue(v) => v.pure[CompilerIO]
                     case _                => compilerAbort(sourced.as("Type expression did not evaluate to a concrete value."))
                   }
    } yield result

  private def reduce(value: ExpressionValue, sourced: Sourced[?]): CompilerIO[ExpressionValue] = value match {
    case ExpressionValue.FunctionApplication(target, arg) =>
      for {
        reducedTarget <- reduce(target, sourced)
        reducedArg    <- reduce(arg, sourced)
        result        <- applyOrKeep(reducedTarget, reducedArg, sourced)
      } yield result
    case FunctionLiteral(name, paramType, body)           =>
      reduce(body, sourced).map(FunctionLiteral(name, paramType, _))
    case other                                            =>
      other.pure[CompilerIO]
  }

  private def applyOrKeep(
      target: ExpressionValue,
      arg: ExpressionValue,
      sourced: Sourced[?]
  ): CompilerIO[ExpressionValue] =
    target match {
      case FunctionLiteral(paramName, _, body) =>
        reduce(substitute(body, paramName, arg), sourced)
      case NativeFunction(_, nativeFn)         =>
        arg match {
          case ConcreteValue(v) => nativeFn(v).pure[CompilerIO]
          case _                => ExpressionValue.FunctionApplication(target, arg).pure[CompilerIO]
        }
      case _                                   =>
        ExpressionValue.FunctionApplication(target, arg).pure[CompilerIO]
    }

  private def substitute(body: ExpressionValue, paramName: String, argValue: ExpressionValue): ExpressionValue =
    body match {
      case ExpressionValue.ParameterReference(name, _) if name == paramName =>
        argValue
      case ExpressionValue.ParameterReference(_, _)                         =>
        body
      case ExpressionValue.FunctionApplication(target, arg)                 =>
        ExpressionValue.FunctionApplication(substitute(target, paramName, argValue), substitute(arg, paramName, argValue))
      case FunctionLiteral(name, paramType, innerBody) if name != paramName =>
        FunctionLiteral(name, paramType, substitute(innerBody, paramName, argValue))
      case _                                                                =>
        body
    }

  /** Extracts parameters from an evaluated signature. Walks the FunctionLiteral chain collecting (name, type) pairs
    * until reaching the self-reference placeholder or a non-FunctionLiteral.
    */
  private def extractParameters(evaluated: ExpressionValue): Seq[(String, Value)] =
    evaluated match {
      case FunctionLiteral(name, paramType, body) =>
        (name, paramType) +: extractParameters(body)
      case _                                      =>
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
