package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.ConcreteType
import com.vanillasource.eliot.eliotc.processor.CompilerIO
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Evaluates ExpressionValue types to ConcreteType with substitution of type parameters to their concrete
  * instantiations.
  */
object TypeEvaluator {

  /** Evaluate an ExpressionValue to a ConcreteType.
    *
    * @param expr
    *   The expression value to evaluate
    * @param substitution
    *   Map from type parameter names to their concrete types
    * @param source
    *   Source location for error reporting
    * @return
    *   The concrete type or abort on error
    */
  def evaluate(
      expr: ExpressionValue,
      substitution: Map[String, ConcreteType],
      source: Sourced[?]
  ): CompilerIO[ConcreteType] =
    expr match {
      case ConcreteValue(value) =>
        valueToConcreteType(value, source)

      case ParameterReference(name, _) =>
        substitution.get(name) match {
          case Some(concreteType) => concreteType.pure[CompilerIO]
          case None               =>
            compilerAbort(source.as(s"Unbound type parameter: $name"))
        }

      case FunctionType(paramType, returnType) =>
        for {
          concreteParam  <- evaluate(paramType, substitution, source)
          concreteReturn <- evaluate(returnType, substitution, source)
        } yield ConcreteType.FunctionType(concreteParam, concreteReturn)

      case FunctionApplication(target, arg) =>
        for {
          concreteTarget <- evaluate(target, substitution, source)
          concreteArg    <- evaluate(arg, substitution, source)
        } yield ConcreteType.TypeApplication(concreteTarget, concreteArg)

      case FunctionLiteral(_, _, _) =>
        compilerAbort(source.as("Type-level lambda not yet supported in monomorphization"))

      case NativeFunction(_, _) =>
        compilerAbort(source.as("Native type function not yet supported in monomorphization"))
    }

  /** Convert a Value to a ConcreteType. Used for ConcreteValue cases where the value represents a type.
    */
  private def valueToConcreteType(value: Value, source: Sourced[?]): CompilerIO[ConcreteType] =
    value match {
      case Value.Structure(fields, Value.Type) =>
        fields.get("$typeName") match {
          case Some(Value.Direct(vfqn: ValueFQN, _)) =>
            ConcreteType.TypeRef(vfqn).pure[CompilerIO]
          case _                                     =>
            compilerAbort(source.as("Cannot convert structure to concrete type: missing $typeName"))
        }

      case Value.Direct(n: BigInt, _) =>
        ConcreteType.IntLiteral(n).pure[CompilerIO]

      case Value.Direct(s: String, _) =>
        ConcreteType.StringLiteral(s).pure[CompilerIO]

      case Value.Type =>
        compilerAbort(source.as("Type of types is not a valid concrete type"))

      case _ =>
        compilerAbort(source.as(s"Cannot convert value to concrete type: $value"))
    }

  /** Build a substitution map from universal type parameters and their concrete arguments.
    *
    * @param typeParams
    *   List of type parameter names (in order)
    * @param typeArgs
    *   Concrete type arguments (in order)
    * @return
    *   Map from parameter names to concrete types
    */
  def buildSubstitution(
      typeParams: Seq[String],
      typeArgs: Seq[ConcreteType]
  ): Map[String, ConcreteType] =
    typeParams.zip(typeArgs).toMap

  /** Extract universal type parameter names from the outermost function literals with Type parameter type. Universal
    * type parameters are represented as FunctionLiteral nodes where the parameter type is Value.Type.
    */
  def extractUniversalParams(signature: ExpressionValue): Seq[String] =
    signature match {
      case FunctionLiteral(name, paramType, body) if paramType == Value.Type =>
        name +: extractUniversalParams(body)
      case _                                                                 => Seq.empty
    }

  /** Strip universal type parameter introductions from the signature, leaving the actual function/value type.
    */
  def stripUniversalIntros(expr: ExpressionValue): ExpressionValue =
    expr match {
      case FunctionLiteral(_, paramType, body) if paramType == Value.Type =>
        stripUniversalIntros(body)
      case other                                                          => other
    }
}
