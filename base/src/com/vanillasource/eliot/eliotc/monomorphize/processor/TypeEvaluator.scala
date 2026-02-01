package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.util.Types
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Evaluates ExpressionValue types to Value with substitution of type parameters to their concrete instantiations.
  * Function types are now represented uniformly as FunctionApplication chains with Function$DataType, eliminating the
  * need for special FunctionType handling.
  */
object TypeEvaluator {

  /** Evaluate an ExpressionValue to a fully evaluated Value.
    *
    * @param expr
    *   The expression value to evaluate
    * @param substitution
    *   Map from type parameter names to their concrete Value types
    * @param source
    *   Source location for error reporting
    * @return
    *   The fully evaluated Value or abort on error
    */
  def evaluate(
      expr: ExpressionValue,
      substitution: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Value] =
    expr match {
      case ConcreteValue(value) =>
        value.pure[CompilerIO]

      case ParameterReference(name, _) =>
        substitution.get(name) match {
          case Some(value) => value.pure[CompilerIO]
          case None        => compilerAbort(source.as(s"Unbound type parameter: $name"))
        }

      case FunctionApplication(target, arg) =>
        for {
          targetValue <- evaluate(target, substitution, source)
          argValue    <- evaluate(arg, substitution, source)
          result      <- applyType(targetValue, argValue, source)
        } yield result

      case FunctionLiteral(_, _, _) =>
        compilerAbort(source.as("Type-level lambda not yet supported in monomorphization"))

      case NativeFunction(_, _) =>
        compilerAbort(source.as("Unexpected NativeFunction in type expression"))
    }

  /** Apply a type constructor to an argument by adding the argument to the structure's next parameter slot.
    */
  private def applyType(target: Value, arg: Value, source: Sourced[?]): CompilerIO[Value] =
    target match {
      case Value.Structure(fields, Value.Type) =>
        val existingParams = fields.keys.filter(k => k.length == 1 && k.head.isUpper).toSeq.sorted
        val nextParam      = if (existingParams.isEmpty) "A" else (existingParams.last.head + 1).toChar.toString
        Value.Structure(fields + (nextParam -> arg), Value.Type).pure[CompilerIO]

      case _ =>
        compilerAbort(source.as(s"Cannot apply type argument to: $target"))
    }

  /** Build a substitution map from universal type parameters and their concrete arguments.
    *
    * @param typeParams
    *   List of type parameter names (in order)
    * @param typeArgs
    *   Concrete type arguments (in order)
    * @return
    *   Map from parameter names to concrete Values
    */
  def buildSubstitution(
      typeParams: Seq[String],
      typeArgs: Seq[Value]
  ): Map[String, Value] =
    typeParams.zip(typeArgs).toMap

  /** Extract universal type parameter names from the outermost function literals with a kind annotation. Universal type
    * parameters are represented as FunctionLiteral nodes where the parameter type is a kind (Type or a function
    * returning Type for higher-kinded types).
    */
  def extractUniversalParams(signature: ExpressionValue): Seq[String] =
    signature match {
      case FunctionLiteral(name, paramType, body) if isKind(paramType) =>
        name +: extractUniversalParams(body)
      case _                                                           => Seq.empty
    }

  /** Strip universal type parameter introductions from the signature, leaving the actual function/value type.
    */
  def stripUniversalIntros(expr: ExpressionValue): ExpressionValue =
    expr match {
      case FunctionLiteral(_, paramType, body) if isKind(paramType) =>
        stripUniversalIntros(body)
      case other                                                    => other
    }

  /** Check if a Value represents a kind (Type or Function returning Type). A kind is:
    *   - Type (for simple type parameters)
    *   - Function(Type, Type) (for type constructors of arity 1)
    *   - Function(Type, Function(Type, Type)) (for type constructors of arity 2)
    *   - etc.
    */
  def isKind(value: Value): Boolean =
    value match {
      case Value.Type =>
        true
      case Value.Structure(fields, Value.Type) =>
        fields.get("$typeName") match {
          case Some(Value.Direct(vfqn: ValueFQN, _)) if vfqn === Types.functionDataTypeFQN =>
            fields.get("A").exists(isKind) &&
            fields.get("B").exists(isKind)
          case _ => false
        }
      case _ => false
    }
}
