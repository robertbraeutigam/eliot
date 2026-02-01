package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.util.{Evaluator, Types}
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Evaluates ExpressionValue types to Value using Evaluator.reduce(). Type parameter substitution is handled by
  * replacing ParameterReferences with ConcreteValues, and data type references are resolved to their NativeFunctions
  * from NamedEvaluable before reduction.
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
    for {
      substituted <- substituteParams(expr, substitution, source)
      resolved    <- resolveDataTypeRefs(substituted)
      reduced     <- Evaluator.reduce(resolved, source)
      value       <- extractValue(reduced, source)
    } yield value

  /** Substitute ParameterReferences with their concrete values from the substitution map. */
  private def substituteParams(
      expr: ExpressionValue,
      substitution: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[ExpressionValue] =
    expr match {
      case ConcreteValue(_) =>
        expr.pure[CompilerIO]

      case ParameterReference(name, _) =>
        substitution.get(name) match {
          case Some(value) => ConcreteValue(value).pure[CompilerIO]
          case None        => compilerAbort(source.as(s"Unbound type parameter: $name"))
        }

      case FunctionApplication(target, arg) =>
        for {
          newTarget <- substituteParams(target, substitution, source)
          newArg    <- substituteParams(arg, substitution, source)
        } yield FunctionApplication(newTarget, newArg)

      case FunctionLiteral(name, paramType, body) =>
        substituteParams(body, substitution, source).map(FunctionLiteral(name, paramType, _))

      case NativeFunction(_, _) =>
        expr.pure[CompilerIO]
    }

  /** Resolve data type references that are targets of FunctionApplication. Only targets need resolution because they
    * represent type constructors that need to be applied. Standalone ConcreteValues are already final type values.
    */
  private def resolveDataTypeRefs(expr: ExpressionValue): CompilerIO[ExpressionValue] =
    expr match {
      case FunctionApplication(target, arg) =>
        for {
          newTarget <- resolveTargetIfNeeded(target)
          newArg    <- resolveDataTypeRefs(arg)
        } yield FunctionApplication(newTarget, newArg)

      case FunctionLiteral(name, paramType, body) =>
        resolveDataTypeRefs(body).map(FunctionLiteral(name, paramType, _))

      case _ =>
        expr.pure[CompilerIO]
    }

  /** Resolve a FunctionApplication target if it's a data type reference. */
  private def resolveTargetIfNeeded(target: ExpressionValue): CompilerIO[ExpressionValue] =
    target match {
      case ConcreteValue(Value.Structure(fields, Value.Type)) if isDataTypeRef(fields) =>
        fields("$typeName") match {
          case Value.Direct(vfqn: ValueFQN, _) =>
            getFactOrAbort(NamedEvaluable.Key(vfqn)).map(_.value)
          case _                               =>
            target.pure[CompilerIO]
        }
      case FunctionApplication(_, _)                                                   =>
        resolveDataTypeRefs(target)
      case _                                                                           =>
        target.pure[CompilerIO]
    }

  /** Check if fields represent a data type reference (only has $typeName, no type parameters). */
  private def isDataTypeRef(fields: Map[String, Value]): Boolean =
    fields.size == 1 && fields.contains("$typeName")

  /** Extract a Value from the reduced ExpressionValue. */
  private def extractValue(reduced: ExpressionValue, source: Sourced[?]): CompilerIO[Value] =
    reduced match {
      case ConcreteValue(v)            => v.pure[CompilerIO]
      case FunctionLiteral(_, _, _)    =>
        compilerAbort(source.as("Type expression reduced to unapplied type function"))
      case NativeFunction(_, _)        =>
        compilerAbort(source.as("Type expression reduced to unapplied native function"))
      case ParameterReference(name, _) =>
        compilerAbort(source.as(s"Type expression contains unsubstituted parameter: $name"))
      case FunctionApplication(_, _)   =>
        compilerAbort(source.as("Type expression could not be fully reduced"))
    }

  /** Build a substitution map from universal type parameters and their concrete arguments.
    */
  def buildSubstitution(
      typeParams: Seq[String],
      typeArgs: Seq[Value]
  ): Map[String, Value] =
    typeParams.zip(typeArgs).toMap

  /** Extract universal type parameter names from the outermost function literals with a kind annotation.
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

  /** Check if a Value represents a kind (Type or Function returning Type).
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
