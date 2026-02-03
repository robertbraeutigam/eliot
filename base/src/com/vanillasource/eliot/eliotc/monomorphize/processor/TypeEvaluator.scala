package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

import scala.annotation.tailrec

/** Evaluates ExpressionValue types to Value using Evaluator.reduce(). Type parameters are applied as function
  * arguments, allowing the normal Evaluator to handle beta reduction. Data type references are resolved to their
  * NativeFunctions from NamedEvaluable before reduction.
  */
object TypeEvaluator {

  /** Evaluate an ExpressionValue to a fully evaluated Value.
    *
    * @param expr
    *   The expression value to evaluate (including type parameter FunctionLiterals)
    * @param typeArgs
    *   Concrete type arguments to apply to the expression
    * @param source
    *   Source location for error reporting
    * @return
    *   The fully evaluated Value or abort on error
    */
  def evaluate(
      expr: ExpressionValue,
      typeArgs: Seq[Value],
      source: Sourced[?]
  ): CompilerIO[Value] = {
    val applied = typeArgs.foldLeft(expr)((e, arg) => FunctionApplication(e, ConcreteValue(arg)))
    for {
      resolved <- resolveDataTypeRefs(applied)
      reduced  <- Evaluator.reduce(resolved, source)
      value    <- extractValue(reduced, source)
    } yield value
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
      case FunctionLiteral(_, _, _)                                                    =>
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

  /** Evaluate an ExpressionValue with a substitution map. Used for evaluating type expressions within function bodies
    * where type parameters need to be substituted with their concrete values.
    */
  def evaluateWithSubstitution(
      expr: ExpressionValue,
      substitution: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Value] = {
    val (params, args) = substitution.toSeq.unzip
    val wrapped        = params.foldRight(expr)((param, body) => FunctionLiteral(param, Value.Type, body))
    evaluate(wrapped, args, source)
  }

  /** Extract type parameter names from the outermost function literals.
    */
  def extractTypeParams(signature: ExpressionValue): Seq[String] =
    signature match {
      case FunctionLiteral(name, _, body) => name +: extractTypeParams(body)
      case _                              => Seq.empty
    }

  /** Strip type parameter introductions from the signature, leaving the actual type.
    */
  @tailrec
  def stripTypeParams(expr: ExpressionValue): ExpressionValue =
    expr match {
      case FunctionLiteral(_, _, body) => stripTypeParams(body)
      case other                       => other
    }
}
