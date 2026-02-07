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
      resolved <- resolveDataTypeRefs(applied, source)
      reduced  <- Evaluator.reduce(resolved, source)
      value    <- extractValue(reduced, source)
    } yield value
  }

  /** Resolve data type references that are targets of FunctionApplication. Only targets need resolution because they
    * represent type constructors that need to be applied. Standalone ConcreteValues are already final type values.
    */
  private def resolveDataTypeRefs(expr: ExpressionValue, source: Sourced[?]): CompilerIO[ExpressionValue] =
    expr match {
      case FunctionApplication(target, arg) =>
        for {
          newTarget <- resolveTargetIfNeeded(target, source)
          newArg    <- resolveDataTypeRefs(arg, source)
        } yield FunctionApplication(newTarget, newArg)

      case FunctionLiteral(name, paramType, body) =>
        resolveDataTypeRefs(body, source).map(FunctionLiteral(name, paramType, _))

      case _ =>
        expr.pure[CompilerIO]
    }

  /** Resolve a FunctionApplication target if it's a data type reference. */
  private def resolveTargetIfNeeded(target: ExpressionValue, source: Sourced[?]): CompilerIO[ExpressionValue] =
    target match {
      case ConcreteValue(Value.Structure(fields, Value.Type)) if isDataTypeRef(fields) =>
        fields("$typeName") match {
          case Value.Direct(vfqn: ValueFQN, _) =>
            getFact(NamedEvaluable.Key(vfqn)).flatMap {
              case Some(value) => value.value.pure[CompilerIO]
              case None        => compilerAbort(source.as(s"Could not resolve type."), Seq(s"Looking for ${vfqn.show}."))
            }
          case _                               =>
            target.pure[CompilerIO]
        }
      case FunctionApplication(_, _)                                                   =>
        resolveDataTypeRefs(target, source)
      case FunctionLiteral(_, _, _)                                                    =>
        resolveDataTypeRefs(target, source)
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
    extractLeadingLambdaParams(signature).map(_._1)
}
