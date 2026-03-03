package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Evaluates ExpressionValue types to Value using Evaluator.reduce(). Type parameters are applied as function
  * arguments, allowing the normal Evaluator to handle beta reduction. Data type references are resolved to their
  * NativeFunctions from NamedEvaluable before reduction.
  */
object TypeEvaluator extends Logging {

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
  ): CompilerIO[Value] =
    for {
      resolvedArgs <- typeArgs.traverse(resolveTypeArgConstructor)
      applied       = applyTypeArgs(expr, resolvedArgs)
      _            <- debug[CompilerIO](s"Applied: ${expr.show} to: ${applied.show} ")
      resolved     <- resolveDataTypeRefs(applied, source)
      _            <- debug[CompilerIO](s"Resolved data type refs to: ${resolved.show} ")
      reduced      <- Evaluator.reduce(resolved, source)
      _            <- debug[CompilerIO](s"Reduced to: ${reduced.show} ")
      value        <- extractValue(reduced, source)
      _            <- debug[CompilerIO](s"Resulting value: ${value.show} ")
    } yield value

  /** Apply type arguments to an expression. FunctionLiterals (from the symbolic phase) are directly beta-reduced via
    * substitution, bypassing the Evaluator's checkType which may reject valid dependent-type arguments (e.g., integer
    * literal 2 for a BigInteger constraint). Remaining arguments that don't match leading FunctionLiterals become
    * FunctionApplications for later reduction by the Evaluator.
    */
  private def applyTypeArgs(expr: ExpressionValue, args: Seq[ExpressionValue]): ExpressionValue =
    args.foldLeft(expr) { (e, arg) =>
      e match {
        case FunctionLiteral(name, _, body) => ExpressionValue.substitute(body.value, name, arg)
        case _                              => FunctionApplication(unsourced(e), unsourced(arg))
      }
    }

  /** Resolve a type argument Value to its constructor form. Data type references that have a registered NamedEvaluable
    * (like parameterized types IO, List, etc.) are resolved to their NativeFunction constructor, so that applying them
    * as functions works naturally during reduction. Non-constructor types are wrapped as ConcreteValue.
    */
  private def resolveTypeArgConstructor(arg: Value): CompilerIO[ExpressionValue] =
    arg match {
      case Value.Structure(fields, Value.Type) if isDataTypeRef(fields) =>
        getDataTypeVfqn(fields) match {
          case Some(vfqn) => getFact(NamedEvaluable.Key(vfqn)).map(_.map(_.value).getOrElse(ConcreteValue(arg)))
          case None       => ConcreteValue(arg).pure[CompilerIO]
        }
      case _                                                            => ConcreteValue(arg).pure[CompilerIO]
    }

  /** Resolve data type references that are targets of FunctionApplication. Only targets need resolution because they
    * represent type constructors that need to be applied. Standalone ConcreteValues are already final type values.
    */
  private def resolveDataTypeRefs(expr: ExpressionValue, source: Sourced[?]): CompilerIO[ExpressionValue] =
    expr match {
      case FunctionApplication(target, arg) =>
        for {
          newTarget <- resolveTargetIfNeeded(target, source)
          newArg    <- resolveDataTypeRefs(arg.value, arg)
        } yield FunctionApplication(target.as(newTarget), arg.as(newArg))

      case FunctionLiteral(name, paramType, body) =>
        resolveDataTypeRefs(body.value, body).map(resolved => FunctionLiteral(name, paramType, body.as(resolved)))

      case _ =>
        expr.pure[CompilerIO]
    }

  /** Resolve a FunctionApplication target if it's a data type reference. */
  private def resolveTargetIfNeeded(target: Sourced[ExpressionValue], source: Sourced[?]): CompilerIO[ExpressionValue] =
    target.value match {
      case ConcreteValue(Value.Structure(fields, Value.Type)) if isDataTypeRef(fields) =>
        getDataTypeVfqn(fields) match {
          case Some(vfqn) =>
            getFact(NamedEvaluable.Key(vfqn)).flatMap {
              case Some(value) => value.value.pure[CompilerIO]
              case None        => compilerAbort(target.as(s"Could not resolve type."), Seq(s"Looking for ${vfqn.show}"))
            }
          case None       => target.value.pure[CompilerIO]
        }
      case FunctionApplication(_, _) | FunctionLiteral(_, _, _)                        =>
        resolveDataTypeRefs(target.value, target)
      case _                                                                           =>
        target.value.pure[CompilerIO]
    }

  /** Check if fields represent a data type reference (only has $typeName, no type parameters). */
  private def isDataTypeRef(fields: Map[String, Value]): Boolean =
    fields.size == 1 && fields.contains("$typeName")

  /** Extract the ValueFQN from a $typeName field, if present and valid. */
  private def getDataTypeVfqn(fields: Map[String, Value]): Option[ValueFQN] =
    fields("$typeName") match {
      case Value.Direct(vfqn: ValueFQN, _) => Some(vfqn)
      case _                               => None
    }

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
        compilerAbort(source.as("Type expression could not be fully reduced"), Seq(s"Value: ${reduced.show}"))
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
    val wrapped        = params.foldRight(expr)((param, body) => FunctionLiteral(param, Value.Type, unsourced(body)))
    evaluate(wrapped, args, source)
  }

  /** Extract type parameter names from the outermost function literals.
    */
  def extractTypeParams(signature: ExpressionValue): Seq[String] =
    extractLeadingLambdaParams(signature).map(_._1)

  /** Extract only type parameter names that appear in the signature body (after stripping leading lambdas). Type
    * parameters that only appear in constraints (e.g. `[I: BigInteger]` where `I` is not used in the return type) are
    * excluded, as they are irrelevant for code generation after ability checking.
    */
  def extractBodyTypeParams(signature: ExpressionValue): Seq[String] = {
    val allParams = extractTypeParams(signature)
    val body      = ExpressionValue.stripLeadingLambdas(signature)
    allParams.filter(ExpressionValue.containsVar(body, _))
  }

  /** Strip leading FunctionLiteral wrappers whose parameters do not appear in the signature body. Preserves
    * FunctionLiterals for parameters that are used in the body type.
    */
  def stripNonBodyUniversals(signature: ExpressionValue): ExpressionValue = {
    val bodyParams = extractBodyTypeParams(signature).toSet
    stripNonBodyUniversalsRec(signature, bodyParams)
  }

  private def stripNonBodyUniversalsRec(sig: ExpressionValue, keep: Set[String]): ExpressionValue =
    sig match {
      case FunctionLiteral(name, paramType, body) if keep.contains(name) =>
        FunctionLiteral(name, paramType, body.map(stripNonBodyUniversalsRec(_, keep)))
      case FunctionLiteral(_, _, body)                                   =>
        stripNonBodyUniversalsRec(body.value, keep)
      case other                                                         => other
    }
}
