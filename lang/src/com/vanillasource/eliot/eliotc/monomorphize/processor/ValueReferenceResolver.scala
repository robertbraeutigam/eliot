package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Resolves value references to their concrete types and monomorphic expressions. Handles type argument resolution
  * (explicit, inferred, or empty), type parameter substitution, and ability resolution.
  */
object ValueReferenceResolver {

  /** The evaluated type information for a referenced value, provided to inference callbacks so they can determine type
    * arguments.
    *
    * @param fullType
    *   The fully evaluated type expression including leading type parameter lambdas.
    * @param bodyType
    *   The type expression with leading lambdas stripped (the "visible" type).
    * @param bodyTypeParams
    *   Type parameters that appear in the body type (excludes phantom/constraint-only params).
    */
  case class TypeInfo(
      fullType: ExpressionValue,
      bodyType: ExpressionValue,
      bodyTypeParams: Seq[(String, Value)]
  )

  /** Resolve a value reference to its concrete type and monomorphic expression.
    *
    * @param inferTypeArgs
    *   Callback to infer type arguments when not explicitly provided. Receives a [[TypeInfo]] containing the
    *   evaluated type signature and body-visible type parameters of the referenced value.
    */
  def resolve(
      vr: OperatorResolvedExpression.ValueReference,
      inferTypeArgs: TypeInfo => CompilerIO[Seq[Value]],
      source: Sourced[?]
  ): CompilerIO[(Value, MonomorphicExpression)] =
    for {
      refTypeExprValue <- evaluateValueType(vr.valueName.value)
      analysis          = TypeParameterAnalysis.fromEvaluatedType(refTypeExprValue)
      bodyExprValue     = ExpressionValue.stripLeadingLambdas(refTypeExprValue)
      typeArgs         <- if (vr.typeArgs.nonEmpty) {
                            for {
                              explicitArgs <- evaluateExplicitTypeArgs(vr.typeArgs, source)
                              _            <- if (explicitArgs.length > analysis.allTypeParams.length)
                                                compilerAbort(
                                                  source.as(
                                                    s"Too many type arguments: expected at most ${analysis.allTypeParams.length}, got ${explicitArgs.length}"
                                                  )
                                                )
                                              else ().pure[CompilerIO]
                              result       <- if (explicitArgs.length == analysis.allTypeParams.length)
                                                explicitArgs.pure[CompilerIO]
                                              else {
                                                val explicitSubst       =
                                                  analysis.allTypeParams.take(explicitArgs.length).map(_._1).zip(explicitArgs).toMap
                                                val remainingBodyParams =
                                                  analysis.bodyTypeParams.filterNot(p => explicitSubst.contains(p._1))
                                                if (remainingBodyParams.nonEmpty)
                                                  inferTypeArgs(
                                                    TypeInfo(refTypeExprValue, bodyExprValue, remainingBodyParams)
                                                  ).map { inferred =>
                                                    val inferredSubst = remainingBodyParams.map(_._1).zip(inferred).toMap
                                                    val combined      = explicitSubst ++ inferredSubst
                                                    analysis.bodyTypeParams.map(p => combined(p._1))
                                                  }
                                                else explicitArgs.pure[CompilerIO]
                                              }
                            } yield result
                          } else if (analysis.bodyTypeParams.nonEmpty) {
                            inferTypeArgs(TypeInfo(refTypeExprValue, bodyExprValue, analysis.bodyTypeParams))
                          } else {
                            Seq.empty[Value].pure[CompilerIO]
                          }
      typeArgSubst      = analysis.buildSubstitution(
                            typeArgs,
                            vr.typeArgs.nonEmpty && vr.typeArgs.length >= analysis.allTypeParams.length
                          )
      concreteType     <- Evaluator.applyTypeArgsStripped(refTypeExprValue, analysis.allTypeParams, typeArgSubst, source)
      resolved         <- if (MonomorphicAbilityResolver.isAbilityRef(vr.valueName.value) && typeArgs.nonEmpty) {
                            MonomorphicAbilityResolver.resolve(vr.valueName, typeArgs, concreteType, source)
                          } else {
                            MonomorphicExpression(
                              concreteType,
                              MonomorphicExpression.MonomorphicValueReference(vr.valueName, typeArgs)
                            ).pure[CompilerIO]
                          }
    } yield (concreteType, resolved)

  /** Evaluate a value's type signature from its OperatorResolvedValue's type stack. */
  private[processor] def evaluateValueType(vfqn: ValueFQN): CompilerIO[ExpressionValue] =
    for {
      resolvedValue <- getFactOrAbort(OperatorResolvedValue.Key(vfqn))
      typeExprValue <- Evaluator.evaluate(resolvedValue.typeStack.as(resolvedValue.typeStack.value.signature))
    } yield typeExprValue

  /** Evaluate explicit type arguments from ValueReference.typeArgs to concrete Values. */
  private def evaluateExplicitTypeArgs(
      typeArgs: Seq[Sourced[OperatorResolvedExpression]],
      source: Sourced[?]
  ): CompilerIO[Seq[Value]] =
    typeArgs.traverse { argExpr =>
      for {
        evaluated <- Evaluator.evaluate(argExpr)
        value     <- ExpressionValue.concreteValueOf(evaluated) match {
                       case Some(v) => v.pure[CompilerIO]
                       case None    =>
                         compilerAbort(argExpr.as("Type argument did not evaluate to concrete value"))
                     }
      } yield value
    }

  /** Extract parameter type and return type from a function type Value. */
  def extractFunctionParamAndReturn(functionType: Value, source: Sourced[?]): CompilerIO[(Value, Value)] =
    functionType.asFunctionType match {
      case Some(result) => result.pure[CompilerIO]
      case None         =>
        compilerAbort(source.as("Expected function type."), Seq(s"Found: ${functionType.show}"))
    }
}
