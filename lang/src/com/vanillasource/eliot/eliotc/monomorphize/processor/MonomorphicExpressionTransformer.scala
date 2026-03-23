package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{Types, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Transforms OperatorResolvedExpression trees into MonomorphicExpression trees, computing concrete types and
  * recursively monomorphizing called functions.
  */
object MonomorphicExpressionTransformer {

  /** Transform an OperatorResolvedExpression to a MonomorphicExpression, computing concrete types and recursively
    * monomorphizing called functions.
    *
    * @param callSiteType
    *   The expected type of this expression from the enclosing context.
    */
  def transformExpression(
      expr: OperatorResolvedExpression,
      callSiteType: Value,
      typeParamSubst: Map[String, Value],
      runtimeParams: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    expr match {
      case OperatorResolvedExpression.IntegerLiteral(v)      =>
        MonomorphicExpression(Types.bigIntType, MonomorphicExpression.IntegerLiteral(v)).pure[CompilerIO]
      case OperatorResolvedExpression.StringLiteral(v)       =>
        MonomorphicExpression(Types.stringType, MonomorphicExpression.StringLiteral(v)).pure[CompilerIO]
      case OperatorResolvedExpression.ParameterReference(n)  =>
        runtimeParams.get(n.value) match {
          case Some(paramType) =>
            MonomorphicExpression(paramType, MonomorphicExpression.ParameterReference(n)).pure[CompilerIO]
          case None            =>
            compilerAbort(n.as(s"Unknown parameter: ${n.value}"))
        }
      case vr: OperatorResolvedExpression.ValueReference     =>
        transformValueReference(vr, callSiteType, typeParamSubst, runtimeParams, source)
      case fa: OperatorResolvedExpression.FunctionApplication =>
        transformFunctionApplication(fa, callSiteType, typeParamSubst, runtimeParams, source)
      case fl: OperatorResolvedExpression.FunctionLiteral    =>
        transformFunctionLiteral(fl, callSiteType, typeParamSubst, runtimeParams, source)
    }

  /** Transform a value reference by determining concrete type arguments for the referenced value. */
  private def transformValueReference(
      vr: OperatorResolvedExpression.ValueReference,
      callSiteType: Value,
      typeParamSubst: Map[String, Value],
      runtimeParams: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    for {
      refTypeExprValue <- evaluateValueType(vr.valueName.value, source)
      allTypeParams     = ExpressionValue.extractLeadingLambdaParams(refTypeExprValue)
      bodyExprValue     = ExpressionValue.stripLeadingLambdas(refTypeExprValue)
      bodyTypeParams    = allTypeParams.filter((name, _) => ExpressionValue.containsVar(bodyExprValue, name))
      typeArgs         <- if (vr.typeArgs.nonEmpty) {
                            evaluateExplicitTypeArgs(vr.typeArgs, source)
                          } else if (bodyTypeParams.nonEmpty) {
                            MonomorphicTypeInference.inferFromCallSite(
                              refTypeExprValue, bodyTypeParams, callSiteType, typeParamSubst, source
                            )
                          } else {
                            Seq.empty[Value].pure[CompilerIO]
                          }
      typeArgSubst      = if (vr.typeArgs.nonEmpty)
                            allTypeParams.map(_._1).zip(typeArgs).toMap
                          else
                            bodyTypeParams.map(_._1).zip(typeArgs).toMap
      concreteType     <- Evaluator.applyTypeArgsStripped(refTypeExprValue, allTypeParams, typeArgSubst, source)
      result           <- if (MonomorphicAbilityResolver.isAbilityRef(vr.valueName.value) && typeArgs.nonEmpty) {
                            MonomorphicAbilityResolver.resolve(vr.valueName, typeArgs, concreteType, source)
                          } else {
                            MonomorphicExpression(
                              concreteType,
                              MonomorphicExpression.MonomorphicValueReference(vr.valueName, typeArgs)
                            ).pure[CompilerIO]
                          }
    } yield result

  /** Transform a function application. For value reference targets, type arguments are inferred from argument types. */
  private def transformFunctionApplication(
      fa: OperatorResolvedExpression.FunctionApplication,
      callSiteType: Value,
      typeParamSubst: Map[String, Value],
      runtimeParams: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    fa.target.value match {
      case vr: OperatorResolvedExpression.ValueReference =>
        transformApplicationWithValueTarget(vr, fa, callSiteType, typeParamSubst, runtimeParams, source)
      case _                                             =>
        val targetCallSiteType = fa.target.value match {
          case _: OperatorResolvedExpression.FunctionApplication => callSiteType
          case _                                                  => Value.Type
        }
        for {
          transformedTarget       <- transformExpression(
                                       fa.target.value,
                                       targetCallSiteType,
                                       typeParamSubst,
                                       runtimeParams,
                                       fa.target
                                     )
          (paramType, returnType) <- extractFunctionParamAndReturn(transformedTarget.expressionType, fa.target)
          transformedArg          <- transformExpression(
                                       fa.argument.value,
                                       paramType,
                                       typeParamSubst,
                                       runtimeParams,
                                       fa.argument
                                     )
        } yield MonomorphicExpression(
          returnType,
          MonomorphicExpression.FunctionApplication(
            fa.target.as(transformedTarget),
            fa.argument.as(transformedArg)
          )
        )
    }

  /** Transform a function application whose target is a value reference. Infers type arguments from argument types,
    * falling back to call-site return type matching for lambdas with inferred parameter types.
    */
  private def transformApplicationWithValueTarget(
      vr: OperatorResolvedExpression.ValueReference,
      fa: OperatorResolvedExpression.FunctionApplication,
      callSiteType: Value,
      typeParamSubst: Map[String, Value],
      runtimeParams: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    for {
      refTypeExprValue <- evaluateValueType(vr.valueName.value, source)
      allTypeParams     = ExpressionValue.extractLeadingLambdaParams(refTypeExprValue)
      bodyType          = ExpressionValue.stripLeadingLambdas(refTypeExprValue)
      bodyTypeParams    = allTypeParams.filter((name, _) => ExpressionValue.containsVar(bodyType, name))
      typeArgs         <- if (vr.typeArgs.nonEmpty) {
                            evaluateExplicitTypeArgs(vr.typeArgs, source)
                          } else if (bodyTypeParams.nonEmpty) {
                            inferTypeArgsForApplication(
                              fa, bodyType, bodyTypeParams, callSiteType, typeParamSubst, runtimeParams, source
                            )
                          } else {
                            Seq.empty[Value].pure[CompilerIO]
                          }
      typeArgSubst      = if (vr.typeArgs.nonEmpty)
                            allTypeParams.map(_._1).zip(typeArgs).toMap
                          else
                            bodyTypeParams.map(_._1).zip(typeArgs).toMap
      concreteType     <- Evaluator.applyTypeArgsStripped(refTypeExprValue, allTypeParams, typeArgSubst, source)
      (paramType, returnType) <- extractFunctionParamAndReturn(concreteType, fa.target)
      transformedArg   <- transformExpression(
                            fa.argument.value,
                            paramType,
                            typeParamSubst,
                            runtimeParams,
                            fa.argument
                          )
      result           <- if (MonomorphicAbilityResolver.isAbilityRef(vr.valueName.value) && typeArgs.nonEmpty) {
                            MonomorphicAbilityResolver.resolve(vr.valueName, typeArgs, concreteType, source).map {
                              implExpr =>
                                MonomorphicExpression(
                                  returnType,
                                  MonomorphicExpression.FunctionApplication(
                                    fa.target.as(implExpr),
                                    fa.argument.as(transformedArg)
                                  )
                                )
                            }
                          } else {
                            MonomorphicExpression(
                              returnType,
                              MonomorphicExpression.FunctionApplication(
                                fa.target.as(
                                  MonomorphicExpression(
                                    concreteType,
                                    MonomorphicExpression.MonomorphicValueReference(vr.valueName, typeArgs)
                                  )
                                ),
                                fa.argument.as(transformedArg)
                              )
                            ).pure[CompilerIO]
                          }
    } yield result

  /** Infer type arguments for a generic function application. For lambdas with inferred parameter types, uses call-site
    * return type matching. Otherwise uses argument-based inference.
    */
  private def inferTypeArgsForApplication(
      fa: OperatorResolvedExpression.FunctionApplication,
      bodyType: ExpressionValue,
      typeParams: Seq[(String, Value)],
      callSiteType: Value,
      typeParamSubst: Map[String, Value],
      runtimeParams: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Seq[Value]] = {
    val argIsLambdaWithInferredType = fa.argument.value match {
      case OperatorResolvedExpression.FunctionLiteral(_, None, _) => true
      case _                                                       => false
    }

    if (argIsLambdaWithInferredType && callSiteType != Value.Type) {
      MonomorphicTypeInference.inferFromReturnType(bodyType, callSiteType, typeParams, typeParamSubst, source)
    } else {
      val argCallSiteType = fa.argument.value match {
        case _: OperatorResolvedExpression.FunctionApplication => callSiteType
        case _                                                  => Value.Type
      }
      for {
        transformedArgForInference <- transformExpression(
                                         fa.argument.value, argCallSiteType, typeParamSubst, runtimeParams, fa.argument
                                       )
        args                       <- MonomorphicTypeInference.inferFromArgumentAndReturn(
                                        bodyType, transformedArgForInference.expressionType, callSiteType,
                                        typeParams, typeParamSubst, source
                                      )
      } yield args
    }
  }

  /** Transform a function literal. */
  private def transformFunctionLiteral(
      fl: OperatorResolvedExpression.FunctionLiteral,
      expectedType: Value,
      typeParamSubst: Map[String, Value],
      runtimeParams: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    for {
      (paramType, returnType) <- extractFunctionParamAndReturn(expectedType, source)
      newRuntimeParams         = runtimeParams + (fl.parameterName.value -> paramType)
      transformedBody         <- transformExpression(
                                   fl.body.value,
                                   returnType,
                                   typeParamSubst,
                                   newRuntimeParams,
                                   fl.body
                                 )
    } yield MonomorphicExpression(
      expectedType,
      MonomorphicExpression.FunctionLiteral(
        fl.parameterName,
        paramType,
        fl.body.as(transformedBody)
      )
    )

  /** Evaluate a value's type signature from its OperatorResolvedValue's type stack. */
  private def evaluateValueType(vfqn: ValueFQN, source: Sourced[?]): CompilerIO[ExpressionValue] =
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
  private def extractFunctionParamAndReturn(functionType: Value, source: Sourced[?]): CompilerIO[(Value, Value)] =
    functionType.asFunctionType match {
      case Some(result) => result.pure[CompilerIO]
      case None         =>
        compilerAbort(source.as("Expected function type."), Seq(s"Found: ${functionType.show}"))
    }
}
