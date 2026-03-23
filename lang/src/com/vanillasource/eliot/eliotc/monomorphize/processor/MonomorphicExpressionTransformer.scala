package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Types, Value}
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

  /** Transform an OperatorResolvedExpression to a MonomorphicExpression using bidirectional type checking.
    *
    * @param expected
    *   The type direction: [[Expected.Check]] when the expected type is known, [[Expected.Synthesize]] when the type
    *   should be inferred bottom-up.
    * @param env
    *   The monomorphization environment containing type parameter substitution and runtime parameter types.
    */
  def transformExpression(
      expr: OperatorResolvedExpression,
      expected: Expected,
      env: MonoEnv,
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    expr match {
      case OperatorResolvedExpression.IntegerLiteral(v)      =>
        MonomorphicExpression(Types.bigIntType, MonomorphicExpression.IntegerLiteral(v)).pure[CompilerIO]
      case OperatorResolvedExpression.StringLiteral(v)       =>
        MonomorphicExpression(Types.stringType, MonomorphicExpression.StringLiteral(v)).pure[CompilerIO]
      case OperatorResolvedExpression.ParameterReference(n)  =>
        env.runtimeParams.get(n.value) match {
          case Some(paramType) =>
            MonomorphicExpression(paramType, MonomorphicExpression.ParameterReference(n)).pure[CompilerIO]
          case None            =>
            compilerAbort(n.as(s"Unknown parameter: ${n.value}"))
        }
      case vr: OperatorResolvedExpression.ValueReference     =>
        transformValueReference(vr, expected, env, source)
      case fa: OperatorResolvedExpression.FunctionApplication =>
        transformFunctionApplication(fa, expected, env, source)
      case fl: OperatorResolvedExpression.FunctionLiteral    =>
        transformFunctionLiteral(fl, expected, env, source)
    }

  /** Transform a value reference by determining concrete type arguments for the referenced value. */
  private def transformValueReference(
      vr: OperatorResolvedExpression.ValueReference,
      expected: Expected,
      env: MonoEnv,
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    resolveValueReference(
      vr,
      (fullType, _, bodyTypeParams) =>
        MonomorphicTypeInference.inferFromCallSite(fullType, bodyTypeParams, expected, env, source),
      source
    ).map(_._2)

  /** Transform a function application. For value reference targets, type arguments are inferred from argument types. */
  private def transformFunctionApplication(
      fa: OperatorResolvedExpression.FunctionApplication,
      expected: Expected,
      env: MonoEnv,
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    fa.target.value match {
      case vr: OperatorResolvedExpression.ValueReference =>
        transformApplicationWithValueTarget(vr, fa, expected, env, source)
      case _                                             =>
        for {
          transformedTarget      <- transformExpression(
                                      fa.target.value,
                                      propagateExpected(fa.target.value, expected),
                                      env,
                                      fa.target
                                    )
          (paramType, returnType) <- extractFunctionParamAndReturn(transformedTarget.expressionType, fa.target)
          transformedArg         <- transformExpression(
                                      fa.argument.value,
                                      Expected.Check(paramType),
                                      env,
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
      expected: Expected,
      env: MonoEnv,
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    for {
      (concreteType, refExpr) <- resolveValueReference(
                                   vr,
                                   (_, bodyType, bodyTypeParams) =>
                                     inferTypeArgsForApplication(
                                       fa, bodyType, bodyTypeParams, expected, env, source
                                     ),
                                   source
                                 )
      (paramType, returnType) <- extractFunctionParamAndReturn(concreteType, fa.target)
      transformedArg          <- transformExpression(
                                   fa.argument.value,
                                   Expected.Check(paramType),
                                   env,
                                   fa.argument
                                 )
    } yield MonomorphicExpression(
      returnType,
      MonomorphicExpression.FunctionApplication(
        fa.target.as(refExpr),
        fa.argument.as(transformedArg)
      )
    )

  /** Infer type arguments for a generic function application. For lambdas with inferred parameter types, uses call-site
    * return type matching. Otherwise uses argument-based inference.
    */
  private def inferTypeArgsForApplication(
      fa: OperatorResolvedExpression.FunctionApplication,
      bodyType: ExpressionValue,
      typeParams: Seq[(String, Value)],
      expected: Expected,
      env: MonoEnv,
      source: Sourced[?]
  ): CompilerIO[Seq[Value]] = {
    val argIsLambdaWithInferredType = fa.argument.value match {
      case OperatorResolvedExpression.FunctionLiteral(_, None, _) => true
      case _                                                       => false
    }

    expected match {
      case Expected.Check(tpe) if argIsLambdaWithInferredType =>
        MonomorphicTypeInference.inferFromReturnType(bodyType, tpe, typeParams, env.typeParamSubst, source)
      case _                                                  =>
        for {
          transformedArgForInference <- transformExpression(
                                          fa.argument.value,
                                          propagateExpected(fa.argument.value, expected),
                                          env,
                                          fa.argument
                                        )
          args                       <- MonomorphicTypeInference.inferFromArgumentAndReturn(
                                          bodyType, transformedArgForInference.expressionType, expected,
                                          typeParams, env.typeParamSubst, source
                                        )
        } yield args
    }
  }

  /** Transform a function literal using bidirectional checking. Requires [[Expected.Check]] to determine the parameter
    * type.
    */
  private def transformFunctionLiteral(
      fl: OperatorResolvedExpression.FunctionLiteral,
      expected: Expected,
      env: MonoEnv,
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    expected match {
      case Expected.Check(expectedType) =>
        for {
          (paramType, returnType) <- extractFunctionParamAndReturn(expectedType, source)
          transformedBody         <- transformExpression(
                                       fl.body.value,
                                       Expected.Check(returnType),
                                       env.withParam(fl.parameterName.value, paramType),
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
      case Expected.Synthesize         =>
        extractFunctionParamAndReturn(Value.Type, source).flatMap(_ => compilerAbort(source.as("unreachable")))
    }

  /** Propagate expected type through application spines. In a curried call chain `f(a)(b)`, the outermost expected type
    * propagates through intermediate applications but not through non-application targets.
    */
  private def propagateExpected(expr: OperatorResolvedExpression, expected: Expected): Expected =
    expr match {
      case _: OperatorResolvedExpression.FunctionApplication => expected
      case _                                                  => Expected.Synthesize
    }

  /** Resolve a value reference to its concrete type and monomorphic expression. Handles type argument resolution
    * (explicit, inferred, or empty) and ability resolution.
    *
    * @param inferTypeArgs
    *   Callback to infer type arguments when not explicitly provided. Receives the full evaluated type, the body type
    *   (with leading lambdas stripped), and the body-visible type parameters.
    */
  private def resolveValueReference(
      vr: OperatorResolvedExpression.ValueReference,
      inferTypeArgs: (ExpressionValue, ExpressionValue, Seq[(String, Value)]) => CompilerIO[Seq[Value]],
      source: Sourced[?]
  ): CompilerIO[(Value, MonomorphicExpression)] =
    for {
      refTypeExprValue <- evaluateValueType(vr.valueName.value)
      analysis          = TypeParameterAnalysis.fromEvaluatedType(refTypeExprValue)
      bodyExprValue     = ExpressionValue.stripLeadingLambdas(refTypeExprValue)
      typeArgs         <- if (vr.typeArgs.nonEmpty) {
                            evaluateExplicitTypeArgs(vr.typeArgs, source)
                          } else if (analysis.bodyTypeParams.nonEmpty) {
                            inferTypeArgs(refTypeExprValue, bodyExprValue, analysis.bodyTypeParams)
                          } else {
                            Seq.empty[Value].pure[CompilerIO]
                          }
      typeArgSubst      = analysis.buildSubstitution(typeArgs, vr.typeArgs.nonEmpty)
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
  private def extractFunctionParamAndReturn(functionType: Value, source: Sourced[?]): CompilerIO[(Value, Value)] =
    functionType.asFunctionType match {
      case Some(result) => result.pure[CompilerIO]
      case None         =>
        compilerAbort(source.as("Expected function type."), Seq(s"Found: ${functionType.show}"))
    }
}
