package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Types, Value}
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
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
        checkSynthesized(
          MonomorphicExpression(Types.bigIntType, MonomorphicExpression.IntegerLiteral(v)),
          expected,
          v
        )
      case OperatorResolvedExpression.StringLiteral(v)       =>
        checkSynthesized(
          MonomorphicExpression(Types.stringType, MonomorphicExpression.StringLiteral(v)),
          expected,
          v
        )
      case OperatorResolvedExpression.ParameterReference(n)  =>
        env.runtimeParams.get(n.value) match {
          case Some(paramType) =>
            checkSynthesized(
              MonomorphicExpression(paramType, MonomorphicExpression.ParameterReference(n)),
              expected,
              n
            )
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
    ValueReferenceResolver.resolve(
      vr,
      info =>
        MonomorphicTypeInference.inferFromCallSite(info.fullType, info.bodyTypeParams, expected, env, source),
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
          (paramType, returnType) <- ValueReferenceResolver.extractFunctionParamAndReturn(transformedTarget.expressionType, fa.target)
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
      (concreteType, refExpr) <- ValueReferenceResolver.resolve(
                                   vr,
                                   info =>
                                     inferTypeArgsFromArgument(
                                       fa, info.bodyType, info.bodyTypeParams, expected, env, source
                                     ),
                                   source
                                 )
      (paramType, returnType) <- ValueReferenceResolver.extractFunctionParamAndReturn(concreteType, fa.target)
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

  /** Infer type arguments for a generic function application from the argument's type and the call-site expected type.
    *
    * For lambdas with inferred parameter types, uses return-type matching against the expected type (since the lambda's
    * parameter type is not yet known). Otherwise, the argument is transformed to discover its synthesized type. Note:
    * the caller will re-transform the argument with `Check(paramType)` to propagate richer type information downward,
    * so the transformation here is only used for type discovery.
    */
  private def inferTypeArgsFromArgument(
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
          argTypeDiscovery <- transformExpression(
                                fa.argument.value,
                                propagateExpected(fa.argument.value, expected),
                                env,
                                fa.argument
                              )
          args             <- MonomorphicTypeInference.inferFromArgumentAndReturn(
                                bodyType, argTypeDiscovery.expressionType, expected,
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
          (paramType, returnType) <- ValueReferenceResolver.extractFunctionParamAndReturn(expectedType, source)
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
        ValueReferenceResolver.extractFunctionParamAndReturn(Value.Type, source).flatMap(_ => compilerAbort(source.as("unreachable")))
    }

  /** Propagate expected type through application spines. In a curried call chain `f(a)(b)`, the outermost expected type
    * propagates through intermediate applications but not through non-application targets.
    */
  private def propagateExpected(expr: OperatorResolvedExpression, expected: Expected): Expected =
    expr match {
      case _: OperatorResolvedExpression.FunctionApplication => expected
      case _                                                  => Expected.Synthesize
    }

  private def checkSynthesized(
      result: MonomorphicExpression,
      expected: Expected,
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    expected match {
      case Expected.Check(expectedType)
          if expectedType != Value.Type && expectedType != result.expressionType =>
        compilerAbort(
          source.as("Type mismatch."),
          Seq(s"Expected: ${expectedType.show}", s"Actual: ${result.expressionType.show}")
        )
      case _ => result.pure[CompilerIO]
    }
}
