package com.vanillasource.eliot.eliotc.uncurry

import cats.implicits.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.DirectTypeReference
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, TypeReference}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem.fact.*
import com.vanillasource.eliot.eliotc.uncurry.{
  UncurriedFunction,
  UncurriedTypedExpression,
  UncurriedTypedFunctionDefinition
}

/** Processor that reverses the currying transformation applied during resolution.
  *
  * Input: TypeCheckedFunction (curried, single-parameter functions) Output: UncurriedFunction (multi-parameter
  * functions and multi-argument applications)
  *
  * Transformations:
  *   - Function types: Function[A, Function[B, C]] → (A, B) -> C
  *   - Function literals: a -> b -> c -> body → (a, b, c) -> body
  *   - Function applications: ((f a) b) c → f(a, b, c)
  */
class UncurryingProcessor
    extends TransformationProcessor[TypeCheckedFunction.Key, UncurriedFunction.Key](key =>
      TypeCheckedFunction.Key(key.ffqn)
    )
    with Logging {

  override def generateFromKeyAndFact(
      key: UncurriedFunction.Key,
      typeCheckedFunction: TypeCheckedFunction
  ): CompilerIO[UncurriedFunction] = {
    val definition = typeCheckedFunction.definition

    // Extract parameters and return type from the curried function type
    val (parameters, returnType) = uncurryFunctionType(definition.body.value.expressionType)

    // Extract parameter definitions and actual body from nested function literals
    val (extractedParams, actualBody) = uncurryFunctionLiteral(definition.body)

    // Use extracted params if available, otherwise derive from type
    val finalParameters = if (extractedParams.nonEmpty) extractedParams else parameters

    // Transform the body expression to uncurry applications
    val uncurriedBody = uncurrySourcedExpression(actualBody)

    val uncurriedDefinition = UncurriedTypedFunctionDefinition(
      name = definition.name,
      genericParameters = definition.genericParameters,
      parameters = finalParameters,
      returnType = returnType,
      body = uncurriedBody
    )

    UncurriedFunction(typeCheckedFunction.ffqn, uncurriedDefinition).pure[CompilerIO]
  }

  /** Extracts parameters and return type from a curried function type.
    *
    * Example: Function[A, Function[B, C]] → (Seq(param_a: A, param_b: B), C)
    */
  private def uncurryFunctionType(typeRef: TypeReference): (Seq[ArgumentDefinition], TypeReference) = {
    typeRef match {
      case TypeReference.DirectTypeReference(typeName, Seq(paramType, bodyType))
          if typeName.value == TypeFQN.systemFunctionType =>
        val (restParams, returnType) = uncurryFunctionType(bodyType)
        // Create a synthetic parameter name (will be overridden by actual param if available)
        val param                    = ArgumentDefinition(
          typeName.as(s"param${restParams.length}"),
          paramType
        )
        (param +: restParams, returnType)
      case other =>
        (Seq.empty, other)
    }
  }

  /** Extracts parameters and actual body from nested function literals.
    *
    * Example: a -> b -> c -> body → (Seq(a, b, c), body)
    */
  private def uncurryFunctionLiteral(
      expr: Sourced[TypedExpression]
  ): (Seq[ArgumentDefinition], Sourced[TypedExpression]) = {
    expr.value.expression match {
      case TypedExpression.FunctionLiteral(parameter, body) =>
        val (restParams, actualBody) = uncurryFunctionLiteral(body)
        (parameter +: restParams, actualBody)
      case _                                                =>
        (Seq.empty, expr)
    }
  }

  /** Transforms a sourced typed expression to uncurry function applications.
    *
    * Returns a Sourced[UncurriedTypedExpression] preserving file and range info.
    */
  private def uncurrySourcedExpression(sourced: Sourced[TypedExpression]): Sourced[UncurriedTypedExpression] =
    sourced.map(expr => uncurryExpression(expr, sourced))

  /** Transforms a typed expression to uncurry function applications.
    *
    * Recursively processes the expression tree, collecting consecutive function applications into multi-argument
    * applications.
    */
  private def uncurryExpression(expr: TypedExpression, sourced: Sourced[TypedExpression]): UncurriedTypedExpression = {
    val uncurriedExpr = expr.expression match {
      case TypedExpression.FunctionApplication(target, argument) =>
        // Collect all consecutive applications
        val (finalTarget, allArguments) = collectApplications(sourced)

        UncurriedTypedExpression.FunctionApplication(
          uncurrySourcedExpression(finalTarget),
          allArguments.map(uncurrySourcedExpression)
        )

      case TypedExpression.FunctionLiteral(parameter, body) =>
        // Collect all consecutive lambda parameters
        val (allParams, actualBody) = collectLambdaParameters(sourced)

        UncurriedTypedExpression.FunctionLiteral(
          allParams,
          uncurrySourcedExpression(actualBody)
        )

      case TypedExpression.IntegerLiteral(value) =>
        UncurriedTypedExpression.IntegerLiteral(value)

      case TypedExpression.StringLiteral(value) =>
        UncurriedTypedExpression.StringLiteral(value)

      case TypedExpression.ParameterReference(name) =>
        UncurriedTypedExpression.ParameterReference(name)

      case TypedExpression.ValueReference(name) =>
        UncurriedTypedExpression.ValueReference(name)
    }

    UncurriedTypedExpression(expr.expressionType, uncurriedExpr)
  }

  /** Collects consecutive function applications into a single target and argument list.
    *
    * Example: ((f a) b) c → (f, [a, b, c])
    */
  private def collectApplications(
      sourced: Sourced[TypedExpression]
  ): (Sourced[TypedExpression], Seq[Sourced[TypedExpression]]) = {
    sourced.value.expression match {
      case TypedExpression.FunctionApplication(target, argument) =>
        val (finalTarget, restArgs) = collectApplications(target)
        (finalTarget, restArgs :+ argument)
      case _                                                     =>
        // Base case: this is the actual function being applied
        (sourced, Seq.empty)
    }
  }

  /** Collects consecutive lambda parameters into a parameter list and actual body.
    *
    * Example: a -> b -> c -> body → (Seq(a, b, c), body)
    */
  private def collectLambdaParameters(
      sourced: Sourced[TypedExpression]
  ): (Seq[ArgumentDefinition], Sourced[TypedExpression]) = {
    sourced.value.expression match {
      case TypedExpression.FunctionLiteral(parameter, body) =>
        val (restParams, actualBody) = collectLambdaParameters(body)
        (parameter +: restParams, actualBody)
      case _                                                =>
        // Base case: this is the actual body
        (Seq.empty, sourced)
    }
  }
}
