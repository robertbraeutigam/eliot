package com.vanillasource.eliot.eliotc.uncurry

import cats.implicits.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{TypeFQN, UnifiedModuleFunction}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, ResolvedFunction, TypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.DirectTypeReference
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.typesystem.fact.*
import com.vanillasource.eliot.eliotc.uncurry.{
  UncurriedFunction,
  UncurriedTypedExpression,
  UncurriedTypedFunctionDefinition
}

/** Processor that reverses the currying transformation applied during resolution.
  *
  * This processor restores the original user-defined function signatures from before currying by fetching the original
  * AST function definition and verifying it matches the curried structure.
  *
  * Input: TypeCheckedFunction (curried, single-parameter functions) Output: UncurriedFunction (multi-parameter
  * functions with original parameter names and multi-argument applications)
  *
  * Transformations:
  *   - Function signatures: Restored from original AST with resolved types from curried form
  *   - Function applications: ((f a) b) c → f(a, b, c)
  *
  * If the curried structure doesn't match the original AST (e.g., parameter counts differ), an error is issued.
  */
class UncurryingProcessor
    extends TransformationProcessor[TypeCheckedFunction.Key, UncurriedFunction.Key](key =>
      TypeCheckedFunction.Key(key.ffqn)
    )
    with Logging {

  override def generateFromKeyAndFact(
      key: UncurriedFunction.Key,
      typeCheckedFunction: TypeCheckedFunction
  ): CompilerIO[UncurriedFunction] =
    for {
      // Fetch the original function definition from before currying
      originalFunction           <- getFactOrAbort(UnifiedModuleFunction.Key(key.ffqn))
      resolvedFunction           <- getFactOrAbort(ResolvedFunction.Key(key.ffqn))
      originalParams              = originalFunction.functionDefinition.args
      // Extract parameter types, return type, and uncurried body based on whether body exists
      (paramTypes, returnType, uncurriedBody) = typeCheckedFunction.definition.body match {
                                         case Some(body) =>
                                           // Function with body - uncurry from nested lambdas
                                           val (lambdaParams, actualBody) = uncurryFunctionLiteral(body)
                                           val returnType                 = actualBody.value.expressionType
                                           val uncurriedBody              = uncurrySourcedExpression(actualBody)
                                           (lambdaParams.map(_.typeReference), returnType, Some(uncurriedBody))
                                         case None       =>
                                           // Function without body - uncurry signature from valueType
                                           val (paramTypes, returnType) = uncurryValueType(resolvedFunction.definition.valueType)
                                           (paramTypes, returnType, None)
                                       }
      // Verify that the structure matches what we expect
      _                          <- verifyStructureMatches(
                                      originalParams.length,
                                      paramTypes.length,
                                      typeCheckedFunction.definition.name
                                    )
      // Restore original parameter names with resolved types
      restoredParameters          = originalParams.zip(paramTypes).map { case (original, typeRef) =>
                                      ArgumentDefinition(original.name, typeRef)
                                    }
      uncurriedDefinition         = UncurriedTypedFunctionDefinition(
                                      name = typeCheckedFunction.definition.name,
                                      genericParameters = typeCheckedFunction.definition.genericParameters,
                                      parameters = restoredParameters,
                                      returnType = returnType,
                                      body = uncurriedBody
                                    )
    } yield UncurriedFunction(typeCheckedFunction.ffqn, uncurriedDefinition)

  /** Verifies that the uncurried structure matches the original user-defined structure.
    *
    * Issues an error if the parameter counts don't match, indicating the AST structure has changed in an unexpected
    * way.
    */
  private def verifyStructureMatches(
      originalCount: Int,
      lambdaCount: Int,
      functionName: Sourced[String]
  ): CompilerIO[Unit] =
    compilerAbort(
      functionName.as(
        s"Cannot restore original function signature, expected $originalCount parameters but found $lambdaCount in type-checked lambdas."
      )
    ).whenA(originalCount != lambdaCount)

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

  /** Extracts parameter types and return type from a curried function type.
    *
    * Example: A -> B -> C -> ReturnType → (Seq(A, B, C), ReturnType)
    */
  private def uncurryValueType(valueType: TypeReference): (Seq[TypeReference], TypeReference) = {
    valueType match {
      case DirectTypeReference(typeName, typeArguments)
          if typeName.value == TypeFQN.systemFunctionType && typeArguments.length == 2 =>
        // This is a function type: paramType -> returnType
        val paramType               = typeArguments(0)
        val (restParams, finalType) = uncurryValueType(typeArguments(1))
        (paramType +: restParams, finalType)
      case _                                                                             =>
        // Base case: this is the final return type
        (Seq.empty, valueType)
    }
  }
}
