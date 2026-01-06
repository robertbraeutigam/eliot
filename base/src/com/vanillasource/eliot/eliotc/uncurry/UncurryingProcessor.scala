package com.vanillasource.eliot.eliotc.uncurry

import cats.implicits.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.UnifiedModuleFunction
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.ArgumentDefinition
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
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
      originalFunction <- getFactOrAbort(UnifiedModuleFunction.Key(key.ffqn))

      // Extract the original parameter structure
      originalParams = originalFunction.functionDefinition.args

      // Extract parameter definitions from nested function literals (these were created during currying)
      // The lambdaParams already contain both the names and resolved types
      (lambdaParams, actualBody) = uncurryFunctionLiteral(typeCheckedFunction.definition.body)

      // The return type is the expression type of the actual body (after stripping lambdas)
      returnType = actualBody.value.expressionType

      // Verify that the structure matches what we expect
      _ <- verifyStructureMatches(
             originalParams.length,
             lambdaParams.length,
             typeCheckedFunction.definition.name
           )

      // Restore original parameter names with resolved types from the type-checked lambdas
      restoredParameters = originalParams.zip(lambdaParams).map { case (original, lambda) =>
                             ArgumentDefinition(original.name, lambda.typeReference)
                           }

      // Transform the body expression to uncurry applications
      uncurriedBody = uncurrySourcedExpression(actualBody)

      uncurriedDefinition = UncurriedTypedFunctionDefinition(
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
    if (originalCount != lambdaCount) {
      compilerError(
        functionName.as(
          s"Cannot restore original function signature: expected $originalCount parameters but found $lambdaCount in type-checked lambdas"
        )
      ) *> abort[Unit]
    } else {
      ().pure[CompilerIO]
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
