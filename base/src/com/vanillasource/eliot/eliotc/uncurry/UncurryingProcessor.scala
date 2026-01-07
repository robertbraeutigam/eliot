package com.vanillasource.eliot.eliotc.uncurry

import cats.implicits.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.UnifiedModuleFunction
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, TypeReference}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.*
import com.vanillasource.eliot.eliotc.typesystem.fact.*
import com.vanillasource.eliot.eliotc.uncurry.{
  UncurriedFunction,
  UncurriedTypedExpression,
  UncurriedTypedFunctionDefinition
}
import scala.annotation.tailrec

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
      // Get the original function definition to determine arity and parameter names
      unifiedFunction          <- getFactOrAbort(UnifiedModuleFunction.Key(key.ffqn))
      originalArity            = unifiedFunction.functionDefinition.args.length
      // Restore original arity
      (args, returnType, body) <-
        restoreArity(
          typeCheckedFunction.definition.name,
          typeCheckedFunction.definition.valueType,
          typeCheckedFunction.definition.body,
          originalArity,
          unifiedFunction.functionDefinition.args
        )
    } yield UncurriedFunction(
      typeCheckedFunction.ffqn,
      UncurriedTypedFunctionDefinition(
        name = typeCheckedFunction.definition.name,
        genericParameters = typeCheckedFunction.definition.genericParameters,
        parameters = args,
        returnType = returnType,
        body = body
      )
    )

  private def restoreArity(
      functionName: Sourced[String],
      typeReference: TypeReference,
      body: Option[Sourced[TypedExpression]],
      arity: Int,
      originalArgs: Seq[com.vanillasource.eliot.eliotc.ast.fact.ArgumentDefinition]
  ): CompilerIO[(Seq[ArgumentDefinition], TypeReference, Option[Sourced[UncurriedTypedExpression]])] =
    if (arity === 0) {
      (Seq.empty, typeReference, body.map(_.map(convertExpression))).pure[CompilerIO]
    } else {
      typeReference match {
        case TypeReference.DirectTypeReference(Sourced(_, _, systemFunctionType), Seq(inputType, outputType)) =>
          // Needs to be a function to deconstruct
          // Get the current parameter index (total args - remaining arity)
          val paramIndex = originalArgs.length - arity
          val originalArg = originalArgs(paramIndex)

          body match {
            case Some(Sourced(start, end, TypedExpression(_, TypedExpression.FunctionLiteral(parameter, innerBody)))) =>
              // Function with body - extract parameter from body
              restoreArity(functionName, outputType, Some(innerBody), arity - 1, originalArgs).map {
                case (restArgs, restType, restBody) =>
                  (parameter +: restArgs, restType, restBody)
              }
            case None =>
              // Function without body - use parameter name from original AST with type from curried signature
              val parameter = ArgumentDefinition(originalArg.name, inputType)
              restoreArity(functionName, outputType, None, arity - 1, originalArgs).map { case (restArgs, restType, restBody) =>
                (parameter +: restArgs, restType, restBody)
              }
            case _ =>
              compilerAbort(functionName.as(s"Expected function literal in body while restoring arity."))
          }
        case _ =>
          compilerAbort(functionName.as(s"Could not restore arity to function."))
      }
    }

  private def convertExpression(expr: TypedExpression): UncurriedTypedExpression =
    expr match {
      case TypedExpression(exprType, TypedExpression.FunctionApplication(target, argument)) =>
        // Flatten curried applications: ((f a) b) c -> f(a, b, c)
        val (finalTarget, arguments) = flattenApplication(target, Seq(argument))
        UncurriedTypedExpression(
          exprType,
          UncurriedTypedExpression.FunctionApplication(
            finalTarget.map(convertExpression),
            arguments.map(_.map(convertExpression))
          )
        )

      case TypedExpression(exprType, TypedExpression.FunctionLiteral(parameter, body)) =>
        // Flatten nested lambdas: a -> b -> c -> body into (a, b, c) -> body
        val (parameters, finalBody) = flattenLambda(Seq(parameter), body)
        UncurriedTypedExpression(
          exprType,
          UncurriedTypedExpression.FunctionLiteral(
            parameters,
            finalBody.map(convertExpression)
          )
        )

      case TypedExpression(exprType, TypedExpression.IntegerLiteral(intLit)) =>
        UncurriedTypedExpression(exprType, UncurriedTypedExpression.IntegerLiteral(intLit))

      case TypedExpression(exprType, TypedExpression.StringLiteral(strLit)) =>
        UncurriedTypedExpression(exprType, UncurriedTypedExpression.StringLiteral(strLit))

      case TypedExpression(exprType, TypedExpression.ParameterReference(paramName)) =>
        UncurriedTypedExpression(exprType, UncurriedTypedExpression.ParameterReference(paramName))

      case TypedExpression(exprType, TypedExpression.ValueReference(valueName)) =>
        UncurriedTypedExpression(exprType, UncurriedTypedExpression.ValueReference(valueName))
    }

  @tailrec
  private def flattenApplication(
      target: Sourced[TypedExpression],
      arguments: Seq[Sourced[TypedExpression]]
  ): (Sourced[TypedExpression], Seq[Sourced[TypedExpression]]) =
    target.value match {
      case TypedExpression(_, TypedExpression.FunctionApplication(innerTarget, innerArgument)) =>
        flattenApplication(innerTarget, innerArgument +: arguments)
      case _ =>
        (target, arguments)
    }

  @tailrec
  private def flattenLambda(
      parameters: Seq[ArgumentDefinition],
      body: Sourced[TypedExpression]
  ): (Seq[ArgumentDefinition], Sourced[TypedExpression]) =
    body.value match {
      case TypedExpression(_, TypedExpression.FunctionLiteral(parameter, innerBody)) =>
        flattenLambda(parameters :+ parameter, innerBody)
      case _ =>
        (parameters, body)
    }
}
