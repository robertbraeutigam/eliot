package com.vanillasource.eliot.eliotc.uncurry.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.expressionValueUserDisplay
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.symbolic.fact.{TypeCheckedValue, TypedExpression}
import com.vanillasource.eliot.eliotc.uncurry.fact.*

import scala.annotation.tailrec

/** Processor that uncurries type-checked values to a specific arity.
  *
  * Input: TypeCheckedValue (curried form with single-argument applications) Output: UncurriedValue (multi-parameter
  * form with specified arity)
  *
  * The arity is specified in the output key and determines how many parameters to uncurry.
  */
class UncurryingProcessor
    extends TransformationProcessor[TypeCheckedValue.Key, UncurriedValue.Key](key => TypeCheckedValue.Key(key.vfqn))
    with Logging {

  override protected def generateFromKeyAndFact(
      key: UncurriedValue.Key,
      typeCheckedValue: TypeCheckedValue
  ): CompilerIO[UncurriedValue] =
    for {
      (parameterTypes, returnType)    <-
        extractParameters(typeCheckedValue.name, dropLambdas(typeCheckedValue.signature), key.arity)
      (parameterNames, convertedBody) <- typeCheckedValue.runtime match {
                                           case Some(body) =>
                                             convertBody(
                                               body.map(expression => TypedExpression(returnType, expression)),
                                               key.arity
                                             )
                                           case None       =>
                                             // Just pick some random parameter names if there is no body with real names
                                             (
                                               Range
                                                 .inclusive(1, key.arity)
                                                 .map(i => typeCheckedValue.name.as("$p" + i)),
                                               None
                                             ).pure[CompilerIO]
                                         }
      _                               <- debug[CompilerIO](
                                           s"Uncurried '${key.vfqn.show}' (arity ${key.arity}), parameterTypes: ${parameterTypes
                                               .map(expressionValueUserDisplay.show)
                                               .mkString(", ")}, body: ${convertedBody.map(_.value.expression.show).getOrElse("<abstract>")}"
                                         )
    } yield UncurriedValue(
      vfqn = key.vfqn,
      arity = key.arity,
      name = typeCheckedValue.name,
      signature = typeCheckedValue.signature,
      parameters = parameterNames.zip(parameterTypes).map(ParameterDefinition(_, _)),
      returnType = returnType,
      body = convertedBody.map(_.map(_.expression))
    )

  @tailrec
  private def dropLambdas(signature: ExpressionValue): ExpressionValue =
    signature match {
      case ExpressionValue.FunctionLiteral(_, _, body) => dropLambdas(body)
      case _                                           => signature
    }

  /** Extract parameters from a function signature up to the specified arity.
    */
  private def extractParameters(
      name: Sourced[String],
      signature: ExpressionValue,
      arity: Int
  ): CompilerIO[(Seq[ExpressionValue], ExpressionValue)] =
    if (arity === 0) {
      (Seq.empty, signature).pure[CompilerIO]
    } else {
      signature match {
        case ExpressionValue.FunctionType(parameterType, returnType) =>
          extractParameters(name, returnType, arity - 1).map { (restParameters, restReturnType) =>
            (parameterType +: restParameters, restReturnType)
          }
        case _                                                       =>
          compilerAbort(
            name.as("Could not extract parameters."),
            Seq(s"Remaining arity: $arity", s"Signature: ${ExpressionValue.expressionValueUserDisplay.show(signature)}")
          )
      }
    }

  private def convertBody(
      expression: Sourced[TypedExpression],
      arity: Int
  ): CompilerIO[(Seq[Sourced[String]], Option[Sourced[UncurriedExpression]])] =
    for {
      (parameterNames, body) <- stripLambdas(expression.value, arity, expression)
    } yield (parameterNames, Some(expression.as(convertExpression(body))))

  /** Strip lambda expressions from the body up to a maximum count.
    *
    * @return
    *   (extracted parameters, remaining body expression)
    */
  private def stripLambdas(
      expression: TypedExpression,
      arity: Int,
      sourced: Sourced[?]
  ): CompilerIO[(Seq[Sourced[String]], TypedExpression)] =
    if (arity === 0) {
      (Seq.empty, expression).pure[CompilerIO]
    } else {
      expression.expression match {
        case TypedExpression.FunctionLiteral(parameterName, parameterType, body) =>
          for {
            (restParameters, restExpression) <- stripLambdas(body.value, arity - 1, sourced)
          } yield (parameterName +: restParameters, restExpression)
        case _                                                                   => compilerAbort(sourced.as("Could not strip enough parameters from expression."))
      }
    }

  /** Convert a TypedExpression to UncurriedExpression.
    */
  private def convertExpression(expr: TypedExpression): UncurriedExpression =
    expr.expression match {
      case TypedExpression.IntegerLiteral(value)                       =>
        UncurriedExpression(expr.expressionType, UncurriedExpression.IntegerLiteral(value))
      case TypedExpression.StringLiteral(value)                        =>
        UncurriedExpression(expr.expressionType, UncurriedExpression.StringLiteral(value))
      case TypedExpression.ParameterReference(paramName)               =>
        UncurriedExpression(expr.expressionType, UncurriedExpression.ParameterReference(paramName))
      case TypedExpression.ValueReference(vfqn)                        =>
        UncurriedExpression(expr.expressionType, UncurriedExpression.ValueReference(vfqn))
      case TypedExpression.FunctionApplication(target, argument)       =>
        UncurriedExpression(expr.expressionType, convertApplication(target, argument))
      case TypedExpression.FunctionLiteral(paramName, paramType, body) =>
        UncurriedExpression(expr.expressionType, convertLambda(paramName, paramType, body))
    }

  /** Convert a function application, flattening nested applications.
    */
  private def convertApplication(
      target: Sourced[TypedExpression],
      argument: Sourced[TypedExpression]
  ): UncurriedExpression.Expression = {
    val (finalTarget, arguments) = flattenApplication(target, Seq(argument))
    val convertedTarget          = finalTarget.as(convertExpression(finalTarget.value))
    val convertedArgs            = arguments.map(se => se.as(convertExpression(se.value)))
    UncurriedExpression.FunctionApplication(convertedTarget, convertedArgs)
  }

  /** Flatten nested function applications into a single application with multiple arguments.
    */
  @tailrec
  private def flattenApplication(
      target: Sourced[TypedExpression],
      arguments: Seq[Sourced[TypedExpression]]
  ): (Sourced[TypedExpression], Seq[Sourced[TypedExpression]]) =
    target.value.expression match {
      case TypedExpression.FunctionApplication(innerTarget, innerArg) =>
        flattenApplication(innerTarget, innerArg +: arguments)
      case _                                                          =>
        (target, arguments)
    }

  /** Convert a function literal, flattening nested lambdas.
    */
  private def convertLambda(
      paramName: Sourced[String],
      paramType: Sourced[ExpressionValue],
      body: Sourced[TypedExpression]
  ): UncurriedExpression.Expression = {
    val firstParam          = ParameterDefinition(paramName, paramType.value)
    val (params, finalBody) = flattenLambda(Seq(firstParam), body)
    UncurriedExpression.FunctionLiteral(params, body.as(convertExpression(finalBody.value)))
  }

  /** Flatten nested lambda expressions into a single lambda with multiple parameters.
    */
  @tailrec
  private def flattenLambda(
      parameters: Seq[ParameterDefinition],
      body: Sourced[TypedExpression]
  ): (Seq[ParameterDefinition], Sourced[TypedExpression]) =
    body.value.expression match {
      case TypedExpression.FunctionLiteral(paramName, paramType, innerBody) =>
        flattenLambda(parameters :+ ParameterDefinition(paramName, paramType.value), innerBody)
      case _                                                                =>
        (parameters, body)
    }
}
