package com.vanillasource.eliot.eliotc.uncurry.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.expressionValueUserDisplay
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
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
  ): CompilerIO[UncurriedValue] = {
    val arity                         = key.arity
    val (bodyParams, convertedBody)   = typeCheckedValue.runtime match {
      case Some(sourcedExpr) =>
        val (params, inner) = stripLambdas(sourcedExpr.value, arity)
        val converted       = convertExpression(inner)
        (params, Some(sourcedExpr.as(converted)))
      case None              =>
        (Seq.empty[ParameterDefinition], None)
    }
    val (signatureParams, returnType) =
      extractParameters(typeCheckedValue.name, typeCheckedValue.signature, arity)
    val parameters                    = if (bodyParams.nonEmpty) bodyParams else signatureParams

    for {
      _ <- debug[CompilerIO](
             s"Uncurried '${key.vfqn.show}' (arity ${key.arity}), parameters: ${bodyParams
                 .map(p => p.name.value + ": " + expressionValueUserDisplay.show(p.parameterType))
                 .mkString(", ")}, body: ${convertedBody.map(_.value.show).getOrElse("<abstract>")}"
           )
    } yield UncurriedValue(
      vfqn = key.vfqn,
      arity = arity,
      name = typeCheckedValue.name,
      signature = typeCheckedValue.signature,
      parameters = parameters,
      returnType = returnType,
      body = convertedBody
    )
  }

  /** Extract parameters from a function signature up to the specified arity.
    *
    * @param name
    *   The function name (used for creating synthetic parameter names)
    * @param signature
    *   The function type signature
    * @param arity
    *   Number of parameters to extract
    * @return
    *   (extracted parameters, return type after extraction)
    */
  private def extractParameters(
      name: Sourced[String],
      signature: ExpressionValue,
      arity: Int
  ): (Seq[ParameterDefinition], ExpressionValue) = {
    @tailrec
    def loop(
        sig: ExpressionValue,
        remaining: Int,
        paramIndex: Int,
        acc: Seq[ParameterDefinition]
    ): (Seq[ParameterDefinition], ExpressionValue) =
      if (remaining <= 0) {
        (acc, sig)
      } else {
        sig match {
          case ExpressionValue.FunctionType(paramType, returnType) =>
            val paramName = name.as(s"_p$paramIndex")
            val param     = ParameterDefinition(paramName, paramType)
            loop(returnType, remaining - 1, paramIndex + 1, acc :+ param)
          case _                                                   =>
            (acc, sig)
        }
      }

    loop(signature, arity, 0, Seq.empty)
  }

  /** Strip lambda expressions from the body up to a maximum count.
    *
    * @return
    *   (extracted parameters, remaining body expression)
    */
  private def stripLambdas(
      expr: TypedExpression.Expression,
      maxLambdas: Int
  ): (Seq[ParameterDefinition], TypedExpression) = {
    @tailrec
    def loop(
        e: TypedExpression.Expression,
        remaining: Int,
        acc: Seq[ParameterDefinition]
    ): (Seq[ParameterDefinition], TypedExpression) =
      if (remaining <= 0) {
        (acc, TypedExpression(ExpressionValue.ConcreteValue(com.vanillasource.eliot.eliotc.eval.fact.Value.Type), e))
      } else {
        e match {
          case TypedExpression.FunctionLiteral(paramName, paramType, body) =>
            val paramExprValue = typeStackToExpressionValue(paramType.value)
            val param          = ParameterDefinition(paramName, paramExprValue)
            loop(body.value.signature.expression, remaining - 1, acc :+ param)
          case _                                                           =>
            (
              acc,
              TypedExpression(ExpressionValue.ConcreteValue(com.vanillasource.eliot.eliotc.eval.fact.Value.Type), e)
            )
        }
      }

    loop(expr, maxLambdas, Seq.empty)
  }

  /** Convert a TypeStack to an ExpressionValue by taking the type of the signature level.
    */
  private def typeStackToExpressionValue(stack: TypeStack[TypedExpression]): ExpressionValue =
    stack.signature.expressionType

  /** Convert a TypedExpression to UncurriedExpression.
    */
  private def convertExpression(expr: TypedExpression): UncurriedExpression.Expression =
    expr.expression match {
      case TypedExpression.IntegerLiteral(value)                       =>
        UncurriedExpression.IntegerLiteral(value)
      case TypedExpression.StringLiteral(value)                        =>
        UncurriedExpression.StringLiteral(value)
      case TypedExpression.ParameterReference(paramName)               =>
        UncurriedExpression.ParameterReference(paramName)
      case TypedExpression.ValueReference(vfqn)                        =>
        UncurriedExpression.ValueReference(vfqn)
      case TypedExpression.FunctionApplication(target, argument)       =>
        convertApplication(target, argument)
      case TypedExpression.FunctionLiteral(paramName, paramType, body) =>
        convertLambda(paramName, paramType, body)
    }

  /** Convert a function application, flattening nested applications.
    */
  private def convertApplication(
      target: Sourced[TypeStack[TypedExpression]],
      argument: Sourced[TypeStack[TypedExpression]]
  ): UncurriedExpression.Expression = {
    val (finalTarget, arguments) = flattenApplication(target, Seq(argument))
    val convertedTarget          = convertSourcedTypeStack(finalTarget)
    val convertedArgs            = arguments.map(convertSourcedTypeStack)
    UncurriedExpression.FunctionApplication(convertedTarget, convertedArgs)
  }

  /** Flatten nested function applications into a single application with multiple arguments.
    */
  @tailrec
  private def flattenApplication(
      target: Sourced[TypeStack[TypedExpression]],
      arguments: Seq[Sourced[TypeStack[TypedExpression]]]
  ): (Sourced[TypeStack[TypedExpression]], Seq[Sourced[TypeStack[TypedExpression]]]) =
    target.value.signature.expression match {
      case TypedExpression.FunctionApplication(innerTarget, innerArg) =>
        flattenApplication(innerTarget, innerArg +: arguments)
      case _                                                          =>
        (target, arguments)
    }

  /** Convert a function literal, flattening nested lambdas.
    */
  private def convertLambda(
      paramName: Sourced[String],
      paramType: Sourced[TypeStack[TypedExpression]],
      body: Sourced[TypeStack[TypedExpression]]
  ): UncurriedExpression.Expression = {
    val paramExprValue      = typeStackToExpressionValue(paramType.value)
    val firstParam          = ParameterDefinition(paramName, paramExprValue)
    val (params, finalBody) = flattenLambda(Seq(firstParam), body)
    val convertedBody       = convertSourcedTypeStack(finalBody)
    UncurriedExpression.FunctionLiteral(params, convertedBody)
  }

  /** Flatten nested lambda expressions into a single lambda with multiple parameters.
    */
  @tailrec
  private def flattenLambda(
      parameters: Seq[ParameterDefinition],
      body: Sourced[TypeStack[TypedExpression]]
  ): (Seq[ParameterDefinition], Sourced[TypeStack[TypedExpression]]) =
    body.value.signature.expression match {
      case TypedExpression.FunctionLiteral(paramName, paramType, innerBody) =>
        val paramExprValue = typeStackToExpressionValue(paramType.value)
        flattenLambda(parameters :+ ParameterDefinition(paramName, paramExprValue), innerBody)
      case _                                                                =>
        (parameters, body)
    }

  private def convertSourcedTypeStack(
      sourced: Sourced[TypeStack[TypedExpression]]
  ): Sourced[UncurriedExpression] = {
    val typedExpr = sourced.value.signature
    val converted = convertExpression(typedExpr)
    sourced.as(UncurriedExpression(typedExpr.expressionType, converted))
  }
}
