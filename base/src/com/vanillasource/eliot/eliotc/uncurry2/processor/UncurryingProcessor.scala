package com.vanillasource.eliot.eliotc.uncurry2.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.monomorphize.fact.{MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.uncurry2.AritySelector
import com.vanillasource.eliot.eliotc.uncurry2.AritySelector.FunctionType
import com.vanillasource.eliot.eliotc.uncurry2.fact.*

import scala.annotation.tailrec

/** Processor that uncurries monomorphic values to their optimal arity based on usage statistics.
  *
  * Input: MonomorphicValue (curried form with single-argument applications) Output: Uncurried2Value (multi-parameter
  * form with optimal arity)
  *
  * Currently defaults to max arity (full uncurrying) since usage statistics are not available at the individual value
  * level.
  */
class UncurryingProcessor extends SingleKeyTypeProcessor[UncurriedValue.Key] with Logging {

  override protected def generateFact(key: UncurriedValue.Key): CompilerIO[Unit] =
    for {
      monomorphicValue             <- getFactOrAbort(MonomorphicValue.Key(key.vfqn, key.typeArguments))
      optimalArity                  = AritySelector.selectOptimalArity(None, monomorphicValue.signature)
      (bodyParams, convertedBody)  <- monomorphicValue.runtime match {
                                        case Some(sourcedExpr) =>
                                          val (params, inner) = stripLambdas(sourcedExpr.value, optimalArity)
                                          convertExpression(inner).map(converted =>
                                            (params, Some(sourcedExpr.as(converted)))
                                          )
                                        case None              =>
                                          (Seq.empty[ParameterDefinition], None).pure[CompilerIO]
                                      }
      (signatureParams, returnType) = extractParameters(monomorphicValue.name, monomorphicValue.signature, optimalArity)
      parameters                    = if (bodyParams.nonEmpty) bodyParams else signatureParams
      result                        = UncurriedValue(
                                        vfqn = key.vfqn,
                                        typeArguments = key.typeArguments,
                                        name = monomorphicValue.name,
                                        signature = monomorphicValue.signature,
                                        parameters = parameters,
                                        returnType = returnType,
                                        body = convertedBody,
                                        targetArity = optimalArity
                                      )
      _                            <- registerFactIfClear(result)
    } yield ()

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
      signature: Value,
      arity: Int
  ): (Seq[ParameterDefinition], Value) = {
    @tailrec
    def loop(
        sig: Value,
        remaining: Int,
        paramIndex: Int,
        acc: Seq[ParameterDefinition]
    ): (Seq[ParameterDefinition], Value) =
      if (remaining <= 0) {
        (acc, sig)
      } else {
        sig match {
          case FunctionType(paramType, returnType) =>
            val paramName = name.as(s"_p$paramIndex")
            val param     = ParameterDefinition(paramName, paramType)
            loop(returnType, remaining - 1, paramIndex + 1, acc :+ param)
          case _                                   =>
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
      expr: MonomorphicExpression.Expression,
      maxLambdas: Int
  ): (Seq[ParameterDefinition], MonomorphicExpression) = {
    @tailrec
    def loop(
        e: MonomorphicExpression.Expression,
        remaining: Int,
        acc: Seq[ParameterDefinition]
    ): (Seq[ParameterDefinition], MonomorphicExpression) =
      if (remaining <= 0) {
        (acc, MonomorphicExpression(Value.Type, e))
      } else {
        e match {
          case MonomorphicExpression.FunctionLiteral(paramName, paramType, body) =>
            val param = ParameterDefinition(paramName, paramType)
            loop(body.value.expression, remaining - 1, acc :+ param)
          case _                                                                 =>
            (acc, MonomorphicExpression(Value.Type, e))
        }
      }

    loop(expr, maxLambdas, Seq.empty)
  }

  /** Convert a MonomorphicExpression to Uncurried2Expression.
    */
  private def convertExpression(expr: MonomorphicExpression): CompilerIO[UncurriedExpression.Expression] =
    expr.expression match {
      case MonomorphicExpression.IntegerLiteral(value)                       =>
        UncurriedExpression.IntegerLiteral(value).pure[CompilerIO]
      case MonomorphicExpression.StringLiteral(value)                        =>
        UncurriedExpression.StringLiteral(value).pure[CompilerIO]
      case MonomorphicExpression.ParameterReference(paramName)               =>
        UncurriedExpression.ParameterReference(paramName).pure[CompilerIO]
      case MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs)   =>
        UncurriedExpression.ValueReference(vfqn, typeArgs).pure[CompilerIO]
      case MonomorphicExpression.FunctionApplication(target, argument)       =>
        convertApplication(target, argument)
      case MonomorphicExpression.FunctionLiteral(paramName, paramType, body) =>
        convertLambda(paramName, paramType, body)
    }

  /** Convert a function application, flattening nested applications.
    */
  private def convertApplication(
      target: Sourced[MonomorphicExpression],
      argument: Sourced[MonomorphicExpression]
  ): CompilerIO[UncurriedExpression.Expression] = {
    val (finalTarget, arguments) = flattenApplication(target, Seq(argument))
    for {
      convertedTarget <- convertSourcedExpression(finalTarget)
      convertedArgs   <- arguments.traverse(convertSourcedExpression)
    } yield UncurriedExpression.FunctionApplication(convertedTarget, convertedArgs)
  }

  /** Flatten nested function applications into a single application with multiple arguments.
    */
  @tailrec
  private def flattenApplication(
      target: Sourced[MonomorphicExpression],
      arguments: Seq[Sourced[MonomorphicExpression]]
  ): (Sourced[MonomorphicExpression], Seq[Sourced[MonomorphicExpression]]) =
    target.value.expression match {
      case MonomorphicExpression.FunctionApplication(innerTarget, innerArg) =>
        flattenApplication(innerTarget, innerArg +: arguments)
      case _                                                                =>
        (target, arguments)
    }

  /** Convert a function literal, flattening nested lambdas.
    */
  private def convertLambda(
      paramName: Sourced[String],
      paramType: Value,
      body: Sourced[MonomorphicExpression]
  ): CompilerIO[UncurriedExpression.Expression] = {
    val firstParam          = ParameterDefinition(paramName, paramType)
    val (params, finalBody) = flattenLambda(Seq(firstParam), body)
    for {
      convertedBody <- convertSourcedExpression(finalBody)
    } yield UncurriedExpression.FunctionLiteral(params, convertedBody)
  }

  /** Flatten nested lambda expressions into a single lambda with multiple parameters.
    */
  @tailrec
  private def flattenLambda(
      parameters: Seq[ParameterDefinition],
      body: Sourced[MonomorphicExpression]
  ): (Seq[ParameterDefinition], Sourced[MonomorphicExpression]) =
    body.value.expression match {
      case MonomorphicExpression.FunctionLiteral(paramName, paramType, innerBody) =>
        flattenLambda(parameters :+ ParameterDefinition(paramName, paramType), innerBody)
      case _                                                                      =>
        (parameters, body)
    }

  private def convertSourcedExpression(
      sourced: Sourced[MonomorphicExpression]
  ): CompilerIO[Sourced[UncurriedExpression]] =
    convertExpression(sourced.value).map(converted =>
      sourced.as(UncurriedExpression(sourced.value.expressionType, converted))
    )
}
