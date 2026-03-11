package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType, typeFQN}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Types, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

object NormalFormEvaluator {

  /** Translates an expression to its structural normal form. Converts the expression tree into an ExpressionValue,
    * inlining and beta-reducing value reference bodies, but not reducing the top-level result.
    */
  def evaluate(
      expression: Sourced[OperatorResolvedExpression],
      evaluating: Set[ValueFQN] = Set.empty,
      paramContext: Map[String, Value] = Map.empty,
      callSite: Option[Sourced[?]] = None
  ): CompilerIO[ExpressionValue] = expression.value match {
    case OperatorResolvedExpression.IntegerLiteral(s)                                 =>
      ConcreteValue(Value.Direct(s.value, bigIntType)).pure[CompilerIO]
    case OperatorResolvedExpression.StringLiteral(s)                                  =>
      ConcreteValue(Value.Direct(s.value, stringType)).pure[CompilerIO]
    case OperatorResolvedExpression.ParameterReference(s)                             =>
      val name = s.value
      paramContext.get(name) match {
        case Some(paramType) => ParameterReference(name, paramType).pure[CompilerIO]
        case None            => compilerAbort(callSite.getOrElse(s).as(s"Unknown parameter: $name"))
      }
    case OperatorResolvedExpression.ValueReference(s, _)                              =>
      evaluateValue(s.value, expression, evaluating)
    case OperatorResolvedExpression.FunctionLiteral(paramName, None, _)               =>
      compilerAbort(paramName.as("Lambda parameter type must be explicit when expression is evaluated."))
    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramType), body) =>
      for {
        evaluatedParamTypeFull <-
          Evaluator.evaluateParamType(paramType.value.signature, evaluating, paramContext, paramType, callSite)
        evaluatedParamType      = concreteValueOf(evaluatedParamTypeFull).getOrElse(Value.Type)
        newContext              = paramContext + (paramName.value -> evaluatedParamType)
        evaluatedBody          <- evaluate(body, evaluating, newContext, callSite)
      } yield FunctionLiteral(paramName.value, evaluatedParamType, body.as(evaluatedBody))
    case OperatorResolvedExpression.FunctionApplication(target, argument)             =>
      for {
        targetValue <- evaluate(target, evaluating, paramContext, callSite)
        argValue    <- evaluate(argument, evaluating, paramContext, callSite)
      } yield FunctionApplication(target.as(targetValue), argument.as(argValue))
  }

  /** Evaluates a value reference by looking up its body and fully evaluating it. Values without a runtime body (data
    * types, built-ins) return a data type marker.
    */
  private def evaluateValue(
      vfqn: ValueFQN,
      sourced: Sourced[?],
      evaluating: Set[ValueFQN]
  ): CompilerIO[ExpressionValue] =
    if (evaluating.contains(vfqn)) {
      compilerAbort(sourced.as("Recursive evaluation detected."))
    } else if (vfqn === typeFQN) {
      ConcreteValue(Types.dataType(vfqn)).pure[CompilerIO]
    } else {
      getFact(OperatorResolvedValue.Key(vfqn)).flatMap {
        case Some(resolved) =>
          resolved.runtime match {
            case Some(body) =>
              val genericParamContext = extractGenericParamContext(resolved.typeStack.value.signature)
              evaluate(body, evaluating + vfqn, genericParamContext, callSite = Some(sourced))
                .map(betaReduce)
            case None       => ConcreteValue(Types.dataType(vfqn)).pure[CompilerIO]
          }
        case None           =>
          ConcreteValue(Types.dataType(vfqn)).pure[CompilerIO]
      }
    }

  /** Beta-reduces an ExpressionValue by substituting FunctionLiteral applications. Does NOT apply NativeFunctions.
    */
  private def betaReduce(value: ExpressionValue): ExpressionValue =
    value match {
      case FunctionApplication(target, arg)       =>
        val reducedTarget = betaReduce(target.value)
        val reducedArg    = betaReduce(arg.value)
        reducedTarget match {
          case FunctionLiteral(paramName, _, body) =>
            betaReduce(substitute(body.value, paramName, reducedArg))
          case _                                   =>
            FunctionApplication(target.as(reducedTarget), arg.as(reducedArg))
        }
      case FunctionLiteral(name, paramType, body) =>
        FunctionLiteral(name, paramType, body.as(betaReduce(body.value)))
      case other                                  =>
        other
    }

  /** Extracts generic type parameters from leading FunctionLiterals of a type stack signature. */
  private def extractGenericParamContext(expr: OperatorResolvedExpression): Map[String, Value] =
    expr match {
      case OperatorResolvedExpression.FunctionLiteral(paramName, Some(_), body) =>
        Map(paramName.value -> Value.Type) ++ extractGenericParamContext(body.value)
      case _                                                                    => Map.empty
    }

}
