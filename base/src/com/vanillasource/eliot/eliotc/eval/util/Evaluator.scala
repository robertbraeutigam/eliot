package com.vanillasource.eliot.eliotc.eval.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

object Evaluator {

  /** Evaluates an expression to an InitialExpressionValue.
    *
    * @param expression
    *   The expression to evaluate.
    * @param evaluating
    *   Set of ValueFQNs currently being evaluated, used for recursion detection.
    * @return
    *   The evaluated value or abort if evaluation fails.
    */
  def evaluate(
      expression: Expression,
      evaluating: Set[ValueFQN] = Set.empty
  ): CompilerIO[InitialExpressionValue] =
    evaluateInternal(expression, evaluating).flatMap {
      case iv: InitialExpressionValue => iv.pure[CompilerIO]
      case ParameterReference(name)   =>
        abort // Unbound parameter reference at top level
      case FunctionApplication(_, _)  =>
        abort // Could not reduce function application
    }

  private def evaluateInternal(
      expression: Expression,
      evaluating: Set[ValueFQN]
  ): CompilerIO[ExpressionValue] = expression match {
    case Expression.IntegerLiteral(sourced)                     =>
      ConcreteValue(Value.LiteralInteger(sourced.value), Value.Structure(Map.empty)).pure[CompilerIO]
    case Expression.StringLiteral(sourced)                      =>
      ConcreteValue(Value.LiteralString(sourced.value), Value.Structure(Map.empty)).pure[CompilerIO]
    case Expression.ParameterReference(sourced)                 =>
      ParameterReference(sourced.value).pure[CompilerIO]
    case Expression.ValueReference(sourced)                     =>
      val vfqn = sourced.value
      if (evaluating.contains(vfqn)) {
        compilerAbort(sourced.as("Recursive evaluation detected."))
      } else {
        getFactOrAbort(NamedEvaluable.Key(vfqn)).map(_.value)
      }
    case Expression.FunctionLiteral(paramName, paramType, body) =>
      for {
        evaluatedParamType <- evaluateTypeToValue(paramType.value, evaluating)
        evaluatedBody      <- body.value.runtime match {
                                case Some(bodyExpr) => evaluateInternal(bodyExpr, evaluating)
                                case None           => abort
                              }
      } yield FunctionLiteral(paramName.value, evaluatedParamType, evaluatedBody)
    case Expression.FunctionApplication(target, argument)       =>
      for {
        targetValue <- target.value.runtime match {
                         case Some(targetExpr) => evaluateInternal(targetExpr, evaluating)
                         case None             => abort
                       }
        argValue    <- argument.value.runtime match {
                         case Some(argExpr) => evaluateInternal(argExpr, evaluating)
                         case None          => abort
                       }
        result      <- applyFunction(targetValue, argValue, target, evaluating)
      } yield result
  }

  private def evaluateTypeToValue(
      typeStack: com.vanillasource.eliot.eliotc.core.fact.ExpressionStack[Expression],
      evaluating: Set[ValueFQN]
  ): CompilerIO[Value] =
    typeStack.signature match {
      case Some(typeExpr) =>
        evaluateInternal(typeExpr, evaluating).flatMap {
          case ConcreteValue(v, _) => v.pure[CompilerIO]
          case _                   => Value.Structure(Map.empty).pure[CompilerIO]
        }
      case None           => Value.Structure(Map.empty).pure[CompilerIO]
    }

  private def applyFunction(
      target: ExpressionValue,
      argument: ExpressionValue,
      targetSourced: Sourced[?],
      evaluating: Set[ValueFQN]
  ): CompilerIO[ExpressionValue] = target match {
    case FunctionLiteral(paramName, _, body) =>
      val substituted = substitute(body, paramName, argument)
      reduceExpressionValue(substituted, evaluating)

    case NativeFunction(_, _, nativeFn) =>
      argument match {
        case ConcreteValue(v, _) => nativeFn(v).pure[CompilerIO]
        case _                   => compilerAbort(targetSourced.as("Native function requires concrete argument."))
      }

    case ParameterReference(_) =>
      FunctionApplication(target, argument).pure[CompilerIO]

    case FunctionApplication(_, _) =>
      FunctionApplication(target, argument).pure[CompilerIO]

    case ConcreteValue(_, _) =>
      compilerAbort(targetSourced.as("Cannot apply concrete value as a function."))
  }

  private def reduceExpressionValue(
      value: ExpressionValue,
      evaluating: Set[ValueFQN]
  ): CompilerIO[ExpressionValue] = value match {
    case FunctionApplication(target, arg) =>
      for {
        reducedTarget <- reduceExpressionValue(target, evaluating)
        reducedArg    <- reduceExpressionValue(arg, evaluating)
        result        <- reducedTarget match {
                           case FunctionLiteral(paramName, _, body) =>
                             val substituted = substitute(body, paramName, reducedArg)
                             reduceExpressionValue(substituted, evaluating)
                           case NativeFunction(_, _, nativeFn)      =>
                             reducedArg match {
                               case ConcreteValue(v, _) => nativeFn(v).pure[CompilerIO]
                               case _                   => FunctionApplication(reducedTarget, reducedArg).pure[CompilerIO]
                             }
                           case _                                   =>
                             FunctionApplication(reducedTarget, reducedArg).pure[CompilerIO]
                         }
      } yield result

    case FunctionLiteral(name, paramType, body) =>
      reduceExpressionValue(body, evaluating).map(FunctionLiteral(name, paramType, _))

    case other => other.pure[CompilerIO]
  }

  private def substitute(
      body: ExpressionValue,
      paramName: String,
      argValue: ExpressionValue
  ): ExpressionValue = body match {
    case ParameterReference(name) if name == paramName =>
      argValue

    case ParameterReference(_) =>
      body

    case FunctionApplication(target, arg) =>
      FunctionApplication(
        substitute(target, paramName, argValue),
        substitute(arg, paramName, argValue)
      )

    case FunctionLiteral(name, paramType, innerBody) if name != paramName =>
      FunctionLiteral(name, paramType, substitute(innerBody, paramName, argValue))

    case _ => body
  }
}
