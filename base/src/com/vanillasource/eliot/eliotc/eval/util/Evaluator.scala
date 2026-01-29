package com.vanillasource.eliot.eliotc.eval.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.util.Types.{bigIntType, dataType, stringType}
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
      ConcreteValue(Value.Direct(sourced.value, bigIntType)).pure[CompilerIO]
    case Expression.StringLiteral(sourced)                      =>
      ConcreteValue(Value.Direct(sourced.value, stringType)).pure[CompilerIO]
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
        evaluatedParamType <- tryEvaluateTypeToValue(paramType.value, evaluating)
        evaluatedBody      <- body.value.runtime.fold(abort)(evaluateInternal(_, evaluating))
      } yield FunctionLiteral(paramName.value, evaluatedParamType, evaluatedBody)
    case Expression.FunctionApplication(target, argument)       =>
      for {
        targetValue <- target.value.runtime.fold(abort)(evaluateInternal(_, evaluating))
        argValue    <- argument.value.runtime.fold(abort)(evaluateInternal(_, evaluating))
        result      <- applyFunction(targetValue, argValue, target, evaluating)
      } yield result
  }

  private def tryEvaluateTypeToValue(
      typeStack: ExpressionStack[Expression],
      evaluating: Set[ValueFQN]
  ): CompilerIO[Value] =
    typeStack.signature.fold(abort) { typeExpr =>
      evaluateInternal(typeExpr, evaluating).flatMap {
        case ConcreteValue(v) => v.pure[CompilerIO]
        case _                => abort
      }
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
    case NativeFunction(_, nativeFn)         =>
      // TODO: check type here
      // TODO: functions are also values!!! Even parameter references!
      // TODO: what about later applications changing this
      argument match {
        case ConcreteValue(v) => nativeFn(v).pure[CompilerIO]
        case _                => compilerAbort(targetSourced.as("Native function requires concrete argument."))
      }
    case ParameterReference(_)               =>
      FunctionApplication(target, argument).pure[CompilerIO]
    case FunctionApplication(_, _)           =>
      FunctionApplication(target, argument).pure[CompilerIO]
    case ConcreteValue(_)                    =>
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
                           case NativeFunction(_, nativeFn)         =>
                             reducedArg match {
                               case ConcreteValue(v) => nativeFn(v).pure[CompilerIO]
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
    case ParameterReference(name) if name == paramName                    => argValue
    case ParameterReference(_)                                            => body
    case FunctionApplication(target, arg)                                 =>
      FunctionApplication(
        substitute(target, paramName, argValue),
        substitute(arg, paramName, argValue)
      )
    case FunctionLiteral(name, paramType, innerBody) if name != paramName =>
      FunctionLiteral(name, paramType, substitute(innerBody, paramName, argValue))
    case _                                                                => body
  }
}
