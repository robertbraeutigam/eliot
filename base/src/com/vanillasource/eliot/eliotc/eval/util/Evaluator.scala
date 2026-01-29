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
    case FunctionLiteral(paramName, paramType, body) =>
      checkType(paramType, argument, targetSourced) >>
        reduceExpressionValue(substitute(body, paramName, argument), evaluating)
    case NativeFunction(paramType, nativeFn)         =>
      argument match {
        case ConcreteValue(v) =>
          checkType(paramType, argument, targetSourced) >>
            nativeFn(v).pure[CompilerIO]
        case _                => compilerAbort(targetSourced.as("Native function requires concrete argument."))
      }
    case ParameterReference(_)                       =>
      FunctionApplication(target, argument).pure[CompilerIO]
    case FunctionApplication(_, _)                   =>
      FunctionApplication(target, argument).pure[CompilerIO]
    case ConcreteValue(_)                            =>
      compilerAbort(targetSourced.as("Cannot apply concrete value as a function."))
  }

  private def checkType(
      expectedType: Value,
      argument: ExpressionValue,
      sourced: Sourced[?]
  ): CompilerIO[Unit] =
    getArgumentType(argument) match {
      case Some(actualType) if actualType != expectedType =>
        compilerAbort(sourced.as(s"Type mismatch: expected $expectedType but got $actualType."))
      case _                                              => ().pure[CompilerIO]
    }

  private def getArgumentType(argument: ExpressionValue): Option[Value] = argument match {
    case ConcreteValue(v)       => Some(v.valueType)
    case FunctionLiteral(_, _, _) => None // TODO: compute function type
    case NativeFunction(_, _)     => None // TODO: compute function type
    case ParameterReference(_)    => None // Type unknown at this point
    case FunctionApplication(_, _) => None // Type unknown until reduced
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
                           case FunctionLiteral(paramName, paramType, body) =>
                             checkTypeOrPass(paramType, reducedArg) >>
                               reduceExpressionValue(substitute(body, paramName, reducedArg), evaluating)
                           case NativeFunction(paramType, nativeFn)         =>
                             reducedArg match {
                               case ConcreteValue(v) =>
                                 checkTypeOrPass(paramType, reducedArg) >>
                                   nativeFn(v).pure[CompilerIO]
                               case _                =>
                                 FunctionApplication(reducedTarget, reducedArg).pure[CompilerIO]
                             }
                           case _                                           =>
                             FunctionApplication(reducedTarget, reducedArg).pure[CompilerIO]
                         }
      } yield result

    case FunctionLiteral(name, paramType, body) =>
      reduceExpressionValue(body, evaluating).map(FunctionLiteral(name, paramType, _))

    case other => other.pure[CompilerIO]
  }

  /** Check type but pass silently if argument type is unknown (e.g., parameter reference). */
  private def checkTypeOrPass(expectedType: Value, argument: ExpressionValue): CompilerIO[Unit] =
    getArgumentType(argument) match {
      case Some(actualType) if actualType != expectedType => abort
      case _                                              => ().pure[CompilerIO]
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
