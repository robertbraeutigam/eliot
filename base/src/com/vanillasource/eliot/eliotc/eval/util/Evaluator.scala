package com.vanillasource.eliot.eliotc.eval.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.util.Types.{bigIntType, stringType}
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression
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
    for {
      value   <- evaluateToValue(expression, evaluating, Map.empty)
      reduced <- reduce(value)
      result  <- reduced match {
                   case iv: InitialExpressionValue  => iv.pure[CompilerIO]
                   case ParameterReference(_, _)    => abort
                   case FunctionApplication(_, _)   => abort
                 }
    } yield result

  private def evaluateToValue(
      expression: Expression,
      evaluating: Set[ValueFQN],
      paramContext: Map[String, Value]
  ): CompilerIO[ExpressionValue] = expression match {
    case Expression.IntegerLiteral(sourced)                     =>
      ConcreteValue(Value.Direct(sourced.value, bigIntType)).pure[CompilerIO]
    case Expression.StringLiteral(sourced)                      =>
      ConcreteValue(Value.Direct(sourced.value, stringType)).pure[CompilerIO]
    case Expression.ParameterReference(sourced)                 =>
      val name = sourced.value
      paramContext.get(name) match {
        case Some(paramType) => ParameterReference(name, paramType).pure[CompilerIO]
        case None            => compilerAbort(sourced.as(s"Unknown parameter: $name"))
      }
    case Expression.ValueReference(sourced)                     =>
      val vfqn = sourced.value
      if (evaluating.contains(vfqn)) {
        compilerAbort(sourced.as("Recursive evaluation detected."))
      } else {
        getFactOrAbort(NamedEvaluable.Key(vfqn)).map(_.value)
      }
    case Expression.FunctionLiteral(paramName, paramType, body) =>
      for {
        evaluatedParamType <- evaluateTypeToValue(paramType.value, evaluating, paramContext)
        newContext          = paramContext + (paramName.value -> evaluatedParamType)
        evaluatedBody      <- body.value.runtime.fold(abort)(evaluateToValue(_, evaluating, newContext))
      } yield FunctionLiteral(paramName.value, evaluatedParamType, evaluatedBody)
    case Expression.FunctionApplication(target, argument)       =>
      for {
        targetValue <- target.value.runtime.fold(abort)(evaluateToValue(_, evaluating, paramContext))
        argValue    <- argument.value.runtime.fold(abort)(evaluateToValue(_, evaluating, paramContext))
      } yield FunctionApplication(targetValue, argValue)
  }

  private def evaluateTypeToValue(
      typeStack: ExpressionStack[Expression],
      evaluating: Set[ValueFQN],
      paramContext: Map[String, Value]
  ): CompilerIO[Value] =
    typeStack.signature.fold(abort) { typeExpr =>
      evaluateToValue(typeExpr, evaluating, paramContext).flatMap {
        case ConcreteValue(v) => v.pure[CompilerIO]
        case _                => abort
      }
    }

  private def reduce(value: ExpressionValue): CompilerIO[ExpressionValue] = value match {
    case FunctionApplication(target, arg) =>
      for {
        reducedTarget <- reduce(target)
        reducedArg    <- reduce(arg)
        result        <- applyOrKeep(reducedTarget, reducedArg)
      } yield result
    case FunctionLiteral(name, paramType, body) =>
      reduce(body).map(FunctionLiteral(name, paramType, _))
    case other =>
      other.pure[CompilerIO]
  }

  private def applyOrKeep(target: ExpressionValue, arg: ExpressionValue): CompilerIO[ExpressionValue] =
    target match {
      case FunctionLiteral(paramName, paramType, body) =>
        checkType(paramType, arg) >>
          reduce(substitute(body, paramName, arg))
      case NativeFunction(paramType, nativeFn)         =>
        arg match {
          case ConcreteValue(v) =>
            checkType(paramType, arg) >> nativeFn(v).pure[CompilerIO]
          case _                =>
            FunctionApplication(target, arg).pure[CompilerIO]
        }
      case _ =>
        FunctionApplication(target, arg).pure[CompilerIO]
    }

  private def checkType(expectedType: Value, argument: ExpressionValue): CompilerIO[Unit] =
    argumentType(argument) match {
      case Some(actualType) if actualType != expectedType => abort
      case _                                              => ().pure[CompilerIO]
    }

  private def argumentType(argument: ExpressionValue): Option[Value] = argument match {
    case ConcreteValue(v)         => Some(v.valueType)
    case ParameterReference(_, t) => Some(t)
    case _                        => None
  }

  private def substitute(body: ExpressionValue, paramName: String, argValue: ExpressionValue): ExpressionValue =
    body match {
      case ParameterReference(name, _) if name == paramName                 => argValue
      case ParameterReference(_, _)                                         => body
      case FunctionApplication(target, arg)                                 =>
        FunctionApplication(substitute(target, paramName, argValue), substitute(arg, paramName, argValue))
      case FunctionLiteral(name, paramType, innerBody) if name != paramName =>
        FunctionLiteral(name, paramType, substitute(innerBody, paramName, argValue))
      case _ => body
    }
}
