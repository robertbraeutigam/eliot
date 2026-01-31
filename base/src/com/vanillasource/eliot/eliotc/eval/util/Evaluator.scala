package com.vanillasource.eliot.eliotc.eval.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.util.Types.{bigIntType, stringType}
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

object Evaluator {

  /** Evaluates an expression to an InitialExpressionValue.
    *
    * @param expression
    *   The sourced expression to evaluate.
    * @param evaluating
    *   Set of ValueFQNs currently being evaluated, used for recursion detection.
    * @return
    *   The evaluated value or abort if evaluation fails.
    */
  def evaluate(
      expression: Sourced[Expression],
      evaluating: Set[ValueFQN] = Set.empty
  ): CompilerIO[InitialExpressionValue] =
    for {
      value   <- evaluateToValue(expression.value, evaluating, Map.empty, expression)
      reduced <- reduce(value, expression)
      result  <- reduced match {
                   case iv: InitialExpressionValue  => iv.pure[CompilerIO]
                   case ParameterReference(name, _) =>
                     compilerAbort(expression.as(s"Unbound parameter reference: $name"))
                   case FunctionApplication(_, _)   =>
                     compilerAbort(expression.as("Could not reduce function application."))
                 }
    } yield result

  private def evaluateToValue(
      expression: Expression,
      evaluating: Set[ValueFQN],
      paramContext: Map[String, Value],
      sourced: Sourced[?]
  ): CompilerIO[ExpressionValue] = expression match {
    case Expression.IntegerLiteral(s)                           =>
      ConcreteValue(Value.Direct(s.value, bigIntType)).pure[CompilerIO]
    case Expression.StringLiteral(s)                            =>
      ConcreteValue(Value.Direct(s.value, stringType)).pure[CompilerIO]
    case Expression.ParameterReference(s)                       =>
      val name = s.value
      paramContext.get(name) match {
        case Some(paramType) => ParameterReference(name, paramType).pure[CompilerIO]
        case None            => compilerAbort(s.as(s"Unknown parameter: $name"))
      }
    case Expression.ValueReference(s)                           =>
      val vfqn = s.value
      if (evaluating.contains(vfqn)) {
        compilerAbort(s.as("Recursive evaluation detected."))
      } else {
        getFactOrAbort(NamedEvaluable.Key(vfqn)).map(_.value)
      }
    case Expression.FunctionLiteral(paramName, paramType, body) =>
      for {
        evaluatedParamType <- evaluateTypeToValue(paramType.value, evaluating, paramContext, paramType)
        newContext          = paramContext + (paramName.value -> evaluatedParamType)
        evaluatedBody      <- body.value.signature.fold(compilerAbort(body.as("Function literal has no body."))) {
                                evaluateToValue(_, evaluating, newContext, body)
                              }
      } yield FunctionLiteral(paramName.value, evaluatedParamType, evaluatedBody)
    case Expression.FunctionApplication(target, argument)       =>
      for {
        targetValue <-
          target.value.signature.fold(compilerAbort(target.as("Function application has no target."))) {
            evaluateToValue(_, evaluating, paramContext, target)
          }
        argValue    <-
          argument.value.signature.fold(compilerAbort(argument.as("Function application has no argument."))) {
            evaluateToValue(_, evaluating, paramContext, argument)
          }
      } yield FunctionApplication(targetValue, argValue)
  }

  private def evaluateTypeToValue(
      typeStack: TypeStack[Expression],
      evaluating: Set[ValueFQN],
      paramContext: Map[String, Value],
      sourced: Sourced[?]
  ): CompilerIO[Value] =
    typeStack.signature.fold(compilerAbort(sourced.as("Type expression has no signature."))) { typeExpr =>
      evaluateToValue(typeExpr, evaluating, paramContext, sourced).flatMap {
        case ConcreteValue(v) => v.pure[CompilerIO]
        case _                => compilerAbort(sourced.as("Type expression did not evaluate to a concrete value."))
      }
    }

  private def reduce(value: ExpressionValue, sourced: Sourced[?]): CompilerIO[ExpressionValue] = value match {
    case FunctionApplication(target, arg)       =>
      for {
        reducedTarget <- reduce(target, sourced)
        reducedArg    <- reduce(arg, sourced)
        result        <- applyOrKeep(reducedTarget, reducedArg, sourced)
      } yield result
    case FunctionLiteral(name, paramType, body) =>
      reduce(body, sourced).map(FunctionLiteral(name, paramType, _))
    case other                                  =>
      other.pure[CompilerIO]
  }

  private def applyOrKeep(
      target: ExpressionValue,
      arg: ExpressionValue,
      sourced: Sourced[?]
  ): CompilerIO[ExpressionValue] =
    target match {
      case FunctionLiteral(paramName, paramType, body) =>
        checkType(paramType, arg, sourced) >>
          reduce(substitute(body, paramName, arg), sourced)
      case NativeFunction(paramType, nativeFn)         =>
        arg match {
          case ConcreteValue(v) =>
            checkType(paramType, arg, sourced) >> nativeFn(v).pure[CompilerIO]
          case _                =>
            FunctionApplication(target, arg).pure[CompilerIO]
        }
      case _                                           =>
        FunctionApplication(target, arg).pure[CompilerIO]
    }

  private def checkType(expectedType: Value, argument: ExpressionValue, sourced: Sourced[?]): CompilerIO[Unit] =
    argumentType(argument) match {
      case Some(actualType) if actualType != expectedType =>
        compilerAbort(sourced.as(s"Type mismatch: expected $expectedType but got $actualType."))
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
      case _                                                                => body
    }
}
