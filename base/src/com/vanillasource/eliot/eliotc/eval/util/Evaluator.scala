package com.vanillasource.eliot.eliotc.eval.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType, typeFQN}
import com.vanillasource.eliot.eliotc.eval.fact.Value.Type
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.Expression
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
      value   <- toExpressionValue(expression.value, evaluating, Map.empty, expression)
      reduced <- reduce(value, expression)
      result  <- reduced match {
                   case iv: InitialExpressionValue  => iv.pure[CompilerIO]
                   case ParameterReference(name, _) =>
                     compilerAbort(expression.as(s"Unbound parameter reference: $name"))
                   case FunctionApplication(_, _)   =>
                     compilerAbort(expression.as("Could not reduce function application."))
                 }
    } yield result

  private def toExpressionValue(
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
        // Don't allow recursions when evaluating, for now
        compilerAbort(s.as("Recursive evaluation detected."))
      } else {
        getFact(NamedEvaluable.Key(vfqn)).flatMap {
          case Some(value) => value.value.pure[CompilerIO]
          case None        =>
            compilerAbort(sourced.as("Could not evaluate expression."), Seq(s"Named value '${vfqn.show}' not found."))
        }
      }
    case Expression.FunctionLiteral(paramName, paramType, body) =>
      for {
        // TODO: Is it ok to ignore the type stack here?
        evaluatedParamTypeFull <- toExpressionValue(paramType.value.signature, evaluating, paramContext, paramType)
        // TODO: We require a monomorphized type value here. This might require some parameters in some cases!
        evaluatedParamType     <- concreteValueOf(evaluatedParamTypeFull).fold(
                                    compilerAbort(sourced.as("Type expression did not evaluate to a concrete value."))
                                  )(_.pure[CompilerIO])
        newContext              = paramContext + (paramName.value -> evaluatedParamType)
        evaluatedBody          <- toExpressionValue(body.value.signature, evaluating, newContext, body)
      } yield FunctionLiteral(paramName.value, evaluatedParamType, evaluatedBody)
    case Expression.FunctionApplication(target, argument)       =>
      for {
        // TODO: Is it ok to ignore the type stack here?
        targetValue <- toExpressionValue(target.value.signature, evaluating, paramContext, target)
        // TODO: Is it ok to ignore the type stack here?
        argValue    <- toExpressionValue(argument.value.signature, evaluating, paramContext, argument)
      } yield FunctionApplication(targetValue, argValue)
  }

  def reduce(value: ExpressionValue, sourced: Sourced[?]): CompilerIO[ExpressionValue] = value match {
    case FunctionApplication(target, arg)       =>
      for {
        reducedTarget <- reduce(target, sourced)
        reducedArg    <- reduce(arg, sourced)
        result        <- reducedTarget match {
                           case FunctionLiteral(paramName, paramType, body) =>
                             checkType(paramType, reducedArg, sourced) >>
                               reduce(substitute(body, paramName, reducedArg), sourced)
                           case NativeFunction(paramType, nativeFn)         =>
                             reducedArg match {
                               case ConcreteValue(v) =>
                                 checkType(paramType, reducedArg, sourced) >> nativeFn(v).pure[CompilerIO]
                               case _                =>
                                 FunctionApplication(reducedTarget, reducedArg).pure[CompilerIO]
                             }
                           case _                                           =>
                             FunctionApplication(reducedTarget, reducedArg).pure[CompilerIO]
                         }
      } yield result
    case FunctionLiteral(name, paramType, body) =>
      reduce(body, sourced).map(FunctionLiteral(name, paramType, _))
    case other                                  =>
      other.pure[CompilerIO]
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
