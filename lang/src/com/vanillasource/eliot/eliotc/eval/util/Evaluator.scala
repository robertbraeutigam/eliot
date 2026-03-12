package com.vanillasource.eliot.eliotc.eval.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType, typeFQN}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
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
      expression: Sourced[OperatorResolvedExpression],
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
      expression: OperatorResolvedExpression,
      evaluating: Set[ValueFQN],
      paramContext: Map[String, Value],
      sourced: Sourced[?],
      callSite: Option[Sourced[?]] = None
  ): CompilerIO[ExpressionValue] = expression match {
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
      val vfqn = s.value
      if (vfqn === typeFQN) {
        ConcreteValue(Value.Type).pure[CompilerIO]
      } else if (evaluating.contains(vfqn)) {
        // Don't allow recursions when evaluating, for now
        compilerAbort(s.as("Recursive evaluation detected."))
      } else {
        getFact(NamedEvaluable.Key(vfqn)).flatMap {
          case Some(value) => value.value.pure[CompilerIO]
          case None        =>
            compilerAbort(sourced.as("Could not evaluate expression."), Seq(s"Named value '${vfqn.show}' not found."))
        }
      }
    case OperatorResolvedExpression.FunctionLiteral(paramName, None, _)               =>
      compilerAbort(paramName.as("Lambda parameter type must be explicit when expression is evaluated."))
    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramType), body) =>
      for {
        // TODO: Is it ok to ignore the type stack here?
        evaluatedParamTypeFull <-
          toExpressionValue(paramType.value.signature, evaluating, paramContext, paramType, callSite)
        // TODO: We require a monomorphized type value here. This might require some parameters in some cases!
        evaluatedParamType     <- concreteValueOf(evaluatedParamTypeFull).fold(
                                    compilerAbort(sourced.as("Type expression did not evaluate to a concrete value."))
                                  )(_.pure[CompilerIO])
        newContext              = paramContext + (paramName.value -> evaluatedParamType)
        evaluatedBody          <- toExpressionValue(body.value, evaluating, newContext, body, callSite)
      } yield FunctionLiteral(paramName.value, evaluatedParamType, body.as(evaluatedBody))
    case OperatorResolvedExpression.FunctionApplication(target, argument)             =>
      for {
        targetValue <- toExpressionValue(target.value, evaluating, paramContext, target, callSite)
        argValue    <- toExpressionValue(argument.value, evaluating, paramContext, argument, callSite)
      } yield FunctionApplication(target.as(targetValue), argument.as(argValue))
  }

  def reduce(value: ExpressionValue, sourced: Sourced[?]): CompilerIO[ExpressionValue] = value match {
    case FunctionApplication(target, arg)       =>
      for {
        reducedTarget <- reduce(target.value, target)
        reducedArg    <- reduce(arg.value, arg)
        result        <- reducedTarget match {
                           case FunctionLiteral(paramName, paramType, body) =>
                             checkType(paramType, reducedArg, arg) >>
                               reduce(substitute(body.value, paramName, reducedArg), body)
                           case NativeFunction(paramType, nativeFn)         =>
                             reducedArg match {
                               case ConcreteValue(v) =>
                                 checkType(paramType, reducedArg, arg) >> reduce(nativeFn(v), arg)
                               case _                =>
                                 FunctionApplication(target.as(reducedTarget), arg.as(reducedArg)).pure[CompilerIO]
                             }
                           case _                                           =>
                             FunctionApplication(target.as(reducedTarget), arg.as(reducedArg)).pure[CompilerIO]
                         }
      } yield result
    case FunctionLiteral(name, paramType, body) =>
      reduce(body.value, body).map(reduced => FunctionLiteral(name, paramType, body.as(reduced)))
    case other                                  =>
      other.pure[CompilerIO]
  }

  private def checkType(expectedType: Value, argument: ExpressionValue, sourced: Sourced[?]): CompilerIO[Unit] =
    argumentType(argument) match {
      case Some(actualType) if actualType != expectedType =>
        compilerAbort(
          sourced.as("Type mismatch."),
          Seq(s"Expected: ${expectedType.show}", s"Actual:   ${actualType.show}")
        )
      case _                                              => ().pure[CompilerIO]
    }

  private def argumentType(argument: ExpressionValue): Option[Value] = argument match {
    case ConcreteValue(v)         => Some(v.valueType)
    case ParameterReference(_, t) => Some(t)
    case _                        => None
  }

}
