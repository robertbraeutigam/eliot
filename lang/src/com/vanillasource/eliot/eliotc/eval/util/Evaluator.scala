package com.vanillasource.eliot.eliotc.eval.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.Qualifier as CoreQualifier
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, functionDataTypeFQN, stringType, typeFQN}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, NamedEvaluable, Value}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

object Evaluator extends Logging {

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
      evaluating: Set[ValueFQN] = Set.empty,
      paramContext: Map[String, Value] = Map.empty
  ): CompilerIO[ExpressionValue] =
    for {
      value   <- toExpressionValue(expression.value, evaluating, paramContext, expression)
      reduced <- reduce(value, expression)
      /*
      result  <- reduced match {
                   case FunctionApplication(_, _)        =>
                     warn[CompilerIO](s"Could not reduce function application: ${reduced.show}") >>
                       compilerAbort(expression.as("Could not reduce function application."))
                   case expressionValue: ExpressionValue => expressionValue.pure[CompilerIO]
                 }*/
    } yield reduced

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
      ParameterReference(s.value).pure[CompilerIO]
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
            vfqn.name.qualifier match {
              case _: CoreQualifier.Ability | _: CoreQualifier.AbilityImplementation =>
                ConcreteValue(Value.Type).pure[CompilerIO]
              case _                                                                 =>
                compilerAbort(
                  sourced.as("Could not evaluate expression."),
                  Seq(s"Named value '${vfqn.show}' not found.")
                )
            }
        }
      }
    case OperatorResolvedExpression.FunctionLiteral(paramName, None, _)               =>
      compilerAbort(paramName.as("Lambda parameter type must be explicit when expression is evaluated."))
    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramType), body) =>
      for {
        // TODO: Is it ok to ignore the type stack here?
        evaluatedParamTypeFull <-
          toExpressionValue(paramType.value.signature, evaluating, paramContext, paramType, callSite)
        evaluatedParamType     <- concreteValueOf(evaluatedParamTypeFull).fold(
                                    Value.Type.pure[CompilerIO]
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
                           case _: ConcreteValue                            =>
                             compilerAbort(target.as("Not a function."))
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
    if (expectedType == Value.Type) ().pure[CompilerIO] // Value.Type acts as wildcard for non-concrete param types
    else
      argumentType(argument) match {
        case Some(actualType) if actualType != expectedType =>
          compilerAbort(
            sourced.as("Type mismatch."),
            Seq(s"Expected: ${expectedType.show}", s"Actual:   ${actualType.show}")
          )
        case _                                              => ().pure[CompilerIO]
      }

  private def argumentType(argument: ExpressionValue): Option[Value] = argument match {
    case ConcreteValue(v) => Some(v.valueType)
    case _                => None
  }

  /** Apply concrete type arguments to a type ExpressionValue. For non-generic types, returns the concrete Value
    * directly. For generic types (FunctionLiterals representing type lambdas), builds an application chain and reduces.
    */
  def applyTypeArgs(
      typeExprValue: ExpressionValue,
      typeArgs: Seq[Value],
      source: Sourced[?]
  ): CompilerIO[Value] =
    applyExpressionTypeArgs(typeExprValue, typeArgs.map(ConcreteValue(_)), source)

  /** Apply type arguments as ExpressionValues to a type ExpressionValue. This supports higher-kinded type arguments
    * (type constructors like Function) that evaluate to NativeFunction rather than ConcreteValue.
    */
  def applyExpressionTypeArgs(
      typeExprValue: ExpressionValue,
      typeArgs: Seq[ExpressionValue],
      source: Sourced[?]
  ): CompilerIO[Value] =
    if (typeArgs.isEmpty) {
      typeExprValue match {
        case ConcreteValue(v) => v.pure[CompilerIO]
        case _                =>
          error[CompilerIO](s"Non-generic type signature did not evaluate to concrete value: ${typeExprValue.show}") >>
            compilerAbort(source.as("Non-generic type signature did not evaluate to concrete value."))
      }
    } else {
      val applied = typeArgs.foldLeft[ExpressionValue](typeExprValue) { (fn, arg) =>
        FunctionApplication(
          source.as(fn),
          source.as(arg)
        )
      }
      reduce(applied, source).flatMap {
        case ConcreteValue(v) => v.pure[CompilerIO]
        case other            =>
          error[CompilerIO](
            s"Type signature did not evaluate to concrete value after applying type arguments, result: ${other.show}"
          ) >>
            compilerAbort(
              source.as("Type signature did not evaluate to concrete value after applying type arguments."),
              Seq(s"Result: ${other.show}")
            )
      }
    }

  /** Strip leading FunctionLiterals whose parameter names are NOT in the set of relevant params, then apply the
    * relevant type arguments.
    */
  def applyTypeArgsStripped(
      typeExprValue: ExpressionValue,
      allTypeParams: Seq[(String, Value)],
      typeParamSubst: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Value] = {
    val stripped     = stripConstraintOnlyLambdas(typeExprValue, typeParamSubst.keySet)
    val relevantArgs = allTypeParams.flatMap { (name, _) => typeParamSubst.get(name) }
    applyTypeArgs(stripped, relevantArgs, source)
  }

  /** Build a function type expression Function(paramVar, retVar) using the evaluated Function constructor. The
    * parameter names are unification variables that remain as ParameterReferences in the result.
    */
  def functionType(paramVarName: String, retVarName: String, source: Sourced[?]): CompilerIO[ExpressionValue] = {
    def s[T](value: T): Sourced[T]           = Sourced(source.uri, source.range, value)
    val paramRef: OperatorResolvedExpression = OperatorResolvedExpression.ParameterReference(s(paramVarName))
    val retRef: OperatorResolvedExpression   = OperatorResolvedExpression.ParameterReference(s(retVarName))
    val funcRef: OperatorResolvedExpression  = OperatorResolvedExpression.ValueReference(s(functionDataTypeFQN))
    val inner: OperatorResolvedExpression    = OperatorResolvedExpression.FunctionApplication(s(funcRef), s(paramRef))
    val ore: OperatorResolvedExpression      = OperatorResolvedExpression.FunctionApplication(s(inner), s(retRef))
    val paramContext: Map[String, Value]     = Map(paramVarName -> Value.Type, retVarName -> Value.Type)
    for {
      value   <- toExpressionValue(ore, Set.empty, paramContext, s(ore))
      reduced <- reduce(value, s(ore))
    } yield reduced
  }

  /** Strip leading FunctionLiterals whose parameter names are NOT in the set of relevant params. */
  private def stripConstraintOnlyLambdas(ev: ExpressionValue, relevantParams: Set[String]): ExpressionValue =
    ev match {
      case FunctionLiteral(name, _, body) if !relevantParams.contains(name) =>
        stripConstraintOnlyLambdas(body.value, relevantParams)
      case _                                                                => ev
    }

}
