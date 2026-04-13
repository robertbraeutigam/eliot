package com.vanillasource.eliot.eliotc.implementation.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Types, Value}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** Evaluates type expressions to ExpressionValue for structural type matching. Unlike the full Evaluator, this
  * evaluator:
  *   - Preserves unbound parameters as ParameterReference nodes
  *   - Uses structural beta-reduction only (no type checking)
  *   - Reads OperatorResolvedValue directly instead of NamedEvaluable
  */
object TypeExpressionEvaluator {

  /** Evaluate a type expression to an ExpressionValue suitable for structural matching. */
  def evaluate(
      expression: Sourced[OperatorResolvedExpression],
      evaluating: Set[ValueFQN] = Set.empty,
      freeVarNames: Set[String] = Set.empty
  ): CompilerIO[ExpressionValue] =
    toExpressionValue(expression.value, evaluating, freeVarNames, expression)
      .map(ExpressionValue.betaReduce)

  private def toExpressionValue(
      expression: OperatorResolvedExpression,
      evaluating: Set[ValueFQN],
      freeVarNames: Set[String],
      sourced: Sourced[?]
  ): CompilerIO[ExpressionValue] = expression match {
    case OperatorResolvedExpression.IntegerLiteral(s)                                 =>
      ConcreteValue(Value.Direct(s.value, bigIntType)).pure[CompilerIO]
    case OperatorResolvedExpression.StringLiteral(s)                                  =>
      ConcreteValue(Value.Direct(s.value, stringType)).pure[CompilerIO]
    case OperatorResolvedExpression.ParameterReference(s)                             =>
      if (freeVarNames.contains(s.value))
        ParameterReference(s.value).pure[CompilerIO]
      else
        compilerAbort(s.as(s"Unknown parameter: ${s.value}"))
    case OperatorResolvedExpression.ValueReference(s, _)                              =>
      evaluateValue(s.value, sourced, evaluating, freeVarNames)
    case OperatorResolvedExpression.FunctionLiteral(paramName, None, _)               =>
      compilerAbort(paramName.as("Lambda parameter type must be explicit when expression is evaluated."))
    case OperatorResolvedExpression.FunctionLiteral(paramName, Some(paramType), body) =>
      for {
        evaluatedParamType <- toExpressionValue(paramType.value.signature, evaluating, freeVarNames, paramType)
        evaluatedBody      <- toExpressionValue(body.value, evaluating, freeVarNames + paramName.value, body)
      } yield FunctionLiteral(paramName.value, Value.Type, body.as(evaluatedBody))
    case OperatorResolvedExpression.FunctionApplication(target, argument)             =>
      for {
        targetValue <- toExpressionValue(target.value, evaluating, freeVarNames, target)
        argValue    <- toExpressionValue(argument.value, evaluating, freeVarNames, argument)
      } yield FunctionApplication(target.as(targetValue), argument.as(argValue))
  }

  private def evaluateValue(
      rawVfqn: ValueFQN,
      sourced: Sourced[?],
      evaluating: Set[ValueFQN],
      freeVarNames: Set[String]
  ): CompilerIO[ExpressionValue] =
    if (evaluating.contains(rawVfqn)) {
      compilerAbort(sourced.as("Recursive evaluation detected."))
    } else if (rawVfqn === Types.typeFQN) {
      ConcreteValue(Value.Type).pure[CompilerIO]
    } else {
      getFact(OperatorResolvedValue.Key(rawVfqn)).flatMap {
        case Some(fact) =>
          fact.runtime match {
            case Some(body) =>
              evaluate(body, evaluating + rawVfqn, freeVarNames)
            case None       =>
              rawVfqn.name.qualifier match {
                case _: Qualifier.Ability | _: Qualifier.AbilityImplementation =>
                  ParameterReference(rawVfqn.show + "$").pure[CompilerIO]
                case _                                                         =>
                  ConcreteValue(Types.dataType(rawVfqn)).pure[CompilerIO]
              }
          }
        case None       =>
          if (rawVfqn.name.qualifier === Qualifier.Type)
            ConcreteValue(Types.dataType(rawVfqn)).pure[CompilerIO]
          else compilerAbort(sourced.as("Can not evaluate referenced value."))
      }
    }

  private val bigIntType: Value =
    Types.dataType(
      ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "BigInteger"), QualifiedName("BigInteger", Qualifier.Type))
    )
  private val stringType: Value =
    Types.dataType(
      ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "String"), QualifiedName("String", Qualifier.Type))
    )
}
